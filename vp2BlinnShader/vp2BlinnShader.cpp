//-
// ==========================================================================
// Copyright 2015 Autodesk, Inc.  All rights reserved.
//
// Use of this software is subject to the terms of the Autodesk
// license agreement provided at the time of installation or download,
// or which otherwise accompanies this software in either electronic
// or hard copy form.
// ==========================================================================
//+
#include <maya/MIOStream.h>
#include <math.h>
#include <cstdlib>

#include <maya/MGlobal.h>
#include <maya/MString.h>
#include <maya/MPlug.h>
#include <maya/MFnNumericAttribute.h>
#include <maya/MFnTypedAttribute.h>
#include <maya/MFnStringData.h>
#include <maya/MFloatVector.h>
#include <maya/MFnPlugin.h>
#include <maya/MFnDependencyNode.h>


// Include For swatch rendering
#include <maya/MHWShaderSwatchGenerator.h>
#include <maya/MRenderUtilities.h>
#include <maya/MImage.h>

#include <maya/MMatrix.h>

// Viewport 2.0 includes
#include <maya/MDrawRegistry.h>
#include <maya/MPxShaderOverride.h>
#include <maya/MDrawContext.h>
#include <maya/MStateManager.h>
#include <maya/MViewport2Renderer.h>
#include <maya/MShaderManager.h>

// Shader fragment includes 
#include <maya/MFragmentManager.h>
#include <maya/MPxShadingNodeOverride.h>
#include <maya/MRenderUtil.h>





//#define ENABLE_TRACE_API_CALLS
#ifdef ENABLE_TRACE_API_CALLS
#define TRACE_API_CALLS(x) cerr << "vp2BlinnShader: "<<(x)<<"\n"
#else
#define TRACE_API_CALLS(x)
#endif

#include "vp2BlinnShader.h"

MTypeId vp2BlinnShader::id( 0x00081102 );

MObject  vp2BlinnShader::aColor;
MObject  vp2BlinnShader::aTransparency;
MObject  vp2BlinnShader::aSpecularColor;
MObject  vp2BlinnShader::aNonTexturedColor;
MObject  vp2BlinnShader::aFileTexturePath;


///////////////////////////////////////////////////////////////////////////////////////////
// Node methods
///////////////////////////////////////////////////////////////////////////////////////////
void * vp2BlinnShader::creator()
{
	TRACE_API_CALLS("creator");
    return new vp2BlinnShader();
}

vp2BlinnShader::vp2BlinnShader()
{
	TRACE_API_CALLS("vp2BlinnShader");
}

vp2BlinnShader::~vp2BlinnShader()
{
	TRACE_API_CALLS("~vp2BlinnShader");
}

MStatus vp2BlinnShader::initialize()
{
	// Make sure that all attributes are cached internal for
	// optimal performance !

	TRACE_API_CALLS("initialize");
    MFnNumericAttribute nAttr;

    // Create input attributes
    aColor = nAttr.createColor( "color", "c");
    nAttr.setStorable(true);
	nAttr.setKeyable(true);
	nAttr.setDefault(0.0f, 0.0f, 0.0f);
	nAttr.setAffectsAppearance( true );

	
	aTransparency = nAttr.create( "transparency", "tr", MFnNumericData::kFloat );
	nAttr.setStorable(true);
	nAttr.setKeyable(true);
	nAttr.setDefault(0.0f);
	nAttr.setMax(1.0f);
	nAttr.setMin(0.0f);
	nAttr.setAffectsAppearance( true );

    aSpecularColor = nAttr.createColor( "specularColor", "sc" );
	nAttr.setStorable(true);
	nAttr.setKeyable(true);
	nAttr.setDefault(1.0f, 1.0f, 1.0f);
	nAttr.setAffectsAppearance( true );
    
    aNonTexturedColor = nAttr.createColor( "nonTexturedColor", "nc");
    nAttr.setStorable(true);
	nAttr.setKeyable(true);
	nAttr.setDefault(1.0f, 0.0f, 0.0f);
	nAttr.setAffectsAppearance( true );

	MFnStringData stringData;
	MFnTypedAttribute tAttr;
	MObject theString = stringData.create();
	aFileTexturePath = tAttr.create("fileTexturePath", "ft", MFnData::kString, theString);
	tAttr.setKeyable(true);
	tAttr.setStorable(true);
	tAttr.setReadable(true);
	tAttr.setWritable(true);

	// create output attributes here
	// outColor is the only output attribute and it is inherited
	// so we do not need to create or add it.
	//

	// Add the attributes here
    addAttribute(aColor);
	addAttribute(aTransparency);
	addAttribute(aSpecularColor);
    addAttribute(aNonTexturedColor);	
	addAttribute(aFileTexturePath);

    attributeAffects (aColor,			outColor);	
    attributeAffects (aTransparency,	outColor);
	attributeAffects (aSpecularColor,	outColor);
	attributeAffects (aNonTexturedColor,outColor);
	attributeAffects (aFileTexturePath, outColor);
    return MS::kSuccess;
}

// DESCRIPTION:
//
MStatus vp2BlinnShader::compute(
const MPlug&      plug,
      MDataBlock& block )
{
	TRACE_API_CALLS("compute");

    if ((plug != outColor) && (plug.parent() != outColor))
		return MS::kUnknownParameter;

	MFloatVector & color  = block.inputValue( aColor ).asFloatVector();

    // set output color attribute
    MDataHandle outColorHandle = block.outputValue( outColor );
    MFloatVector& outColor = outColorHandle.asFloatVector();
	outColor = color;

    outColorHandle.setClean();
    return MS::kSuccess;
}

////////////////////////////////////////////////////////////////////////////////////
// VP1 interface
////////////////////////////////////////////////////////////////////////////////////
/* virtual */
MStatus
vp2BlinnShader::renderSwatchImage( MImage & outImage )
{	
	if (MHWRender::MRenderer::theRenderer())
	{
			// Use some sample objects for display		    
		    // Valid values include "meshSphere", "meshPlane", "meshShaderball", "meshTeapot", and "meshCloth".
		    // http://help.autodesk.com/view/MAYAUL/2016/ENU/?guid=__cpp_ref_class_m_h_w_render_1_1_m_render_utilities_html
			MString meshSphere("meshSphere");			
			MString meshShaderball("meshShaderball");
			unsigned int targetW, targetH;
			outImage.getSize(targetW, targetH);
	
			return MHWRender::MRenderUtilities::renderMaterialViewerGeometry(targetW > 128 ? meshShaderball : meshSphere,
																			thisMObject(),
																			outImage,
																			MHWRender::MRenderUtilities::kPerspectiveCamera,
																			MHWRender::MRenderUtilities::kSwatchLight);
	}
	return MS::kSuccess;
}

////////////////////////////////////////////////////////////////////////////////////
// Viewport 2.0 implementation for the shader
////////////////////////////////////////////////////////////////////////////////////
class vp2BlinnShaderOverride : public MHWRender::MPxShaderOverride
{
public:
	static MHWRender::MPxShaderOverride* Creator(const MObject& obj)
	{
		return new vp2BlinnShaderOverride(obj);
	}

	virtual ~vp2BlinnShaderOverride()
	{
		MHWRender::MRenderer* theRenderer = MHWRender::MRenderer::theRenderer();
		if (theRenderer)
		{
			const MHWRender::MShaderManager* shaderMgr = theRenderer->getShaderManager();
			if (shaderMgr)
			{
				if (fColorShaderInstance)
				{
					shaderMgr->releaseShader(fColorShaderInstance);
				}
				fColorShaderInstance = NULL;

				if (fNonTexturedColorShaderInstance)
				{
					shaderMgr->releaseShader(fNonTexturedColorShaderInstance);
				}
				fColorShaderInstance = NULL;
			}

			if (fDefaultTexture)
			{
				MHWRender::MTextureManager* textureManager =
					theRenderer->getTextureManager();
				if (textureManager)
				{
					if (fDefaultTexture)
					{
						textureManager->releaseTexture(fDefaultTexture);
					}
					fDefaultTexture = NULL;
				}
				
			}
		}

		
		
	}

	
	// Initialize phase
	virtual MString initialize(const MInitContext& initContext,
									 MInitFeedback& initFeedback)
	{
		TRACE_API_CALLS("vp2BlinnShaderOverride::initialize");

		if (fColorShaderInstance)
		{
			// This plugin is using the utility method
			// MPxShaderOverride::drawGeometry(). For DX11 drawing,
			// a shader signature is required. We use
			// the signature from the same MShaderInstance used to
			// set the geometry requirements so that the signature
			// will match the requirements.
			//
			addShaderSignature( *fColorShaderInstance );
		}

		// Set the geometry requirements for drawing. Only need
		// position and normals.
		MString empty;

		MHWRender::MVertexBufferDescriptor positionDesc(
			empty,
			MHWRender::MGeometry::kPosition,
			MHWRender::MGeometry::kFloat,
			3);

		MHWRender::MVertexBufferDescriptor normalDesc(
			empty,
			MHWRender::MGeometry::kNormal,
			MHWRender::MGeometry::kFloat,
			3);
		MHWRender::MVertexBufferDescriptor textureDesc(
			empty,
			MHWRender::MGeometry::kTexture,
			MHWRender::MGeometry::kFloat,
			2);
		addGeometryRequirement(positionDesc);
		addGeometryRequirement(normalDesc);
		addGeometryRequirement(textureDesc);

        return MString("Autodesk Maya vp2 Blinn Fragment Shader Override");
    }

	// Access the node attributes and cache the values to update
	// during updateDevice()
	//
	virtual void updateDG(MObject object)
	{
		TRACE_API_CALLS("vp2BlinnShaderOverride::updateDG");

		if (object == MObject::kNullObj)
			return;
		TRACE_API_CALLS("Get shader object");
		// Get the hardware shader node from the MObject.
		vp2BlinnShader *shaderNode = (vp2BlinnShader *) MPxHwShaderNode::getHwShaderNodePtr( object );		
		if (!shaderNode)
			return;
		TRACE_API_CALLS("Get dependency node");
		MStatus status;
		MFnDependencyNode node(object, &status);
		if (status)
		{
			node.findPlug("colorR").getValue(fDiffuse[0]);
			node.findPlug("colorG").getValue(fDiffuse[1]);
			node.findPlug("colorB").getValue(fDiffuse[2]);
			node.findPlug("transparency").getValue(fTransparency);
			fDiffuse[3] = 1.0f - fTransparency;	
					
			node.findPlug("specularColorR").getValue(fSpecular[0]);
			node.findPlug("specularColorG").getValue(fSpecular[1]);
			node.findPlug("specularColorB").getValue(fSpecular[2]);

			node.findPlug("nonTexturedColorR").getValue(fNonTextured[0]);
			node.findPlug("nonTexturedColorG").getValue(fNonTextured[1]);
			node.findPlug("nonTexturedColorB").getValue(fNonTextured[2]);

			node.findPlug("fileTexturePath").getValue(fFileTexturePath);
			TRACE_API_CALLS(fFileTexturePath);			
			this->name = node.name();			
		}
	}

	virtual void updateDevice()
	{
		updateShaderInstance();

	}
	virtual void endUpdate()
	{
		TRACE_API_CALLS("vp2BlinnShaderOverride::endUpdate");
	}

	virtual MHWRender::MShaderInstance* shaderInstance() const
	{
		return fColorShaderInstance;
	}

	// Bind the shader on activateKey() and
	// the termination occur in terminateKey().
    virtual void activateKey(MHWRender::MDrawContext& context, const MString& key)
	{
		TRACE_API_CALLS("vp2BlinnShaderOverride::activateKey");
		fColorShaderInstance->bind( context );
	}

	// Unbind / terminate the shader instance here.
    virtual void terminateKey(MHWRender::MDrawContext& context, const MString& key)
	{
		TRACE_API_CALLS("vp2BlinnShaderOverride::terminateKey");
		fColorShaderInstance->unbind( context );
	}
	
	// Use custom shader with custom blend state if required for transparency
	// handling.
	//
	virtual bool draw(MHWRender::MDrawContext& context,
				 const MHWRender::MRenderItemList& renderItemList) const
	{
		TRACE_API_CALLS("vp2BlinnShaderOverride::draw");

		MHWRender::MStateManager* stateMgr = context.getStateManager();

		// initialize vp2BlinnShader blend state once
		if(sBlendState == NULL)
		{
			MHWRender::MBlendStateDesc blendStateDesc;

			for(int i = 0; i < (blendStateDesc.independentBlendEnable ? MHWRender::MBlendState::kMaxTargets : 1); ++i)
			{
				blendStateDesc.targetBlends[i].blendEnable = true;
				blendStateDesc.targetBlends[i].sourceBlend = MHWRender::MBlendState::kSourceAlpha;
				blendStateDesc.targetBlends[i].destinationBlend = MHWRender::MBlendState::kInvSourceAlpha;
				blendStateDesc.targetBlends[i].blendOperation = MHWRender::MBlendState::kAdd;
				blendStateDesc.targetBlends[i].alphaSourceBlend = MHWRender::MBlendState::kOne;
				blendStateDesc.targetBlends[i].alphaDestinationBlend = MHWRender::MBlendState::kInvSourceAlpha;
				blendStateDesc.targetBlends[i].alphaBlendOperation = MHWRender::MBlendState::kAdd;
			}

			blendStateDesc.blendFactor[0] = 1.0f;
			blendStateDesc.blendFactor[1] = 1.0f;
			blendStateDesc.blendFactor[2] = 1.0f;
			blendStateDesc.blendFactor[3] = 1.0f;

			sBlendState = stateMgr->acquireBlendState(blendStateDesc);
		}

		// Save old blend state
		const MHWRender::MBlendState* pOldBlendState = stateMgr->getBlendState();

		bool needBlending = false;
		if (fTransparency > 0.0f)
		{
			needBlending = true;
			stateMgr->setBlendState(sBlendState);
		}

		// Activate all the shader passes and draw using internal draw methods.
		unsigned int passCount = fColorShaderInstance->getPassCount( context );
		for (unsigned int i=0; i<passCount; i++)
		{
			fColorShaderInstance->activatePass( context, i );
			MHWRender::MPxShaderOverride::drawGeometry(context);
		}

		// Restore blend state
		if (needBlending)
		{
			stateMgr->setBlendState(pOldBlendState);
		}
		return true;
	}
	
	// We are using an internal resources so we support all draw APIs
	// automatically.
	virtual MHWRender::DrawAPI supportedDrawAPIs() const
	{
		return (MHWRender::kOpenGL | MHWRender::kDirectX11 | MHWRender::kOpenGLCoreProfile);
	}

	// Transparency indicator
	virtual bool isTransparent()
	{
		TRACE_API_CALLS("vp2BlinnShaderOverride::isTransparent");
		return (fTransparency > 0.0f);
	}

	virtual MHWRender::MShaderInstance* nonTexturedShaderInstance(bool &monitor) const
	{
		if (fNonTexturedColorShaderInstance)
		{
			monitor = true;
			return fNonTexturedColorShaderInstance;
		}
		return NULL;
	}

	virtual bool overridesDrawState()
	{
		return true;
	}	

	// Code to create a cached MShaderInstance using a stock internal Blinn shader
	void createShaderInstance()
	{
		TRACE_API_CALLS("vp2BlinnShaderOverride::createShaderInstance");
		MHWRender::MRenderer *renderer = MHWRender::MRenderer::theRenderer();
		const MHWRender::MShaderManager* shaderMgr = renderer ? renderer->getShaderManager() : NULL;
		if (!shaderMgr)
			return;

		// Create texture fragment shader
		initializeFragmentShaders();		
		createCustomMappings();

		if (!fColorShaderInstance)
		{
			// We only need to add texture for color shader instance used by Texture enabled viewport.
			fColorShaderInstance = shaderMgr->getStockShader( MHWRender::MShaderManager::k3dBlinnShader );			
			fColorShaderInstance->addInputFragment(fFragmentName, MString("output"), MString("diffuseColor"));
		}
		if (!fNonTexturedColorShaderInstance)
		{
			fNonTexturedColorShaderInstance = shaderMgr->getStockShader( MHWRender::MShaderManager::k3dBlinnShader );
			if (fNonTexturedColorShaderInstance)
			{
				fNonTexturedColorShaderInstance->setParameter("diffuseColor", &fNonTextured[0]);
			}
		}
	}

	// Update the shader using the values cached during DG evaluation
	//
	void updateShaderInstance()
	{
		TRACE_API_CALLS("vp2BlinnShaderOverride::updateShaderInstance");
		if (fColorShaderInstance)
		{
			// Update shader to mark it as drawing with transparency or not.
			fColorShaderInstance->setIsTransparent( isTransparent() );				
			fColorShaderInstance->setParameter("specularColor", &fSpecular[0] );

			// Update texture shader
			updateTextureShaderFragment();
		}
		if (fNonTexturedColorShaderInstance)
		{
			fNonTexturedColorShaderInstance->setParameter("diffuseColor", &fNonTextured[0]);
		}
	}

protected:
	vp2BlinnShaderOverride(const MObject& obj)
	: MHWRender::MPxShaderOverride(obj)
	, fColorShaderInstance(NULL)
	, fNonTexturedColorShaderInstance(NULL)
	, fTransparency(0.0f)
	, fSamplerState(NULL)
	, fFragmentName(MString(""))
	, fFileTexturePath(MString(""))
	, fDefaultTexture(NULL)
	{		
		for (int i = 0; i < 4 /* 32 * 32*/; ++i)
		{
			fDiffuse[i] = 0.0f;
		}
		fSpecular[0] = fSpecular[1] = fSpecular[2] = 0.0f;
		fNonTextured[0] = 1.0; fNonTextured[1] = fNonTextured[2] = 0.0f;

		// Create a shader instance to use for drawing
		//
		createShaderInstance();

	}

	// override blend state when there is blending
    static const MHWRender::MBlendState *sBlendState;

	// Shader inputs values
	float fTransparency;
	float fDiffuse[4];
	unsigned char fDiffuseByte[4];
	float fSpecular[3];
	float fShininess[3];
	float fNonTextured[3];

	// Shader to use to draw with
	MHWRender::MShaderInstance *fColorShaderInstance;
	// Shader to use to draw non-textured with
	MHWRender::MShaderInstance *fNonTexturedColorShaderInstance;

	MString fFragmentName;
	MObject fObject;
	MString fFileTexturePath;
	MString name;
	const MHWRender::MSamplerState* fSamplerState;
	MHWRender::MTexture *fDefaultTexture;

	mutable MString fResolvedMapName;
	mutable MString fResolvedSamplerName;
	MHWRender::MAttributeParameterMappingList mappings;

	MHWRender::MShaderInstance *fFragmentTextureShader;

	private:
		//Add texture fragment shader to shader management
		void initializeFragmentShaders()
		{

			static const MString sFragmentName("fileTexturePluginFragment");
			static const char* sFragmentBody =
				"<fragment uiName=\"fileTexturePluginFragment\" name=\"fileTexturePluginFragment\" type=\"plumbing\" class=\"ShadeFragment\" version=\"1.0\">"
				"	<description><![CDATA[Simple file texture fragment]]></description>"
				"	<properties>"
				"		<float2 name=\"uvCoord\" semantic=\"mayaUvCoordSemantic\" flags=\"varyingInputParam\" />"
				"		<texture2 name=\"map\" />"
				"		<sampler name=\"textureSampler\" />"
				"	</properties>"
				"	<values>"
				"	</values>"
				"	<outputs>"
				"		<float4 name=\"output\" />"
				"	</outputs>"
				"	<implementation>"
				"	<implementation render=\"OGSRenderer\" language=\"Cg\" lang_version=\"2.100000\">"
				"		<function_name val=\"fileTexturePluginFragment\" />"
				"		<source><![CDATA["
				"float4 fileTexturePluginFragment(float2 uv, texture2D map, sampler2D mapSampler) \n"
				"{ \n"
				"	uv -= floor(uv); \n"
				"	uv.y = 1.0f - uv.y; \n"
				"	float4 color = tex2D(mapSampler, uv); \n"
				"	return color.rgba; \n"
				"} \n]]>"
				"		</source>"
				"	</implementation>"
				"	<implementation render=\"OGSRenderer\" language=\"HLSL\" lang_version=\"11.000000\">"
				"		<function_name val=\"fileTexturePluginFragment\" />"
				"		<source><![CDATA["
				"float4 fileTexturePluginFragment(float2 uv, Texture2D map, sampler mapSampler) \n"
				"{ \n"
				"	uv -= floor(uv); \n"
				"	uv.y = 1.0f - uv.y; \n"
				"	float4 color = map.Sample(mapSampler, uv); \n"
				"	return color.rgba; \n"
				"} \n]]>"
				"		</source>"
				"	</implementation>"
				"	<implementation render=\"OGSRenderer\" language=\"GLSL\" lang_version=\"3.0\">"
				"		<function_name val=\"fileTexturePluginFragment\" />"
				"		<source><![CDATA["
				"float4 fileTexturePluginFragment(vec2 uv, sampler2D mapSampler) \n"
				"{ \n"
				"	uv -= floor(uv); \n"
				"	uv.y = 1.0f - uv.y; \n"
				"	vec4 color = texture(mapSampler, uv); \n"
				"	return color.rgba; \n"
				"} \n]]>"
				"		</source>"
				"	</implementation>"
				"	</implementation>"
				"</fragment>";
			// Register fragments with the manager if needed
			//
			MHWRender::MRenderer* theRenderer = MHWRender::MRenderer::theRenderer();
			if (theRenderer)
			{
				MHWRender::MFragmentManager* fragmentMgr =
					theRenderer->getFragmentManager();
				if (fragmentMgr)
				{
					// Add fragments if needed
					bool fragAdded = fragmentMgr->hasFragment(sFragmentName);					
					if (!fragAdded)
					{
						fragAdded = (sFragmentName == fragmentMgr->addShadeFragmentFromBuffer(sFragmentBody, false));
					}				
					if (fragAdded)
					{
						fFragmentName = sFragmentName;
						TRACE_API_CALLS("Added Fragment");
					}
				}
			}
		}

		void createCustomMappings()
		{
			// Set up some mappings for the parameters on the file texture fragment,
			// there is no correspondence to attributes on the node for the texture
			// parameters.
			MHWRender::MAttributeParameterMapping mapMapping(
				"map", "", false, true);
			mappings.append(mapMapping);

			MHWRender::MAttributeParameterMapping textureSamplerMapping(
				"textureSampler", "", false, true);
			mappings.append(textureSamplerMapping);
		}

		// Update texture
		void assignTexture(MHWRender::MTexture* texture, MHWRender::MTextureManager *textureManager)
		{
			MHWRender::MTextureAssignment textureAssignment;
			textureAssignment.texture = texture;
			
			TRACE_API_CALLS("Update texture parameter");
			fFragmentTextureShader->setParameter(fResolvedMapName, textureAssignment);			
		}

		// Update Texture fragment with latest parameter. 
		// If there is no file specified, it will generate an 1x1 image based on diffuseColor parameter as default texture.
		// Notice that, use MImage is faster than providing raw memory block.
		void updateTextureShaderFragment()
		{
			TRACE_API_CALLS("Update texture");
			MHWRender::MRenderer *renderer = MHWRender::MRenderer::theRenderer();
			fFragmentTextureShader = fColorShaderInstance;
			if(!fFragmentTextureShader)
			{
				TRACE_API_CALLS("Can't get fragment texture");
				return;
			}

			if (fResolvedMapName.length() == 0)
			{

				const MHWRender::MAttributeParameterMapping* mapping =
					mappings.findByParameterName("map");
				if (mapping)
				{
					TRACE_API_CALLS("Resolved mapping parameter");

					fResolvedMapName = mapping->resolvedParameterName();
				}
			}
			if (fResolvedSamplerName.length() == 0)
			{
				const MHWRender::MAttributeParameterMapping* mapping =
					mappings.findByParameterName("textureSampler");
				if (mapping)
				{
					TRACE_API_CALLS("Resolved texture sampler");
					fResolvedSamplerName = mapping->resolvedParameterName();
				}
			}

			// Set the parameters on the shader
			if (fResolvedMapName.length() > 0 && fResolvedSamplerName.length() > 0)
			{
				// Set sampler to linear-wrap
				if (!fSamplerState)
				{
					MHWRender::MSamplerStateDesc desc;
					desc.filter = MHWRender::MSamplerState::kAnisotropic;
					desc.maxAnisotropy = 16;
					fSamplerState = MHWRender::MStateManager::acquireSamplerState(desc);
				}
				if (fSamplerState)
				{
					TRACE_API_CALLS("Update sampler parameter");
					fFragmentTextureShader->setParameter(fResolvedSamplerName, *fSamplerState);
				}

				// Set texture
				MHWRender::MRenderer* renderer = MHWRender::MRenderer::theRenderer();
				if (renderer) 
				{
					MHWRender::MTextureManager* textureManager =
						renderer->getTextureManager();
					if (textureManager)
					{
						TRACE_API_CALLS("Accquire file texture");
						TRACE_API_CALLS(fFileTexturePath);
						MHWRender::MTexture* texture =
							textureManager->acquireTexture(fFileTexturePath, name);
						if (texture)
						{							
							assignTexture(texture, textureManager);
							
							// release our reference now that it is set on the shader
							textureManager->releaseTexture(texture);
						}
						else
						{
							
							MImage image;							
							for (int i = 0; i < 4; ++i)
							{
								fDiffuseByte[i] = fDiffuse[i] * 255;
							}
							image.setPixels(fDiffuseByte, 1, 1);
														
							if (!fDefaultTexture)
							{
								const MString vp2BlinnShaderDummyTextureName = MString("");
								MHWRender::MTextureDescription desc;
								desc.setToDefault2DTexture();
								desc.fHeight = 1;
								desc.fWidth = 1;
								desc.fFormat = MHWRender::kR8G8B8A8_UNORM;
								fDefaultTexture =
									textureManager->acquireTexture(vp2BlinnShaderDummyTextureName, desc, fDiffuseByte, false);
							}
							else
							{
								MStatus status = fDefaultTexture->update(image, false);
								cerr << status << endl;
							}
							if (fDefaultTexture)
							{
								cerr << fDiffuse[0] << " " << fDiffuse[1] << " " << fDiffuse[2] << " " << fDiffuse[3] << endl;
								// It is cached by OGS, update to make sure texture has been updated.								
								TRACE_API_CALLS("Accquired default texture");
								assignTexture(fDefaultTexture, textureManager);
							}							
						}
					}
				}
			}
		}

		
		
};

const MHWRender::MBlendState* vp2BlinnShaderOverride::sBlendState = NULL;

/////////////////////////////////////////////////////////////////////////////////////////
// Plug-in handling
/////////////////////////////////////////////////////////////////////////////////////////
static const MString svp2BlinnShaderRegistrantId("vp2BlinnShaderRegistrantId");
static const MString smayaVP2BlinnSwatchName = MString("vp2BlinnSwatchRender");
MStatus initializePlugin( MObject obj )
{
	TRACE_API_CALLS("initializePlugin");
	MStatus   status;
	
	MSwatchRenderRegister::registerSwatchRender(smayaVP2BlinnSwatchName, MHWShaderSwatchGenerator::createObj);
	const MString UserClassify( "shader/surface/utility/:drawdb/shader/surface/vp2BlinnShader:swatch/" + smayaVP2BlinnSwatchName );

	MFnPlugin plugin( obj, PLUGIN_COMPANY, "4.5", "Any");
	status = plugin.registerNode( "vp2BlinnShader", vp2BlinnShader::id,
			                      vp2BlinnShader::creator, vp2BlinnShader::initialize,
								  MPxNode::kHwShaderNode, &UserClassify );
	if (!status) {
		status.perror("registerNode");
		return status;
	}

	// Register a shader override for this node
	MHWRender::MDrawRegistry::registerShaderOverrideCreator(
		"drawdb/shader/surface/vp2BlinnShader",
		svp2BlinnShaderRegistrantId,
		vp2BlinnShaderOverride::Creator);
	if (status != MS::kSuccess) return status;

	return MS::kSuccess;
}

MStatus uninitializePlugin( MObject obj )
{
	TRACE_API_CALLS("uninitializePlugin");
	MStatus   status;

	MFnPlugin plugin( obj );

	// Unregister all chamelion shader nodes
	plugin.deregisterNode( vp2BlinnShader::id );
	if (!status) {
		status.perror("deregisterNode");
		return status;
	}

	// Deregister the shader override
	status = MHWRender::MDrawRegistry::deregisterShaderOverrideCreator(
		"drawdb/shader/surface/vp2BlinnShader", svp2BlinnShaderRegistrantId);

	MSwatchRenderRegister::unregisterSwatchRender(smayaVP2BlinnSwatchName);
	if (status != MS::kSuccess) return status;

	return MS::kSuccess;
}

