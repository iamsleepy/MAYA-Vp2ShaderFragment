# Vp2ShaderFragment

Using shader fragment as input parameter for stock shader.

Based on vp2blinnshader and fileTexture sample in the devkit.

Still cleaning up the code and writing the blog post.

# Known issues

MTexture::Update could only work with MImage. After discussed with Murray Yang, we don't know the reason for it but he is pretty sure that MImage one is faster than update through memory.

