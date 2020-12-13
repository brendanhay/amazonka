{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectEnvironment
  ( ProjectEnvironment (..),

    -- * Smart constructor
    mkProjectEnvironment,

    -- * Lenses
    peImage,
    peImagePullCredentialsType,
    pePrivilegedMode,
    peRegistryCredential,
    peCertificate,
    peComputeType,
    peEnvironmentVariables,
    peType,
  )
where

import Network.AWS.CodeBuild.Types.ComputeType
import Network.AWS.CodeBuild.Types.EnvironmentType
import Network.AWS.CodeBuild.Types.EnvironmentVariable
import Network.AWS.CodeBuild.Types.ImagePullCredentialsType
import Network.AWS.CodeBuild.Types.RegistryCredential
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the build environment of the build project.
--
-- /See:/ 'mkProjectEnvironment' smart constructor.
data ProjectEnvironment = ProjectEnvironment'
  { -- | The image tag or image digest that identifies the Docker image to use for this build project. Use the following formats:
    --
    --
    --     * For an image tag: @<registry>/<repository>:<tag>@ . For example, in the Docker repository that CodeBuild uses to manage its Docker images, this would be @aws/codebuild/standard:4.0@ .
    --
    --
    --     * For an image digest: @<registry>/<repository>@<digest>@ . For example, to specify an image with the digest "sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf," use @<registry>/<repository>@sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf@ .
    image :: Lude.Text,
    -- | The type of credentials AWS CodeBuild uses to pull images in your build. There are two valid values:
    --
    --
    --     * @CODEBUILD@ specifies that AWS CodeBuild uses its own credentials. This requires that you modify your ECR repository policy to trust AWS CodeBuild's service principal.
    --
    --
    --     * @SERVICE_ROLE@ specifies that AWS CodeBuild uses your build project's service role.
    --
    --
    -- When you use a cross-account or private registry image, you must use SERVICE_ROLE credentials. When you use an AWS CodeBuild curated image, you must use CODEBUILD credentials.
    imagePullCredentialsType :: Lude.Maybe ImagePullCredentialsType,
    -- | Enables running the Docker daemon inside a Docker container. Set to true only if the build project is used to build Docker images. Otherwise, a build that attempts to interact with the Docker daemon fails. The default setting is @false@ .
    --
    -- You can initialize the Docker daemon during the install phase of your build by adding one of the following sets of commands to the install phase of your buildspec file:
    -- If the operating system's base image is Ubuntu Linux:
    -- @- nohup /usr/local/bin/dockerd --host=unix:///var/run/docker.sock --host=tcp://0.0.0.0:2375 --storage-driver=overlay&@
    -- @- timeout 15 sh -c "until docker info; do echo .; sleep 1; done"@
    -- If the operating system's base image is Alpine Linux and the previous command does not work, add the @-t@ argument to @timeout@ :
    -- @- nohup /usr/local/bin/dockerd --host=unix:///var/run/docker.sock --host=tcp://0.0.0.0:2375 --storage-driver=overlay&@
    -- @- timeout -t 15 sh -c "until docker info; do echo .; sleep 1; done"@
    privilegedMode :: Lude.Maybe Lude.Bool,
    -- | The credentials for access to a private registry.
    registryCredential :: Lude.Maybe RegistryCredential,
    -- | The ARN of the Amazon Simple Storage Service (Amazon S3) bucket, path prefix, and object key that contains the PEM-encoded certificate for the build project. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/create-project-cli.html#cli.environment.certificate certificate> in the /AWS CodeBuild User Guide/ .
    certificate :: Lude.Maybe Lude.Text,
    -- | Information about the compute resources the build project uses. Available values include:
    --
    --
    --     * @BUILD_GENERAL1_SMALL@ : Use up to 3 GB memory and 2 vCPUs for builds.
    --
    --
    --     * @BUILD_GENERAL1_MEDIUM@ : Use up to 7 GB memory and 4 vCPUs for builds.
    --
    --
    --     * @BUILD_GENERAL1_LARGE@ : Use up to 16 GB memory and 8 vCPUs for builds, depending on your environment type.
    --
    --
    --     * @BUILD_GENERAL1_2XLARGE@ : Use up to 145 GB memory, 72 vCPUs, and 824 GB of SSD storage for builds. This compute type supports Docker images up to 100 GB uncompressed.
    --
    --
    -- If you use @BUILD_GENERAL1_LARGE@ :
    --
    --     * For environment type @LINUX_CONTAINER@ , you can use up to 15 GB memory and 8 vCPUs for builds.
    --
    --
    --     * For environment type @LINUX_GPU_CONTAINER@ , you can use up to 255 GB memory, 32 vCPUs, and 4 NVIDIA Tesla V100 GPUs for builds.
    --
    --
    --     * For environment type @ARM_CONTAINER@ , you can use up to 16 GB memory and 8 vCPUs on ARM-based processors for builds.
    --
    --
    -- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build Environment Compute Types> in the /AWS CodeBuild User Guide./
    computeType :: ComputeType,
    -- | A set of environment variables to make available to builds for this build project.
    environmentVariables :: Lude.Maybe [EnvironmentVariable],
    -- | The type of build environment to use for related builds.
    --
    --
    --     * The environment type @ARM_CONTAINER@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), EU (Ireland), Asia Pacific (Mumbai), Asia Pacific (Tokyo), Asia Pacific (Sydney), and EU (Frankfurt).
    --
    --
    --     * The environment type @LINUX_CONTAINER@ with compute type @build.general1.2xlarge@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), Canada (Central), EU (Ireland), EU (London), EU (Frankfurt), Asia Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific (Sydney), China (Beijing), and China (Ningxia).
    --
    --
    --     * The environment type @LINUX_GPU_CONTAINER@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), Canada (Central), EU (Ireland), EU (London), EU (Frankfurt), Asia Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific (Sydney) , China (Beijing), and China (Ningxia).
    type' :: EnvironmentType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProjectEnvironment' with the minimum fields required to make a request.
--
-- * 'image' - The image tag or image digest that identifies the Docker image to use for this build project. Use the following formats:
--
--
--     * For an image tag: @<registry>/<repository>:<tag>@ . For example, in the Docker repository that CodeBuild uses to manage its Docker images, this would be @aws/codebuild/standard:4.0@ .
--
--
--     * For an image digest: @<registry>/<repository>@<digest>@ . For example, to specify an image with the digest "sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf," use @<registry>/<repository>@sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf@ .
--
--
-- * 'imagePullCredentialsType' - The type of credentials AWS CodeBuild uses to pull images in your build. There are two valid values:
--
--
--     * @CODEBUILD@ specifies that AWS CodeBuild uses its own credentials. This requires that you modify your ECR repository policy to trust AWS CodeBuild's service principal.
--
--
--     * @SERVICE_ROLE@ specifies that AWS CodeBuild uses your build project's service role.
--
--
-- When you use a cross-account or private registry image, you must use SERVICE_ROLE credentials. When you use an AWS CodeBuild curated image, you must use CODEBUILD credentials.
-- * 'privilegedMode' - Enables running the Docker daemon inside a Docker container. Set to true only if the build project is used to build Docker images. Otherwise, a build that attempts to interact with the Docker daemon fails. The default setting is @false@ .
--
-- You can initialize the Docker daemon during the install phase of your build by adding one of the following sets of commands to the install phase of your buildspec file:
-- If the operating system's base image is Ubuntu Linux:
-- @- nohup /usr/local/bin/dockerd --host=unix:///var/run/docker.sock --host=tcp://0.0.0.0:2375 --storage-driver=overlay&@
-- @- timeout 15 sh -c "until docker info; do echo .; sleep 1; done"@
-- If the operating system's base image is Alpine Linux and the previous command does not work, add the @-t@ argument to @timeout@ :
-- @- nohup /usr/local/bin/dockerd --host=unix:///var/run/docker.sock --host=tcp://0.0.0.0:2375 --storage-driver=overlay&@
-- @- timeout -t 15 sh -c "until docker info; do echo .; sleep 1; done"@
-- * 'registryCredential' - The credentials for access to a private registry.
-- * 'certificate' - The ARN of the Amazon Simple Storage Service (Amazon S3) bucket, path prefix, and object key that contains the PEM-encoded certificate for the build project. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/create-project-cli.html#cli.environment.certificate certificate> in the /AWS CodeBuild User Guide/ .
-- * 'computeType' - Information about the compute resources the build project uses. Available values include:
--
--
--     * @BUILD_GENERAL1_SMALL@ : Use up to 3 GB memory and 2 vCPUs for builds.
--
--
--     * @BUILD_GENERAL1_MEDIUM@ : Use up to 7 GB memory and 4 vCPUs for builds.
--
--
--     * @BUILD_GENERAL1_LARGE@ : Use up to 16 GB memory and 8 vCPUs for builds, depending on your environment type.
--
--
--     * @BUILD_GENERAL1_2XLARGE@ : Use up to 145 GB memory, 72 vCPUs, and 824 GB of SSD storage for builds. This compute type supports Docker images up to 100 GB uncompressed.
--
--
-- If you use @BUILD_GENERAL1_LARGE@ :
--
--     * For environment type @LINUX_CONTAINER@ , you can use up to 15 GB memory and 8 vCPUs for builds.
--
--
--     * For environment type @LINUX_GPU_CONTAINER@ , you can use up to 255 GB memory, 32 vCPUs, and 4 NVIDIA Tesla V100 GPUs for builds.
--
--
--     * For environment type @ARM_CONTAINER@ , you can use up to 16 GB memory and 8 vCPUs on ARM-based processors for builds.
--
--
-- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build Environment Compute Types> in the /AWS CodeBuild User Guide./
-- * 'environmentVariables' - A set of environment variables to make available to builds for this build project.
-- * 'type'' - The type of build environment to use for related builds.
--
--
--     * The environment type @ARM_CONTAINER@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), EU (Ireland), Asia Pacific (Mumbai), Asia Pacific (Tokyo), Asia Pacific (Sydney), and EU (Frankfurt).
--
--
--     * The environment type @LINUX_CONTAINER@ with compute type @build.general1.2xlarge@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), Canada (Central), EU (Ireland), EU (London), EU (Frankfurt), Asia Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific (Sydney), China (Beijing), and China (Ningxia).
--
--
--     * The environment type @LINUX_GPU_CONTAINER@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), Canada (Central), EU (Ireland), EU (London), EU (Frankfurt), Asia Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific (Sydney) , China (Beijing), and China (Ningxia).
mkProjectEnvironment ::
  -- | 'image'
  Lude.Text ->
  -- | 'computeType'
  ComputeType ->
  -- | 'type''
  EnvironmentType ->
  ProjectEnvironment
mkProjectEnvironment pImage_ pComputeType_ pType_ =
  ProjectEnvironment'
    { image = pImage_,
      imagePullCredentialsType = Lude.Nothing,
      privilegedMode = Lude.Nothing,
      registryCredential = Lude.Nothing,
      certificate = Lude.Nothing,
      computeType = pComputeType_,
      environmentVariables = Lude.Nothing,
      type' = pType_
    }

-- | The image tag or image digest that identifies the Docker image to use for this build project. Use the following formats:
--
--
--     * For an image tag: @<registry>/<repository>:<tag>@ . For example, in the Docker repository that CodeBuild uses to manage its Docker images, this would be @aws/codebuild/standard:4.0@ .
--
--
--     * For an image digest: @<registry>/<repository>@<digest>@ . For example, to specify an image with the digest "sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf," use @<registry>/<repository>@sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf@ .
--
--
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peImage :: Lens.Lens' ProjectEnvironment Lude.Text
peImage = Lens.lens (image :: ProjectEnvironment -> Lude.Text) (\s a -> s {image = a} :: ProjectEnvironment)
{-# DEPRECATED peImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The type of credentials AWS CodeBuild uses to pull images in your build. There are two valid values:
--
--
--     * @CODEBUILD@ specifies that AWS CodeBuild uses its own credentials. This requires that you modify your ECR repository policy to trust AWS CodeBuild's service principal.
--
--
--     * @SERVICE_ROLE@ specifies that AWS CodeBuild uses your build project's service role.
--
--
-- When you use a cross-account or private registry image, you must use SERVICE_ROLE credentials. When you use an AWS CodeBuild curated image, you must use CODEBUILD credentials.
--
-- /Note:/ Consider using 'imagePullCredentialsType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peImagePullCredentialsType :: Lens.Lens' ProjectEnvironment (Lude.Maybe ImagePullCredentialsType)
peImagePullCredentialsType = Lens.lens (imagePullCredentialsType :: ProjectEnvironment -> Lude.Maybe ImagePullCredentialsType) (\s a -> s {imagePullCredentialsType = a} :: ProjectEnvironment)
{-# DEPRECATED peImagePullCredentialsType "Use generic-lens or generic-optics with 'imagePullCredentialsType' instead." #-}

-- | Enables running the Docker daemon inside a Docker container. Set to true only if the build project is used to build Docker images. Otherwise, a build that attempts to interact with the Docker daemon fails. The default setting is @false@ .
--
-- You can initialize the Docker daemon during the install phase of your build by adding one of the following sets of commands to the install phase of your buildspec file:
-- If the operating system's base image is Ubuntu Linux:
-- @- nohup /usr/local/bin/dockerd --host=unix:///var/run/docker.sock --host=tcp://0.0.0.0:2375 --storage-driver=overlay&@
-- @- timeout 15 sh -c "until docker info; do echo .; sleep 1; done"@
-- If the operating system's base image is Alpine Linux and the previous command does not work, add the @-t@ argument to @timeout@ :
-- @- nohup /usr/local/bin/dockerd --host=unix:///var/run/docker.sock --host=tcp://0.0.0.0:2375 --storage-driver=overlay&@
-- @- timeout -t 15 sh -c "until docker info; do echo .; sleep 1; done"@
--
-- /Note:/ Consider using 'privilegedMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pePrivilegedMode :: Lens.Lens' ProjectEnvironment (Lude.Maybe Lude.Bool)
pePrivilegedMode = Lens.lens (privilegedMode :: ProjectEnvironment -> Lude.Maybe Lude.Bool) (\s a -> s {privilegedMode = a} :: ProjectEnvironment)
{-# DEPRECATED pePrivilegedMode "Use generic-lens or generic-optics with 'privilegedMode' instead." #-}

-- | The credentials for access to a private registry.
--
-- /Note:/ Consider using 'registryCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peRegistryCredential :: Lens.Lens' ProjectEnvironment (Lude.Maybe RegistryCredential)
peRegistryCredential = Lens.lens (registryCredential :: ProjectEnvironment -> Lude.Maybe RegistryCredential) (\s a -> s {registryCredential = a} :: ProjectEnvironment)
{-# DEPRECATED peRegistryCredential "Use generic-lens or generic-optics with 'registryCredential' instead." #-}

-- | The ARN of the Amazon Simple Storage Service (Amazon S3) bucket, path prefix, and object key that contains the PEM-encoded certificate for the build project. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/create-project-cli.html#cli.environment.certificate certificate> in the /AWS CodeBuild User Guide/ .
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peCertificate :: Lens.Lens' ProjectEnvironment (Lude.Maybe Lude.Text)
peCertificate = Lens.lens (certificate :: ProjectEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {certificate = a} :: ProjectEnvironment)
{-# DEPRECATED peCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | Information about the compute resources the build project uses. Available values include:
--
--
--     * @BUILD_GENERAL1_SMALL@ : Use up to 3 GB memory and 2 vCPUs for builds.
--
--
--     * @BUILD_GENERAL1_MEDIUM@ : Use up to 7 GB memory and 4 vCPUs for builds.
--
--
--     * @BUILD_GENERAL1_LARGE@ : Use up to 16 GB memory and 8 vCPUs for builds, depending on your environment type.
--
--
--     * @BUILD_GENERAL1_2XLARGE@ : Use up to 145 GB memory, 72 vCPUs, and 824 GB of SSD storage for builds. This compute type supports Docker images up to 100 GB uncompressed.
--
--
-- If you use @BUILD_GENERAL1_LARGE@ :
--
--     * For environment type @LINUX_CONTAINER@ , you can use up to 15 GB memory and 8 vCPUs for builds.
--
--
--     * For environment type @LINUX_GPU_CONTAINER@ , you can use up to 255 GB memory, 32 vCPUs, and 4 NVIDIA Tesla V100 GPUs for builds.
--
--
--     * For environment type @ARM_CONTAINER@ , you can use up to 16 GB memory and 8 vCPUs on ARM-based processors for builds.
--
--
-- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build Environment Compute Types> in the /AWS CodeBuild User Guide./
--
-- /Note:/ Consider using 'computeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peComputeType :: Lens.Lens' ProjectEnvironment ComputeType
peComputeType = Lens.lens (computeType :: ProjectEnvironment -> ComputeType) (\s a -> s {computeType = a} :: ProjectEnvironment)
{-# DEPRECATED peComputeType "Use generic-lens or generic-optics with 'computeType' instead." #-}

-- | A set of environment variables to make available to builds for this build project.
--
-- /Note:/ Consider using 'environmentVariables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peEnvironmentVariables :: Lens.Lens' ProjectEnvironment (Lude.Maybe [EnvironmentVariable])
peEnvironmentVariables = Lens.lens (environmentVariables :: ProjectEnvironment -> Lude.Maybe [EnvironmentVariable]) (\s a -> s {environmentVariables = a} :: ProjectEnvironment)
{-# DEPRECATED peEnvironmentVariables "Use generic-lens or generic-optics with 'environmentVariables' instead." #-}

-- | The type of build environment to use for related builds.
--
--
--     * The environment type @ARM_CONTAINER@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), EU (Ireland), Asia Pacific (Mumbai), Asia Pacific (Tokyo), Asia Pacific (Sydney), and EU (Frankfurt).
--
--
--     * The environment type @LINUX_CONTAINER@ with compute type @build.general1.2xlarge@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), Canada (Central), EU (Ireland), EU (London), EU (Frankfurt), Asia Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific (Sydney), China (Beijing), and China (Ningxia).
--
--
--     * The environment type @LINUX_GPU_CONTAINER@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), Canada (Central), EU (Ireland), EU (London), EU (Frankfurt), Asia Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific (Sydney) , China (Beijing), and China (Ningxia).
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peType :: Lens.Lens' ProjectEnvironment EnvironmentType
peType = Lens.lens (type' :: ProjectEnvironment -> EnvironmentType) (\s a -> s {type' = a} :: ProjectEnvironment)
{-# DEPRECATED peType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON ProjectEnvironment where
  parseJSON =
    Lude.withObject
      "ProjectEnvironment"
      ( \x ->
          ProjectEnvironment'
            Lude.<$> (x Lude..: "image")
            Lude.<*> (x Lude..:? "imagePullCredentialsType")
            Lude.<*> (x Lude..:? "privilegedMode")
            Lude.<*> (x Lude..:? "registryCredential")
            Lude.<*> (x Lude..:? "certificate")
            Lude.<*> (x Lude..: "computeType")
            Lude.<*> (x Lude..:? "environmentVariables" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "type")
      )

instance Lude.ToJSON ProjectEnvironment where
  toJSON ProjectEnvironment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("image" Lude..= image),
            ("imagePullCredentialsType" Lude..=)
              Lude.<$> imagePullCredentialsType,
            ("privilegedMode" Lude..=) Lude.<$> privilegedMode,
            ("registryCredential" Lude..=) Lude.<$> registryCredential,
            ("certificate" Lude..=) Lude.<$> certificate,
            Lude.Just ("computeType" Lude..= computeType),
            ("environmentVariables" Lude..=) Lude.<$> environmentVariables,
            Lude.Just ("type" Lude..= type')
          ]
      )
