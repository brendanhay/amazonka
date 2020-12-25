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
    peType,
    peImage,
    peComputeType,
    peCertificate,
    peEnvironmentVariables,
    peImagePullCredentialsType,
    pePrivilegedMode,
    peRegistryCredential,
  )
where

import qualified Network.AWS.CodeBuild.Types.ComputeType as Types
import qualified Network.AWS.CodeBuild.Types.EnvironmentType as Types
import qualified Network.AWS.CodeBuild.Types.EnvironmentVariable as Types
import qualified Network.AWS.CodeBuild.Types.ImagePullCredentialsType as Types
import qualified Network.AWS.CodeBuild.Types.NonEmptyString as Types
import qualified Network.AWS.CodeBuild.Types.RegistryCredential as Types
import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the build environment of the build project.
--
-- /See:/ 'mkProjectEnvironment' smart constructor.
data ProjectEnvironment = ProjectEnvironment'
  { -- | The type of build environment to use for related builds.
    --
    --
    --     * The environment type @ARM_CONTAINER@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), EU (Ireland), Asia Pacific (Mumbai), Asia Pacific (Tokyo), Asia Pacific (Sydney), and EU (Frankfurt).
    --
    --
    --     * The environment type @LINUX_CONTAINER@ with compute type @build.general1.2xlarge@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), Canada (Central), EU (Ireland), EU (London), EU (Frankfurt), Asia Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific (Sydney), China (Beijing), and China (Ningxia).
    --
    --
    --     * The environment type @LINUX_GPU_CONTAINER@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), Canada (Central), EU (Ireland), EU (London), EU (Frankfurt), Asia Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific (Sydney) , China (Beijing), and China (Ningxia).
    type' :: Types.EnvironmentType,
    -- | The image tag or image digest that identifies the Docker image to use for this build project. Use the following formats:
    --
    --
    --     * For an image tag: @<registry>/<repository>:<tag>@ . For example, in the Docker repository that CodeBuild uses to manage its Docker images, this would be @aws/codebuild/standard:4.0@ .
    --
    --
    --     * For an image digest: @<registry>/<repository>@<digest>@ . For example, to specify an image with the digest "sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf," use @<registry>/<repository>@sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf@ .
    image :: Types.NonEmptyString,
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
    computeType :: Types.ComputeType,
    -- | The ARN of the Amazon Simple Storage Service (Amazon S3) bucket, path prefix, and object key that contains the PEM-encoded certificate for the build project. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/create-project-cli.html#cli.environment.certificate certificate> in the /AWS CodeBuild User Guide/ .
    certificate :: Core.Maybe Types.String,
    -- | A set of environment variables to make available to builds for this build project.
    environmentVariables :: Core.Maybe [Types.EnvironmentVariable],
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
    imagePullCredentialsType :: Core.Maybe Types.ImagePullCredentialsType,
    -- | Enables running the Docker daemon inside a Docker container. Set to true only if the build project is used to build Docker images. Otherwise, a build that attempts to interact with the Docker daemon fails. The default setting is @false@ .
    --
    -- You can initialize the Docker daemon during the install phase of your build by adding one of the following sets of commands to the install phase of your buildspec file:
    -- If the operating system's base image is Ubuntu Linux:
    -- @- nohup /usr/local/bin/dockerd --host=unix:///var/run/docker.sock --host=tcp://0.0.0.0:2375 --storage-driver=overlay&@
    -- @- timeout 15 sh -c "until docker info; do echo .; sleep 1; done"@
    -- If the operating system's base image is Alpine Linux and the previous command does not work, add the @-t@ argument to @timeout@ :
    -- @- nohup /usr/local/bin/dockerd --host=unix:///var/run/docker.sock --host=tcp://0.0.0.0:2375 --storage-driver=overlay&@
    -- @- timeout -t 15 sh -c "until docker info; do echo .; sleep 1; done"@
    privilegedMode :: Core.Maybe Core.Bool,
    -- | The credentials for access to a private registry.
    registryCredential :: Core.Maybe Types.RegistryCredential
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProjectEnvironment' value with any optional fields omitted.
mkProjectEnvironment ::
  -- | 'type\''
  Types.EnvironmentType ->
  -- | 'image'
  Types.NonEmptyString ->
  -- | 'computeType'
  Types.ComputeType ->
  ProjectEnvironment
mkProjectEnvironment type' image computeType =
  ProjectEnvironment'
    { type',
      image,
      computeType,
      certificate = Core.Nothing,
      environmentVariables = Core.Nothing,
      imagePullCredentialsType = Core.Nothing,
      privilegedMode = Core.Nothing,
      registryCredential = Core.Nothing
    }

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
peType :: Lens.Lens' ProjectEnvironment Types.EnvironmentType
peType = Lens.field @"type'"
{-# DEPRECATED peType "Use generic-lens or generic-optics with 'type'' instead." #-}

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
peImage :: Lens.Lens' ProjectEnvironment Types.NonEmptyString
peImage = Lens.field @"image"
{-# DEPRECATED peImage "Use generic-lens or generic-optics with 'image' instead." #-}

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
peComputeType :: Lens.Lens' ProjectEnvironment Types.ComputeType
peComputeType = Lens.field @"computeType"
{-# DEPRECATED peComputeType "Use generic-lens or generic-optics with 'computeType' instead." #-}

-- | The ARN of the Amazon Simple Storage Service (Amazon S3) bucket, path prefix, and object key that contains the PEM-encoded certificate for the build project. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/create-project-cli.html#cli.environment.certificate certificate> in the /AWS CodeBuild User Guide/ .
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peCertificate :: Lens.Lens' ProjectEnvironment (Core.Maybe Types.String)
peCertificate = Lens.field @"certificate"
{-# DEPRECATED peCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | A set of environment variables to make available to builds for this build project.
--
-- /Note:/ Consider using 'environmentVariables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peEnvironmentVariables :: Lens.Lens' ProjectEnvironment (Core.Maybe [Types.EnvironmentVariable])
peEnvironmentVariables = Lens.field @"environmentVariables"
{-# DEPRECATED peEnvironmentVariables "Use generic-lens or generic-optics with 'environmentVariables' instead." #-}

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
peImagePullCredentialsType :: Lens.Lens' ProjectEnvironment (Core.Maybe Types.ImagePullCredentialsType)
peImagePullCredentialsType = Lens.field @"imagePullCredentialsType"
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
pePrivilegedMode :: Lens.Lens' ProjectEnvironment (Core.Maybe Core.Bool)
pePrivilegedMode = Lens.field @"privilegedMode"
{-# DEPRECATED pePrivilegedMode "Use generic-lens or generic-optics with 'privilegedMode' instead." #-}

-- | The credentials for access to a private registry.
--
-- /Note:/ Consider using 'registryCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peRegistryCredential :: Lens.Lens' ProjectEnvironment (Core.Maybe Types.RegistryCredential)
peRegistryCredential = Lens.field @"registryCredential"
{-# DEPRECATED peRegistryCredential "Use generic-lens or generic-optics with 'registryCredential' instead." #-}

instance Core.FromJSON ProjectEnvironment where
  toJSON ProjectEnvironment {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("type" Core..= type'),
            Core.Just ("image" Core..= image),
            Core.Just ("computeType" Core..= computeType),
            ("certificate" Core..=) Core.<$> certificate,
            ("environmentVariables" Core..=) Core.<$> environmentVariables,
            ("imagePullCredentialsType" Core..=)
              Core.<$> imagePullCredentialsType,
            ("privilegedMode" Core..=) Core.<$> privilegedMode,
            ("registryCredential" Core..=) Core.<$> registryCredential
          ]
      )

instance Core.FromJSON ProjectEnvironment where
  parseJSON =
    Core.withObject "ProjectEnvironment" Core.$
      \x ->
        ProjectEnvironment'
          Core.<$> (x Core..: "type")
          Core.<*> (x Core..: "image")
          Core.<*> (x Core..: "computeType")
          Core.<*> (x Core..:? "certificate")
          Core.<*> (x Core..:? "environmentVariables")
          Core.<*> (x Core..:? "imagePullCredentialsType")
          Core.<*> (x Core..:? "privilegedMode")
          Core.<*> (x Core..:? "registryCredential")
