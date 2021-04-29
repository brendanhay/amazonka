{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectEnvironment where

import Network.AWS.CodeBuild.Types.ComputeType
import Network.AWS.CodeBuild.Types.EnvironmentType
import Network.AWS.CodeBuild.Types.EnvironmentVariable
import Network.AWS.CodeBuild.Types.ImagePullCredentialsType
import Network.AWS.CodeBuild.Types.RegistryCredential
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the build environment of the build project.
--
-- /See:/ 'newProjectEnvironment' smart constructor.
data ProjectEnvironment = ProjectEnvironment'
  { -- | Enables running the Docker daemon inside a Docker container. Set to true
    -- only if the build project is used to build Docker images. Otherwise, a
    -- build that attempts to interact with the Docker daemon fails. The
    -- default setting is @false@.
    --
    -- You can initialize the Docker daemon during the install phase of your
    -- build by adding one of the following sets of commands to the install
    -- phase of your buildspec file:
    --
    -- If the operating system\'s base image is Ubuntu Linux:
    --
    -- @- nohup \/usr\/local\/bin\/dockerd --host=unix:\/\/\/var\/run\/docker.sock --host=tcp:\/\/0.0.0.0:2375 --storage-driver=overlay&@
    --
    -- @- timeout 15 sh -c \"until docker info; do echo .; sleep 1; done\"@
    --
    -- If the operating system\'s base image is Alpine Linux and the previous
    -- command does not work, add the @-t@ argument to @timeout@:
    --
    -- @- nohup \/usr\/local\/bin\/dockerd --host=unix:\/\/\/var\/run\/docker.sock --host=tcp:\/\/0.0.0.0:2375 --storage-driver=overlay&@
    --
    -- @- timeout -t 15 sh -c \"until docker info; do echo .; sleep 1; done\"@
    privilegedMode :: Prelude.Maybe Prelude.Bool,
    -- | The type of credentials AWS CodeBuild uses to pull images in your build.
    -- There are two valid values:
    --
    -- -   @CODEBUILD@ specifies that AWS CodeBuild uses its own credentials.
    --     This requires that you modify your ECR repository policy to trust
    --     AWS CodeBuild\'s service principal.
    --
    -- -   @SERVICE_ROLE@ specifies that AWS CodeBuild uses your build
    --     project\'s service role.
    --
    -- When you use a cross-account or private registry image, you must use
    -- SERVICE_ROLE credentials. When you use an AWS CodeBuild curated image,
    -- you must use CODEBUILD credentials.
    imagePullCredentialsType :: Prelude.Maybe ImagePullCredentialsType,
    -- | The credentials for access to a private registry.
    registryCredential :: Prelude.Maybe RegistryCredential,
    -- | A set of environment variables to make available to builds for this
    -- build project.
    environmentVariables :: Prelude.Maybe [EnvironmentVariable],
    -- | The ARN of the Amazon S3 bucket, path prefix, and object key that
    -- contains the PEM-encoded certificate for the build project. For more
    -- information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/create-project-cli.html#cli.environment.certificate certificate>
    -- in the /AWS CodeBuild User Guide/.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | The type of build environment to use for related builds.
    --
    -- -   The environment type @ARM_CONTAINER@ is available only in regions US
    --     East (N. Virginia), US East (Ohio), US West (Oregon), EU (Ireland),
    --     Asia Pacific (Mumbai), Asia Pacific (Tokyo), Asia Pacific (Sydney),
    --     and EU (Frankfurt).
    --
    -- -   The environment type @LINUX_CONTAINER@ with compute type
    --     @build.general1.2xlarge@ is available only in regions US East (N.
    --     Virginia), US East (Ohio), US West (Oregon), Canada (Central), EU
    --     (Ireland), EU (London), EU (Frankfurt), Asia Pacific (Tokyo), Asia
    --     Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific (Sydney),
    --     China (Beijing), and China (Ningxia).
    --
    -- -   The environment type @LINUX_GPU_CONTAINER@ is available only in
    --     regions US East (N. Virginia), US East (Ohio), US West (Oregon),
    --     Canada (Central), EU (Ireland), EU (London), EU (Frankfurt), Asia
    --     Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore),
    --     Asia Pacific (Sydney) , China (Beijing), and China (Ningxia).
    type' :: EnvironmentType,
    -- | The image tag or image digest that identifies the Docker image to use
    -- for this build project. Use the following formats:
    --
    -- -   For an image tag: @\<registry>\/\<repository>:\<tag>@. For example,
    --     in the Docker repository that CodeBuild uses to manage its Docker
    --     images, this would be @aws\/codebuild\/standard:4.0@.
    --
    -- -   For an image digest: @\<registry>\/\<repository>\@\<digest>@. For
    --     example, to specify an image with the digest
    --     \"sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf,\"
    --     use
    --     @\<registry>\/\<repository>\@sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf@.
    image :: Prelude.Text,
    -- | Information about the compute resources the build project uses.
    -- Available values include:
    --
    -- -   @BUILD_GENERAL1_SMALL@: Use up to 3 GB memory and 2 vCPUs for
    --     builds.
    --
    -- -   @BUILD_GENERAL1_MEDIUM@: Use up to 7 GB memory and 4 vCPUs for
    --     builds.
    --
    -- -   @BUILD_GENERAL1_LARGE@: Use up to 16 GB memory and 8 vCPUs for
    --     builds, depending on your environment type.
    --
    -- -   @BUILD_GENERAL1_2XLARGE@: Use up to 145 GB memory, 72 vCPUs, and 824
    --     GB of SSD storage for builds. This compute type supports Docker
    --     images up to 100 GB uncompressed.
    --
    -- If you use @BUILD_GENERAL1_LARGE@:
    --
    -- -   For environment type @LINUX_CONTAINER@, you can use up to 15 GB
    --     memory and 8 vCPUs for builds.
    --
    -- -   For environment type @LINUX_GPU_CONTAINER@, you can use up to 255 GB
    --     memory, 32 vCPUs, and 4 NVIDIA Tesla V100 GPUs for builds.
    --
    -- -   For environment type @ARM_CONTAINER@, you can use up to 16 GB memory
    --     and 8 vCPUs on ARM-based processors for builds.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build Environment Compute Types>
    -- in the /AWS CodeBuild User Guide./
    computeType :: ComputeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProjectEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privilegedMode', 'projectEnvironment_privilegedMode' - Enables running the Docker daemon inside a Docker container. Set to true
-- only if the build project is used to build Docker images. Otherwise, a
-- build that attempts to interact with the Docker daemon fails. The
-- default setting is @false@.
--
-- You can initialize the Docker daemon during the install phase of your
-- build by adding one of the following sets of commands to the install
-- phase of your buildspec file:
--
-- If the operating system\'s base image is Ubuntu Linux:
--
-- @- nohup \/usr\/local\/bin\/dockerd --host=unix:\/\/\/var\/run\/docker.sock --host=tcp:\/\/0.0.0.0:2375 --storage-driver=overlay&@
--
-- @- timeout 15 sh -c \"until docker info; do echo .; sleep 1; done\"@
--
-- If the operating system\'s base image is Alpine Linux and the previous
-- command does not work, add the @-t@ argument to @timeout@:
--
-- @- nohup \/usr\/local\/bin\/dockerd --host=unix:\/\/\/var\/run\/docker.sock --host=tcp:\/\/0.0.0.0:2375 --storage-driver=overlay&@
--
-- @- timeout -t 15 sh -c \"until docker info; do echo .; sleep 1; done\"@
--
-- 'imagePullCredentialsType', 'projectEnvironment_imagePullCredentialsType' - The type of credentials AWS CodeBuild uses to pull images in your build.
-- There are two valid values:
--
-- -   @CODEBUILD@ specifies that AWS CodeBuild uses its own credentials.
--     This requires that you modify your ECR repository policy to trust
--     AWS CodeBuild\'s service principal.
--
-- -   @SERVICE_ROLE@ specifies that AWS CodeBuild uses your build
--     project\'s service role.
--
-- When you use a cross-account or private registry image, you must use
-- SERVICE_ROLE credentials. When you use an AWS CodeBuild curated image,
-- you must use CODEBUILD credentials.
--
-- 'registryCredential', 'projectEnvironment_registryCredential' - The credentials for access to a private registry.
--
-- 'environmentVariables', 'projectEnvironment_environmentVariables' - A set of environment variables to make available to builds for this
-- build project.
--
-- 'certificate', 'projectEnvironment_certificate' - The ARN of the Amazon S3 bucket, path prefix, and object key that
-- contains the PEM-encoded certificate for the build project. For more
-- information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/create-project-cli.html#cli.environment.certificate certificate>
-- in the /AWS CodeBuild User Guide/.
--
-- 'type'', 'projectEnvironment_type' - The type of build environment to use for related builds.
--
-- -   The environment type @ARM_CONTAINER@ is available only in regions US
--     East (N. Virginia), US East (Ohio), US West (Oregon), EU (Ireland),
--     Asia Pacific (Mumbai), Asia Pacific (Tokyo), Asia Pacific (Sydney),
--     and EU (Frankfurt).
--
-- -   The environment type @LINUX_CONTAINER@ with compute type
--     @build.general1.2xlarge@ is available only in regions US East (N.
--     Virginia), US East (Ohio), US West (Oregon), Canada (Central), EU
--     (Ireland), EU (London), EU (Frankfurt), Asia Pacific (Tokyo), Asia
--     Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific (Sydney),
--     China (Beijing), and China (Ningxia).
--
-- -   The environment type @LINUX_GPU_CONTAINER@ is available only in
--     regions US East (N. Virginia), US East (Ohio), US West (Oregon),
--     Canada (Central), EU (Ireland), EU (London), EU (Frankfurt), Asia
--     Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore),
--     Asia Pacific (Sydney) , China (Beijing), and China (Ningxia).
--
-- 'image', 'projectEnvironment_image' - The image tag or image digest that identifies the Docker image to use
-- for this build project. Use the following formats:
--
-- -   For an image tag: @\<registry>\/\<repository>:\<tag>@. For example,
--     in the Docker repository that CodeBuild uses to manage its Docker
--     images, this would be @aws\/codebuild\/standard:4.0@.
--
-- -   For an image digest: @\<registry>\/\<repository>\@\<digest>@. For
--     example, to specify an image with the digest
--     \"sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf,\"
--     use
--     @\<registry>\/\<repository>\@sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf@.
--
-- 'computeType', 'projectEnvironment_computeType' - Information about the compute resources the build project uses.
-- Available values include:
--
-- -   @BUILD_GENERAL1_SMALL@: Use up to 3 GB memory and 2 vCPUs for
--     builds.
--
-- -   @BUILD_GENERAL1_MEDIUM@: Use up to 7 GB memory and 4 vCPUs for
--     builds.
--
-- -   @BUILD_GENERAL1_LARGE@: Use up to 16 GB memory and 8 vCPUs for
--     builds, depending on your environment type.
--
-- -   @BUILD_GENERAL1_2XLARGE@: Use up to 145 GB memory, 72 vCPUs, and 824
--     GB of SSD storage for builds. This compute type supports Docker
--     images up to 100 GB uncompressed.
--
-- If you use @BUILD_GENERAL1_LARGE@:
--
-- -   For environment type @LINUX_CONTAINER@, you can use up to 15 GB
--     memory and 8 vCPUs for builds.
--
-- -   For environment type @LINUX_GPU_CONTAINER@, you can use up to 255 GB
--     memory, 32 vCPUs, and 4 NVIDIA Tesla V100 GPUs for builds.
--
-- -   For environment type @ARM_CONTAINER@, you can use up to 16 GB memory
--     and 8 vCPUs on ARM-based processors for builds.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build Environment Compute Types>
-- in the /AWS CodeBuild User Guide./
newProjectEnvironment ::
  -- | 'type''
  EnvironmentType ->
  -- | 'image'
  Prelude.Text ->
  -- | 'computeType'
  ComputeType ->
  ProjectEnvironment
newProjectEnvironment pType_ pImage_ pComputeType_ =
  ProjectEnvironment'
    { privilegedMode =
        Prelude.Nothing,
      imagePullCredentialsType = Prelude.Nothing,
      registryCredential = Prelude.Nothing,
      environmentVariables = Prelude.Nothing,
      certificate = Prelude.Nothing,
      type' = pType_,
      image = pImage_,
      computeType = pComputeType_
    }

-- | Enables running the Docker daemon inside a Docker container. Set to true
-- only if the build project is used to build Docker images. Otherwise, a
-- build that attempts to interact with the Docker daemon fails. The
-- default setting is @false@.
--
-- You can initialize the Docker daemon during the install phase of your
-- build by adding one of the following sets of commands to the install
-- phase of your buildspec file:
--
-- If the operating system\'s base image is Ubuntu Linux:
--
-- @- nohup \/usr\/local\/bin\/dockerd --host=unix:\/\/\/var\/run\/docker.sock --host=tcp:\/\/0.0.0.0:2375 --storage-driver=overlay&@
--
-- @- timeout 15 sh -c \"until docker info; do echo .; sleep 1; done\"@
--
-- If the operating system\'s base image is Alpine Linux and the previous
-- command does not work, add the @-t@ argument to @timeout@:
--
-- @- nohup \/usr\/local\/bin\/dockerd --host=unix:\/\/\/var\/run\/docker.sock --host=tcp:\/\/0.0.0.0:2375 --storage-driver=overlay&@
--
-- @- timeout -t 15 sh -c \"until docker info; do echo .; sleep 1; done\"@
projectEnvironment_privilegedMode :: Lens.Lens' ProjectEnvironment (Prelude.Maybe Prelude.Bool)
projectEnvironment_privilegedMode = Lens.lens (\ProjectEnvironment' {privilegedMode} -> privilegedMode) (\s@ProjectEnvironment' {} a -> s {privilegedMode = a} :: ProjectEnvironment)

-- | The type of credentials AWS CodeBuild uses to pull images in your build.
-- There are two valid values:
--
-- -   @CODEBUILD@ specifies that AWS CodeBuild uses its own credentials.
--     This requires that you modify your ECR repository policy to trust
--     AWS CodeBuild\'s service principal.
--
-- -   @SERVICE_ROLE@ specifies that AWS CodeBuild uses your build
--     project\'s service role.
--
-- When you use a cross-account or private registry image, you must use
-- SERVICE_ROLE credentials. When you use an AWS CodeBuild curated image,
-- you must use CODEBUILD credentials.
projectEnvironment_imagePullCredentialsType :: Lens.Lens' ProjectEnvironment (Prelude.Maybe ImagePullCredentialsType)
projectEnvironment_imagePullCredentialsType = Lens.lens (\ProjectEnvironment' {imagePullCredentialsType} -> imagePullCredentialsType) (\s@ProjectEnvironment' {} a -> s {imagePullCredentialsType = a} :: ProjectEnvironment)

-- | The credentials for access to a private registry.
projectEnvironment_registryCredential :: Lens.Lens' ProjectEnvironment (Prelude.Maybe RegistryCredential)
projectEnvironment_registryCredential = Lens.lens (\ProjectEnvironment' {registryCredential} -> registryCredential) (\s@ProjectEnvironment' {} a -> s {registryCredential = a} :: ProjectEnvironment)

-- | A set of environment variables to make available to builds for this
-- build project.
projectEnvironment_environmentVariables :: Lens.Lens' ProjectEnvironment (Prelude.Maybe [EnvironmentVariable])
projectEnvironment_environmentVariables = Lens.lens (\ProjectEnvironment' {environmentVariables} -> environmentVariables) (\s@ProjectEnvironment' {} a -> s {environmentVariables = a} :: ProjectEnvironment) Prelude.. Lens.mapping Prelude._Coerce

-- | The ARN of the Amazon S3 bucket, path prefix, and object key that
-- contains the PEM-encoded certificate for the build project. For more
-- information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/create-project-cli.html#cli.environment.certificate certificate>
-- in the /AWS CodeBuild User Guide/.
projectEnvironment_certificate :: Lens.Lens' ProjectEnvironment (Prelude.Maybe Prelude.Text)
projectEnvironment_certificate = Lens.lens (\ProjectEnvironment' {certificate} -> certificate) (\s@ProjectEnvironment' {} a -> s {certificate = a} :: ProjectEnvironment)

-- | The type of build environment to use for related builds.
--
-- -   The environment type @ARM_CONTAINER@ is available only in regions US
--     East (N. Virginia), US East (Ohio), US West (Oregon), EU (Ireland),
--     Asia Pacific (Mumbai), Asia Pacific (Tokyo), Asia Pacific (Sydney),
--     and EU (Frankfurt).
--
-- -   The environment type @LINUX_CONTAINER@ with compute type
--     @build.general1.2xlarge@ is available only in regions US East (N.
--     Virginia), US East (Ohio), US West (Oregon), Canada (Central), EU
--     (Ireland), EU (London), EU (Frankfurt), Asia Pacific (Tokyo), Asia
--     Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific (Sydney),
--     China (Beijing), and China (Ningxia).
--
-- -   The environment type @LINUX_GPU_CONTAINER@ is available only in
--     regions US East (N. Virginia), US East (Ohio), US West (Oregon),
--     Canada (Central), EU (Ireland), EU (London), EU (Frankfurt), Asia
--     Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore),
--     Asia Pacific (Sydney) , China (Beijing), and China (Ningxia).
projectEnvironment_type :: Lens.Lens' ProjectEnvironment EnvironmentType
projectEnvironment_type = Lens.lens (\ProjectEnvironment' {type'} -> type') (\s@ProjectEnvironment' {} a -> s {type' = a} :: ProjectEnvironment)

-- | The image tag or image digest that identifies the Docker image to use
-- for this build project. Use the following formats:
--
-- -   For an image tag: @\<registry>\/\<repository>:\<tag>@. For example,
--     in the Docker repository that CodeBuild uses to manage its Docker
--     images, this would be @aws\/codebuild\/standard:4.0@.
--
-- -   For an image digest: @\<registry>\/\<repository>\@\<digest>@. For
--     example, to specify an image with the digest
--     \"sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf,\"
--     use
--     @\<registry>\/\<repository>\@sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf@.
projectEnvironment_image :: Lens.Lens' ProjectEnvironment Prelude.Text
projectEnvironment_image = Lens.lens (\ProjectEnvironment' {image} -> image) (\s@ProjectEnvironment' {} a -> s {image = a} :: ProjectEnvironment)

-- | Information about the compute resources the build project uses.
-- Available values include:
--
-- -   @BUILD_GENERAL1_SMALL@: Use up to 3 GB memory and 2 vCPUs for
--     builds.
--
-- -   @BUILD_GENERAL1_MEDIUM@: Use up to 7 GB memory and 4 vCPUs for
--     builds.
--
-- -   @BUILD_GENERAL1_LARGE@: Use up to 16 GB memory and 8 vCPUs for
--     builds, depending on your environment type.
--
-- -   @BUILD_GENERAL1_2XLARGE@: Use up to 145 GB memory, 72 vCPUs, and 824
--     GB of SSD storage for builds. This compute type supports Docker
--     images up to 100 GB uncompressed.
--
-- If you use @BUILD_GENERAL1_LARGE@:
--
-- -   For environment type @LINUX_CONTAINER@, you can use up to 15 GB
--     memory and 8 vCPUs for builds.
--
-- -   For environment type @LINUX_GPU_CONTAINER@, you can use up to 255 GB
--     memory, 32 vCPUs, and 4 NVIDIA Tesla V100 GPUs for builds.
--
-- -   For environment type @ARM_CONTAINER@, you can use up to 16 GB memory
--     and 8 vCPUs on ARM-based processors for builds.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build Environment Compute Types>
-- in the /AWS CodeBuild User Guide./
projectEnvironment_computeType :: Lens.Lens' ProjectEnvironment ComputeType
projectEnvironment_computeType = Lens.lens (\ProjectEnvironment' {computeType} -> computeType) (\s@ProjectEnvironment' {} a -> s {computeType = a} :: ProjectEnvironment)

instance Prelude.FromJSON ProjectEnvironment where
  parseJSON =
    Prelude.withObject
      "ProjectEnvironment"
      ( \x ->
          ProjectEnvironment'
            Prelude.<$> (x Prelude..:? "privilegedMode")
            Prelude.<*> (x Prelude..:? "imagePullCredentialsType")
            Prelude.<*> (x Prelude..:? "registryCredential")
            Prelude.<*> ( x Prelude..:? "environmentVariables"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "certificate")
            Prelude.<*> (x Prelude..: "type")
            Prelude.<*> (x Prelude..: "image")
            Prelude.<*> (x Prelude..: "computeType")
      )

instance Prelude.Hashable ProjectEnvironment

instance Prelude.NFData ProjectEnvironment

instance Prelude.ToJSON ProjectEnvironment where
  toJSON ProjectEnvironment' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("privilegedMode" Prelude..=)
              Prelude.<$> privilegedMode,
            ("imagePullCredentialsType" Prelude..=)
              Prelude.<$> imagePullCredentialsType,
            ("registryCredential" Prelude..=)
              Prelude.<$> registryCredential,
            ("environmentVariables" Prelude..=)
              Prelude.<$> environmentVariables,
            ("certificate" Prelude..=) Prelude.<$> certificate,
            Prelude.Just ("type" Prelude..= type'),
            Prelude.Just ("image" Prelude..= image),
            Prelude.Just ("computeType" Prelude..= computeType)
          ]
      )
