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
-- Module      : Amazonka.CodeBuild.Types.ProjectEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ProjectEnvironment where

import Amazonka.CodeBuild.Types.ComputeType
import Amazonka.CodeBuild.Types.EnvironmentType
import Amazonka.CodeBuild.Types.EnvironmentVariable
import Amazonka.CodeBuild.Types.ImagePullCredentialsType
import Amazonka.CodeBuild.Types.RegistryCredential
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the build environment of the build project.
--
-- /See:/ 'newProjectEnvironment' smart constructor.
data ProjectEnvironment = ProjectEnvironment'
  { -- | The ARN of the Amazon S3 bucket, path prefix, and object key that
    -- contains the PEM-encoded certificate for the build project. For more
    -- information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/create-project-cli.html#cli.environment.certificate certificate>
    -- in the /CodeBuild User Guide/.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | A set of environment variables to make available to builds for this
    -- build project.
    environmentVariables :: Prelude.Maybe [EnvironmentVariable],
    -- | The type of credentials CodeBuild uses to pull images in your build.
    -- There are two valid values:
    --
    -- -   @CODEBUILD@ specifies that CodeBuild uses its own credentials. This
    --     requires that you modify your ECR repository policy to trust
    --     CodeBuild service principal.
    --
    -- -   @SERVICE_ROLE@ specifies that CodeBuild uses your build project\'s
    --     service role.
    --
    -- When you use a cross-account or private registry image, you must use
    -- SERVICE_ROLE credentials. When you use an CodeBuild curated image, you
    -- must use CODEBUILD credentials.
    imagePullCredentialsType :: Prelude.Maybe ImagePullCredentialsType,
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
    privilegedMode :: Prelude.Maybe Prelude.Bool,
    -- | The credentials for access to a private registry.
    registryCredential :: Prelude.Maybe RegistryCredential,
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
    --
    -- -   The environment types @WINDOWS_CONTAINER@ and
    --     @WINDOWS_SERVER_2019_CONTAINER@ are available only in regions US
    --     East (N. Virginia), US East (Ohio), US West (Oregon), and EU
    --     (Ireland).
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build environment compute types>
    -- in the /CodeBuild user guide/.
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
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-available.html Docker images provided by CodeBuild>
    -- in the /CodeBuild user guide/.
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
    -- in the /CodeBuild User Guide./
    computeType :: ComputeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificate', 'projectEnvironment_certificate' - The ARN of the Amazon S3 bucket, path prefix, and object key that
-- contains the PEM-encoded certificate for the build project. For more
-- information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/create-project-cli.html#cli.environment.certificate certificate>
-- in the /CodeBuild User Guide/.
--
-- 'environmentVariables', 'projectEnvironment_environmentVariables' - A set of environment variables to make available to builds for this
-- build project.
--
-- 'imagePullCredentialsType', 'projectEnvironment_imagePullCredentialsType' - The type of credentials CodeBuild uses to pull images in your build.
-- There are two valid values:
--
-- -   @CODEBUILD@ specifies that CodeBuild uses its own credentials. This
--     requires that you modify your ECR repository policy to trust
--     CodeBuild service principal.
--
-- -   @SERVICE_ROLE@ specifies that CodeBuild uses your build project\'s
--     service role.
--
-- When you use a cross-account or private registry image, you must use
-- SERVICE_ROLE credentials. When you use an CodeBuild curated image, you
-- must use CODEBUILD credentials.
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
-- 'registryCredential', 'projectEnvironment_registryCredential' - The credentials for access to a private registry.
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
-- -   The environment types @WINDOWS_CONTAINER@ and
--     @WINDOWS_SERVER_2019_CONTAINER@ are available only in regions US
--     East (N. Virginia), US East (Ohio), US West (Oregon), and EU
--     (Ireland).
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build environment compute types>
-- in the /CodeBuild user guide/.
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
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-available.html Docker images provided by CodeBuild>
-- in the /CodeBuild user guide/.
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
-- in the /CodeBuild User Guide./
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
    { certificate = Prelude.Nothing,
      environmentVariables = Prelude.Nothing,
      imagePullCredentialsType = Prelude.Nothing,
      privilegedMode = Prelude.Nothing,
      registryCredential = Prelude.Nothing,
      type' = pType_,
      image = pImage_,
      computeType = pComputeType_
    }

-- | The ARN of the Amazon S3 bucket, path prefix, and object key that
-- contains the PEM-encoded certificate for the build project. For more
-- information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/create-project-cli.html#cli.environment.certificate certificate>
-- in the /CodeBuild User Guide/.
projectEnvironment_certificate :: Lens.Lens' ProjectEnvironment (Prelude.Maybe Prelude.Text)
projectEnvironment_certificate = Lens.lens (\ProjectEnvironment' {certificate} -> certificate) (\s@ProjectEnvironment' {} a -> s {certificate = a} :: ProjectEnvironment)

-- | A set of environment variables to make available to builds for this
-- build project.
projectEnvironment_environmentVariables :: Lens.Lens' ProjectEnvironment (Prelude.Maybe [EnvironmentVariable])
projectEnvironment_environmentVariables = Lens.lens (\ProjectEnvironment' {environmentVariables} -> environmentVariables) (\s@ProjectEnvironment' {} a -> s {environmentVariables = a} :: ProjectEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The type of credentials CodeBuild uses to pull images in your build.
-- There are two valid values:
--
-- -   @CODEBUILD@ specifies that CodeBuild uses its own credentials. This
--     requires that you modify your ECR repository policy to trust
--     CodeBuild service principal.
--
-- -   @SERVICE_ROLE@ specifies that CodeBuild uses your build project\'s
--     service role.
--
-- When you use a cross-account or private registry image, you must use
-- SERVICE_ROLE credentials. When you use an CodeBuild curated image, you
-- must use CODEBUILD credentials.
projectEnvironment_imagePullCredentialsType :: Lens.Lens' ProjectEnvironment (Prelude.Maybe ImagePullCredentialsType)
projectEnvironment_imagePullCredentialsType = Lens.lens (\ProjectEnvironment' {imagePullCredentialsType} -> imagePullCredentialsType) (\s@ProjectEnvironment' {} a -> s {imagePullCredentialsType = a} :: ProjectEnvironment)

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

-- | The credentials for access to a private registry.
projectEnvironment_registryCredential :: Lens.Lens' ProjectEnvironment (Prelude.Maybe RegistryCredential)
projectEnvironment_registryCredential = Lens.lens (\ProjectEnvironment' {registryCredential} -> registryCredential) (\s@ProjectEnvironment' {} a -> s {registryCredential = a} :: ProjectEnvironment)

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
--
-- -   The environment types @WINDOWS_CONTAINER@ and
--     @WINDOWS_SERVER_2019_CONTAINER@ are available only in regions US
--     East (N. Virginia), US East (Ohio), US West (Oregon), and EU
--     (Ireland).
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build environment compute types>
-- in the /CodeBuild user guide/.
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
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-available.html Docker images provided by CodeBuild>
-- in the /CodeBuild user guide/.
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
-- in the /CodeBuild User Guide./
projectEnvironment_computeType :: Lens.Lens' ProjectEnvironment ComputeType
projectEnvironment_computeType = Lens.lens (\ProjectEnvironment' {computeType} -> computeType) (\s@ProjectEnvironment' {} a -> s {computeType = a} :: ProjectEnvironment)

instance Data.FromJSON ProjectEnvironment where
  parseJSON =
    Data.withObject
      "ProjectEnvironment"
      ( \x ->
          ProjectEnvironment'
            Prelude.<$> (x Data..:? "certificate")
            Prelude.<*> ( x
                            Data..:? "environmentVariables"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "imagePullCredentialsType")
            Prelude.<*> (x Data..:? "privilegedMode")
            Prelude.<*> (x Data..:? "registryCredential")
            Prelude.<*> (x Data..: "type")
            Prelude.<*> (x Data..: "image")
            Prelude.<*> (x Data..: "computeType")
      )

instance Prelude.Hashable ProjectEnvironment where
  hashWithSalt _salt ProjectEnvironment' {..} =
    _salt
      `Prelude.hashWithSalt` certificate
      `Prelude.hashWithSalt` environmentVariables
      `Prelude.hashWithSalt` imagePullCredentialsType
      `Prelude.hashWithSalt` privilegedMode
      `Prelude.hashWithSalt` registryCredential
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` image
      `Prelude.hashWithSalt` computeType

instance Prelude.NFData ProjectEnvironment where
  rnf ProjectEnvironment' {..} =
    Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf environmentVariables
      `Prelude.seq` Prelude.rnf imagePullCredentialsType
      `Prelude.seq` Prelude.rnf privilegedMode
      `Prelude.seq` Prelude.rnf registryCredential
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf image
      `Prelude.seq` Prelude.rnf computeType

instance Data.ToJSON ProjectEnvironment where
  toJSON ProjectEnvironment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("certificate" Data..=) Prelude.<$> certificate,
            ("environmentVariables" Data..=)
              Prelude.<$> environmentVariables,
            ("imagePullCredentialsType" Data..=)
              Prelude.<$> imagePullCredentialsType,
            ("privilegedMode" Data..=)
              Prelude.<$> privilegedMode,
            ("registryCredential" Data..=)
              Prelude.<$> registryCredential,
            Prelude.Just ("type" Data..= type'),
            Prelude.Just ("image" Data..= image),
            Prelude.Just ("computeType" Data..= computeType)
          ]
      )
