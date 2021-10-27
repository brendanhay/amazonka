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
-- Module      : Network.AWS.SecurityHub.Types.AwsCodeBuildProjectEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsCodeBuildProjectEnvironment where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
import Network.AWS.SecurityHub.Types.AwsCodeBuildProjectEnvironmentRegistryCredential

-- | Information about the build environment for this build project.
--
-- /See:/ 'newAwsCodeBuildProjectEnvironment' smart constructor.
data AwsCodeBuildProjectEnvironment = AwsCodeBuildProjectEnvironment'
  { -- | The type of credentials CodeBuild uses to pull images in your build.
    --
    -- Valid values:
    --
    -- -   @CODEBUILD@ specifies that CodeBuild uses its own credentials. This
    --     requires that you modify your ECR repository policy to trust the
    --     CodeBuild service principal.
    --
    -- -   @SERVICE_ROLE@ specifies that CodeBuild uses your build project\'s
    --     service role.
    --
    -- When you use a cross-account or private registry image, you must use
    -- @SERVICE_ROLE@ credentials. When you use an CodeBuild curated image, you
    -- must use @CODEBUILD@ credentials.
    imagePullCredentialsType :: Prelude.Maybe Prelude.Text,
    -- | Whether to allow the Docker daemon to run inside a Docker container. Set
    -- to @true@ if the build project is used to build Docker images.
    privilegedMode :: Prelude.Maybe Prelude.Bool,
    -- | The credentials for access to a private registry.
    registryCredential :: Prelude.Maybe AwsCodeBuildProjectEnvironmentRegistryCredential,
    -- | The certificate to use with this build project.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | A set of environment variables to make available to builds for the build
    -- project.
    environmentVariables :: Prelude.Maybe [AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails],
    -- | The type of build environment to use for related builds.
    --
    -- The environment type @ARM_CONTAINER@ is available only in Regions US
    -- East (N. Virginia), US East (Ohio), US West (Oregon), Europe (Ireland),
    -- Asia Pacific (Mumbai), Asia Pacific (Tokyo), Asia Pacific (Sydney), and
    -- Europe (Frankfurt).
    --
    -- The environment type @LINUX_CONTAINER@ with compute type
    -- build.general1.2xlarge is available only in Regions US East (N.
    -- Virginia), US East (N. Virginia), US West (Oregon), Canada (Central),
    -- Europe (Ireland), Europe (London), Europe (Frankfurt), Asia Pacific
    -- (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific
    -- (Sydney), China (Beijing), and China (Ningxia).
    --
    -- The environment type @LINUX_GPU_CONTAINER@ is available only in Regions
    -- US East (N. Virginia), US East (N. Virginia), US West (Oregon), Canada
    -- (Central), Europe (Ireland), Europe (London), Europe (Frankfurt), Asia
    -- Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia
    -- Pacific (Sydney), China (Beijing), and China (Ningxia).
    --
    -- Valid values: @WINDOWS_CONTAINER@ | @LINUX_CONTAINER@ |
    -- @LINUX_GPU_CONTAINER@ | @ARM_CONTAINER@
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCodeBuildProjectEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imagePullCredentialsType', 'awsCodeBuildProjectEnvironment_imagePullCredentialsType' - The type of credentials CodeBuild uses to pull images in your build.
--
-- Valid values:
--
-- -   @CODEBUILD@ specifies that CodeBuild uses its own credentials. This
--     requires that you modify your ECR repository policy to trust the
--     CodeBuild service principal.
--
-- -   @SERVICE_ROLE@ specifies that CodeBuild uses your build project\'s
--     service role.
--
-- When you use a cross-account or private registry image, you must use
-- @SERVICE_ROLE@ credentials. When you use an CodeBuild curated image, you
-- must use @CODEBUILD@ credentials.
--
-- 'privilegedMode', 'awsCodeBuildProjectEnvironment_privilegedMode' - Whether to allow the Docker daemon to run inside a Docker container. Set
-- to @true@ if the build project is used to build Docker images.
--
-- 'registryCredential', 'awsCodeBuildProjectEnvironment_registryCredential' - The credentials for access to a private registry.
--
-- 'certificate', 'awsCodeBuildProjectEnvironment_certificate' - The certificate to use with this build project.
--
-- 'environmentVariables', 'awsCodeBuildProjectEnvironment_environmentVariables' - A set of environment variables to make available to builds for the build
-- project.
--
-- 'type'', 'awsCodeBuildProjectEnvironment_type' - The type of build environment to use for related builds.
--
-- The environment type @ARM_CONTAINER@ is available only in Regions US
-- East (N. Virginia), US East (Ohio), US West (Oregon), Europe (Ireland),
-- Asia Pacific (Mumbai), Asia Pacific (Tokyo), Asia Pacific (Sydney), and
-- Europe (Frankfurt).
--
-- The environment type @LINUX_CONTAINER@ with compute type
-- build.general1.2xlarge is available only in Regions US East (N.
-- Virginia), US East (N. Virginia), US West (Oregon), Canada (Central),
-- Europe (Ireland), Europe (London), Europe (Frankfurt), Asia Pacific
-- (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific
-- (Sydney), China (Beijing), and China (Ningxia).
--
-- The environment type @LINUX_GPU_CONTAINER@ is available only in Regions
-- US East (N. Virginia), US East (N. Virginia), US West (Oregon), Canada
-- (Central), Europe (Ireland), Europe (London), Europe (Frankfurt), Asia
-- Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia
-- Pacific (Sydney), China (Beijing), and China (Ningxia).
--
-- Valid values: @WINDOWS_CONTAINER@ | @LINUX_CONTAINER@ |
-- @LINUX_GPU_CONTAINER@ | @ARM_CONTAINER@
newAwsCodeBuildProjectEnvironment ::
  AwsCodeBuildProjectEnvironment
newAwsCodeBuildProjectEnvironment =
  AwsCodeBuildProjectEnvironment'
    { imagePullCredentialsType =
        Prelude.Nothing,
      privilegedMode = Prelude.Nothing,
      registryCredential = Prelude.Nothing,
      certificate = Prelude.Nothing,
      environmentVariables = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The type of credentials CodeBuild uses to pull images in your build.
--
-- Valid values:
--
-- -   @CODEBUILD@ specifies that CodeBuild uses its own credentials. This
--     requires that you modify your ECR repository policy to trust the
--     CodeBuild service principal.
--
-- -   @SERVICE_ROLE@ specifies that CodeBuild uses your build project\'s
--     service role.
--
-- When you use a cross-account or private registry image, you must use
-- @SERVICE_ROLE@ credentials. When you use an CodeBuild curated image, you
-- must use @CODEBUILD@ credentials.
awsCodeBuildProjectEnvironment_imagePullCredentialsType :: Lens.Lens' AwsCodeBuildProjectEnvironment (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectEnvironment_imagePullCredentialsType = Lens.lens (\AwsCodeBuildProjectEnvironment' {imagePullCredentialsType} -> imagePullCredentialsType) (\s@AwsCodeBuildProjectEnvironment' {} a -> s {imagePullCredentialsType = a} :: AwsCodeBuildProjectEnvironment)

-- | Whether to allow the Docker daemon to run inside a Docker container. Set
-- to @true@ if the build project is used to build Docker images.
awsCodeBuildProjectEnvironment_privilegedMode :: Lens.Lens' AwsCodeBuildProjectEnvironment (Prelude.Maybe Prelude.Bool)
awsCodeBuildProjectEnvironment_privilegedMode = Lens.lens (\AwsCodeBuildProjectEnvironment' {privilegedMode} -> privilegedMode) (\s@AwsCodeBuildProjectEnvironment' {} a -> s {privilegedMode = a} :: AwsCodeBuildProjectEnvironment)

-- | The credentials for access to a private registry.
awsCodeBuildProjectEnvironment_registryCredential :: Lens.Lens' AwsCodeBuildProjectEnvironment (Prelude.Maybe AwsCodeBuildProjectEnvironmentRegistryCredential)
awsCodeBuildProjectEnvironment_registryCredential = Lens.lens (\AwsCodeBuildProjectEnvironment' {registryCredential} -> registryCredential) (\s@AwsCodeBuildProjectEnvironment' {} a -> s {registryCredential = a} :: AwsCodeBuildProjectEnvironment)

-- | The certificate to use with this build project.
awsCodeBuildProjectEnvironment_certificate :: Lens.Lens' AwsCodeBuildProjectEnvironment (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectEnvironment_certificate = Lens.lens (\AwsCodeBuildProjectEnvironment' {certificate} -> certificate) (\s@AwsCodeBuildProjectEnvironment' {} a -> s {certificate = a} :: AwsCodeBuildProjectEnvironment)

-- | A set of environment variables to make available to builds for the build
-- project.
awsCodeBuildProjectEnvironment_environmentVariables :: Lens.Lens' AwsCodeBuildProjectEnvironment (Prelude.Maybe [AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails])
awsCodeBuildProjectEnvironment_environmentVariables = Lens.lens (\AwsCodeBuildProjectEnvironment' {environmentVariables} -> environmentVariables) (\s@AwsCodeBuildProjectEnvironment' {} a -> s {environmentVariables = a} :: AwsCodeBuildProjectEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The type of build environment to use for related builds.
--
-- The environment type @ARM_CONTAINER@ is available only in Regions US
-- East (N. Virginia), US East (Ohio), US West (Oregon), Europe (Ireland),
-- Asia Pacific (Mumbai), Asia Pacific (Tokyo), Asia Pacific (Sydney), and
-- Europe (Frankfurt).
--
-- The environment type @LINUX_CONTAINER@ with compute type
-- build.general1.2xlarge is available only in Regions US East (N.
-- Virginia), US East (N. Virginia), US West (Oregon), Canada (Central),
-- Europe (Ireland), Europe (London), Europe (Frankfurt), Asia Pacific
-- (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific
-- (Sydney), China (Beijing), and China (Ningxia).
--
-- The environment type @LINUX_GPU_CONTAINER@ is available only in Regions
-- US East (N. Virginia), US East (N. Virginia), US West (Oregon), Canada
-- (Central), Europe (Ireland), Europe (London), Europe (Frankfurt), Asia
-- Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia
-- Pacific (Sydney), China (Beijing), and China (Ningxia).
--
-- Valid values: @WINDOWS_CONTAINER@ | @LINUX_CONTAINER@ |
-- @LINUX_GPU_CONTAINER@ | @ARM_CONTAINER@
awsCodeBuildProjectEnvironment_type :: Lens.Lens' AwsCodeBuildProjectEnvironment (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectEnvironment_type = Lens.lens (\AwsCodeBuildProjectEnvironment' {type'} -> type') (\s@AwsCodeBuildProjectEnvironment' {} a -> s {type' = a} :: AwsCodeBuildProjectEnvironment)

instance Core.FromJSON AwsCodeBuildProjectEnvironment where
  parseJSON =
    Core.withObject
      "AwsCodeBuildProjectEnvironment"
      ( \x ->
          AwsCodeBuildProjectEnvironment'
            Prelude.<$> (x Core..:? "ImagePullCredentialsType")
            Prelude.<*> (x Core..:? "PrivilegedMode")
            Prelude.<*> (x Core..:? "RegistryCredential")
            Prelude.<*> (x Core..:? "Certificate")
            Prelude.<*> ( x Core..:? "EnvironmentVariables"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Type")
      )

instance
  Prelude.Hashable
    AwsCodeBuildProjectEnvironment

instance
  Prelude.NFData
    AwsCodeBuildProjectEnvironment

instance Core.ToJSON AwsCodeBuildProjectEnvironment where
  toJSON AwsCodeBuildProjectEnvironment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ImagePullCredentialsType" Core..=)
              Prelude.<$> imagePullCredentialsType,
            ("PrivilegedMode" Core..=)
              Prelude.<$> privilegedMode,
            ("RegistryCredential" Core..=)
              Prelude.<$> registryCredential,
            ("Certificate" Core..=) Prelude.<$> certificate,
            ("EnvironmentVariables" Core..=)
              Prelude.<$> environmentVariables,
            ("Type" Core..=) Prelude.<$> type'
          ]
      )
