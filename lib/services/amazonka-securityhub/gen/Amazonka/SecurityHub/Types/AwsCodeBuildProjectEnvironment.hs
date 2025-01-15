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
-- Module      : Amazonka.SecurityHub.Types.AwsCodeBuildProjectEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCodeBuildProjectEnvironment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectEnvironmentRegistryCredential

-- | Information about the build environment for this build project.
--
-- /See:/ 'newAwsCodeBuildProjectEnvironment' smart constructor.
data AwsCodeBuildProjectEnvironment = AwsCodeBuildProjectEnvironment'
  { -- | The certificate to use with this build project.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | A set of environment variables to make available to builds for the build
    -- project.
    environmentVariables :: Prelude.Maybe [AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails],
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
    imagePullCredentialsType :: Prelude.Maybe Prelude.Text,
    -- | Whether to allow the Docker daemon to run inside a Docker container. Set
    -- to @true@ if the build project is used to build Docker images.
    privilegedMode :: Prelude.Maybe Prelude.Bool,
    -- | The credentials for access to a private registry.
    registryCredential :: Prelude.Maybe AwsCodeBuildProjectEnvironmentRegistryCredential,
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
-- 'certificate', 'awsCodeBuildProjectEnvironment_certificate' - The certificate to use with this build project.
--
-- 'environmentVariables', 'awsCodeBuildProjectEnvironment_environmentVariables' - A set of environment variables to make available to builds for the build
-- project.
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
    { certificate =
        Prelude.Nothing,
      environmentVariables = Prelude.Nothing,
      imagePullCredentialsType = Prelude.Nothing,
      privilegedMode = Prelude.Nothing,
      registryCredential = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The certificate to use with this build project.
awsCodeBuildProjectEnvironment_certificate :: Lens.Lens' AwsCodeBuildProjectEnvironment (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectEnvironment_certificate = Lens.lens (\AwsCodeBuildProjectEnvironment' {certificate} -> certificate) (\s@AwsCodeBuildProjectEnvironment' {} a -> s {certificate = a} :: AwsCodeBuildProjectEnvironment)

-- | A set of environment variables to make available to builds for the build
-- project.
awsCodeBuildProjectEnvironment_environmentVariables :: Lens.Lens' AwsCodeBuildProjectEnvironment (Prelude.Maybe [AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails])
awsCodeBuildProjectEnvironment_environmentVariables = Lens.lens (\AwsCodeBuildProjectEnvironment' {environmentVariables} -> environmentVariables) (\s@AwsCodeBuildProjectEnvironment' {} a -> s {environmentVariables = a} :: AwsCodeBuildProjectEnvironment) Prelude.. Lens.mapping Lens.coerced

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

instance Data.FromJSON AwsCodeBuildProjectEnvironment where
  parseJSON =
    Data.withObject
      "AwsCodeBuildProjectEnvironment"
      ( \x ->
          AwsCodeBuildProjectEnvironment'
            Prelude.<$> (x Data..:? "Certificate")
            Prelude.<*> ( x
                            Data..:? "EnvironmentVariables"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ImagePullCredentialsType")
            Prelude.<*> (x Data..:? "PrivilegedMode")
            Prelude.<*> (x Data..:? "RegistryCredential")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AwsCodeBuildProjectEnvironment
  where
  hashWithSalt
    _salt
    AwsCodeBuildProjectEnvironment' {..} =
      _salt
        `Prelude.hashWithSalt` certificate
        `Prelude.hashWithSalt` environmentVariables
        `Prelude.hashWithSalt` imagePullCredentialsType
        `Prelude.hashWithSalt` privilegedMode
        `Prelude.hashWithSalt` registryCredential
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsCodeBuildProjectEnvironment
  where
  rnf AwsCodeBuildProjectEnvironment' {..} =
    Prelude.rnf certificate `Prelude.seq`
      Prelude.rnf environmentVariables `Prelude.seq`
        Prelude.rnf imagePullCredentialsType `Prelude.seq`
          Prelude.rnf privilegedMode `Prelude.seq`
            Prelude.rnf registryCredential `Prelude.seq`
              Prelude.rnf type'

instance Data.ToJSON AwsCodeBuildProjectEnvironment where
  toJSON AwsCodeBuildProjectEnvironment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Certificate" Data..=) Prelude.<$> certificate,
            ("EnvironmentVariables" Data..=)
              Prelude.<$> environmentVariables,
            ("ImagePullCredentialsType" Data..=)
              Prelude.<$> imagePullCredentialsType,
            ("PrivilegedMode" Data..=)
              Prelude.<$> privilegedMode,
            ("RegistryCredential" Data..=)
              Prelude.<$> registryCredential,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
