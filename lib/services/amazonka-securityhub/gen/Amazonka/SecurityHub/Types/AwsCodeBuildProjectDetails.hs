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
-- Module      : Amazonka.SecurityHub.Types.AwsCodeBuildProjectDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCodeBuildProjectDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectArtifactsDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectEnvironment
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectSource
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectVpcConfig

-- | Information about an CodeBuild project.
--
-- /See:/ 'newAwsCodeBuildProjectDetails' smart constructor.
data AwsCodeBuildProjectDetails = AwsCodeBuildProjectDetails'
  { -- | Information about the build artifacts for the CodeBuild project.
    artifacts :: Prelude.Maybe [AwsCodeBuildProjectArtifactsDetails],
    -- | Information about the build environment for this build project.
    environment :: Prelude.Maybe AwsCodeBuildProjectEnvironment,
    -- | Information about the VPC configuration that CodeBuild accesses.
    vpcConfig :: Prelude.Maybe AwsCodeBuildProjectVpcConfig,
    -- | The name of the build project.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the build input source code for this build project.
    source :: Prelude.Maybe AwsCodeBuildProjectSource,
    -- | Information about logs for the build project.
    logsConfig :: Prelude.Maybe AwsCodeBuildProjectLogsConfigDetails,
    -- | The KMS key used to encrypt the build output artifacts.
    --
    -- You can specify either the ARN of the KMS key or, if available, the KMS
    -- key alias (using the format alias\/alias-name).
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that enables CodeBuild to interact with
    -- dependent Amazon Web Services services on behalf of the Amazon Web
    -- Services account.
    serviceRole :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCodeBuildProjectDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifacts', 'awsCodeBuildProjectDetails_artifacts' - Information about the build artifacts for the CodeBuild project.
--
-- 'environment', 'awsCodeBuildProjectDetails_environment' - Information about the build environment for this build project.
--
-- 'vpcConfig', 'awsCodeBuildProjectDetails_vpcConfig' - Information about the VPC configuration that CodeBuild accesses.
--
-- 'name', 'awsCodeBuildProjectDetails_name' - The name of the build project.
--
-- 'source', 'awsCodeBuildProjectDetails_source' - Information about the build input source code for this build project.
--
-- 'logsConfig', 'awsCodeBuildProjectDetails_logsConfig' - Information about logs for the build project.
--
-- 'encryptionKey', 'awsCodeBuildProjectDetails_encryptionKey' - The KMS key used to encrypt the build output artifacts.
--
-- You can specify either the ARN of the KMS key or, if available, the KMS
-- key alias (using the format alias\/alias-name).
--
-- 'serviceRole', 'awsCodeBuildProjectDetails_serviceRole' - The ARN of the IAM role that enables CodeBuild to interact with
-- dependent Amazon Web Services services on behalf of the Amazon Web
-- Services account.
newAwsCodeBuildProjectDetails ::
  AwsCodeBuildProjectDetails
newAwsCodeBuildProjectDetails =
  AwsCodeBuildProjectDetails'
    { artifacts =
        Prelude.Nothing,
      environment = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      name = Prelude.Nothing,
      source = Prelude.Nothing,
      logsConfig = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      serviceRole = Prelude.Nothing
    }

-- | Information about the build artifacts for the CodeBuild project.
awsCodeBuildProjectDetails_artifacts :: Lens.Lens' AwsCodeBuildProjectDetails (Prelude.Maybe [AwsCodeBuildProjectArtifactsDetails])
awsCodeBuildProjectDetails_artifacts = Lens.lens (\AwsCodeBuildProjectDetails' {artifacts} -> artifacts) (\s@AwsCodeBuildProjectDetails' {} a -> s {artifacts = a} :: AwsCodeBuildProjectDetails) Prelude.. Lens.mapping Lens.coerced

-- | Information about the build environment for this build project.
awsCodeBuildProjectDetails_environment :: Lens.Lens' AwsCodeBuildProjectDetails (Prelude.Maybe AwsCodeBuildProjectEnvironment)
awsCodeBuildProjectDetails_environment = Lens.lens (\AwsCodeBuildProjectDetails' {environment} -> environment) (\s@AwsCodeBuildProjectDetails' {} a -> s {environment = a} :: AwsCodeBuildProjectDetails)

-- | Information about the VPC configuration that CodeBuild accesses.
awsCodeBuildProjectDetails_vpcConfig :: Lens.Lens' AwsCodeBuildProjectDetails (Prelude.Maybe AwsCodeBuildProjectVpcConfig)
awsCodeBuildProjectDetails_vpcConfig = Lens.lens (\AwsCodeBuildProjectDetails' {vpcConfig} -> vpcConfig) (\s@AwsCodeBuildProjectDetails' {} a -> s {vpcConfig = a} :: AwsCodeBuildProjectDetails)

-- | The name of the build project.
awsCodeBuildProjectDetails_name :: Lens.Lens' AwsCodeBuildProjectDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectDetails_name = Lens.lens (\AwsCodeBuildProjectDetails' {name} -> name) (\s@AwsCodeBuildProjectDetails' {} a -> s {name = a} :: AwsCodeBuildProjectDetails)

-- | Information about the build input source code for this build project.
awsCodeBuildProjectDetails_source :: Lens.Lens' AwsCodeBuildProjectDetails (Prelude.Maybe AwsCodeBuildProjectSource)
awsCodeBuildProjectDetails_source = Lens.lens (\AwsCodeBuildProjectDetails' {source} -> source) (\s@AwsCodeBuildProjectDetails' {} a -> s {source = a} :: AwsCodeBuildProjectDetails)

-- | Information about logs for the build project.
awsCodeBuildProjectDetails_logsConfig :: Lens.Lens' AwsCodeBuildProjectDetails (Prelude.Maybe AwsCodeBuildProjectLogsConfigDetails)
awsCodeBuildProjectDetails_logsConfig = Lens.lens (\AwsCodeBuildProjectDetails' {logsConfig} -> logsConfig) (\s@AwsCodeBuildProjectDetails' {} a -> s {logsConfig = a} :: AwsCodeBuildProjectDetails)

-- | The KMS key used to encrypt the build output artifacts.
--
-- You can specify either the ARN of the KMS key or, if available, the KMS
-- key alias (using the format alias\/alias-name).
awsCodeBuildProjectDetails_encryptionKey :: Lens.Lens' AwsCodeBuildProjectDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectDetails_encryptionKey = Lens.lens (\AwsCodeBuildProjectDetails' {encryptionKey} -> encryptionKey) (\s@AwsCodeBuildProjectDetails' {} a -> s {encryptionKey = a} :: AwsCodeBuildProjectDetails)

-- | The ARN of the IAM role that enables CodeBuild to interact with
-- dependent Amazon Web Services services on behalf of the Amazon Web
-- Services account.
awsCodeBuildProjectDetails_serviceRole :: Lens.Lens' AwsCodeBuildProjectDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectDetails_serviceRole = Lens.lens (\AwsCodeBuildProjectDetails' {serviceRole} -> serviceRole) (\s@AwsCodeBuildProjectDetails' {} a -> s {serviceRole = a} :: AwsCodeBuildProjectDetails)

instance Core.FromJSON AwsCodeBuildProjectDetails where
  parseJSON =
    Core.withObject
      "AwsCodeBuildProjectDetails"
      ( \x ->
          AwsCodeBuildProjectDetails'
            Prelude.<$> (x Core..:? "Artifacts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Environment")
            Prelude.<*> (x Core..:? "VpcConfig")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Source")
            Prelude.<*> (x Core..:? "LogsConfig")
            Prelude.<*> (x Core..:? "EncryptionKey")
            Prelude.<*> (x Core..:? "ServiceRole")
      )

instance Prelude.Hashable AwsCodeBuildProjectDetails where
  hashWithSalt _salt AwsCodeBuildProjectDetails' {..} =
    _salt `Prelude.hashWithSalt` artifacts
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` logsConfig
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` serviceRole

instance Prelude.NFData AwsCodeBuildProjectDetails where
  rnf AwsCodeBuildProjectDetails' {..} =
    Prelude.rnf artifacts
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf logsConfig
      `Prelude.seq` Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf serviceRole

instance Core.ToJSON AwsCodeBuildProjectDetails where
  toJSON AwsCodeBuildProjectDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Artifacts" Core..=) Prelude.<$> artifacts,
            ("Environment" Core..=) Prelude.<$> environment,
            ("VpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("Name" Core..=) Prelude.<$> name,
            ("Source" Core..=) Prelude.<$> source,
            ("LogsConfig" Core..=) Prelude.<$> logsConfig,
            ("EncryptionKey" Core..=) Prelude.<$> encryptionKey,
            ("ServiceRole" Core..=) Prelude.<$> serviceRole
          ]
      )
