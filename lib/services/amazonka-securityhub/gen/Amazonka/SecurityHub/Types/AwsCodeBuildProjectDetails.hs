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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCodeBuildProjectDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The name of the build project.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the build environment for this build project.
    environment :: Prelude.Maybe AwsCodeBuildProjectEnvironment,
    -- | Information about the VPC configuration that CodeBuild accesses.
    vpcConfig :: Prelude.Maybe AwsCodeBuildProjectVpcConfig,
    -- | Information about the secondary artifacts for the CodeBuild project.
    secondaryArtifacts :: Prelude.Maybe [AwsCodeBuildProjectArtifactsDetails],
    -- | The ARN of the IAM role that enables CodeBuild to interact with
    -- dependent Amazon Web Services services on behalf of the Amazon Web
    -- Services account.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | Information about the build input source code for this build project.
    source :: Prelude.Maybe AwsCodeBuildProjectSource,
    -- | Information about logs for the build project.
    logsConfig :: Prelude.Maybe AwsCodeBuildProjectLogsConfigDetails,
    -- | The KMS key used to encrypt the build output artifacts.
    --
    -- You can specify either the ARN of the KMS key or, if available, the KMS
    -- key alias (using the format alias\/alias-name).
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | Information about the build artifacts for the CodeBuild project.
    artifacts :: Prelude.Maybe [AwsCodeBuildProjectArtifactsDetails]
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
-- 'name', 'awsCodeBuildProjectDetails_name' - The name of the build project.
--
-- 'environment', 'awsCodeBuildProjectDetails_environment' - Information about the build environment for this build project.
--
-- 'vpcConfig', 'awsCodeBuildProjectDetails_vpcConfig' - Information about the VPC configuration that CodeBuild accesses.
--
-- 'secondaryArtifacts', 'awsCodeBuildProjectDetails_secondaryArtifacts' - Information about the secondary artifacts for the CodeBuild project.
--
-- 'serviceRole', 'awsCodeBuildProjectDetails_serviceRole' - The ARN of the IAM role that enables CodeBuild to interact with
-- dependent Amazon Web Services services on behalf of the Amazon Web
-- Services account.
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
-- 'artifacts', 'awsCodeBuildProjectDetails_artifacts' - Information about the build artifacts for the CodeBuild project.
newAwsCodeBuildProjectDetails ::
  AwsCodeBuildProjectDetails
newAwsCodeBuildProjectDetails =
  AwsCodeBuildProjectDetails'
    { name = Prelude.Nothing,
      environment = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      secondaryArtifacts = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      source = Prelude.Nothing,
      logsConfig = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      artifacts = Prelude.Nothing
    }

-- | The name of the build project.
awsCodeBuildProjectDetails_name :: Lens.Lens' AwsCodeBuildProjectDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectDetails_name = Lens.lens (\AwsCodeBuildProjectDetails' {name} -> name) (\s@AwsCodeBuildProjectDetails' {} a -> s {name = a} :: AwsCodeBuildProjectDetails)

-- | Information about the build environment for this build project.
awsCodeBuildProjectDetails_environment :: Lens.Lens' AwsCodeBuildProjectDetails (Prelude.Maybe AwsCodeBuildProjectEnvironment)
awsCodeBuildProjectDetails_environment = Lens.lens (\AwsCodeBuildProjectDetails' {environment} -> environment) (\s@AwsCodeBuildProjectDetails' {} a -> s {environment = a} :: AwsCodeBuildProjectDetails)

-- | Information about the VPC configuration that CodeBuild accesses.
awsCodeBuildProjectDetails_vpcConfig :: Lens.Lens' AwsCodeBuildProjectDetails (Prelude.Maybe AwsCodeBuildProjectVpcConfig)
awsCodeBuildProjectDetails_vpcConfig = Lens.lens (\AwsCodeBuildProjectDetails' {vpcConfig} -> vpcConfig) (\s@AwsCodeBuildProjectDetails' {} a -> s {vpcConfig = a} :: AwsCodeBuildProjectDetails)

-- | Information about the secondary artifacts for the CodeBuild project.
awsCodeBuildProjectDetails_secondaryArtifacts :: Lens.Lens' AwsCodeBuildProjectDetails (Prelude.Maybe [AwsCodeBuildProjectArtifactsDetails])
awsCodeBuildProjectDetails_secondaryArtifacts = Lens.lens (\AwsCodeBuildProjectDetails' {secondaryArtifacts} -> secondaryArtifacts) (\s@AwsCodeBuildProjectDetails' {} a -> s {secondaryArtifacts = a} :: AwsCodeBuildProjectDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the IAM role that enables CodeBuild to interact with
-- dependent Amazon Web Services services on behalf of the Amazon Web
-- Services account.
awsCodeBuildProjectDetails_serviceRole :: Lens.Lens' AwsCodeBuildProjectDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectDetails_serviceRole = Lens.lens (\AwsCodeBuildProjectDetails' {serviceRole} -> serviceRole) (\s@AwsCodeBuildProjectDetails' {} a -> s {serviceRole = a} :: AwsCodeBuildProjectDetails)

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

-- | Information about the build artifacts for the CodeBuild project.
awsCodeBuildProjectDetails_artifacts :: Lens.Lens' AwsCodeBuildProjectDetails (Prelude.Maybe [AwsCodeBuildProjectArtifactsDetails])
awsCodeBuildProjectDetails_artifacts = Lens.lens (\AwsCodeBuildProjectDetails' {artifacts} -> artifacts) (\s@AwsCodeBuildProjectDetails' {} a -> s {artifacts = a} :: AwsCodeBuildProjectDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AwsCodeBuildProjectDetails where
  parseJSON =
    Data.withObject
      "AwsCodeBuildProjectDetails"
      ( \x ->
          AwsCodeBuildProjectDetails'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Environment")
            Prelude.<*> (x Data..:? "VpcConfig")
            Prelude.<*> ( x Data..:? "SecondaryArtifacts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ServiceRole")
            Prelude.<*> (x Data..:? "Source")
            Prelude.<*> (x Data..:? "LogsConfig")
            Prelude.<*> (x Data..:? "EncryptionKey")
            Prelude.<*> (x Data..:? "Artifacts" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AwsCodeBuildProjectDetails where
  hashWithSalt _salt AwsCodeBuildProjectDetails' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` secondaryArtifacts
      `Prelude.hashWithSalt` serviceRole
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` logsConfig
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` artifacts

instance Prelude.NFData AwsCodeBuildProjectDetails where
  rnf AwsCodeBuildProjectDetails' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf secondaryArtifacts
      `Prelude.seq` Prelude.rnf serviceRole
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf logsConfig
      `Prelude.seq` Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf artifacts

instance Data.ToJSON AwsCodeBuildProjectDetails where
  toJSON AwsCodeBuildProjectDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Environment" Data..=) Prelude.<$> environment,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig,
            ("SecondaryArtifacts" Data..=)
              Prelude.<$> secondaryArtifacts,
            ("ServiceRole" Data..=) Prelude.<$> serviceRole,
            ("Source" Data..=) Prelude.<$> source,
            ("LogsConfig" Data..=) Prelude.<$> logsConfig,
            ("EncryptionKey" Data..=) Prelude.<$> encryptionKey,
            ("Artifacts" Data..=) Prelude.<$> artifacts
          ]
      )
