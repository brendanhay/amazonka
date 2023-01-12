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
-- Module      : Amazonka.FIS.Types.CreateExperimentTemplateLogConfigurationInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.CreateExperimentTemplateLogConfigurationInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types.ExperimentTemplateCloudWatchLogsLogConfigurationInput
import Amazonka.FIS.Types.ExperimentTemplateS3LogConfigurationInput
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration for experiment logging.
--
-- /See:/ 'newCreateExperimentTemplateLogConfigurationInput' smart constructor.
data CreateExperimentTemplateLogConfigurationInput = CreateExperimentTemplateLogConfigurationInput'
  { -- | The configuration for experiment logging to Amazon CloudWatch Logs.
    cloudWatchLogsConfiguration :: Prelude.Maybe ExperimentTemplateCloudWatchLogsLogConfigurationInput,
    -- | The configuration for experiment logging to Amazon S3.
    s3Configuration :: Prelude.Maybe ExperimentTemplateS3LogConfigurationInput,
    -- | The schema version.
    logSchemaVersion :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExperimentTemplateLogConfigurationInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogsConfiguration', 'createExperimentTemplateLogConfigurationInput_cloudWatchLogsConfiguration' - The configuration for experiment logging to Amazon CloudWatch Logs.
--
-- 's3Configuration', 'createExperimentTemplateLogConfigurationInput_s3Configuration' - The configuration for experiment logging to Amazon S3.
--
-- 'logSchemaVersion', 'createExperimentTemplateLogConfigurationInput_logSchemaVersion' - The schema version.
newCreateExperimentTemplateLogConfigurationInput ::
  -- | 'logSchemaVersion'
  Prelude.Int ->
  CreateExperimentTemplateLogConfigurationInput
newCreateExperimentTemplateLogConfigurationInput
  pLogSchemaVersion_ =
    CreateExperimentTemplateLogConfigurationInput'
      { cloudWatchLogsConfiguration =
          Prelude.Nothing,
        s3Configuration =
          Prelude.Nothing,
        logSchemaVersion =
          pLogSchemaVersion_
      }

-- | The configuration for experiment logging to Amazon CloudWatch Logs.
createExperimentTemplateLogConfigurationInput_cloudWatchLogsConfiguration :: Lens.Lens' CreateExperimentTemplateLogConfigurationInput (Prelude.Maybe ExperimentTemplateCloudWatchLogsLogConfigurationInput)
createExperimentTemplateLogConfigurationInput_cloudWatchLogsConfiguration = Lens.lens (\CreateExperimentTemplateLogConfigurationInput' {cloudWatchLogsConfiguration} -> cloudWatchLogsConfiguration) (\s@CreateExperimentTemplateLogConfigurationInput' {} a -> s {cloudWatchLogsConfiguration = a} :: CreateExperimentTemplateLogConfigurationInput)

-- | The configuration for experiment logging to Amazon S3.
createExperimentTemplateLogConfigurationInput_s3Configuration :: Lens.Lens' CreateExperimentTemplateLogConfigurationInput (Prelude.Maybe ExperimentTemplateS3LogConfigurationInput)
createExperimentTemplateLogConfigurationInput_s3Configuration = Lens.lens (\CreateExperimentTemplateLogConfigurationInput' {s3Configuration} -> s3Configuration) (\s@CreateExperimentTemplateLogConfigurationInput' {} a -> s {s3Configuration = a} :: CreateExperimentTemplateLogConfigurationInput)

-- | The schema version.
createExperimentTemplateLogConfigurationInput_logSchemaVersion :: Lens.Lens' CreateExperimentTemplateLogConfigurationInput Prelude.Int
createExperimentTemplateLogConfigurationInput_logSchemaVersion = Lens.lens (\CreateExperimentTemplateLogConfigurationInput' {logSchemaVersion} -> logSchemaVersion) (\s@CreateExperimentTemplateLogConfigurationInput' {} a -> s {logSchemaVersion = a} :: CreateExperimentTemplateLogConfigurationInput)

instance
  Prelude.Hashable
    CreateExperimentTemplateLogConfigurationInput
  where
  hashWithSalt
    _salt
    CreateExperimentTemplateLogConfigurationInput' {..} =
      _salt
        `Prelude.hashWithSalt` cloudWatchLogsConfiguration
        `Prelude.hashWithSalt` s3Configuration
        `Prelude.hashWithSalt` logSchemaVersion

instance
  Prelude.NFData
    CreateExperimentTemplateLogConfigurationInput
  where
  rnf
    CreateExperimentTemplateLogConfigurationInput' {..} =
      Prelude.rnf cloudWatchLogsConfiguration
        `Prelude.seq` Prelude.rnf s3Configuration
        `Prelude.seq` Prelude.rnf logSchemaVersion

instance
  Data.ToJSON
    CreateExperimentTemplateLogConfigurationInput
  where
  toJSON
    CreateExperimentTemplateLogConfigurationInput' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("cloudWatchLogsConfiguration" Data..=)
                Prelude.<$> cloudWatchLogsConfiguration,
              ("s3Configuration" Data..=)
                Prelude.<$> s3Configuration,
              Prelude.Just
                ("logSchemaVersion" Data..= logSchemaVersion)
            ]
        )
