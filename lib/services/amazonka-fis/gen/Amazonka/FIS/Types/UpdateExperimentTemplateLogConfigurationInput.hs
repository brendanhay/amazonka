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
-- Module      : Amazonka.FIS.Types.UpdateExperimentTemplateLogConfigurationInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.UpdateExperimentTemplateLogConfigurationInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FIS.Types.ExperimentTemplateCloudWatchLogsLogConfigurationInput
import Amazonka.FIS.Types.ExperimentTemplateS3LogConfigurationInput
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration for experiment logging.
--
-- /See:/ 'newUpdateExperimentTemplateLogConfigurationInput' smart constructor.
data UpdateExperimentTemplateLogConfigurationInput = UpdateExperimentTemplateLogConfigurationInput'
  { -- | The configuration for experiment logging to Amazon S3.
    s3Configuration :: Prelude.Maybe ExperimentTemplateS3LogConfigurationInput,
    -- | The schema version.
    logSchemaVersion :: Prelude.Maybe Prelude.Int,
    -- | The configuration for experiment logging to Amazon CloudWatch Logs.
    cloudWatchLogsConfiguration :: Prelude.Maybe ExperimentTemplateCloudWatchLogsLogConfigurationInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateExperimentTemplateLogConfigurationInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Configuration', 'updateExperimentTemplateLogConfigurationInput_s3Configuration' - The configuration for experiment logging to Amazon S3.
--
-- 'logSchemaVersion', 'updateExperimentTemplateLogConfigurationInput_logSchemaVersion' - The schema version.
--
-- 'cloudWatchLogsConfiguration', 'updateExperimentTemplateLogConfigurationInput_cloudWatchLogsConfiguration' - The configuration for experiment logging to Amazon CloudWatch Logs.
newUpdateExperimentTemplateLogConfigurationInput ::
  UpdateExperimentTemplateLogConfigurationInput
newUpdateExperimentTemplateLogConfigurationInput =
  UpdateExperimentTemplateLogConfigurationInput'
    { s3Configuration =
        Prelude.Nothing,
      logSchemaVersion =
        Prelude.Nothing,
      cloudWatchLogsConfiguration =
        Prelude.Nothing
    }

-- | The configuration for experiment logging to Amazon S3.
updateExperimentTemplateLogConfigurationInput_s3Configuration :: Lens.Lens' UpdateExperimentTemplateLogConfigurationInput (Prelude.Maybe ExperimentTemplateS3LogConfigurationInput)
updateExperimentTemplateLogConfigurationInput_s3Configuration = Lens.lens (\UpdateExperimentTemplateLogConfigurationInput' {s3Configuration} -> s3Configuration) (\s@UpdateExperimentTemplateLogConfigurationInput' {} a -> s {s3Configuration = a} :: UpdateExperimentTemplateLogConfigurationInput)

-- | The schema version.
updateExperimentTemplateLogConfigurationInput_logSchemaVersion :: Lens.Lens' UpdateExperimentTemplateLogConfigurationInput (Prelude.Maybe Prelude.Int)
updateExperimentTemplateLogConfigurationInput_logSchemaVersion = Lens.lens (\UpdateExperimentTemplateLogConfigurationInput' {logSchemaVersion} -> logSchemaVersion) (\s@UpdateExperimentTemplateLogConfigurationInput' {} a -> s {logSchemaVersion = a} :: UpdateExperimentTemplateLogConfigurationInput)

-- | The configuration for experiment logging to Amazon CloudWatch Logs.
updateExperimentTemplateLogConfigurationInput_cloudWatchLogsConfiguration :: Lens.Lens' UpdateExperimentTemplateLogConfigurationInput (Prelude.Maybe ExperimentTemplateCloudWatchLogsLogConfigurationInput)
updateExperimentTemplateLogConfigurationInput_cloudWatchLogsConfiguration = Lens.lens (\UpdateExperimentTemplateLogConfigurationInput' {cloudWatchLogsConfiguration} -> cloudWatchLogsConfiguration) (\s@UpdateExperimentTemplateLogConfigurationInput' {} a -> s {cloudWatchLogsConfiguration = a} :: UpdateExperimentTemplateLogConfigurationInput)

instance
  Prelude.Hashable
    UpdateExperimentTemplateLogConfigurationInput
  where
  hashWithSalt
    _salt
    UpdateExperimentTemplateLogConfigurationInput' {..} =
      _salt `Prelude.hashWithSalt` s3Configuration
        `Prelude.hashWithSalt` logSchemaVersion
        `Prelude.hashWithSalt` cloudWatchLogsConfiguration

instance
  Prelude.NFData
    UpdateExperimentTemplateLogConfigurationInput
  where
  rnf
    UpdateExperimentTemplateLogConfigurationInput' {..} =
      Prelude.rnf s3Configuration
        `Prelude.seq` Prelude.rnf logSchemaVersion
        `Prelude.seq` Prelude.rnf cloudWatchLogsConfiguration

instance
  Core.ToJSON
    UpdateExperimentTemplateLogConfigurationInput
  where
  toJSON
    UpdateExperimentTemplateLogConfigurationInput' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("s3Configuration" Core..=)
                Prelude.<$> s3Configuration,
              ("logSchemaVersion" Core..=)
                Prelude.<$> logSchemaVersion,
              ("cloudWatchLogsConfiguration" Core..=)
                Prelude.<$> cloudWatchLogsConfiguration
            ]
        )
