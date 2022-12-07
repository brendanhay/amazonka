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
-- Module      : Amazonka.FIS.Types.ExperimentTemplateLogConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentTemplateLogConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types.ExperimentTemplateCloudWatchLogsLogConfiguration
import Amazonka.FIS.Types.ExperimentTemplateS3LogConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for experiment logging.
--
-- /See:/ 'newExperimentTemplateLogConfiguration' smart constructor.
data ExperimentTemplateLogConfiguration = ExperimentTemplateLogConfiguration'
  { -- | The configuration for experiment logging to Amazon S3.
    s3Configuration :: Prelude.Maybe ExperimentTemplateS3LogConfiguration,
    -- | The schema version.
    logSchemaVersion :: Prelude.Maybe Prelude.Int,
    -- | The configuration for experiment logging to Amazon CloudWatch Logs.
    cloudWatchLogsConfiguration :: Prelude.Maybe ExperimentTemplateCloudWatchLogsLogConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentTemplateLogConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Configuration', 'experimentTemplateLogConfiguration_s3Configuration' - The configuration for experiment logging to Amazon S3.
--
-- 'logSchemaVersion', 'experimentTemplateLogConfiguration_logSchemaVersion' - The schema version.
--
-- 'cloudWatchLogsConfiguration', 'experimentTemplateLogConfiguration_cloudWatchLogsConfiguration' - The configuration for experiment logging to Amazon CloudWatch Logs.
newExperimentTemplateLogConfiguration ::
  ExperimentTemplateLogConfiguration
newExperimentTemplateLogConfiguration =
  ExperimentTemplateLogConfiguration'
    { s3Configuration =
        Prelude.Nothing,
      logSchemaVersion = Prelude.Nothing,
      cloudWatchLogsConfiguration =
        Prelude.Nothing
    }

-- | The configuration for experiment logging to Amazon S3.
experimentTemplateLogConfiguration_s3Configuration :: Lens.Lens' ExperimentTemplateLogConfiguration (Prelude.Maybe ExperimentTemplateS3LogConfiguration)
experimentTemplateLogConfiguration_s3Configuration = Lens.lens (\ExperimentTemplateLogConfiguration' {s3Configuration} -> s3Configuration) (\s@ExperimentTemplateLogConfiguration' {} a -> s {s3Configuration = a} :: ExperimentTemplateLogConfiguration)

-- | The schema version.
experimentTemplateLogConfiguration_logSchemaVersion :: Lens.Lens' ExperimentTemplateLogConfiguration (Prelude.Maybe Prelude.Int)
experimentTemplateLogConfiguration_logSchemaVersion = Lens.lens (\ExperimentTemplateLogConfiguration' {logSchemaVersion} -> logSchemaVersion) (\s@ExperimentTemplateLogConfiguration' {} a -> s {logSchemaVersion = a} :: ExperimentTemplateLogConfiguration)

-- | The configuration for experiment logging to Amazon CloudWatch Logs.
experimentTemplateLogConfiguration_cloudWatchLogsConfiguration :: Lens.Lens' ExperimentTemplateLogConfiguration (Prelude.Maybe ExperimentTemplateCloudWatchLogsLogConfiguration)
experimentTemplateLogConfiguration_cloudWatchLogsConfiguration = Lens.lens (\ExperimentTemplateLogConfiguration' {cloudWatchLogsConfiguration} -> cloudWatchLogsConfiguration) (\s@ExperimentTemplateLogConfiguration' {} a -> s {cloudWatchLogsConfiguration = a} :: ExperimentTemplateLogConfiguration)

instance
  Data.FromJSON
    ExperimentTemplateLogConfiguration
  where
  parseJSON =
    Data.withObject
      "ExperimentTemplateLogConfiguration"
      ( \x ->
          ExperimentTemplateLogConfiguration'
            Prelude.<$> (x Data..:? "s3Configuration")
            Prelude.<*> (x Data..:? "logSchemaVersion")
            Prelude.<*> (x Data..:? "cloudWatchLogsConfiguration")
      )

instance
  Prelude.Hashable
    ExperimentTemplateLogConfiguration
  where
  hashWithSalt
    _salt
    ExperimentTemplateLogConfiguration' {..} =
      _salt `Prelude.hashWithSalt` s3Configuration
        `Prelude.hashWithSalt` logSchemaVersion
        `Prelude.hashWithSalt` cloudWatchLogsConfiguration

instance
  Prelude.NFData
    ExperimentTemplateLogConfiguration
  where
  rnf ExperimentTemplateLogConfiguration' {..} =
    Prelude.rnf s3Configuration
      `Prelude.seq` Prelude.rnf logSchemaVersion
      `Prelude.seq` Prelude.rnf cloudWatchLogsConfiguration
