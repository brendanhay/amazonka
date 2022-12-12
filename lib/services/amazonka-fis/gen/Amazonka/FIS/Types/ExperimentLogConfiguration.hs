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
-- Module      : Amazonka.FIS.Types.ExperimentLogConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentLogConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types.ExperimentCloudWatchLogsLogConfiguration
import Amazonka.FIS.Types.ExperimentS3LogConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for experiment logging.
--
-- /See:/ 'newExperimentLogConfiguration' smart constructor.
data ExperimentLogConfiguration = ExperimentLogConfiguration'
  { -- | The configuration for experiment logging to Amazon CloudWatch Logs.
    cloudWatchLogsConfiguration :: Prelude.Maybe ExperimentCloudWatchLogsLogConfiguration,
    -- | The schema version.
    logSchemaVersion :: Prelude.Maybe Prelude.Int,
    -- | The configuration for experiment logging to Amazon S3.
    s3Configuration :: Prelude.Maybe ExperimentS3LogConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentLogConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogsConfiguration', 'experimentLogConfiguration_cloudWatchLogsConfiguration' - The configuration for experiment logging to Amazon CloudWatch Logs.
--
-- 'logSchemaVersion', 'experimentLogConfiguration_logSchemaVersion' - The schema version.
--
-- 's3Configuration', 'experimentLogConfiguration_s3Configuration' - The configuration for experiment logging to Amazon S3.
newExperimentLogConfiguration ::
  ExperimentLogConfiguration
newExperimentLogConfiguration =
  ExperimentLogConfiguration'
    { cloudWatchLogsConfiguration =
        Prelude.Nothing,
      logSchemaVersion = Prelude.Nothing,
      s3Configuration = Prelude.Nothing
    }

-- | The configuration for experiment logging to Amazon CloudWatch Logs.
experimentLogConfiguration_cloudWatchLogsConfiguration :: Lens.Lens' ExperimentLogConfiguration (Prelude.Maybe ExperimentCloudWatchLogsLogConfiguration)
experimentLogConfiguration_cloudWatchLogsConfiguration = Lens.lens (\ExperimentLogConfiguration' {cloudWatchLogsConfiguration} -> cloudWatchLogsConfiguration) (\s@ExperimentLogConfiguration' {} a -> s {cloudWatchLogsConfiguration = a} :: ExperimentLogConfiguration)

-- | The schema version.
experimentLogConfiguration_logSchemaVersion :: Lens.Lens' ExperimentLogConfiguration (Prelude.Maybe Prelude.Int)
experimentLogConfiguration_logSchemaVersion = Lens.lens (\ExperimentLogConfiguration' {logSchemaVersion} -> logSchemaVersion) (\s@ExperimentLogConfiguration' {} a -> s {logSchemaVersion = a} :: ExperimentLogConfiguration)

-- | The configuration for experiment logging to Amazon S3.
experimentLogConfiguration_s3Configuration :: Lens.Lens' ExperimentLogConfiguration (Prelude.Maybe ExperimentS3LogConfiguration)
experimentLogConfiguration_s3Configuration = Lens.lens (\ExperimentLogConfiguration' {s3Configuration} -> s3Configuration) (\s@ExperimentLogConfiguration' {} a -> s {s3Configuration = a} :: ExperimentLogConfiguration)

instance Data.FromJSON ExperimentLogConfiguration where
  parseJSON =
    Data.withObject
      "ExperimentLogConfiguration"
      ( \x ->
          ExperimentLogConfiguration'
            Prelude.<$> (x Data..:? "cloudWatchLogsConfiguration")
            Prelude.<*> (x Data..:? "logSchemaVersion")
            Prelude.<*> (x Data..:? "s3Configuration")
      )

instance Prelude.Hashable ExperimentLogConfiguration where
  hashWithSalt _salt ExperimentLogConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogsConfiguration
      `Prelude.hashWithSalt` logSchemaVersion
      `Prelude.hashWithSalt` s3Configuration

instance Prelude.NFData ExperimentLogConfiguration where
  rnf ExperimentLogConfiguration' {..} =
    Prelude.rnf cloudWatchLogsConfiguration
      `Prelude.seq` Prelude.rnf logSchemaVersion
      `Prelude.seq` Prelude.rnf s3Configuration
