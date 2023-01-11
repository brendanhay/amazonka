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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.RunConfigurationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.RunConfigurationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.ApplicationRestoreConfiguration
import Amazonka.KinesisAnalyticsV2.Types.FlinkRunConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes the updates to the starting parameters for a Kinesis Data
-- Analytics application.
--
-- /See:/ 'newRunConfigurationUpdate' smart constructor.
data RunConfigurationUpdate = RunConfigurationUpdate'
  { -- | Describes updates to the restore behavior of a restarting application.
    applicationRestoreConfiguration :: Prelude.Maybe ApplicationRestoreConfiguration,
    -- | Describes the starting parameters for a Flink-based Kinesis Data
    -- Analytics application.
    flinkRunConfiguration :: Prelude.Maybe FlinkRunConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationRestoreConfiguration', 'runConfigurationUpdate_applicationRestoreConfiguration' - Describes updates to the restore behavior of a restarting application.
--
-- 'flinkRunConfiguration', 'runConfigurationUpdate_flinkRunConfiguration' - Describes the starting parameters for a Flink-based Kinesis Data
-- Analytics application.
newRunConfigurationUpdate ::
  RunConfigurationUpdate
newRunConfigurationUpdate =
  RunConfigurationUpdate'
    { applicationRestoreConfiguration =
        Prelude.Nothing,
      flinkRunConfiguration = Prelude.Nothing
    }

-- | Describes updates to the restore behavior of a restarting application.
runConfigurationUpdate_applicationRestoreConfiguration :: Lens.Lens' RunConfigurationUpdate (Prelude.Maybe ApplicationRestoreConfiguration)
runConfigurationUpdate_applicationRestoreConfiguration = Lens.lens (\RunConfigurationUpdate' {applicationRestoreConfiguration} -> applicationRestoreConfiguration) (\s@RunConfigurationUpdate' {} a -> s {applicationRestoreConfiguration = a} :: RunConfigurationUpdate)

-- | Describes the starting parameters for a Flink-based Kinesis Data
-- Analytics application.
runConfigurationUpdate_flinkRunConfiguration :: Lens.Lens' RunConfigurationUpdate (Prelude.Maybe FlinkRunConfiguration)
runConfigurationUpdate_flinkRunConfiguration = Lens.lens (\RunConfigurationUpdate' {flinkRunConfiguration} -> flinkRunConfiguration) (\s@RunConfigurationUpdate' {} a -> s {flinkRunConfiguration = a} :: RunConfigurationUpdate)

instance Prelude.Hashable RunConfigurationUpdate where
  hashWithSalt _salt RunConfigurationUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` applicationRestoreConfiguration
      `Prelude.hashWithSalt` flinkRunConfiguration

instance Prelude.NFData RunConfigurationUpdate where
  rnf RunConfigurationUpdate' {..} =
    Prelude.rnf applicationRestoreConfiguration
      `Prelude.seq` Prelude.rnf flinkRunConfiguration

instance Data.ToJSON RunConfigurationUpdate where
  toJSON RunConfigurationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplicationRestoreConfiguration" Data..=)
              Prelude.<$> applicationRestoreConfiguration,
            ("FlinkRunConfiguration" Data..=)
              Prelude.<$> flinkRunConfiguration
          ]
      )
