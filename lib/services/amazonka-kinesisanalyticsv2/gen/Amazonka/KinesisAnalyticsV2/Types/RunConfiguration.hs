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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.RunConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.RunConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.KinesisAnalyticsV2.Types.ApplicationRestoreConfiguration
import Amazonka.KinesisAnalyticsV2.Types.FlinkRunConfiguration
import Amazonka.KinesisAnalyticsV2.Types.SqlRunConfiguration
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the starting parameters for an Kinesis Data Analytics
-- application.
--
-- /See:/ 'newRunConfiguration' smart constructor.
data RunConfiguration = RunConfiguration'
  { -- | Describes the starting parameters for a SQL-based Kinesis Data Analytics
    -- application application.
    sqlRunConfigurations :: Prelude.Maybe [SqlRunConfiguration],
    -- | Describes the starting parameters for a Flink-based Kinesis Data
    -- Analytics application.
    flinkRunConfiguration :: Prelude.Maybe FlinkRunConfiguration,
    -- | Describes the restore behavior of a restarting application.
    applicationRestoreConfiguration :: Prelude.Maybe ApplicationRestoreConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sqlRunConfigurations', 'runConfiguration_sqlRunConfigurations' - Describes the starting parameters for a SQL-based Kinesis Data Analytics
-- application application.
--
-- 'flinkRunConfiguration', 'runConfiguration_flinkRunConfiguration' - Describes the starting parameters for a Flink-based Kinesis Data
-- Analytics application.
--
-- 'applicationRestoreConfiguration', 'runConfiguration_applicationRestoreConfiguration' - Describes the restore behavior of a restarting application.
newRunConfiguration ::
  RunConfiguration
newRunConfiguration =
  RunConfiguration'
    { sqlRunConfigurations =
        Prelude.Nothing,
      flinkRunConfiguration = Prelude.Nothing,
      applicationRestoreConfiguration = Prelude.Nothing
    }

-- | Describes the starting parameters for a SQL-based Kinesis Data Analytics
-- application application.
runConfiguration_sqlRunConfigurations :: Lens.Lens' RunConfiguration (Prelude.Maybe [SqlRunConfiguration])
runConfiguration_sqlRunConfigurations = Lens.lens (\RunConfiguration' {sqlRunConfigurations} -> sqlRunConfigurations) (\s@RunConfiguration' {} a -> s {sqlRunConfigurations = a} :: RunConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Describes the starting parameters for a Flink-based Kinesis Data
-- Analytics application.
runConfiguration_flinkRunConfiguration :: Lens.Lens' RunConfiguration (Prelude.Maybe FlinkRunConfiguration)
runConfiguration_flinkRunConfiguration = Lens.lens (\RunConfiguration' {flinkRunConfiguration} -> flinkRunConfiguration) (\s@RunConfiguration' {} a -> s {flinkRunConfiguration = a} :: RunConfiguration)

-- | Describes the restore behavior of a restarting application.
runConfiguration_applicationRestoreConfiguration :: Lens.Lens' RunConfiguration (Prelude.Maybe ApplicationRestoreConfiguration)
runConfiguration_applicationRestoreConfiguration = Lens.lens (\RunConfiguration' {applicationRestoreConfiguration} -> applicationRestoreConfiguration) (\s@RunConfiguration' {} a -> s {applicationRestoreConfiguration = a} :: RunConfiguration)

instance Prelude.Hashable RunConfiguration where
  hashWithSalt salt' RunConfiguration' {..} =
    salt'
      `Prelude.hashWithSalt` applicationRestoreConfiguration
      `Prelude.hashWithSalt` flinkRunConfiguration
      `Prelude.hashWithSalt` sqlRunConfigurations

instance Prelude.NFData RunConfiguration where
  rnf RunConfiguration' {..} =
    Prelude.rnf sqlRunConfigurations
      `Prelude.seq` Prelude.rnf applicationRestoreConfiguration
      `Prelude.seq` Prelude.rnf flinkRunConfiguration

instance Core.ToJSON RunConfiguration where
  toJSON RunConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SqlRunConfigurations" Core..=)
              Prelude.<$> sqlRunConfigurations,
            ("FlinkRunConfiguration" Core..=)
              Prelude.<$> flinkRunConfiguration,
            ("ApplicationRestoreConfiguration" Core..=)
              Prelude.<$> applicationRestoreConfiguration
          ]
      )
