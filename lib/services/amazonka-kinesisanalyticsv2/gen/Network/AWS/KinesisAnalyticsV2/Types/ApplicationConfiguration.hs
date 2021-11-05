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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ApplicationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ApplicationConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.KinesisAnalyticsV2.Types.ApplicationCodeConfiguration
import Amazonka.KinesisAnalyticsV2.Types.ApplicationSnapshotConfiguration
import Amazonka.KinesisAnalyticsV2.Types.EnvironmentProperties
import Amazonka.KinesisAnalyticsV2.Types.FlinkApplicationConfiguration
import Amazonka.KinesisAnalyticsV2.Types.SqlApplicationConfiguration
import Amazonka.KinesisAnalyticsV2.Types.VpcConfiguration
import Amazonka.KinesisAnalyticsV2.Types.ZeppelinApplicationConfiguration
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the creation parameters for a Kinesis Data Analytics
-- application.
--
-- /See:/ 'newApplicationConfiguration' smart constructor.
data ApplicationConfiguration = ApplicationConfiguration'
  { -- | Describes whether snapshots are enabled for a Flink-based Kinesis Data
    -- Analytics application.
    applicationSnapshotConfiguration :: Prelude.Maybe ApplicationSnapshotConfiguration,
    -- | The array of descriptions of VPC configurations available to the
    -- application.
    vpcConfigurations :: Prelude.Maybe [VpcConfiguration],
    -- | Describes execution properties for a Flink-based Kinesis Data Analytics
    -- application.
    environmentProperties :: Prelude.Maybe EnvironmentProperties,
    -- | The creation and update parameters for a Flink-based Kinesis Data
    -- Analytics application.
    flinkApplicationConfiguration :: Prelude.Maybe FlinkApplicationConfiguration,
    -- | The creation and update parameters for a SQL-based Kinesis Data
    -- Analytics application.
    sqlApplicationConfiguration :: Prelude.Maybe SqlApplicationConfiguration,
    -- | The code location and type parameters for a Flink-based Kinesis Data
    -- Analytics application.
    applicationCodeConfiguration :: Prelude.Maybe ApplicationCodeConfiguration,
    -- | The configuration parameters for a Kinesis Data Analytics Studio
    -- notebook.
    zeppelinApplicationConfiguration :: Prelude.Maybe ZeppelinApplicationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationSnapshotConfiguration', 'applicationConfiguration_applicationSnapshotConfiguration' - Describes whether snapshots are enabled for a Flink-based Kinesis Data
-- Analytics application.
--
-- 'vpcConfigurations', 'applicationConfiguration_vpcConfigurations' - The array of descriptions of VPC configurations available to the
-- application.
--
-- 'environmentProperties', 'applicationConfiguration_environmentProperties' - Describes execution properties for a Flink-based Kinesis Data Analytics
-- application.
--
-- 'flinkApplicationConfiguration', 'applicationConfiguration_flinkApplicationConfiguration' - The creation and update parameters for a Flink-based Kinesis Data
-- Analytics application.
--
-- 'sqlApplicationConfiguration', 'applicationConfiguration_sqlApplicationConfiguration' - The creation and update parameters for a SQL-based Kinesis Data
-- Analytics application.
--
-- 'applicationCodeConfiguration', 'applicationConfiguration_applicationCodeConfiguration' - The code location and type parameters for a Flink-based Kinesis Data
-- Analytics application.
--
-- 'zeppelinApplicationConfiguration', 'applicationConfiguration_zeppelinApplicationConfiguration' - The configuration parameters for a Kinesis Data Analytics Studio
-- notebook.
newApplicationConfiguration ::
  ApplicationConfiguration
newApplicationConfiguration =
  ApplicationConfiguration'
    { applicationSnapshotConfiguration =
        Prelude.Nothing,
      vpcConfigurations = Prelude.Nothing,
      environmentProperties = Prelude.Nothing,
      flinkApplicationConfiguration = Prelude.Nothing,
      sqlApplicationConfiguration = Prelude.Nothing,
      applicationCodeConfiguration = Prelude.Nothing,
      zeppelinApplicationConfiguration =
        Prelude.Nothing
    }

-- | Describes whether snapshots are enabled for a Flink-based Kinesis Data
-- Analytics application.
applicationConfiguration_applicationSnapshotConfiguration :: Lens.Lens' ApplicationConfiguration (Prelude.Maybe ApplicationSnapshotConfiguration)
applicationConfiguration_applicationSnapshotConfiguration = Lens.lens (\ApplicationConfiguration' {applicationSnapshotConfiguration} -> applicationSnapshotConfiguration) (\s@ApplicationConfiguration' {} a -> s {applicationSnapshotConfiguration = a} :: ApplicationConfiguration)

-- | The array of descriptions of VPC configurations available to the
-- application.
applicationConfiguration_vpcConfigurations :: Lens.Lens' ApplicationConfiguration (Prelude.Maybe [VpcConfiguration])
applicationConfiguration_vpcConfigurations = Lens.lens (\ApplicationConfiguration' {vpcConfigurations} -> vpcConfigurations) (\s@ApplicationConfiguration' {} a -> s {vpcConfigurations = a} :: ApplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Describes execution properties for a Flink-based Kinesis Data Analytics
-- application.
applicationConfiguration_environmentProperties :: Lens.Lens' ApplicationConfiguration (Prelude.Maybe EnvironmentProperties)
applicationConfiguration_environmentProperties = Lens.lens (\ApplicationConfiguration' {environmentProperties} -> environmentProperties) (\s@ApplicationConfiguration' {} a -> s {environmentProperties = a} :: ApplicationConfiguration)

-- | The creation and update parameters for a Flink-based Kinesis Data
-- Analytics application.
applicationConfiguration_flinkApplicationConfiguration :: Lens.Lens' ApplicationConfiguration (Prelude.Maybe FlinkApplicationConfiguration)
applicationConfiguration_flinkApplicationConfiguration = Lens.lens (\ApplicationConfiguration' {flinkApplicationConfiguration} -> flinkApplicationConfiguration) (\s@ApplicationConfiguration' {} a -> s {flinkApplicationConfiguration = a} :: ApplicationConfiguration)

-- | The creation and update parameters for a SQL-based Kinesis Data
-- Analytics application.
applicationConfiguration_sqlApplicationConfiguration :: Lens.Lens' ApplicationConfiguration (Prelude.Maybe SqlApplicationConfiguration)
applicationConfiguration_sqlApplicationConfiguration = Lens.lens (\ApplicationConfiguration' {sqlApplicationConfiguration} -> sqlApplicationConfiguration) (\s@ApplicationConfiguration' {} a -> s {sqlApplicationConfiguration = a} :: ApplicationConfiguration)

-- | The code location and type parameters for a Flink-based Kinesis Data
-- Analytics application.
applicationConfiguration_applicationCodeConfiguration :: Lens.Lens' ApplicationConfiguration (Prelude.Maybe ApplicationCodeConfiguration)
applicationConfiguration_applicationCodeConfiguration = Lens.lens (\ApplicationConfiguration' {applicationCodeConfiguration} -> applicationCodeConfiguration) (\s@ApplicationConfiguration' {} a -> s {applicationCodeConfiguration = a} :: ApplicationConfiguration)

-- | The configuration parameters for a Kinesis Data Analytics Studio
-- notebook.
applicationConfiguration_zeppelinApplicationConfiguration :: Lens.Lens' ApplicationConfiguration (Prelude.Maybe ZeppelinApplicationConfiguration)
applicationConfiguration_zeppelinApplicationConfiguration = Lens.lens (\ApplicationConfiguration' {zeppelinApplicationConfiguration} -> zeppelinApplicationConfiguration) (\s@ApplicationConfiguration' {} a -> s {zeppelinApplicationConfiguration = a} :: ApplicationConfiguration)

instance Prelude.Hashable ApplicationConfiguration

instance Prelude.NFData ApplicationConfiguration

instance Core.ToJSON ApplicationConfiguration where
  toJSON ApplicationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ApplicationSnapshotConfiguration" Core..=)
              Prelude.<$> applicationSnapshotConfiguration,
            ("VpcConfigurations" Core..=)
              Prelude.<$> vpcConfigurations,
            ("EnvironmentProperties" Core..=)
              Prelude.<$> environmentProperties,
            ("FlinkApplicationConfiguration" Core..=)
              Prelude.<$> flinkApplicationConfiguration,
            ("SqlApplicationConfiguration" Core..=)
              Prelude.<$> sqlApplicationConfiguration,
            ("ApplicationCodeConfiguration" Core..=)
              Prelude.<$> applicationCodeConfiguration,
            ("ZeppelinApplicationConfiguration" Core..=)
              Prelude.<$> zeppelinApplicationConfiguration
          ]
      )
