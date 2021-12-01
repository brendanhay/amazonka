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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ApplicationConfigurationDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ApplicationConfigurationDescription where

import qualified Amazonka.Core as Core
import Amazonka.KinesisAnalyticsV2.Types.ApplicationCodeConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.ApplicationSnapshotConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.EnvironmentPropertyDescriptions
import Amazonka.KinesisAnalyticsV2.Types.FlinkApplicationConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.RunConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.SqlApplicationConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.VpcConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.ZeppelinApplicationConfigurationDescription
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes details about the application code and starting parameters for
-- a Kinesis Data Analytics application.
--
-- /See:/ 'newApplicationConfigurationDescription' smart constructor.
data ApplicationConfigurationDescription = ApplicationConfigurationDescription'
  { -- | The configuration parameters for a Kinesis Data Analytics Studio
    -- notebook.
    zeppelinApplicationConfigurationDescription :: Prelude.Maybe ZeppelinApplicationConfigurationDescription,
    -- | The array of descriptions of VPC configurations available to the
    -- application.
    vpcConfigurationDescriptions :: Prelude.Maybe [VpcConfigurationDescription],
    -- | The details about the application code for a Flink-based Kinesis Data
    -- Analytics application.
    applicationCodeConfigurationDescription :: Prelude.Maybe ApplicationCodeConfigurationDescription,
    -- | Describes execution properties for a Flink-based Kinesis Data Analytics
    -- application.
    environmentPropertyDescriptions :: Prelude.Maybe EnvironmentPropertyDescriptions,
    -- | The details about the starting properties for a Kinesis Data Analytics
    -- application.
    runConfigurationDescription :: Prelude.Maybe RunConfigurationDescription,
    -- | The details about inputs, outputs, and reference data sources for a
    -- SQL-based Kinesis Data Analytics application.
    sqlApplicationConfigurationDescription :: Prelude.Maybe SqlApplicationConfigurationDescription,
    -- | The details about a Flink-based Kinesis Data Analytics application.
    flinkApplicationConfigurationDescription :: Prelude.Maybe FlinkApplicationConfigurationDescription,
    -- | Describes whether snapshots are enabled for a Flink-based Kinesis Data
    -- Analytics application.
    applicationSnapshotConfigurationDescription :: Prelude.Maybe ApplicationSnapshotConfigurationDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'zeppelinApplicationConfigurationDescription', 'applicationConfigurationDescription_zeppelinApplicationConfigurationDescription' - The configuration parameters for a Kinesis Data Analytics Studio
-- notebook.
--
-- 'vpcConfigurationDescriptions', 'applicationConfigurationDescription_vpcConfigurationDescriptions' - The array of descriptions of VPC configurations available to the
-- application.
--
-- 'applicationCodeConfigurationDescription', 'applicationConfigurationDescription_applicationCodeConfigurationDescription' - The details about the application code for a Flink-based Kinesis Data
-- Analytics application.
--
-- 'environmentPropertyDescriptions', 'applicationConfigurationDescription_environmentPropertyDescriptions' - Describes execution properties for a Flink-based Kinesis Data Analytics
-- application.
--
-- 'runConfigurationDescription', 'applicationConfigurationDescription_runConfigurationDescription' - The details about the starting properties for a Kinesis Data Analytics
-- application.
--
-- 'sqlApplicationConfigurationDescription', 'applicationConfigurationDescription_sqlApplicationConfigurationDescription' - The details about inputs, outputs, and reference data sources for a
-- SQL-based Kinesis Data Analytics application.
--
-- 'flinkApplicationConfigurationDescription', 'applicationConfigurationDescription_flinkApplicationConfigurationDescription' - The details about a Flink-based Kinesis Data Analytics application.
--
-- 'applicationSnapshotConfigurationDescription', 'applicationConfigurationDescription_applicationSnapshotConfigurationDescription' - Describes whether snapshots are enabled for a Flink-based Kinesis Data
-- Analytics application.
newApplicationConfigurationDescription ::
  ApplicationConfigurationDescription
newApplicationConfigurationDescription =
  ApplicationConfigurationDescription'
    { zeppelinApplicationConfigurationDescription =
        Prelude.Nothing,
      vpcConfigurationDescriptions =
        Prelude.Nothing,
      applicationCodeConfigurationDescription =
        Prelude.Nothing,
      environmentPropertyDescriptions =
        Prelude.Nothing,
      runConfigurationDescription =
        Prelude.Nothing,
      sqlApplicationConfigurationDescription =
        Prelude.Nothing,
      flinkApplicationConfigurationDescription =
        Prelude.Nothing,
      applicationSnapshotConfigurationDescription =
        Prelude.Nothing
    }

-- | The configuration parameters for a Kinesis Data Analytics Studio
-- notebook.
applicationConfigurationDescription_zeppelinApplicationConfigurationDescription :: Lens.Lens' ApplicationConfigurationDescription (Prelude.Maybe ZeppelinApplicationConfigurationDescription)
applicationConfigurationDescription_zeppelinApplicationConfigurationDescription = Lens.lens (\ApplicationConfigurationDescription' {zeppelinApplicationConfigurationDescription} -> zeppelinApplicationConfigurationDescription) (\s@ApplicationConfigurationDescription' {} a -> s {zeppelinApplicationConfigurationDescription = a} :: ApplicationConfigurationDescription)

-- | The array of descriptions of VPC configurations available to the
-- application.
applicationConfigurationDescription_vpcConfigurationDescriptions :: Lens.Lens' ApplicationConfigurationDescription (Prelude.Maybe [VpcConfigurationDescription])
applicationConfigurationDescription_vpcConfigurationDescriptions = Lens.lens (\ApplicationConfigurationDescription' {vpcConfigurationDescriptions} -> vpcConfigurationDescriptions) (\s@ApplicationConfigurationDescription' {} a -> s {vpcConfigurationDescriptions = a} :: ApplicationConfigurationDescription) Prelude.. Lens.mapping Lens.coerced

-- | The details about the application code for a Flink-based Kinesis Data
-- Analytics application.
applicationConfigurationDescription_applicationCodeConfigurationDescription :: Lens.Lens' ApplicationConfigurationDescription (Prelude.Maybe ApplicationCodeConfigurationDescription)
applicationConfigurationDescription_applicationCodeConfigurationDescription = Lens.lens (\ApplicationConfigurationDescription' {applicationCodeConfigurationDescription} -> applicationCodeConfigurationDescription) (\s@ApplicationConfigurationDescription' {} a -> s {applicationCodeConfigurationDescription = a} :: ApplicationConfigurationDescription)

-- | Describes execution properties for a Flink-based Kinesis Data Analytics
-- application.
applicationConfigurationDescription_environmentPropertyDescriptions :: Lens.Lens' ApplicationConfigurationDescription (Prelude.Maybe EnvironmentPropertyDescriptions)
applicationConfigurationDescription_environmentPropertyDescriptions = Lens.lens (\ApplicationConfigurationDescription' {environmentPropertyDescriptions} -> environmentPropertyDescriptions) (\s@ApplicationConfigurationDescription' {} a -> s {environmentPropertyDescriptions = a} :: ApplicationConfigurationDescription)

-- | The details about the starting properties for a Kinesis Data Analytics
-- application.
applicationConfigurationDescription_runConfigurationDescription :: Lens.Lens' ApplicationConfigurationDescription (Prelude.Maybe RunConfigurationDescription)
applicationConfigurationDescription_runConfigurationDescription = Lens.lens (\ApplicationConfigurationDescription' {runConfigurationDescription} -> runConfigurationDescription) (\s@ApplicationConfigurationDescription' {} a -> s {runConfigurationDescription = a} :: ApplicationConfigurationDescription)

-- | The details about inputs, outputs, and reference data sources for a
-- SQL-based Kinesis Data Analytics application.
applicationConfigurationDescription_sqlApplicationConfigurationDescription :: Lens.Lens' ApplicationConfigurationDescription (Prelude.Maybe SqlApplicationConfigurationDescription)
applicationConfigurationDescription_sqlApplicationConfigurationDescription = Lens.lens (\ApplicationConfigurationDescription' {sqlApplicationConfigurationDescription} -> sqlApplicationConfigurationDescription) (\s@ApplicationConfigurationDescription' {} a -> s {sqlApplicationConfigurationDescription = a} :: ApplicationConfigurationDescription)

-- | The details about a Flink-based Kinesis Data Analytics application.
applicationConfigurationDescription_flinkApplicationConfigurationDescription :: Lens.Lens' ApplicationConfigurationDescription (Prelude.Maybe FlinkApplicationConfigurationDescription)
applicationConfigurationDescription_flinkApplicationConfigurationDescription = Lens.lens (\ApplicationConfigurationDescription' {flinkApplicationConfigurationDescription} -> flinkApplicationConfigurationDescription) (\s@ApplicationConfigurationDescription' {} a -> s {flinkApplicationConfigurationDescription = a} :: ApplicationConfigurationDescription)

-- | Describes whether snapshots are enabled for a Flink-based Kinesis Data
-- Analytics application.
applicationConfigurationDescription_applicationSnapshotConfigurationDescription :: Lens.Lens' ApplicationConfigurationDescription (Prelude.Maybe ApplicationSnapshotConfigurationDescription)
applicationConfigurationDescription_applicationSnapshotConfigurationDescription = Lens.lens (\ApplicationConfigurationDescription' {applicationSnapshotConfigurationDescription} -> applicationSnapshotConfigurationDescription) (\s@ApplicationConfigurationDescription' {} a -> s {applicationSnapshotConfigurationDescription = a} :: ApplicationConfigurationDescription)

instance
  Core.FromJSON
    ApplicationConfigurationDescription
  where
  parseJSON =
    Core.withObject
      "ApplicationConfigurationDescription"
      ( \x ->
          ApplicationConfigurationDescription'
            Prelude.<$> ( x
                            Core..:? "ZeppelinApplicationConfigurationDescription"
                        )
            Prelude.<*> ( x Core..:? "VpcConfigurationDescriptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Core..:? "ApplicationCodeConfigurationDescription"
                        )
            Prelude.<*> (x Core..:? "EnvironmentPropertyDescriptions")
            Prelude.<*> (x Core..:? "RunConfigurationDescription")
            Prelude.<*> (x Core..:? "SqlApplicationConfigurationDescription")
            Prelude.<*> ( x
                            Core..:? "FlinkApplicationConfigurationDescription"
                        )
            Prelude.<*> ( x
                            Core..:? "ApplicationSnapshotConfigurationDescription"
                        )
      )

instance
  Prelude.Hashable
    ApplicationConfigurationDescription
  where
  hashWithSalt
    salt'
    ApplicationConfigurationDescription' {..} =
      salt'
        `Prelude.hashWithSalt` applicationSnapshotConfigurationDescription
        `Prelude.hashWithSalt` flinkApplicationConfigurationDescription
        `Prelude.hashWithSalt` sqlApplicationConfigurationDescription
        `Prelude.hashWithSalt` runConfigurationDescription
        `Prelude.hashWithSalt` environmentPropertyDescriptions
        `Prelude.hashWithSalt` applicationCodeConfigurationDescription
        `Prelude.hashWithSalt` vpcConfigurationDescriptions
        `Prelude.hashWithSalt` zeppelinApplicationConfigurationDescription

instance
  Prelude.NFData
    ApplicationConfigurationDescription
  where
  rnf ApplicationConfigurationDescription' {..} =
    Prelude.rnf
      zeppelinApplicationConfigurationDescription
      `Prelude.seq` Prelude.rnf
        applicationSnapshotConfigurationDescription
      `Prelude.seq` Prelude.rnf flinkApplicationConfigurationDescription
      `Prelude.seq` Prelude.rnf sqlApplicationConfigurationDescription
      `Prelude.seq` Prelude.rnf runConfigurationDescription
      `Prelude.seq` Prelude.rnf environmentPropertyDescriptions
      `Prelude.seq` Prelude.rnf applicationCodeConfigurationDescription
      `Prelude.seq` Prelude.rnf vpcConfigurationDescriptions
