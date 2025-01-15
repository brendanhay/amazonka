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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ApplicationConfigurationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ApplicationConfigurationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.ApplicationCodeConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.ApplicationSnapshotConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.EnvironmentPropertyUpdates
import Amazonka.KinesisAnalyticsV2.Types.FlinkApplicationConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.SqlApplicationConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.VpcConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.ZeppelinApplicationConfigurationUpdate
import qualified Amazonka.Prelude as Prelude

-- | Describes updates to an application\'s configuration.
--
-- /See:/ 'newApplicationConfigurationUpdate' smart constructor.
data ApplicationConfigurationUpdate = ApplicationConfigurationUpdate'
  { -- | Describes updates to an application\'s code configuration.
    applicationCodeConfigurationUpdate :: Prelude.Maybe ApplicationCodeConfigurationUpdate,
    -- | Describes whether snapshots are enabled for a Flink-based Kinesis Data
    -- Analytics application.
    applicationSnapshotConfigurationUpdate :: Prelude.Maybe ApplicationSnapshotConfigurationUpdate,
    -- | Describes updates to the environment properties for a Flink-based
    -- Kinesis Data Analytics application.
    environmentPropertyUpdates :: Prelude.Maybe EnvironmentPropertyUpdates,
    -- | Describes updates to a Flink-based Kinesis Data Analytics application\'s
    -- configuration.
    flinkApplicationConfigurationUpdate :: Prelude.Maybe FlinkApplicationConfigurationUpdate,
    -- | Describes updates to a SQL-based Kinesis Data Analytics application\'s
    -- configuration.
    sqlApplicationConfigurationUpdate :: Prelude.Maybe SqlApplicationConfigurationUpdate,
    -- | Updates to the array of descriptions of VPC configurations available to
    -- the application.
    vpcConfigurationUpdates :: Prelude.Maybe [VpcConfigurationUpdate],
    -- | Updates to the configuration of a Kinesis Data Analytics Studio
    -- notebook.
    zeppelinApplicationConfigurationUpdate :: Prelude.Maybe ZeppelinApplicationConfigurationUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationCodeConfigurationUpdate', 'applicationConfigurationUpdate_applicationCodeConfigurationUpdate' - Describes updates to an application\'s code configuration.
--
-- 'applicationSnapshotConfigurationUpdate', 'applicationConfigurationUpdate_applicationSnapshotConfigurationUpdate' - Describes whether snapshots are enabled for a Flink-based Kinesis Data
-- Analytics application.
--
-- 'environmentPropertyUpdates', 'applicationConfigurationUpdate_environmentPropertyUpdates' - Describes updates to the environment properties for a Flink-based
-- Kinesis Data Analytics application.
--
-- 'flinkApplicationConfigurationUpdate', 'applicationConfigurationUpdate_flinkApplicationConfigurationUpdate' - Describes updates to a Flink-based Kinesis Data Analytics application\'s
-- configuration.
--
-- 'sqlApplicationConfigurationUpdate', 'applicationConfigurationUpdate_sqlApplicationConfigurationUpdate' - Describes updates to a SQL-based Kinesis Data Analytics application\'s
-- configuration.
--
-- 'vpcConfigurationUpdates', 'applicationConfigurationUpdate_vpcConfigurationUpdates' - Updates to the array of descriptions of VPC configurations available to
-- the application.
--
-- 'zeppelinApplicationConfigurationUpdate', 'applicationConfigurationUpdate_zeppelinApplicationConfigurationUpdate' - Updates to the configuration of a Kinesis Data Analytics Studio
-- notebook.
newApplicationConfigurationUpdate ::
  ApplicationConfigurationUpdate
newApplicationConfigurationUpdate =
  ApplicationConfigurationUpdate'
    { applicationCodeConfigurationUpdate =
        Prelude.Nothing,
      applicationSnapshotConfigurationUpdate =
        Prelude.Nothing,
      environmentPropertyUpdates =
        Prelude.Nothing,
      flinkApplicationConfigurationUpdate =
        Prelude.Nothing,
      sqlApplicationConfigurationUpdate =
        Prelude.Nothing,
      vpcConfigurationUpdates = Prelude.Nothing,
      zeppelinApplicationConfigurationUpdate =
        Prelude.Nothing
    }

-- | Describes updates to an application\'s code configuration.
applicationConfigurationUpdate_applicationCodeConfigurationUpdate :: Lens.Lens' ApplicationConfigurationUpdate (Prelude.Maybe ApplicationCodeConfigurationUpdate)
applicationConfigurationUpdate_applicationCodeConfigurationUpdate = Lens.lens (\ApplicationConfigurationUpdate' {applicationCodeConfigurationUpdate} -> applicationCodeConfigurationUpdate) (\s@ApplicationConfigurationUpdate' {} a -> s {applicationCodeConfigurationUpdate = a} :: ApplicationConfigurationUpdate)

-- | Describes whether snapshots are enabled for a Flink-based Kinesis Data
-- Analytics application.
applicationConfigurationUpdate_applicationSnapshotConfigurationUpdate :: Lens.Lens' ApplicationConfigurationUpdate (Prelude.Maybe ApplicationSnapshotConfigurationUpdate)
applicationConfigurationUpdate_applicationSnapshotConfigurationUpdate = Lens.lens (\ApplicationConfigurationUpdate' {applicationSnapshotConfigurationUpdate} -> applicationSnapshotConfigurationUpdate) (\s@ApplicationConfigurationUpdate' {} a -> s {applicationSnapshotConfigurationUpdate = a} :: ApplicationConfigurationUpdate)

-- | Describes updates to the environment properties for a Flink-based
-- Kinesis Data Analytics application.
applicationConfigurationUpdate_environmentPropertyUpdates :: Lens.Lens' ApplicationConfigurationUpdate (Prelude.Maybe EnvironmentPropertyUpdates)
applicationConfigurationUpdate_environmentPropertyUpdates = Lens.lens (\ApplicationConfigurationUpdate' {environmentPropertyUpdates} -> environmentPropertyUpdates) (\s@ApplicationConfigurationUpdate' {} a -> s {environmentPropertyUpdates = a} :: ApplicationConfigurationUpdate)

-- | Describes updates to a Flink-based Kinesis Data Analytics application\'s
-- configuration.
applicationConfigurationUpdate_flinkApplicationConfigurationUpdate :: Lens.Lens' ApplicationConfigurationUpdate (Prelude.Maybe FlinkApplicationConfigurationUpdate)
applicationConfigurationUpdate_flinkApplicationConfigurationUpdate = Lens.lens (\ApplicationConfigurationUpdate' {flinkApplicationConfigurationUpdate} -> flinkApplicationConfigurationUpdate) (\s@ApplicationConfigurationUpdate' {} a -> s {flinkApplicationConfigurationUpdate = a} :: ApplicationConfigurationUpdate)

-- | Describes updates to a SQL-based Kinesis Data Analytics application\'s
-- configuration.
applicationConfigurationUpdate_sqlApplicationConfigurationUpdate :: Lens.Lens' ApplicationConfigurationUpdate (Prelude.Maybe SqlApplicationConfigurationUpdate)
applicationConfigurationUpdate_sqlApplicationConfigurationUpdate = Lens.lens (\ApplicationConfigurationUpdate' {sqlApplicationConfigurationUpdate} -> sqlApplicationConfigurationUpdate) (\s@ApplicationConfigurationUpdate' {} a -> s {sqlApplicationConfigurationUpdate = a} :: ApplicationConfigurationUpdate)

-- | Updates to the array of descriptions of VPC configurations available to
-- the application.
applicationConfigurationUpdate_vpcConfigurationUpdates :: Lens.Lens' ApplicationConfigurationUpdate (Prelude.Maybe [VpcConfigurationUpdate])
applicationConfigurationUpdate_vpcConfigurationUpdates = Lens.lens (\ApplicationConfigurationUpdate' {vpcConfigurationUpdates} -> vpcConfigurationUpdates) (\s@ApplicationConfigurationUpdate' {} a -> s {vpcConfigurationUpdates = a} :: ApplicationConfigurationUpdate) Prelude.. Lens.mapping Lens.coerced

-- | Updates to the configuration of a Kinesis Data Analytics Studio
-- notebook.
applicationConfigurationUpdate_zeppelinApplicationConfigurationUpdate :: Lens.Lens' ApplicationConfigurationUpdate (Prelude.Maybe ZeppelinApplicationConfigurationUpdate)
applicationConfigurationUpdate_zeppelinApplicationConfigurationUpdate = Lens.lens (\ApplicationConfigurationUpdate' {zeppelinApplicationConfigurationUpdate} -> zeppelinApplicationConfigurationUpdate) (\s@ApplicationConfigurationUpdate' {} a -> s {zeppelinApplicationConfigurationUpdate = a} :: ApplicationConfigurationUpdate)

instance
  Prelude.Hashable
    ApplicationConfigurationUpdate
  where
  hashWithSalt
    _salt
    ApplicationConfigurationUpdate' {..} =
      _salt
        `Prelude.hashWithSalt` applicationCodeConfigurationUpdate
        `Prelude.hashWithSalt` applicationSnapshotConfigurationUpdate
        `Prelude.hashWithSalt` environmentPropertyUpdates
        `Prelude.hashWithSalt` flinkApplicationConfigurationUpdate
        `Prelude.hashWithSalt` sqlApplicationConfigurationUpdate
        `Prelude.hashWithSalt` vpcConfigurationUpdates
        `Prelude.hashWithSalt` zeppelinApplicationConfigurationUpdate

instance
  Prelude.NFData
    ApplicationConfigurationUpdate
  where
  rnf ApplicationConfigurationUpdate' {..} =
    Prelude.rnf applicationCodeConfigurationUpdate `Prelude.seq`
      Prelude.rnf applicationSnapshotConfigurationUpdate `Prelude.seq`
        Prelude.rnf environmentPropertyUpdates `Prelude.seq`
          Prelude.rnf flinkApplicationConfigurationUpdate `Prelude.seq`
            Prelude.rnf sqlApplicationConfigurationUpdate `Prelude.seq`
              Prelude.rnf vpcConfigurationUpdates `Prelude.seq`
                Prelude.rnf zeppelinApplicationConfigurationUpdate

instance Data.ToJSON ApplicationConfigurationUpdate where
  toJSON ApplicationConfigurationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplicationCodeConfigurationUpdate" Data..=)
              Prelude.<$> applicationCodeConfigurationUpdate,
            ("ApplicationSnapshotConfigurationUpdate" Data..=)
              Prelude.<$> applicationSnapshotConfigurationUpdate,
            ("EnvironmentPropertyUpdates" Data..=)
              Prelude.<$> environmentPropertyUpdates,
            ("FlinkApplicationConfigurationUpdate" Data..=)
              Prelude.<$> flinkApplicationConfigurationUpdate,
            ("SqlApplicationConfigurationUpdate" Data..=)
              Prelude.<$> sqlApplicationConfigurationUpdate,
            ("VpcConfigurationUpdates" Data..=)
              Prelude.<$> vpcConfigurationUpdates,
            ("ZeppelinApplicationConfigurationUpdate" Data..=)
              Prelude.<$> zeppelinApplicationConfigurationUpdate
          ]
      )
