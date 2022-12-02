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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.FlinkApplicationConfigurationUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.FlinkApplicationConfigurationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.CheckpointConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.MonitoringConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.ParallelismConfigurationUpdate
import qualified Amazonka.Prelude as Prelude

-- | Describes updates to the configuration parameters for a Flink-based
-- Kinesis Data Analytics application.
--
-- /See:/ 'newFlinkApplicationConfigurationUpdate' smart constructor.
data FlinkApplicationConfigurationUpdate = FlinkApplicationConfigurationUpdate'
  { -- | Describes updates to the configuration parameters for Amazon CloudWatch
    -- logging for an application.
    monitoringConfigurationUpdate :: Prelude.Maybe MonitoringConfigurationUpdate,
    -- | Describes updates to the parameters for how an application executes
    -- multiple tasks simultaneously.
    parallelismConfigurationUpdate :: Prelude.Maybe ParallelismConfigurationUpdate,
    -- | Describes updates to an application\'s checkpointing configuration.
    -- Checkpointing is the process of persisting application state for fault
    -- tolerance.
    checkpointConfigurationUpdate :: Prelude.Maybe CheckpointConfigurationUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlinkApplicationConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringConfigurationUpdate', 'flinkApplicationConfigurationUpdate_monitoringConfigurationUpdate' - Describes updates to the configuration parameters for Amazon CloudWatch
-- logging for an application.
--
-- 'parallelismConfigurationUpdate', 'flinkApplicationConfigurationUpdate_parallelismConfigurationUpdate' - Describes updates to the parameters for how an application executes
-- multiple tasks simultaneously.
--
-- 'checkpointConfigurationUpdate', 'flinkApplicationConfigurationUpdate_checkpointConfigurationUpdate' - Describes updates to an application\'s checkpointing configuration.
-- Checkpointing is the process of persisting application state for fault
-- tolerance.
newFlinkApplicationConfigurationUpdate ::
  FlinkApplicationConfigurationUpdate
newFlinkApplicationConfigurationUpdate =
  FlinkApplicationConfigurationUpdate'
    { monitoringConfigurationUpdate =
        Prelude.Nothing,
      parallelismConfigurationUpdate =
        Prelude.Nothing,
      checkpointConfigurationUpdate =
        Prelude.Nothing
    }

-- | Describes updates to the configuration parameters for Amazon CloudWatch
-- logging for an application.
flinkApplicationConfigurationUpdate_monitoringConfigurationUpdate :: Lens.Lens' FlinkApplicationConfigurationUpdate (Prelude.Maybe MonitoringConfigurationUpdate)
flinkApplicationConfigurationUpdate_monitoringConfigurationUpdate = Lens.lens (\FlinkApplicationConfigurationUpdate' {monitoringConfigurationUpdate} -> monitoringConfigurationUpdate) (\s@FlinkApplicationConfigurationUpdate' {} a -> s {monitoringConfigurationUpdate = a} :: FlinkApplicationConfigurationUpdate)

-- | Describes updates to the parameters for how an application executes
-- multiple tasks simultaneously.
flinkApplicationConfigurationUpdate_parallelismConfigurationUpdate :: Lens.Lens' FlinkApplicationConfigurationUpdate (Prelude.Maybe ParallelismConfigurationUpdate)
flinkApplicationConfigurationUpdate_parallelismConfigurationUpdate = Lens.lens (\FlinkApplicationConfigurationUpdate' {parallelismConfigurationUpdate} -> parallelismConfigurationUpdate) (\s@FlinkApplicationConfigurationUpdate' {} a -> s {parallelismConfigurationUpdate = a} :: FlinkApplicationConfigurationUpdate)

-- | Describes updates to an application\'s checkpointing configuration.
-- Checkpointing is the process of persisting application state for fault
-- tolerance.
flinkApplicationConfigurationUpdate_checkpointConfigurationUpdate :: Lens.Lens' FlinkApplicationConfigurationUpdate (Prelude.Maybe CheckpointConfigurationUpdate)
flinkApplicationConfigurationUpdate_checkpointConfigurationUpdate = Lens.lens (\FlinkApplicationConfigurationUpdate' {checkpointConfigurationUpdate} -> checkpointConfigurationUpdate) (\s@FlinkApplicationConfigurationUpdate' {} a -> s {checkpointConfigurationUpdate = a} :: FlinkApplicationConfigurationUpdate)

instance
  Prelude.Hashable
    FlinkApplicationConfigurationUpdate
  where
  hashWithSalt
    _salt
    FlinkApplicationConfigurationUpdate' {..} =
      _salt
        `Prelude.hashWithSalt` monitoringConfigurationUpdate
        `Prelude.hashWithSalt` parallelismConfigurationUpdate
        `Prelude.hashWithSalt` checkpointConfigurationUpdate

instance
  Prelude.NFData
    FlinkApplicationConfigurationUpdate
  where
  rnf FlinkApplicationConfigurationUpdate' {..} =
    Prelude.rnf monitoringConfigurationUpdate
      `Prelude.seq` Prelude.rnf parallelismConfigurationUpdate
      `Prelude.seq` Prelude.rnf checkpointConfigurationUpdate

instance
  Data.ToJSON
    FlinkApplicationConfigurationUpdate
  where
  toJSON FlinkApplicationConfigurationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MonitoringConfigurationUpdate" Data..=)
              Prelude.<$> monitoringConfigurationUpdate,
            ("ParallelismConfigurationUpdate" Data..=)
              Prelude.<$> parallelismConfigurationUpdate,
            ("CheckpointConfigurationUpdate" Data..=)
              Prelude.<$> checkpointConfigurationUpdate
          ]
      )
