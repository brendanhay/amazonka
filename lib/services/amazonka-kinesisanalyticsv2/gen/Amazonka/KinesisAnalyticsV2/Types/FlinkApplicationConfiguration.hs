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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.FlinkApplicationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.FlinkApplicationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types.CheckpointConfiguration
import Amazonka.KinesisAnalyticsV2.Types.MonitoringConfiguration
import Amazonka.KinesisAnalyticsV2.Types.ParallelismConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes configuration parameters for a Flink-based Kinesis Data
-- Analytics application or a Studio notebook.
--
-- /See:/ 'newFlinkApplicationConfiguration' smart constructor.
data FlinkApplicationConfiguration = FlinkApplicationConfiguration'
  { -- | Describes parameters for how an application executes multiple tasks
    -- simultaneously.
    parallelismConfiguration :: Prelude.Maybe ParallelismConfiguration,
    -- | Describes configuration parameters for Amazon CloudWatch logging for an
    -- application.
    monitoringConfiguration :: Prelude.Maybe MonitoringConfiguration,
    -- | Describes an application\'s checkpointing configuration. Checkpointing
    -- is the process of persisting application state for fault tolerance. For
    -- more information, see
    -- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/concepts/programming-model.html#checkpoints-for-fault-tolerance Checkpoints for Fault Tolerance>
    -- in the
    -- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ Apache Flink Documentation>.
    checkpointConfiguration :: Prelude.Maybe CheckpointConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlinkApplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parallelismConfiguration', 'flinkApplicationConfiguration_parallelismConfiguration' - Describes parameters for how an application executes multiple tasks
-- simultaneously.
--
-- 'monitoringConfiguration', 'flinkApplicationConfiguration_monitoringConfiguration' - Describes configuration parameters for Amazon CloudWatch logging for an
-- application.
--
-- 'checkpointConfiguration', 'flinkApplicationConfiguration_checkpointConfiguration' - Describes an application\'s checkpointing configuration. Checkpointing
-- is the process of persisting application state for fault tolerance. For
-- more information, see
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/concepts/programming-model.html#checkpoints-for-fault-tolerance Checkpoints for Fault Tolerance>
-- in the
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ Apache Flink Documentation>.
newFlinkApplicationConfiguration ::
  FlinkApplicationConfiguration
newFlinkApplicationConfiguration =
  FlinkApplicationConfiguration'
    { parallelismConfiguration =
        Prelude.Nothing,
      monitoringConfiguration = Prelude.Nothing,
      checkpointConfiguration = Prelude.Nothing
    }

-- | Describes parameters for how an application executes multiple tasks
-- simultaneously.
flinkApplicationConfiguration_parallelismConfiguration :: Lens.Lens' FlinkApplicationConfiguration (Prelude.Maybe ParallelismConfiguration)
flinkApplicationConfiguration_parallelismConfiguration = Lens.lens (\FlinkApplicationConfiguration' {parallelismConfiguration} -> parallelismConfiguration) (\s@FlinkApplicationConfiguration' {} a -> s {parallelismConfiguration = a} :: FlinkApplicationConfiguration)

-- | Describes configuration parameters for Amazon CloudWatch logging for an
-- application.
flinkApplicationConfiguration_monitoringConfiguration :: Lens.Lens' FlinkApplicationConfiguration (Prelude.Maybe MonitoringConfiguration)
flinkApplicationConfiguration_monitoringConfiguration = Lens.lens (\FlinkApplicationConfiguration' {monitoringConfiguration} -> monitoringConfiguration) (\s@FlinkApplicationConfiguration' {} a -> s {monitoringConfiguration = a} :: FlinkApplicationConfiguration)

-- | Describes an application\'s checkpointing configuration. Checkpointing
-- is the process of persisting application state for fault tolerance. For
-- more information, see
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/concepts/programming-model.html#checkpoints-for-fault-tolerance Checkpoints for Fault Tolerance>
-- in the
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ Apache Flink Documentation>.
flinkApplicationConfiguration_checkpointConfiguration :: Lens.Lens' FlinkApplicationConfiguration (Prelude.Maybe CheckpointConfiguration)
flinkApplicationConfiguration_checkpointConfiguration = Lens.lens (\FlinkApplicationConfiguration' {checkpointConfiguration} -> checkpointConfiguration) (\s@FlinkApplicationConfiguration' {} a -> s {checkpointConfiguration = a} :: FlinkApplicationConfiguration)

instance
  Prelude.Hashable
    FlinkApplicationConfiguration
  where
  hashWithSalt _salt FlinkApplicationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` parallelismConfiguration
      `Prelude.hashWithSalt` monitoringConfiguration
      `Prelude.hashWithSalt` checkpointConfiguration

instance Prelude.NFData FlinkApplicationConfiguration where
  rnf FlinkApplicationConfiguration' {..} =
    Prelude.rnf parallelismConfiguration
      `Prelude.seq` Prelude.rnf monitoringConfiguration
      `Prelude.seq` Prelude.rnf checkpointConfiguration

instance Core.ToJSON FlinkApplicationConfiguration where
  toJSON FlinkApplicationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ParallelismConfiguration" Core..=)
              Prelude.<$> parallelismConfiguration,
            ("MonitoringConfiguration" Core..=)
              Prelude.<$> monitoringConfiguration,
            ("CheckpointConfiguration" Core..=)
              Prelude.<$> checkpointConfiguration
          ]
      )
