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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.FlinkApplicationConfigurationDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.FlinkApplicationConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types.CheckpointConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.MonitoringConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.ParallelismConfigurationDescription
import qualified Amazonka.Prelude as Prelude

-- | Describes configuration parameters for a Flink-based Kinesis Data
-- Analytics application.
--
-- /See:/ 'newFlinkApplicationConfigurationDescription' smart constructor.
data FlinkApplicationConfigurationDescription = FlinkApplicationConfigurationDescription'
  { -- | Describes an application\'s checkpointing configuration. Checkpointing
    -- is the process of persisting application state for fault tolerance.
    checkpointConfigurationDescription :: Prelude.Maybe CheckpointConfigurationDescription,
    -- | The job plan for an application. For more information about the job
    -- plan, see
    -- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/internals/job_scheduling.html Jobs and Scheduling>
    -- in the
    -- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ Apache Flink Documentation>.
    -- To retrieve the job plan for the application, use the
    -- DescribeApplicationRequest$IncludeAdditionalDetails parameter of the
    -- DescribeApplication operation.
    jobPlanDescription :: Prelude.Maybe Prelude.Text,
    -- | Describes parameters for how an application executes multiple tasks
    -- simultaneously.
    parallelismConfigurationDescription :: Prelude.Maybe ParallelismConfigurationDescription,
    -- | Describes configuration parameters for Amazon CloudWatch logging for an
    -- application.
    monitoringConfigurationDescription :: Prelude.Maybe MonitoringConfigurationDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlinkApplicationConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkpointConfigurationDescription', 'flinkApplicationConfigurationDescription_checkpointConfigurationDescription' - Describes an application\'s checkpointing configuration. Checkpointing
-- is the process of persisting application state for fault tolerance.
--
-- 'jobPlanDescription', 'flinkApplicationConfigurationDescription_jobPlanDescription' - The job plan for an application. For more information about the job
-- plan, see
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/internals/job_scheduling.html Jobs and Scheduling>
-- in the
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ Apache Flink Documentation>.
-- To retrieve the job plan for the application, use the
-- DescribeApplicationRequest$IncludeAdditionalDetails parameter of the
-- DescribeApplication operation.
--
-- 'parallelismConfigurationDescription', 'flinkApplicationConfigurationDescription_parallelismConfigurationDescription' - Describes parameters for how an application executes multiple tasks
-- simultaneously.
--
-- 'monitoringConfigurationDescription', 'flinkApplicationConfigurationDescription_monitoringConfigurationDescription' - Describes configuration parameters for Amazon CloudWatch logging for an
-- application.
newFlinkApplicationConfigurationDescription ::
  FlinkApplicationConfigurationDescription
newFlinkApplicationConfigurationDescription =
  FlinkApplicationConfigurationDescription'
    { checkpointConfigurationDescription =
        Prelude.Nothing,
      jobPlanDescription =
        Prelude.Nothing,
      parallelismConfigurationDescription =
        Prelude.Nothing,
      monitoringConfigurationDescription =
        Prelude.Nothing
    }

-- | Describes an application\'s checkpointing configuration. Checkpointing
-- is the process of persisting application state for fault tolerance.
flinkApplicationConfigurationDescription_checkpointConfigurationDescription :: Lens.Lens' FlinkApplicationConfigurationDescription (Prelude.Maybe CheckpointConfigurationDescription)
flinkApplicationConfigurationDescription_checkpointConfigurationDescription = Lens.lens (\FlinkApplicationConfigurationDescription' {checkpointConfigurationDescription} -> checkpointConfigurationDescription) (\s@FlinkApplicationConfigurationDescription' {} a -> s {checkpointConfigurationDescription = a} :: FlinkApplicationConfigurationDescription)

-- | The job plan for an application. For more information about the job
-- plan, see
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/internals/job_scheduling.html Jobs and Scheduling>
-- in the
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ Apache Flink Documentation>.
-- To retrieve the job plan for the application, use the
-- DescribeApplicationRequest$IncludeAdditionalDetails parameter of the
-- DescribeApplication operation.
flinkApplicationConfigurationDescription_jobPlanDescription :: Lens.Lens' FlinkApplicationConfigurationDescription (Prelude.Maybe Prelude.Text)
flinkApplicationConfigurationDescription_jobPlanDescription = Lens.lens (\FlinkApplicationConfigurationDescription' {jobPlanDescription} -> jobPlanDescription) (\s@FlinkApplicationConfigurationDescription' {} a -> s {jobPlanDescription = a} :: FlinkApplicationConfigurationDescription)

-- | Describes parameters for how an application executes multiple tasks
-- simultaneously.
flinkApplicationConfigurationDescription_parallelismConfigurationDescription :: Lens.Lens' FlinkApplicationConfigurationDescription (Prelude.Maybe ParallelismConfigurationDescription)
flinkApplicationConfigurationDescription_parallelismConfigurationDescription = Lens.lens (\FlinkApplicationConfigurationDescription' {parallelismConfigurationDescription} -> parallelismConfigurationDescription) (\s@FlinkApplicationConfigurationDescription' {} a -> s {parallelismConfigurationDescription = a} :: FlinkApplicationConfigurationDescription)

-- | Describes configuration parameters for Amazon CloudWatch logging for an
-- application.
flinkApplicationConfigurationDescription_monitoringConfigurationDescription :: Lens.Lens' FlinkApplicationConfigurationDescription (Prelude.Maybe MonitoringConfigurationDescription)
flinkApplicationConfigurationDescription_monitoringConfigurationDescription = Lens.lens (\FlinkApplicationConfigurationDescription' {monitoringConfigurationDescription} -> monitoringConfigurationDescription) (\s@FlinkApplicationConfigurationDescription' {} a -> s {monitoringConfigurationDescription = a} :: FlinkApplicationConfigurationDescription)

instance
  Core.FromJSON
    FlinkApplicationConfigurationDescription
  where
  parseJSON =
    Core.withObject
      "FlinkApplicationConfigurationDescription"
      ( \x ->
          FlinkApplicationConfigurationDescription'
            Prelude.<$> (x Core..:? "CheckpointConfigurationDescription")
            Prelude.<*> (x Core..:? "JobPlanDescription")
            Prelude.<*> (x Core..:? "ParallelismConfigurationDescription")
            Prelude.<*> (x Core..:? "MonitoringConfigurationDescription")
      )

instance
  Prelude.Hashable
    FlinkApplicationConfigurationDescription
  where
  hashWithSalt
    _salt
    FlinkApplicationConfigurationDescription' {..} =
      _salt
        `Prelude.hashWithSalt` checkpointConfigurationDescription
        `Prelude.hashWithSalt` jobPlanDescription
        `Prelude.hashWithSalt` parallelismConfigurationDescription
        `Prelude.hashWithSalt` monitoringConfigurationDescription

instance
  Prelude.NFData
    FlinkApplicationConfigurationDescription
  where
  rnf FlinkApplicationConfigurationDescription' {..} =
    Prelude.rnf checkpointConfigurationDescription
      `Prelude.seq` Prelude.rnf jobPlanDescription
      `Prelude.seq` Prelude.rnf parallelismConfigurationDescription
      `Prelude.seq` Prelude.rnf monitoringConfigurationDescription
