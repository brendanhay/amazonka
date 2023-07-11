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
-- Module      : Amazonka.SageMaker.Types.MonitoringBaselineConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringBaselineConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringConstraintsResource
import Amazonka.SageMaker.Types.MonitoringStatisticsResource

-- | Configuration for monitoring constraints and monitoring statistics.
-- These baseline resources are compared against the results of the current
-- job from the series of jobs scheduled to collect data periodically.
--
-- /See:/ 'newMonitoringBaselineConfig' smart constructor.
data MonitoringBaselineConfig = MonitoringBaselineConfig'
  { -- | The name of the job that performs baselining for the monitoring job.
    baseliningJobName :: Prelude.Maybe Prelude.Text,
    -- | The baseline constraint file in Amazon S3 that the current monitoring
    -- job should validated against.
    constraintsResource :: Prelude.Maybe MonitoringConstraintsResource,
    -- | The baseline statistics file in Amazon S3 that the current monitoring
    -- job should be validated against.
    statisticsResource :: Prelude.Maybe MonitoringStatisticsResource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringBaselineConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseliningJobName', 'monitoringBaselineConfig_baseliningJobName' - The name of the job that performs baselining for the monitoring job.
--
-- 'constraintsResource', 'monitoringBaselineConfig_constraintsResource' - The baseline constraint file in Amazon S3 that the current monitoring
-- job should validated against.
--
-- 'statisticsResource', 'monitoringBaselineConfig_statisticsResource' - The baseline statistics file in Amazon S3 that the current monitoring
-- job should be validated against.
newMonitoringBaselineConfig ::
  MonitoringBaselineConfig
newMonitoringBaselineConfig =
  MonitoringBaselineConfig'
    { baseliningJobName =
        Prelude.Nothing,
      constraintsResource = Prelude.Nothing,
      statisticsResource = Prelude.Nothing
    }

-- | The name of the job that performs baselining for the monitoring job.
monitoringBaselineConfig_baseliningJobName :: Lens.Lens' MonitoringBaselineConfig (Prelude.Maybe Prelude.Text)
monitoringBaselineConfig_baseliningJobName = Lens.lens (\MonitoringBaselineConfig' {baseliningJobName} -> baseliningJobName) (\s@MonitoringBaselineConfig' {} a -> s {baseliningJobName = a} :: MonitoringBaselineConfig)

-- | The baseline constraint file in Amazon S3 that the current monitoring
-- job should validated against.
monitoringBaselineConfig_constraintsResource :: Lens.Lens' MonitoringBaselineConfig (Prelude.Maybe MonitoringConstraintsResource)
monitoringBaselineConfig_constraintsResource = Lens.lens (\MonitoringBaselineConfig' {constraintsResource} -> constraintsResource) (\s@MonitoringBaselineConfig' {} a -> s {constraintsResource = a} :: MonitoringBaselineConfig)

-- | The baseline statistics file in Amazon S3 that the current monitoring
-- job should be validated against.
monitoringBaselineConfig_statisticsResource :: Lens.Lens' MonitoringBaselineConfig (Prelude.Maybe MonitoringStatisticsResource)
monitoringBaselineConfig_statisticsResource = Lens.lens (\MonitoringBaselineConfig' {statisticsResource} -> statisticsResource) (\s@MonitoringBaselineConfig' {} a -> s {statisticsResource = a} :: MonitoringBaselineConfig)

instance Data.FromJSON MonitoringBaselineConfig where
  parseJSON =
    Data.withObject
      "MonitoringBaselineConfig"
      ( \x ->
          MonitoringBaselineConfig'
            Prelude.<$> (x Data..:? "BaseliningJobName")
            Prelude.<*> (x Data..:? "ConstraintsResource")
            Prelude.<*> (x Data..:? "StatisticsResource")
      )

instance Prelude.Hashable MonitoringBaselineConfig where
  hashWithSalt _salt MonitoringBaselineConfig' {..} =
    _salt
      `Prelude.hashWithSalt` baseliningJobName
      `Prelude.hashWithSalt` constraintsResource
      `Prelude.hashWithSalt` statisticsResource

instance Prelude.NFData MonitoringBaselineConfig where
  rnf MonitoringBaselineConfig' {..} =
    Prelude.rnf baseliningJobName
      `Prelude.seq` Prelude.rnf constraintsResource
      `Prelude.seq` Prelude.rnf statisticsResource

instance Data.ToJSON MonitoringBaselineConfig where
  toJSON MonitoringBaselineConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BaseliningJobName" Data..=)
              Prelude.<$> baseliningJobName,
            ("ConstraintsResource" Data..=)
              Prelude.<$> constraintsResource,
            ("StatisticsResource" Data..=)
              Prelude.<$> statisticsResource
          ]
      )
