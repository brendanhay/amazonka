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
-- Module      : Network.AWS.SageMaker.Types.DataQualityBaselineConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DataQualityBaselineConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.MonitoringConstraintsResource
import Network.AWS.SageMaker.Types.MonitoringStatisticsResource

-- | Configuration for monitoring constraints and monitoring statistics.
-- These baseline resources are compared against the results of the current
-- job from the series of jobs scheduled to collect data periodically.
--
-- /See:/ 'newDataQualityBaselineConfig' smart constructor.
data DataQualityBaselineConfig = DataQualityBaselineConfig'
  { statisticsResource :: Core.Maybe MonitoringStatisticsResource,
    constraintsResource :: Core.Maybe MonitoringConstraintsResource,
    -- | The name of the job that performs baselining for the data quality
    -- monitoring job.
    baseliningJobName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DataQualityBaselineConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statisticsResource', 'dataQualityBaselineConfig_statisticsResource' - Undocumented member.
--
-- 'constraintsResource', 'dataQualityBaselineConfig_constraintsResource' - Undocumented member.
--
-- 'baseliningJobName', 'dataQualityBaselineConfig_baseliningJobName' - The name of the job that performs baselining for the data quality
-- monitoring job.
newDataQualityBaselineConfig ::
  DataQualityBaselineConfig
newDataQualityBaselineConfig =
  DataQualityBaselineConfig'
    { statisticsResource =
        Core.Nothing,
      constraintsResource = Core.Nothing,
      baseliningJobName = Core.Nothing
    }

-- | Undocumented member.
dataQualityBaselineConfig_statisticsResource :: Lens.Lens' DataQualityBaselineConfig (Core.Maybe MonitoringStatisticsResource)
dataQualityBaselineConfig_statisticsResource = Lens.lens (\DataQualityBaselineConfig' {statisticsResource} -> statisticsResource) (\s@DataQualityBaselineConfig' {} a -> s {statisticsResource = a} :: DataQualityBaselineConfig)

-- | Undocumented member.
dataQualityBaselineConfig_constraintsResource :: Lens.Lens' DataQualityBaselineConfig (Core.Maybe MonitoringConstraintsResource)
dataQualityBaselineConfig_constraintsResource = Lens.lens (\DataQualityBaselineConfig' {constraintsResource} -> constraintsResource) (\s@DataQualityBaselineConfig' {} a -> s {constraintsResource = a} :: DataQualityBaselineConfig)

-- | The name of the job that performs baselining for the data quality
-- monitoring job.
dataQualityBaselineConfig_baseliningJobName :: Lens.Lens' DataQualityBaselineConfig (Core.Maybe Core.Text)
dataQualityBaselineConfig_baseliningJobName = Lens.lens (\DataQualityBaselineConfig' {baseliningJobName} -> baseliningJobName) (\s@DataQualityBaselineConfig' {} a -> s {baseliningJobName = a} :: DataQualityBaselineConfig)

instance Core.FromJSON DataQualityBaselineConfig where
  parseJSON =
    Core.withObject
      "DataQualityBaselineConfig"
      ( \x ->
          DataQualityBaselineConfig'
            Core.<$> (x Core..:? "StatisticsResource")
            Core.<*> (x Core..:? "ConstraintsResource")
            Core.<*> (x Core..:? "BaseliningJobName")
      )

instance Core.Hashable DataQualityBaselineConfig

instance Core.NFData DataQualityBaselineConfig

instance Core.ToJSON DataQualityBaselineConfig where
  toJSON DataQualityBaselineConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("StatisticsResource" Core..=)
              Core.<$> statisticsResource,
            ("ConstraintsResource" Core..=)
              Core.<$> constraintsResource,
            ("BaseliningJobName" Core..=)
              Core.<$> baseliningJobName
          ]
      )
