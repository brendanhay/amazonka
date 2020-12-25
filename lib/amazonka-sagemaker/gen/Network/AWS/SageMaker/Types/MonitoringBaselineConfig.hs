{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringBaselineConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringBaselineConfig
  ( MonitoringBaselineConfig (..),

    -- * Smart constructor
    mkMonitoringBaselineConfig,

    -- * Lenses
    mbcConstraintsResource,
    mbcStatisticsResource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.MonitoringConstraintsResource as Types
import qualified Network.AWS.SageMaker.Types.MonitoringStatisticsResource as Types

-- | Configuration for monitoring constraints and monitoring statistics. These baseline resources are compared against the results of the current job from the series of jobs scheduled to collect data periodically.
--
-- /See:/ 'mkMonitoringBaselineConfig' smart constructor.
data MonitoringBaselineConfig = MonitoringBaselineConfig'
  { -- | The baseline constraint file in Amazon S3 that the current monitoring job should validated against.
    constraintsResource :: Core.Maybe Types.MonitoringConstraintsResource,
    -- | The baseline statistics file in Amazon S3 that the current monitoring job should be validated against.
    statisticsResource :: Core.Maybe Types.MonitoringStatisticsResource
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MonitoringBaselineConfig' value with any optional fields omitted.
mkMonitoringBaselineConfig ::
  MonitoringBaselineConfig
mkMonitoringBaselineConfig =
  MonitoringBaselineConfig'
    { constraintsResource = Core.Nothing,
      statisticsResource = Core.Nothing
    }

-- | The baseline constraint file in Amazon S3 that the current monitoring job should validated against.
--
-- /Note:/ Consider using 'constraintsResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbcConstraintsResource :: Lens.Lens' MonitoringBaselineConfig (Core.Maybe Types.MonitoringConstraintsResource)
mbcConstraintsResource = Lens.field @"constraintsResource"
{-# DEPRECATED mbcConstraintsResource "Use generic-lens or generic-optics with 'constraintsResource' instead." #-}

-- | The baseline statistics file in Amazon S3 that the current monitoring job should be validated against.
--
-- /Note:/ Consider using 'statisticsResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbcStatisticsResource :: Lens.Lens' MonitoringBaselineConfig (Core.Maybe Types.MonitoringStatisticsResource)
mbcStatisticsResource = Lens.field @"statisticsResource"
{-# DEPRECATED mbcStatisticsResource "Use generic-lens or generic-optics with 'statisticsResource' instead." #-}

instance Core.FromJSON MonitoringBaselineConfig where
  toJSON MonitoringBaselineConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConstraintsResource" Core..=) Core.<$> constraintsResource,
            ("StatisticsResource" Core..=) Core.<$> statisticsResource
          ]
      )

instance Core.FromJSON MonitoringBaselineConfig where
  parseJSON =
    Core.withObject "MonitoringBaselineConfig" Core.$
      \x ->
        MonitoringBaselineConfig'
          Core.<$> (x Core..:? "ConstraintsResource")
          Core.<*> (x Core..:? "StatisticsResource")
