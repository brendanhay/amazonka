{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MetricToRetain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MetricToRetain
  ( MetricToRetain (..),

    -- * Smart constructor
    mkMetricToRetain,

    -- * Lenses
    mtrMetric,
    mtrMetricDimension,
  )
where

import qualified Network.AWS.IoT.Types.BehaviorMetric as Types
import qualified Network.AWS.IoT.Types.MetricDimension as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The metric you want to retain. Dimensions are optional.
--
-- /See:/ 'mkMetricToRetain' smart constructor.
data MetricToRetain = MetricToRetain'
  { -- | What is measured by the behavior.
    metric :: Types.BehaviorMetric,
    -- | The dimension of a metric.
    metricDimension :: Core.Maybe Types.MetricDimension
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricToRetain' value with any optional fields omitted.
mkMetricToRetain ::
  -- | 'metric'
  Types.BehaviorMetric ->
  MetricToRetain
mkMetricToRetain metric =
  MetricToRetain' {metric, metricDimension = Core.Nothing}

-- | What is measured by the behavior.
--
-- /Note:/ Consider using 'metric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtrMetric :: Lens.Lens' MetricToRetain Types.BehaviorMetric
mtrMetric = Lens.field @"metric"
{-# DEPRECATED mtrMetric "Use generic-lens or generic-optics with 'metric' instead." #-}

-- | The dimension of a metric.
--
-- /Note:/ Consider using 'metricDimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtrMetricDimension :: Lens.Lens' MetricToRetain (Core.Maybe Types.MetricDimension)
mtrMetricDimension = Lens.field @"metricDimension"
{-# DEPRECATED mtrMetricDimension "Use generic-lens or generic-optics with 'metricDimension' instead." #-}

instance Core.FromJSON MetricToRetain where
  toJSON MetricToRetain {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("metric" Core..= metric),
            ("metricDimension" Core..=) Core.<$> metricDimension
          ]
      )

instance Core.FromJSON MetricToRetain where
  parseJSON =
    Core.withObject "MetricToRetain" Core.$
      \x ->
        MetricToRetain'
          Core.<$> (x Core..: "metric") Core.<*> (x Core..:? "metricDimension")
