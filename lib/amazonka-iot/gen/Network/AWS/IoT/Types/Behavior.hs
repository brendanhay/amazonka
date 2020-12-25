{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Behavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Behavior
  ( Behavior (..),

    -- * Smart constructor
    mkBehavior,

    -- * Lenses
    bName,
    bCriteria,
    bMetric,
    bMetricDimension,
  )
where

import qualified Network.AWS.IoT.Types.BehaviorCriteria as Types
import qualified Network.AWS.IoT.Types.BehaviorMetric as Types
import qualified Network.AWS.IoT.Types.BehaviorName as Types
import qualified Network.AWS.IoT.Types.MetricDimension as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A Device Defender security profile behavior.
--
-- /See:/ 'mkBehavior' smart constructor.
data Behavior = Behavior'
  { -- | The name you have given to the behavior.
    name :: Types.BehaviorName,
    -- | The criteria that determine if a device is behaving normally in regard to the @metric@ .
    criteria :: Core.Maybe Types.BehaviorCriteria,
    -- | What is measured by the behavior.
    metric :: Core.Maybe Types.BehaviorMetric,
    -- | The dimension for a metric in your behavior. For example, using a @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric only to MQTT topics whose name match the pattern specified in the dimension.
    metricDimension :: Core.Maybe Types.MetricDimension
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Behavior' value with any optional fields omitted.
mkBehavior ::
  -- | 'name'
  Types.BehaviorName ->
  Behavior
mkBehavior name =
  Behavior'
    { name,
      criteria = Core.Nothing,
      metric = Core.Nothing,
      metricDimension = Core.Nothing
    }

-- | The name you have given to the behavior.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bName :: Lens.Lens' Behavior Types.BehaviorName
bName = Lens.field @"name"
{-# DEPRECATED bName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The criteria that determine if a device is behaving normally in regard to the @metric@ .
--
-- /Note:/ Consider using 'criteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCriteria :: Lens.Lens' Behavior (Core.Maybe Types.BehaviorCriteria)
bCriteria = Lens.field @"criteria"
{-# DEPRECATED bCriteria "Use generic-lens or generic-optics with 'criteria' instead." #-}

-- | What is measured by the behavior.
--
-- /Note:/ Consider using 'metric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bMetric :: Lens.Lens' Behavior (Core.Maybe Types.BehaviorMetric)
bMetric = Lens.field @"metric"
{-# DEPRECATED bMetric "Use generic-lens or generic-optics with 'metric' instead." #-}

-- | The dimension for a metric in your behavior. For example, using a @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric only to MQTT topics whose name match the pattern specified in the dimension.
--
-- /Note:/ Consider using 'metricDimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bMetricDimension :: Lens.Lens' Behavior (Core.Maybe Types.MetricDimension)
bMetricDimension = Lens.field @"metricDimension"
{-# DEPRECATED bMetricDimension "Use generic-lens or generic-optics with 'metricDimension' instead." #-}

instance Core.FromJSON Behavior where
  toJSON Behavior {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            ("criteria" Core..=) Core.<$> criteria,
            ("metric" Core..=) Core.<$> metric,
            ("metricDimension" Core..=) Core.<$> metricDimension
          ]
      )

instance Core.FromJSON Behavior where
  parseJSON =
    Core.withObject "Behavior" Core.$
      \x ->
        Behavior'
          Core.<$> (x Core..: "name")
          Core.<*> (x Core..:? "criteria")
          Core.<*> (x Core..:? "metric")
          Core.<*> (x Core..:? "metricDimension")
