{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.StepAdjustment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.StepAdjustment
  ( StepAdjustment (..),

    -- * Smart constructor
    mkStepAdjustment,

    -- * Lenses
    saScalingAdjustment,
    saMetricIntervalLowerBound,
    saMetricIntervalUpperBound,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes information used to create a step adjustment for a step scaling policy.
--
-- For the following examples, suppose that you have an alarm with a breach threshold of 50:
--
--     * To trigger the adjustment when the metric is greater than or equal to 50 and less than 60, specify a lower bound of 0 and an upper bound of 10.
--
--
--     * To trigger the adjustment when the metric is greater than 40 and less than or equal to 50, specify a lower bound of -10 and an upper bound of 0.
--
--
-- There are a few rules for the step adjustments for your step policy:
--
--     * The ranges of your step adjustments can't overlap or have a gap.
--
--
--     * At most, one step adjustment can have a null lower bound. If one step adjustment has a negative lower bound, then there must be a step adjustment with a null lower bound.
--
--
--     * At most, one step adjustment can have a null upper bound. If one step adjustment has a positive upper bound, then there must be a step adjustment with a null upper bound.
--
--
--     * The upper and lower bound can't be null in the same step adjustment.
--
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-steps Step adjustments> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /See:/ 'mkStepAdjustment' smart constructor.
data StepAdjustment = StepAdjustment'
  { -- | The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity.
    scalingAdjustment :: Core.Int,
    -- | The lower bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the lower bound is inclusive (the metric must be greater than or equal to the threshold plus the lower bound). Otherwise, it is exclusive (the metric must be greater than the threshold plus the lower bound). A null value indicates negative infinity.
    metricIntervalLowerBound :: Core.Maybe Core.Double,
    -- | The upper bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the upper bound is exclusive (the metric must be less than the threshold plus the upper bound). Otherwise, it is inclusive (the metric must be less than or equal to the threshold plus the upper bound). A null value indicates positive infinity.
    --
    -- The upper bound must be greater than the lower bound.
    metricIntervalUpperBound :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StepAdjustment' value with any optional fields omitted.
mkStepAdjustment ::
  -- | 'scalingAdjustment'
  Core.Int ->
  StepAdjustment
mkStepAdjustment scalingAdjustment =
  StepAdjustment'
    { scalingAdjustment,
      metricIntervalLowerBound = Core.Nothing,
      metricIntervalUpperBound = Core.Nothing
    }

-- | The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity.
--
-- /Note:/ Consider using 'scalingAdjustment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saScalingAdjustment :: Lens.Lens' StepAdjustment Core.Int
saScalingAdjustment = Lens.field @"scalingAdjustment"
{-# DEPRECATED saScalingAdjustment "Use generic-lens or generic-optics with 'scalingAdjustment' instead." #-}

-- | The lower bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the lower bound is inclusive (the metric must be greater than or equal to the threshold plus the lower bound). Otherwise, it is exclusive (the metric must be greater than the threshold plus the lower bound). A null value indicates negative infinity.
--
-- /Note:/ Consider using 'metricIntervalLowerBound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saMetricIntervalLowerBound :: Lens.Lens' StepAdjustment (Core.Maybe Core.Double)
saMetricIntervalLowerBound = Lens.field @"metricIntervalLowerBound"
{-# DEPRECATED saMetricIntervalLowerBound "Use generic-lens or generic-optics with 'metricIntervalLowerBound' instead." #-}

-- | The upper bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the upper bound is exclusive (the metric must be less than the threshold plus the upper bound). Otherwise, it is inclusive (the metric must be less than or equal to the threshold plus the upper bound). A null value indicates positive infinity.
--
-- The upper bound must be greater than the lower bound.
--
-- /Note:/ Consider using 'metricIntervalUpperBound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saMetricIntervalUpperBound :: Lens.Lens' StepAdjustment (Core.Maybe Core.Double)
saMetricIntervalUpperBound = Lens.field @"metricIntervalUpperBound"
{-# DEPRECATED saMetricIntervalUpperBound "Use generic-lens or generic-optics with 'metricIntervalUpperBound' instead." #-}

instance Core.FromXML StepAdjustment where
  parseXML x =
    StepAdjustment'
      Core.<$> (x Core..@ "ScalingAdjustment")
      Core.<*> (x Core..@? "MetricIntervalLowerBound")
      Core.<*> (x Core..@? "MetricIntervalUpperBound")
