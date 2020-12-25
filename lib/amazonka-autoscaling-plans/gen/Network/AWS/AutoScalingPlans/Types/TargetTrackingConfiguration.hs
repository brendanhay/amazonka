{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration
  ( TargetTrackingConfiguration (..),

    -- * Smart constructor
    mkTargetTrackingConfiguration,

    -- * Lenses
    ttcTargetValue,
    ttcCustomizedScalingMetricSpecification,
    ttcDisableScaleIn,
    ttcEstimatedInstanceWarmup,
    ttcPredefinedScalingMetricSpecification,
    ttcScaleInCooldown,
    ttcScaleOutCooldown,
  )
where

import qualified Network.AWS.AutoScalingPlans.Types.CustomizedScalingMetricSpecification as Types
import qualified Network.AWS.AutoScalingPlans.Types.PredefinedScalingMetricSpecification as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a target tracking configuration to use with AWS Auto Scaling. Used with 'ScalingInstruction' and 'ScalingPolicy' .
--
-- /See:/ 'mkTargetTrackingConfiguration' smart constructor.
data TargetTrackingConfiguration = TargetTrackingConfiguration'
  { -- | The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
    targetValue :: Core.Double,
    -- | A customized metric. You can specify either a predefined metric or a customized metric.
    customizedScalingMetricSpecification :: Core.Maybe Types.CustomizedScalingMetricSpecification,
    -- | Indicates whether scale in by the target tracking scaling policy is disabled. If the value is @true@ , scale in is disabled and the target tracking scaling policy doesn't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking scaling policy can remove capacity from the scalable resource.
    --
    -- The default value is @false@ .
    disableScaleIn :: Core.Maybe Core.Bool,
    -- | The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics. This value is used only if the resource is an Auto Scaling group.
    estimatedInstanceWarmup :: Core.Maybe Core.Int,
    -- | A predefined metric. You can specify either a predefined metric or a customized metric.
    predefinedScalingMetricSpecification :: Core.Maybe Types.PredefinedScalingMetricSpecification,
    -- | The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. This value is not used if the scalable resource is an Auto Scaling group.
    --
    -- The cooldown period is used to block subsequent scale in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale-out policy during the cooldown period after a scale-in, AWS Auto Scaling scales out your scalable target immediately.
    scaleInCooldown :: Core.Maybe Core.Int,
    -- | The amount of time, in seconds, after a scale-out activity completes before another scale-out activity can start. This value is not used if the scalable resource is an Auto Scaling group.
    --
    -- While the cooldown period is in effect, the capacity that has been added by the previous scale-out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out.
    scaleOutCooldown :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetTrackingConfiguration' value with any optional fields omitted.
mkTargetTrackingConfiguration ::
  -- | 'targetValue'
  Core.Double ->
  TargetTrackingConfiguration
mkTargetTrackingConfiguration targetValue =
  TargetTrackingConfiguration'
    { targetValue,
      customizedScalingMetricSpecification = Core.Nothing,
      disableScaleIn = Core.Nothing,
      estimatedInstanceWarmup = Core.Nothing,
      predefinedScalingMetricSpecification = Core.Nothing,
      scaleInCooldown = Core.Nothing,
      scaleOutCooldown = Core.Nothing
    }

-- | The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
--
-- /Note:/ Consider using 'targetValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttcTargetValue :: Lens.Lens' TargetTrackingConfiguration Core.Double
ttcTargetValue = Lens.field @"targetValue"
{-# DEPRECATED ttcTargetValue "Use generic-lens or generic-optics with 'targetValue' instead." #-}

-- | A customized metric. You can specify either a predefined metric or a customized metric.
--
-- /Note:/ Consider using 'customizedScalingMetricSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttcCustomizedScalingMetricSpecification :: Lens.Lens' TargetTrackingConfiguration (Core.Maybe Types.CustomizedScalingMetricSpecification)
ttcCustomizedScalingMetricSpecification = Lens.field @"customizedScalingMetricSpecification"
{-# DEPRECATED ttcCustomizedScalingMetricSpecification "Use generic-lens or generic-optics with 'customizedScalingMetricSpecification' instead." #-}

-- | Indicates whether scale in by the target tracking scaling policy is disabled. If the value is @true@ , scale in is disabled and the target tracking scaling policy doesn't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking scaling policy can remove capacity from the scalable resource.
--
-- The default value is @false@ .
--
-- /Note:/ Consider using 'disableScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttcDisableScaleIn :: Lens.Lens' TargetTrackingConfiguration (Core.Maybe Core.Bool)
ttcDisableScaleIn = Lens.field @"disableScaleIn"
{-# DEPRECATED ttcDisableScaleIn "Use generic-lens or generic-optics with 'disableScaleIn' instead." #-}

-- | The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics. This value is used only if the resource is an Auto Scaling group.
--
-- /Note:/ Consider using 'estimatedInstanceWarmup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttcEstimatedInstanceWarmup :: Lens.Lens' TargetTrackingConfiguration (Core.Maybe Core.Int)
ttcEstimatedInstanceWarmup = Lens.field @"estimatedInstanceWarmup"
{-# DEPRECATED ttcEstimatedInstanceWarmup "Use generic-lens or generic-optics with 'estimatedInstanceWarmup' instead." #-}

-- | A predefined metric. You can specify either a predefined metric or a customized metric.
--
-- /Note:/ Consider using 'predefinedScalingMetricSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttcPredefinedScalingMetricSpecification :: Lens.Lens' TargetTrackingConfiguration (Core.Maybe Types.PredefinedScalingMetricSpecification)
ttcPredefinedScalingMetricSpecification = Lens.field @"predefinedScalingMetricSpecification"
{-# DEPRECATED ttcPredefinedScalingMetricSpecification "Use generic-lens or generic-optics with 'predefinedScalingMetricSpecification' instead." #-}

-- | The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. This value is not used if the scalable resource is an Auto Scaling group.
--
-- The cooldown period is used to block subsequent scale in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale-out policy during the cooldown period after a scale-in, AWS Auto Scaling scales out your scalable target immediately.
--
-- /Note:/ Consider using 'scaleInCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttcScaleInCooldown :: Lens.Lens' TargetTrackingConfiguration (Core.Maybe Core.Int)
ttcScaleInCooldown = Lens.field @"scaleInCooldown"
{-# DEPRECATED ttcScaleInCooldown "Use generic-lens or generic-optics with 'scaleInCooldown' instead." #-}

-- | The amount of time, in seconds, after a scale-out activity completes before another scale-out activity can start. This value is not used if the scalable resource is an Auto Scaling group.
--
-- While the cooldown period is in effect, the capacity that has been added by the previous scale-out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out.
--
-- /Note:/ Consider using 'scaleOutCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttcScaleOutCooldown :: Lens.Lens' TargetTrackingConfiguration (Core.Maybe Core.Int)
ttcScaleOutCooldown = Lens.field @"scaleOutCooldown"
{-# DEPRECATED ttcScaleOutCooldown "Use generic-lens or generic-optics with 'scaleOutCooldown' instead." #-}

instance Core.FromJSON TargetTrackingConfiguration where
  toJSON TargetTrackingConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TargetValue" Core..= targetValue),
            ("CustomizedScalingMetricSpecification" Core..=)
              Core.<$> customizedScalingMetricSpecification,
            ("DisableScaleIn" Core..=) Core.<$> disableScaleIn,
            ("EstimatedInstanceWarmup" Core..=)
              Core.<$> estimatedInstanceWarmup,
            ("PredefinedScalingMetricSpecification" Core..=)
              Core.<$> predefinedScalingMetricSpecification,
            ("ScaleInCooldown" Core..=) Core.<$> scaleInCooldown,
            ("ScaleOutCooldown" Core..=) Core.<$> scaleOutCooldown
          ]
      )

instance Core.FromJSON TargetTrackingConfiguration where
  parseJSON =
    Core.withObject "TargetTrackingConfiguration" Core.$
      \x ->
        TargetTrackingConfiguration'
          Core.<$> (x Core..: "TargetValue")
          Core.<*> (x Core..:? "CustomizedScalingMetricSpecification")
          Core.<*> (x Core..:? "DisableScaleIn")
          Core.<*> (x Core..:? "EstimatedInstanceWarmup")
          Core.<*> (x Core..:? "PredefinedScalingMetricSpecification")
          Core.<*> (x Core..:? "ScaleInCooldown")
          Core.<*> (x Core..:? "ScaleOutCooldown")
