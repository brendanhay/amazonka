{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.TargetTrackingScalingPolicyConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.TargetTrackingScalingPolicyConfiguration
  ( TargetTrackingScalingPolicyConfiguration (..),

    -- * Smart constructor
    mkTargetTrackingScalingPolicyConfiguration,

    -- * Lenses
    ttspcTargetValue,
    ttspcCustomizedMetricSpecification,
    ttspcDisableScaleIn,
    ttspcPredefinedMetricSpecification,
    ttspcScaleInCooldown,
    ttspcScaleOutCooldown,
  )
where

import qualified Network.AWS.ApplicationAutoScaling.Types.CustomizedMetricSpecification as Types
import qualified Network.AWS.ApplicationAutoScaling.Types.PredefinedMetricSpecification as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a target tracking scaling policy configuration to use with Application Auto Scaling.
--
-- /See:/ 'mkTargetTrackingScalingPolicyConfiguration' smart constructor.
data TargetTrackingScalingPolicyConfiguration = TargetTrackingScalingPolicyConfiguration'
  { -- | The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
    targetValue :: Core.Double,
    -- | A customized metric. You can specify either a predefined metric or a customized metric.
    customizedMetricSpecification :: Core.Maybe Types.CustomizedMetricSpecification,
    -- | Indicates whether scale in by the target tracking scaling policy is disabled. If the value is @true@ , scale in is disabled and the target tracking scaling policy won't remove capacity from the scalable target. Otherwise, scale in is enabled and the target tracking scaling policy can remove capacity from the scalable target. The default value is @false@ .
    disableScaleIn :: Core.Maybe Core.Bool,
    -- | A predefined metric. You can specify either a predefined metric or a customized metric.
    predefinedMetricSpecification :: Core.Maybe Types.PredefinedMetricSpecification,
    -- | The amount of time, in seconds, after a scale-in activity completes before another scale-in activity can start.
    --
    -- With the /scale-in cooldown period/ , the intention is to scale in conservatively to protect your application’s availability, so scale-in activities are blocked until the cooldown period has expired. However, if another alarm triggers a scale-out activity during the scale-in cooldown period, Application Auto Scaling scales out the target immediately. In this case, the scale-in cooldown period stops and doesn't complete.
    -- Application Auto Scaling provides a default value of 300 for the following scalable targets:
    --
    --     * ECS services
    --
    --
    --     * Spot Fleet requests
    --
    --
    --     * EMR clusters
    --
    --
    --     * AppStream 2.0 fleets
    --
    --
    --     * Aurora DB clusters
    --
    --
    --     * Amazon SageMaker endpoint variants
    --
    --
    --     * Custom resources
    --
    --
    -- For all other scalable targets, the default value is 0:
    --
    --     * DynamoDB tables
    --
    --
    --     * DynamoDB global secondary indexes
    --
    --
    --     * Amazon Comprehend document classification and entity recognizer endpoints
    --
    --
    --     * Lambda provisioned concurrency
    --
    --
    --     * Amazon Keyspaces tables
    --
    --
    --     * Amazon MSK cluster storage
    scaleInCooldown :: Core.Maybe Core.Int,
    -- | The amount of time, in seconds, to wait for a previous scale-out activity to take effect.
    --
    -- With the /scale-out cooldown period/ , the intention is to continuously (but not excessively) scale out. After Application Auto Scaling successfully scales out using a target tracking scaling policy, it starts to calculate the cooldown time. The scaling policy won't increase the desired capacity again unless either a larger scale out is triggered or the cooldown period ends. While the cooldown period is in effect, the capacity added by the initiating scale-out activity is calculated as part of the desired capacity for the next scale-out activity.
    -- Application Auto Scaling provides a default value of 300 for the following scalable targets:
    --
    --     * ECS services
    --
    --
    --     * Spot Fleet requests
    --
    --
    --     * EMR clusters
    --
    --
    --     * AppStream 2.0 fleets
    --
    --
    --     * Aurora DB clusters
    --
    --
    --     * Amazon SageMaker endpoint variants
    --
    --
    --     * Custom resources
    --
    --
    -- For all other scalable targets, the default value is 0:
    --
    --     * DynamoDB tables
    --
    --
    --     * DynamoDB global secondary indexes
    --
    --
    --     * Amazon Comprehend document classification and entity recognizer endpoints
    --
    --
    --     * Lambda provisioned concurrency
    --
    --
    --     * Amazon Keyspaces tables
    --
    --
    --     * Amazon MSK cluster storage
    scaleOutCooldown :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetTrackingScalingPolicyConfiguration' value with any optional fields omitted.
mkTargetTrackingScalingPolicyConfiguration ::
  -- | 'targetValue'
  Core.Double ->
  TargetTrackingScalingPolicyConfiguration
mkTargetTrackingScalingPolicyConfiguration targetValue =
  TargetTrackingScalingPolicyConfiguration'
    { targetValue,
      customizedMetricSpecification = Core.Nothing,
      disableScaleIn = Core.Nothing,
      predefinedMetricSpecification = Core.Nothing,
      scaleInCooldown = Core.Nothing,
      scaleOutCooldown = Core.Nothing
    }

-- | The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
--
-- /Note:/ Consider using 'targetValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttspcTargetValue :: Lens.Lens' TargetTrackingScalingPolicyConfiguration Core.Double
ttspcTargetValue = Lens.field @"targetValue"
{-# DEPRECATED ttspcTargetValue "Use generic-lens or generic-optics with 'targetValue' instead." #-}

-- | A customized metric. You can specify either a predefined metric or a customized metric.
--
-- /Note:/ Consider using 'customizedMetricSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttspcCustomizedMetricSpecification :: Lens.Lens' TargetTrackingScalingPolicyConfiguration (Core.Maybe Types.CustomizedMetricSpecification)
ttspcCustomizedMetricSpecification = Lens.field @"customizedMetricSpecification"
{-# DEPRECATED ttspcCustomizedMetricSpecification "Use generic-lens or generic-optics with 'customizedMetricSpecification' instead." #-}

-- | Indicates whether scale in by the target tracking scaling policy is disabled. If the value is @true@ , scale in is disabled and the target tracking scaling policy won't remove capacity from the scalable target. Otherwise, scale in is enabled and the target tracking scaling policy can remove capacity from the scalable target. The default value is @false@ .
--
-- /Note:/ Consider using 'disableScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttspcDisableScaleIn :: Lens.Lens' TargetTrackingScalingPolicyConfiguration (Core.Maybe Core.Bool)
ttspcDisableScaleIn = Lens.field @"disableScaleIn"
{-# DEPRECATED ttspcDisableScaleIn "Use generic-lens or generic-optics with 'disableScaleIn' instead." #-}

-- | A predefined metric. You can specify either a predefined metric or a customized metric.
--
-- /Note:/ Consider using 'predefinedMetricSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttspcPredefinedMetricSpecification :: Lens.Lens' TargetTrackingScalingPolicyConfiguration (Core.Maybe Types.PredefinedMetricSpecification)
ttspcPredefinedMetricSpecification = Lens.field @"predefinedMetricSpecification"
{-# DEPRECATED ttspcPredefinedMetricSpecification "Use generic-lens or generic-optics with 'predefinedMetricSpecification' instead." #-}

-- | The amount of time, in seconds, after a scale-in activity completes before another scale-in activity can start.
--
-- With the /scale-in cooldown period/ , the intention is to scale in conservatively to protect your application’s availability, so scale-in activities are blocked until the cooldown period has expired. However, if another alarm triggers a scale-out activity during the scale-in cooldown period, Application Auto Scaling scales out the target immediately. In this case, the scale-in cooldown period stops and doesn't complete.
-- Application Auto Scaling provides a default value of 300 for the following scalable targets:
--
--     * ECS services
--
--
--     * Spot Fleet requests
--
--
--     * EMR clusters
--
--
--     * AppStream 2.0 fleets
--
--
--     * Aurora DB clusters
--
--
--     * Amazon SageMaker endpoint variants
--
--
--     * Custom resources
--
--
-- For all other scalable targets, the default value is 0:
--
--     * DynamoDB tables
--
--
--     * DynamoDB global secondary indexes
--
--
--     * Amazon Comprehend document classification and entity recognizer endpoints
--
--
--     * Lambda provisioned concurrency
--
--
--     * Amazon Keyspaces tables
--
--
--     * Amazon MSK cluster storage
--
--
--
-- /Note:/ Consider using 'scaleInCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttspcScaleInCooldown :: Lens.Lens' TargetTrackingScalingPolicyConfiguration (Core.Maybe Core.Int)
ttspcScaleInCooldown = Lens.field @"scaleInCooldown"
{-# DEPRECATED ttspcScaleInCooldown "Use generic-lens or generic-optics with 'scaleInCooldown' instead." #-}

-- | The amount of time, in seconds, to wait for a previous scale-out activity to take effect.
--
-- With the /scale-out cooldown period/ , the intention is to continuously (but not excessively) scale out. After Application Auto Scaling successfully scales out using a target tracking scaling policy, it starts to calculate the cooldown time. The scaling policy won't increase the desired capacity again unless either a larger scale out is triggered or the cooldown period ends. While the cooldown period is in effect, the capacity added by the initiating scale-out activity is calculated as part of the desired capacity for the next scale-out activity.
-- Application Auto Scaling provides a default value of 300 for the following scalable targets:
--
--     * ECS services
--
--
--     * Spot Fleet requests
--
--
--     * EMR clusters
--
--
--     * AppStream 2.0 fleets
--
--
--     * Aurora DB clusters
--
--
--     * Amazon SageMaker endpoint variants
--
--
--     * Custom resources
--
--
-- For all other scalable targets, the default value is 0:
--
--     * DynamoDB tables
--
--
--     * DynamoDB global secondary indexes
--
--
--     * Amazon Comprehend document classification and entity recognizer endpoints
--
--
--     * Lambda provisioned concurrency
--
--
--     * Amazon Keyspaces tables
--
--
--     * Amazon MSK cluster storage
--
--
--
-- /Note:/ Consider using 'scaleOutCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttspcScaleOutCooldown :: Lens.Lens' TargetTrackingScalingPolicyConfiguration (Core.Maybe Core.Int)
ttspcScaleOutCooldown = Lens.field @"scaleOutCooldown"
{-# DEPRECATED ttspcScaleOutCooldown "Use generic-lens or generic-optics with 'scaleOutCooldown' instead." #-}

instance Core.FromJSON TargetTrackingScalingPolicyConfiguration where
  toJSON TargetTrackingScalingPolicyConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TargetValue" Core..= targetValue),
            ("CustomizedMetricSpecification" Core..=)
              Core.<$> customizedMetricSpecification,
            ("DisableScaleIn" Core..=) Core.<$> disableScaleIn,
            ("PredefinedMetricSpecification" Core..=)
              Core.<$> predefinedMetricSpecification,
            ("ScaleInCooldown" Core..=) Core.<$> scaleInCooldown,
            ("ScaleOutCooldown" Core..=) Core.<$> scaleOutCooldown
          ]
      )

instance Core.FromJSON TargetTrackingScalingPolicyConfiguration where
  parseJSON =
    Core.withObject "TargetTrackingScalingPolicyConfiguration" Core.$
      \x ->
        TargetTrackingScalingPolicyConfiguration'
          Core.<$> (x Core..: "TargetValue")
          Core.<*> (x Core..:? "CustomizedMetricSpecification")
          Core.<*> (x Core..:? "DisableScaleIn")
          Core.<*> (x Core..:? "PredefinedMetricSpecification")
          Core.<*> (x Core..:? "ScaleInCooldown")
          Core.<*> (x Core..:? "ScaleOutCooldown")
