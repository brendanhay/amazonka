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
    ttspcPredefinedMetricSpecification,
    ttspcScaleInCooldown,
    ttspcCustomizedMetricSpecification,
    ttspcDisableScaleIn,
    ttspcScaleOutCooldown,
    ttspcTargetValue,
  )
where

import Network.AWS.ApplicationAutoScaling.Types.CustomizedMetricSpecification
import Network.AWS.ApplicationAutoScaling.Types.PredefinedMetricSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a target tracking scaling policy configuration to use with Application Auto Scaling.
--
-- /See:/ 'mkTargetTrackingScalingPolicyConfiguration' smart constructor.
data TargetTrackingScalingPolicyConfiguration = TargetTrackingScalingPolicyConfiguration'
  { predefinedMetricSpecification ::
      Lude.Maybe
        PredefinedMetricSpecification,
    scaleInCooldown ::
      Lude.Maybe
        Lude.Int,
    customizedMetricSpecification ::
      Lude.Maybe
        CustomizedMetricSpecification,
    disableScaleIn ::
      Lude.Maybe
        Lude.Bool,
    scaleOutCooldown ::
      Lude.Maybe
        Lude.Int,
    targetValue ::
      Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetTrackingScalingPolicyConfiguration' with the minimum fields required to make a request.
--
-- * 'customizedMetricSpecification' - A customized metric. You can specify either a predefined metric or a customized metric.
-- * 'disableScaleIn' - Indicates whether scale in by the target tracking scaling policy is disabled. If the value is @true@ , scale in is disabled and the target tracking scaling policy won't remove capacity from the scalable target. Otherwise, scale in is enabled and the target tracking scaling policy can remove capacity from the scalable target. The default value is @false@ .
-- * 'predefinedMetricSpecification' - A predefined metric. You can specify either a predefined metric or a customized metric.
-- * 'scaleInCooldown' - The amount of time, in seconds, after a scale-in activity completes before another scale-in activity can start.
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
-- * 'scaleOutCooldown' - The amount of time, in seconds, to wait for a previous scale-out activity to take effect.
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
-- * 'targetValue' - The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
mkTargetTrackingScalingPolicyConfiguration ::
  -- | 'targetValue'
  Lude.Double ->
  TargetTrackingScalingPolicyConfiguration
mkTargetTrackingScalingPolicyConfiguration pTargetValue_ =
  TargetTrackingScalingPolicyConfiguration'
    { predefinedMetricSpecification =
        Lude.Nothing,
      scaleInCooldown = Lude.Nothing,
      customizedMetricSpecification = Lude.Nothing,
      disableScaleIn = Lude.Nothing,
      scaleOutCooldown = Lude.Nothing,
      targetValue = pTargetValue_
    }

-- | A predefined metric. You can specify either a predefined metric or a customized metric.
--
-- /Note:/ Consider using 'predefinedMetricSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttspcPredefinedMetricSpecification :: Lens.Lens' TargetTrackingScalingPolicyConfiguration (Lude.Maybe PredefinedMetricSpecification)
ttspcPredefinedMetricSpecification = Lens.lens (predefinedMetricSpecification :: TargetTrackingScalingPolicyConfiguration -> Lude.Maybe PredefinedMetricSpecification) (\s a -> s {predefinedMetricSpecification = a} :: TargetTrackingScalingPolicyConfiguration)
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
ttspcScaleInCooldown :: Lens.Lens' TargetTrackingScalingPolicyConfiguration (Lude.Maybe Lude.Int)
ttspcScaleInCooldown = Lens.lens (scaleInCooldown :: TargetTrackingScalingPolicyConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {scaleInCooldown = a} :: TargetTrackingScalingPolicyConfiguration)
{-# DEPRECATED ttspcScaleInCooldown "Use generic-lens or generic-optics with 'scaleInCooldown' instead." #-}

-- | A customized metric. You can specify either a predefined metric or a customized metric.
--
-- /Note:/ Consider using 'customizedMetricSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttspcCustomizedMetricSpecification :: Lens.Lens' TargetTrackingScalingPolicyConfiguration (Lude.Maybe CustomizedMetricSpecification)
ttspcCustomizedMetricSpecification = Lens.lens (customizedMetricSpecification :: TargetTrackingScalingPolicyConfiguration -> Lude.Maybe CustomizedMetricSpecification) (\s a -> s {customizedMetricSpecification = a} :: TargetTrackingScalingPolicyConfiguration)
{-# DEPRECATED ttspcCustomizedMetricSpecification "Use generic-lens or generic-optics with 'customizedMetricSpecification' instead." #-}

-- | Indicates whether scale in by the target tracking scaling policy is disabled. If the value is @true@ , scale in is disabled and the target tracking scaling policy won't remove capacity from the scalable target. Otherwise, scale in is enabled and the target tracking scaling policy can remove capacity from the scalable target. The default value is @false@ .
--
-- /Note:/ Consider using 'disableScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttspcDisableScaleIn :: Lens.Lens' TargetTrackingScalingPolicyConfiguration (Lude.Maybe Lude.Bool)
ttspcDisableScaleIn = Lens.lens (disableScaleIn :: TargetTrackingScalingPolicyConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {disableScaleIn = a} :: TargetTrackingScalingPolicyConfiguration)
{-# DEPRECATED ttspcDisableScaleIn "Use generic-lens or generic-optics with 'disableScaleIn' instead." #-}

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
ttspcScaleOutCooldown :: Lens.Lens' TargetTrackingScalingPolicyConfiguration (Lude.Maybe Lude.Int)
ttspcScaleOutCooldown = Lens.lens (scaleOutCooldown :: TargetTrackingScalingPolicyConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {scaleOutCooldown = a} :: TargetTrackingScalingPolicyConfiguration)
{-# DEPRECATED ttspcScaleOutCooldown "Use generic-lens or generic-optics with 'scaleOutCooldown' instead." #-}

-- | The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
--
-- /Note:/ Consider using 'targetValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttspcTargetValue :: Lens.Lens' TargetTrackingScalingPolicyConfiguration Lude.Double
ttspcTargetValue = Lens.lens (targetValue :: TargetTrackingScalingPolicyConfiguration -> Lude.Double) (\s a -> s {targetValue = a} :: TargetTrackingScalingPolicyConfiguration)
{-# DEPRECATED ttspcTargetValue "Use generic-lens or generic-optics with 'targetValue' instead." #-}

instance Lude.FromJSON TargetTrackingScalingPolicyConfiguration where
  parseJSON =
    Lude.withObject
      "TargetTrackingScalingPolicyConfiguration"
      ( \x ->
          TargetTrackingScalingPolicyConfiguration'
            Lude.<$> (x Lude..:? "PredefinedMetricSpecification")
            Lude.<*> (x Lude..:? "ScaleInCooldown")
            Lude.<*> (x Lude..:? "CustomizedMetricSpecification")
            Lude.<*> (x Lude..:? "DisableScaleIn")
            Lude.<*> (x Lude..:? "ScaleOutCooldown")
            Lude.<*> (x Lude..: "TargetValue")
      )

instance Lude.ToJSON TargetTrackingScalingPolicyConfiguration where
  toJSON TargetTrackingScalingPolicyConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PredefinedMetricSpecification" Lude..=)
              Lude.<$> predefinedMetricSpecification,
            ("ScaleInCooldown" Lude..=) Lude.<$> scaleInCooldown,
            ("CustomizedMetricSpecification" Lude..=)
              Lude.<$> customizedMetricSpecification,
            ("DisableScaleIn" Lude..=) Lude.<$> disableScaleIn,
            ("ScaleOutCooldown" Lude..=) Lude.<$> scaleOutCooldown,
            Lude.Just ("TargetValue" Lude..= targetValue)
          ]
      )
