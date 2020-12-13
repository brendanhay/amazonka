{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration
  ( StepScalingPolicyConfiguration (..),

    -- * Smart constructor
    mkStepScalingPolicyConfiguration,

    -- * Lenses
    sspcStepAdjustments,
    sspcAdjustmentType,
    sspcCooldown,
    sspcMetricAggregationType,
    sspcMinAdjustmentMagnitude,
  )
where

import Network.AWS.ApplicationAutoScaling.Types.AdjustmentType
import Network.AWS.ApplicationAutoScaling.Types.MetricAggregationType
import Network.AWS.ApplicationAutoScaling.Types.StepAdjustment
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a step scaling policy configuration to use with Application Auto Scaling.
--
-- /See:/ 'mkStepScalingPolicyConfiguration' smart constructor.
data StepScalingPolicyConfiguration = StepScalingPolicyConfiguration'
  { -- | A set of adjustments that enable you to scale based on the size of the alarm breach.
    --
    -- At least one step adjustment is required if you are adding a new step scaling policy configuration.
    stepAdjustments :: Lude.Maybe [StepAdjustment],
    -- | Specifies how the @ScalingAdjustment@ value in a <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepAdjustment.html StepAdjustment> is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
    --
    -- @AdjustmentType@ is required if you are adding a new step scaling policy configuration.
    adjustmentType :: Lude.Maybe AdjustmentType,
    -- | The amount of time, in seconds, to wait for a previous scaling activity to take effect.
    --
    -- With scale-out policies, the intention is to continuously (but not excessively) scale out. After Application Auto Scaling successfully scales out using a step scaling policy, it starts to calculate the cooldown time. The scaling policy won't increase the desired capacity again unless either a larger scale out is triggered or the cooldown period ends. While the cooldown period is in effect, capacity added by the initiating scale-out activity is calculated as part of the desired capacity for the next scale-out activity. For example, when an alarm triggers a step scaling policy to increase the capacity by 2, the scaling activity completes successfully, and a cooldown period starts. If the alarm triggers again during the cooldown period but at a more aggressive step adjustment of 3, the previous increase of 2 is considered part of the current capacity. Therefore, only 1 is added to the capacity.
    -- With scale-in policies, the intention is to scale in conservatively to protect your application’s availability, so scale-in activities are blocked until the cooldown period has expired. However, if another alarm triggers a scale-out activity during the cooldown period after a scale-in activity, Application Auto Scaling scales out the target immediately. In this case, the cooldown period for the scale-in activity stops and doesn't complete.
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
    cooldown :: Lude.Maybe Lude.Int,
    -- | The aggregation type for the CloudWatch metrics. Valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ .
    metricAggregationType :: Lude.Maybe MetricAggregationType,
    -- | The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ . For example, suppose that you create a step scaling policy to scale out an Amazon ECS service by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the service has 4 tasks and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Application Auto Scaling scales out the service by 2 tasks.
    minAdjustmentMagnitude :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StepScalingPolicyConfiguration' with the minimum fields required to make a request.
--
-- * 'stepAdjustments' - A set of adjustments that enable you to scale based on the size of the alarm breach.
--
-- At least one step adjustment is required if you are adding a new step scaling policy configuration.
-- * 'adjustmentType' - Specifies how the @ScalingAdjustment@ value in a <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepAdjustment.html StepAdjustment> is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
--
-- @AdjustmentType@ is required if you are adding a new step scaling policy configuration.
-- * 'cooldown' - The amount of time, in seconds, to wait for a previous scaling activity to take effect.
--
-- With scale-out policies, the intention is to continuously (but not excessively) scale out. After Application Auto Scaling successfully scales out using a step scaling policy, it starts to calculate the cooldown time. The scaling policy won't increase the desired capacity again unless either a larger scale out is triggered or the cooldown period ends. While the cooldown period is in effect, capacity added by the initiating scale-out activity is calculated as part of the desired capacity for the next scale-out activity. For example, when an alarm triggers a step scaling policy to increase the capacity by 2, the scaling activity completes successfully, and a cooldown period starts. If the alarm triggers again during the cooldown period but at a more aggressive step adjustment of 3, the previous increase of 2 is considered part of the current capacity. Therefore, only 1 is added to the capacity.
-- With scale-in policies, the intention is to scale in conservatively to protect your application’s availability, so scale-in activities are blocked until the cooldown period has expired. However, if another alarm triggers a scale-out activity during the cooldown period after a scale-in activity, Application Auto Scaling scales out the target immediately. In this case, the cooldown period for the scale-in activity stops and doesn't complete.
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
-- * 'metricAggregationType' - The aggregation type for the CloudWatch metrics. Valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ .
-- * 'minAdjustmentMagnitude' - The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ . For example, suppose that you create a step scaling policy to scale out an Amazon ECS service by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the service has 4 tasks and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Application Auto Scaling scales out the service by 2 tasks.
mkStepScalingPolicyConfiguration ::
  StepScalingPolicyConfiguration
mkStepScalingPolicyConfiguration =
  StepScalingPolicyConfiguration'
    { stepAdjustments = Lude.Nothing,
      adjustmentType = Lude.Nothing,
      cooldown = Lude.Nothing,
      metricAggregationType = Lude.Nothing,
      minAdjustmentMagnitude = Lude.Nothing
    }

-- | A set of adjustments that enable you to scale based on the size of the alarm breach.
--
-- At least one step adjustment is required if you are adding a new step scaling policy configuration.
--
-- /Note:/ Consider using 'stepAdjustments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspcStepAdjustments :: Lens.Lens' StepScalingPolicyConfiguration (Lude.Maybe [StepAdjustment])
sspcStepAdjustments = Lens.lens (stepAdjustments :: StepScalingPolicyConfiguration -> Lude.Maybe [StepAdjustment]) (\s a -> s {stepAdjustments = a} :: StepScalingPolicyConfiguration)
{-# DEPRECATED sspcStepAdjustments "Use generic-lens or generic-optics with 'stepAdjustments' instead." #-}

-- | Specifies how the @ScalingAdjustment@ value in a <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepAdjustment.html StepAdjustment> is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
--
-- @AdjustmentType@ is required if you are adding a new step scaling policy configuration.
--
-- /Note:/ Consider using 'adjustmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspcAdjustmentType :: Lens.Lens' StepScalingPolicyConfiguration (Lude.Maybe AdjustmentType)
sspcAdjustmentType = Lens.lens (adjustmentType :: StepScalingPolicyConfiguration -> Lude.Maybe AdjustmentType) (\s a -> s {adjustmentType = a} :: StepScalingPolicyConfiguration)
{-# DEPRECATED sspcAdjustmentType "Use generic-lens or generic-optics with 'adjustmentType' instead." #-}

-- | The amount of time, in seconds, to wait for a previous scaling activity to take effect.
--
-- With scale-out policies, the intention is to continuously (but not excessively) scale out. After Application Auto Scaling successfully scales out using a step scaling policy, it starts to calculate the cooldown time. The scaling policy won't increase the desired capacity again unless either a larger scale out is triggered or the cooldown period ends. While the cooldown period is in effect, capacity added by the initiating scale-out activity is calculated as part of the desired capacity for the next scale-out activity. For example, when an alarm triggers a step scaling policy to increase the capacity by 2, the scaling activity completes successfully, and a cooldown period starts. If the alarm triggers again during the cooldown period but at a more aggressive step adjustment of 3, the previous increase of 2 is considered part of the current capacity. Therefore, only 1 is added to the capacity.
-- With scale-in policies, the intention is to scale in conservatively to protect your application’s availability, so scale-in activities are blocked until the cooldown period has expired. However, if another alarm triggers a scale-out activity during the cooldown period after a scale-in activity, Application Auto Scaling scales out the target immediately. In this case, the cooldown period for the scale-in activity stops and doesn't complete.
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
-- /Note:/ Consider using 'cooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspcCooldown :: Lens.Lens' StepScalingPolicyConfiguration (Lude.Maybe Lude.Int)
sspcCooldown = Lens.lens (cooldown :: StepScalingPolicyConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {cooldown = a} :: StepScalingPolicyConfiguration)
{-# DEPRECATED sspcCooldown "Use generic-lens or generic-optics with 'cooldown' instead." #-}

-- | The aggregation type for the CloudWatch metrics. Valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ .
--
-- /Note:/ Consider using 'metricAggregationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspcMetricAggregationType :: Lens.Lens' StepScalingPolicyConfiguration (Lude.Maybe MetricAggregationType)
sspcMetricAggregationType = Lens.lens (metricAggregationType :: StepScalingPolicyConfiguration -> Lude.Maybe MetricAggregationType) (\s a -> s {metricAggregationType = a} :: StepScalingPolicyConfiguration)
{-# DEPRECATED sspcMetricAggregationType "Use generic-lens or generic-optics with 'metricAggregationType' instead." #-}

-- | The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ . For example, suppose that you create a step scaling policy to scale out an Amazon ECS service by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the service has 4 tasks and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Application Auto Scaling scales out the service by 2 tasks.
--
-- /Note:/ Consider using 'minAdjustmentMagnitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspcMinAdjustmentMagnitude :: Lens.Lens' StepScalingPolicyConfiguration (Lude.Maybe Lude.Int)
sspcMinAdjustmentMagnitude = Lens.lens (minAdjustmentMagnitude :: StepScalingPolicyConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {minAdjustmentMagnitude = a} :: StepScalingPolicyConfiguration)
{-# DEPRECATED sspcMinAdjustmentMagnitude "Use generic-lens or generic-optics with 'minAdjustmentMagnitude' instead." #-}

instance Lude.FromJSON StepScalingPolicyConfiguration where
  parseJSON =
    Lude.withObject
      "StepScalingPolicyConfiguration"
      ( \x ->
          StepScalingPolicyConfiguration'
            Lude.<$> (x Lude..:? "StepAdjustments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AdjustmentType")
            Lude.<*> (x Lude..:? "Cooldown")
            Lude.<*> (x Lude..:? "MetricAggregationType")
            Lude.<*> (x Lude..:? "MinAdjustmentMagnitude")
      )

instance Lude.ToJSON StepScalingPolicyConfiguration where
  toJSON StepScalingPolicyConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StepAdjustments" Lude..=) Lude.<$> stepAdjustments,
            ("AdjustmentType" Lude..=) Lude.<$> adjustmentType,
            ("Cooldown" Lude..=) Lude.<$> cooldown,
            ("MetricAggregationType" Lude..=) Lude.<$> metricAggregationType,
            ("MinAdjustmentMagnitude" Lude..=)
              Lude.<$> minAdjustmentMagnitude
          ]
      )
