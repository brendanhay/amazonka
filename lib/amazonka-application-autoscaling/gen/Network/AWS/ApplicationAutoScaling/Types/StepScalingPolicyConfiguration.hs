{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration
  ( StepScalingPolicyConfiguration (..)
  -- * Smart constructor
  , mkStepScalingPolicyConfiguration
  -- * Lenses
  , sspcAdjustmentType
  , sspcCooldown
  , sspcMetricAggregationType
  , sspcMinAdjustmentMagnitude
  , sspcStepAdjustments
  ) where

import qualified Network.AWS.ApplicationAutoScaling.Types.AdjustmentType as Types
import qualified Network.AWS.ApplicationAutoScaling.Types.MetricAggregationType as Types
import qualified Network.AWS.ApplicationAutoScaling.Types.StepAdjustment as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a step scaling policy configuration to use with Application Auto Scaling.
--
-- /See:/ 'mkStepScalingPolicyConfiguration' smart constructor.
data StepScalingPolicyConfiguration = StepScalingPolicyConfiguration'
  { adjustmentType :: Core.Maybe Types.AdjustmentType
    -- ^ Specifies how the @ScalingAdjustment@ value in a <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepAdjustment.html StepAdjustment> is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ . 
--
-- @AdjustmentType@ is required if you are adding a new step scaling policy configuration.
  , cooldown :: Core.Maybe Core.Int
    -- ^ The amount of time, in seconds, to wait for a previous scaling activity to take effect. 
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
  , metricAggregationType :: Core.Maybe Types.MetricAggregationType
    -- ^ The aggregation type for the CloudWatch metrics. Valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ .
  , minAdjustmentMagnitude :: Core.Maybe Core.Int
    -- ^ The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ . For example, suppose that you create a step scaling policy to scale out an Amazon ECS service by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the service has 4 tasks and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Application Auto Scaling scales out the service by 2 tasks.
  , stepAdjustments :: Core.Maybe [Types.StepAdjustment]
    -- ^ A set of adjustments that enable you to scale based on the size of the alarm breach.
--
-- At least one step adjustment is required if you are adding a new step scaling policy configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StepScalingPolicyConfiguration' value with any optional fields omitted.
mkStepScalingPolicyConfiguration
    :: StepScalingPolicyConfiguration
mkStepScalingPolicyConfiguration
  = StepScalingPolicyConfiguration'{adjustmentType = Core.Nothing,
                                    cooldown = Core.Nothing, metricAggregationType = Core.Nothing,
                                    minAdjustmentMagnitude = Core.Nothing,
                                    stepAdjustments = Core.Nothing}

-- | Specifies how the @ScalingAdjustment@ value in a <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepAdjustment.html StepAdjustment> is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ . 
--
-- @AdjustmentType@ is required if you are adding a new step scaling policy configuration.
--
-- /Note:/ Consider using 'adjustmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspcAdjustmentType :: Lens.Lens' StepScalingPolicyConfiguration (Core.Maybe Types.AdjustmentType)
sspcAdjustmentType = Lens.field @"adjustmentType"
{-# INLINEABLE sspcAdjustmentType #-}
{-# DEPRECATED adjustmentType "Use generic-lens or generic-optics with 'adjustmentType' instead"  #-}

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
sspcCooldown :: Lens.Lens' StepScalingPolicyConfiguration (Core.Maybe Core.Int)
sspcCooldown = Lens.field @"cooldown"
{-# INLINEABLE sspcCooldown #-}
{-# DEPRECATED cooldown "Use generic-lens or generic-optics with 'cooldown' instead"  #-}

-- | The aggregation type for the CloudWatch metrics. Valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ .
--
-- /Note:/ Consider using 'metricAggregationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspcMetricAggregationType :: Lens.Lens' StepScalingPolicyConfiguration (Core.Maybe Types.MetricAggregationType)
sspcMetricAggregationType = Lens.field @"metricAggregationType"
{-# INLINEABLE sspcMetricAggregationType #-}
{-# DEPRECATED metricAggregationType "Use generic-lens or generic-optics with 'metricAggregationType' instead"  #-}

-- | The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ . For example, suppose that you create a step scaling policy to scale out an Amazon ECS service by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the service has 4 tasks and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Application Auto Scaling scales out the service by 2 tasks.
--
-- /Note:/ Consider using 'minAdjustmentMagnitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspcMinAdjustmentMagnitude :: Lens.Lens' StepScalingPolicyConfiguration (Core.Maybe Core.Int)
sspcMinAdjustmentMagnitude = Lens.field @"minAdjustmentMagnitude"
{-# INLINEABLE sspcMinAdjustmentMagnitude #-}
{-# DEPRECATED minAdjustmentMagnitude "Use generic-lens or generic-optics with 'minAdjustmentMagnitude' instead"  #-}

-- | A set of adjustments that enable you to scale based on the size of the alarm breach.
--
-- At least one step adjustment is required if you are adding a new step scaling policy configuration.
--
-- /Note:/ Consider using 'stepAdjustments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspcStepAdjustments :: Lens.Lens' StepScalingPolicyConfiguration (Core.Maybe [Types.StepAdjustment])
sspcStepAdjustments = Lens.field @"stepAdjustments"
{-# INLINEABLE sspcStepAdjustments #-}
{-# DEPRECATED stepAdjustments "Use generic-lens or generic-optics with 'stepAdjustments' instead"  #-}

instance Core.FromJSON StepScalingPolicyConfiguration where
        toJSON StepScalingPolicyConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("AdjustmentType" Core..=) Core.<$> adjustmentType,
                  ("Cooldown" Core..=) Core.<$> cooldown,
                  ("MetricAggregationType" Core..=) Core.<$> metricAggregationType,
                  ("MinAdjustmentMagnitude" Core..=) Core.<$> minAdjustmentMagnitude,
                  ("StepAdjustments" Core..=) Core.<$> stepAdjustments])

instance Core.FromJSON StepScalingPolicyConfiguration where
        parseJSON
          = Core.withObject "StepScalingPolicyConfiguration" Core.$
              \ x ->
                StepScalingPolicyConfiguration' Core.<$>
                  (x Core..:? "AdjustmentType") Core.<*> x Core..:? "Cooldown"
                    Core.<*> x Core..:? "MetricAggregationType"
                    Core.<*> x Core..:? "MinAdjustmentMagnitude"
                    Core.<*> x Core..:? "StepAdjustments"
