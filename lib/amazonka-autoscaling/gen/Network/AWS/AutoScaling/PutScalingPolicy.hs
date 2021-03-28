{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.PutScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scaling policy for an Auto Scaling group.
--
-- For more information about using scaling policies to scale your Auto Scaling group, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-target-tracking.html Target tracking scaling policies> and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html Step and simple scaling policies> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.PutScalingPolicy
    (
    -- * Creating a request
      PutScalingPolicy (..)
    , mkPutScalingPolicy
    -- ** Request lenses
    , pspAutoScalingGroupName
    , pspPolicyName
    , pspAdjustmentType
    , pspCooldown
    , pspEnabled
    , pspEstimatedInstanceWarmup
    , pspMetricAggregationType
    , pspMinAdjustmentMagnitude
    , pspMinAdjustmentStep
    , pspPolicyType
    , pspScalingAdjustment
    , pspStepAdjustments
    , pspTargetTrackingConfiguration

    -- * Destructuring the response
    , PutScalingPolicyResponse (..)
    , mkPutScalingPolicyResponse
    -- ** Response lenses
    , psprrsAlarms
    , psprrsPolicyARN
    , psprrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutScalingPolicy' smart constructor.
data PutScalingPolicy = PutScalingPolicy'
  { autoScalingGroupName :: Types.ResourceName
    -- ^ The name of the Auto Scaling group.
  , policyName :: Types.XmlStringMaxLen255
    -- ^ The name of the policy.
  , adjustmentType :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ Specifies how the scaling adjustment is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
--
-- Required if the policy type is @StepScaling@ or @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types> in the /Amazon EC2 Auto Scaling User Guide/ .
  , cooldown :: Core.Maybe Core.Int
    -- ^ The duration of the policy's cooldown period, in seconds. When a cooldown period is specified here, it overrides the default cooldown period defined for the Auto Scaling group.
--
-- Valid only if the policy type is @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
  , enabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether the scaling policy is enabled or disabled. The default is enabled. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enable-disable-scaling-policy.html Disabling a scaling policy for an Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
  , estimatedInstanceWarmup :: Core.Maybe Core.Int
    -- ^ The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics. If not provided, the default is to use the value from the default cooldown period for the Auto Scaling group.
--
-- Valid only if the policy type is @TargetTrackingScaling@ or @StepScaling@ .
  , metricAggregationType :: Core.Maybe Types.MetricAggregationType
    -- ^ The aggregation type for the CloudWatch metrics. The valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ .
--
-- Valid only if the policy type is @StepScaling@ .
  , minAdjustmentMagnitude :: Core.Maybe Core.Int
    -- ^ The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ . For example, suppose that you create a step scaling policy to scale out an Auto Scaling group by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the group has 4 instances and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Amazon EC2 Auto Scaling scales out the group by 2 instances.
--
-- Valid only if the policy type is @StepScaling@ or @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types> in the /Amazon EC2 Auto Scaling User Guide/ .
  , minAdjustmentStep :: Core.Maybe Core.Int
    -- ^ Available for backward compatibility. Use @MinAdjustmentMagnitude@ instead.
  , policyType :: Core.Maybe Types.XmlStringMaxLen64
    -- ^ One of the following policy types: 
--
--
--     * @TargetTrackingScaling@ 
--
--
--     * @StepScaling@ 
--
--
--     * @SimpleScaling@ (default)
--
--
  , scalingAdjustment :: Core.Maybe Core.Int
    -- ^ The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity. For exact capacity, you must specify a positive value.
--
-- Required if the policy type is @SimpleScaling@ . (Not used with any other policy type.) 
  , stepAdjustments :: Core.Maybe [Types.StepAdjustment]
    -- ^ A set of adjustments that enable you to scale based on the size of the alarm breach.
--
-- Required if the policy type is @StepScaling@ . (Not used with any other policy type.) 
  , targetTrackingConfiguration :: Core.Maybe Types.TargetTrackingConfiguration
    -- ^ A target tracking scaling policy. Includes support for predefined or customized metrics.
--
-- The following predefined metrics are available:
--
--     * @ASGAverageCPUUtilization@ 
--
--
--     * @ASGAverageNetworkIn@ 
--
--
--     * @ASGAverageNetworkOut@ 
--
--
--     * @ALBRequestCountPerTarget@ 
--
--
-- If you specify @ALBRequestCountPerTarget@ for the metric, you must specify the @ResourceLabel@ parameter with the @PredefinedMetricSpecification@ .
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_TargetTrackingConfiguration.html TargetTrackingConfiguration> in the /Amazon EC2 Auto Scaling API Reference/ .
-- Required if the policy type is @TargetTrackingScaling@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutScalingPolicy' value with any optional fields omitted.
mkPutScalingPolicy
    :: Types.ResourceName -- ^ 'autoScalingGroupName'
    -> Types.XmlStringMaxLen255 -- ^ 'policyName'
    -> PutScalingPolicy
mkPutScalingPolicy autoScalingGroupName policyName
  = PutScalingPolicy'{autoScalingGroupName, policyName,
                      adjustmentType = Core.Nothing, cooldown = Core.Nothing,
                      enabled = Core.Nothing, estimatedInstanceWarmup = Core.Nothing,
                      metricAggregationType = Core.Nothing,
                      minAdjustmentMagnitude = Core.Nothing,
                      minAdjustmentStep = Core.Nothing, policyType = Core.Nothing,
                      scalingAdjustment = Core.Nothing, stepAdjustments = Core.Nothing,
                      targetTrackingConfiguration = Core.Nothing}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspAutoScalingGroupName :: Lens.Lens' PutScalingPolicy Types.ResourceName
pspAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE pspAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspPolicyName :: Lens.Lens' PutScalingPolicy Types.XmlStringMaxLen255
pspPolicyName = Lens.field @"policyName"
{-# INLINEABLE pspPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | Specifies how the scaling adjustment is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
--
-- Required if the policy type is @StepScaling@ or @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'adjustmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspAdjustmentType :: Lens.Lens' PutScalingPolicy (Core.Maybe Types.XmlStringMaxLen255)
pspAdjustmentType = Lens.field @"adjustmentType"
{-# INLINEABLE pspAdjustmentType #-}
{-# DEPRECATED adjustmentType "Use generic-lens or generic-optics with 'adjustmentType' instead"  #-}

-- | The duration of the policy's cooldown period, in seconds. When a cooldown period is specified here, it overrides the default cooldown period defined for the Auto Scaling group.
--
-- Valid only if the policy type is @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'cooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspCooldown :: Lens.Lens' PutScalingPolicy (Core.Maybe Core.Int)
pspCooldown = Lens.field @"cooldown"
{-# INLINEABLE pspCooldown #-}
{-# DEPRECATED cooldown "Use generic-lens or generic-optics with 'cooldown' instead"  #-}

-- | Indicates whether the scaling policy is enabled or disabled. The default is enabled. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enable-disable-scaling-policy.html Disabling a scaling policy for an Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspEnabled :: Lens.Lens' PutScalingPolicy (Core.Maybe Core.Bool)
pspEnabled = Lens.field @"enabled"
{-# INLINEABLE pspEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics. If not provided, the default is to use the value from the default cooldown period for the Auto Scaling group.
--
-- Valid only if the policy type is @TargetTrackingScaling@ or @StepScaling@ .
--
-- /Note:/ Consider using 'estimatedInstanceWarmup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspEstimatedInstanceWarmup :: Lens.Lens' PutScalingPolicy (Core.Maybe Core.Int)
pspEstimatedInstanceWarmup = Lens.field @"estimatedInstanceWarmup"
{-# INLINEABLE pspEstimatedInstanceWarmup #-}
{-# DEPRECATED estimatedInstanceWarmup "Use generic-lens or generic-optics with 'estimatedInstanceWarmup' instead"  #-}

-- | The aggregation type for the CloudWatch metrics. The valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ .
--
-- Valid only if the policy type is @StepScaling@ .
--
-- /Note:/ Consider using 'metricAggregationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspMetricAggregationType :: Lens.Lens' PutScalingPolicy (Core.Maybe Types.MetricAggregationType)
pspMetricAggregationType = Lens.field @"metricAggregationType"
{-# INLINEABLE pspMetricAggregationType #-}
{-# DEPRECATED metricAggregationType "Use generic-lens or generic-optics with 'metricAggregationType' instead"  #-}

-- | The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ . For example, suppose that you create a step scaling policy to scale out an Auto Scaling group by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the group has 4 instances and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Amazon EC2 Auto Scaling scales out the group by 2 instances.
--
-- Valid only if the policy type is @StepScaling@ or @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'minAdjustmentMagnitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspMinAdjustmentMagnitude :: Lens.Lens' PutScalingPolicy (Core.Maybe Core.Int)
pspMinAdjustmentMagnitude = Lens.field @"minAdjustmentMagnitude"
{-# INLINEABLE pspMinAdjustmentMagnitude #-}
{-# DEPRECATED minAdjustmentMagnitude "Use generic-lens or generic-optics with 'minAdjustmentMagnitude' instead"  #-}

-- | Available for backward compatibility. Use @MinAdjustmentMagnitude@ instead.
--
-- /Note:/ Consider using 'minAdjustmentStep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspMinAdjustmentStep :: Lens.Lens' PutScalingPolicy (Core.Maybe Core.Int)
pspMinAdjustmentStep = Lens.field @"minAdjustmentStep"
{-# INLINEABLE pspMinAdjustmentStep #-}
{-# DEPRECATED minAdjustmentStep "Use generic-lens or generic-optics with 'minAdjustmentStep' instead"  #-}

-- | One of the following policy types: 
--
--
--     * @TargetTrackingScaling@ 
--
--
--     * @StepScaling@ 
--
--
--     * @SimpleScaling@ (default)
--
--
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspPolicyType :: Lens.Lens' PutScalingPolicy (Core.Maybe Types.XmlStringMaxLen64)
pspPolicyType = Lens.field @"policyType"
{-# INLINEABLE pspPolicyType #-}
{-# DEPRECATED policyType "Use generic-lens or generic-optics with 'policyType' instead"  #-}

-- | The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity. For exact capacity, you must specify a positive value.
--
-- Required if the policy type is @SimpleScaling@ . (Not used with any other policy type.) 
--
-- /Note:/ Consider using 'scalingAdjustment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspScalingAdjustment :: Lens.Lens' PutScalingPolicy (Core.Maybe Core.Int)
pspScalingAdjustment = Lens.field @"scalingAdjustment"
{-# INLINEABLE pspScalingAdjustment #-}
{-# DEPRECATED scalingAdjustment "Use generic-lens or generic-optics with 'scalingAdjustment' instead"  #-}

-- | A set of adjustments that enable you to scale based on the size of the alarm breach.
--
-- Required if the policy type is @StepScaling@ . (Not used with any other policy type.) 
--
-- /Note:/ Consider using 'stepAdjustments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspStepAdjustments :: Lens.Lens' PutScalingPolicy (Core.Maybe [Types.StepAdjustment])
pspStepAdjustments = Lens.field @"stepAdjustments"
{-# INLINEABLE pspStepAdjustments #-}
{-# DEPRECATED stepAdjustments "Use generic-lens or generic-optics with 'stepAdjustments' instead"  #-}

-- | A target tracking scaling policy. Includes support for predefined or customized metrics.
--
-- The following predefined metrics are available:
--
--     * @ASGAverageCPUUtilization@ 
--
--
--     * @ASGAverageNetworkIn@ 
--
--
--     * @ASGAverageNetworkOut@ 
--
--
--     * @ALBRequestCountPerTarget@ 
--
--
-- If you specify @ALBRequestCountPerTarget@ for the metric, you must specify the @ResourceLabel@ parameter with the @PredefinedMetricSpecification@ .
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_TargetTrackingConfiguration.html TargetTrackingConfiguration> in the /Amazon EC2 Auto Scaling API Reference/ .
-- Required if the policy type is @TargetTrackingScaling@ .
--
-- /Note:/ Consider using 'targetTrackingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspTargetTrackingConfiguration :: Lens.Lens' PutScalingPolicy (Core.Maybe Types.TargetTrackingConfiguration)
pspTargetTrackingConfiguration = Lens.field @"targetTrackingConfiguration"
{-# INLINEABLE pspTargetTrackingConfiguration #-}
{-# DEPRECATED targetTrackingConfiguration "Use generic-lens or generic-optics with 'targetTrackingConfiguration' instead"  #-}

instance Core.ToQuery PutScalingPolicy where
        toQuery PutScalingPolicy{..}
          = Core.toQueryPair "Action" ("PutScalingPolicy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<> Core.toQueryPair "PolicyName" policyName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AdjustmentType")
                adjustmentType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Cooldown") cooldown
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Enabled") enabled
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EstimatedInstanceWarmup")
                estimatedInstanceWarmup
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MetricAggregationType")
                metricAggregationType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MinAdjustmentMagnitude")
                minAdjustmentMagnitude
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MinAdjustmentStep")
                minAdjustmentStep
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PolicyType") policyType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ScalingAdjustment")
                scalingAdjustment
              Core.<>
              Core.toQueryPair "StepAdjustments"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   stepAdjustments)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "TargetTrackingConfiguration")
                targetTrackingConfiguration

instance Core.ToHeaders PutScalingPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PutScalingPolicy where
        type Rs PutScalingPolicy = PutScalingPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "PutScalingPolicyResult"
              (\ s h x ->
                 PutScalingPolicyResponse' Core.<$>
                   (x Core..@? "Alarms" Core..<@> Core.parseXMLList "member") Core.<*>
                     x Core..@? "PolicyARN"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of PutScalingPolicy.
--
-- /See:/ 'mkPutScalingPolicyResponse' smart constructor.
data PutScalingPolicyResponse = PutScalingPolicyResponse'
  { alarms :: Core.Maybe [Types.Alarm]
    -- ^ The CloudWatch alarms created for the target tracking scaling policy.
  , policyARN :: Core.Maybe Types.ResourceName
    -- ^ The Amazon Resource Name (ARN) of the policy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutScalingPolicyResponse' value with any optional fields omitted.
mkPutScalingPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutScalingPolicyResponse
mkPutScalingPolicyResponse responseStatus
  = PutScalingPolicyResponse'{alarms = Core.Nothing,
                              policyARN = Core.Nothing, responseStatus}

-- | The CloudWatch alarms created for the target tracking scaling policy.
--
-- /Note:/ Consider using 'alarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psprrsAlarms :: Lens.Lens' PutScalingPolicyResponse (Core.Maybe [Types.Alarm])
psprrsAlarms = Lens.field @"alarms"
{-# INLINEABLE psprrsAlarms #-}
{-# DEPRECATED alarms "Use generic-lens or generic-optics with 'alarms' instead"  #-}

-- | The Amazon Resource Name (ARN) of the policy.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psprrsPolicyARN :: Lens.Lens' PutScalingPolicyResponse (Core.Maybe Types.ResourceName)
psprrsPolicyARN = Lens.field @"policyARN"
{-# INLINEABLE psprrsPolicyARN #-}
{-# DEPRECATED policyARN "Use generic-lens or generic-optics with 'policyARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psprrsResponseStatus :: Lens.Lens' PutScalingPolicyResponse Core.Int
psprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE psprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
