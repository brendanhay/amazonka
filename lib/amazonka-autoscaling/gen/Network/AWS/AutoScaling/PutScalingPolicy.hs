{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PutScalingPolicy (..),
    mkPutScalingPolicy,

    -- ** Request lenses
    pspMinAdjustmentStep,
    pspEstimatedInstanceWarmup,
    pspPolicyName,
    pspEnabled,
    pspPolicyType,
    pspStepAdjustments,
    pspTargetTrackingConfiguration,
    pspAdjustmentType,
    pspAutoScalingGroupName,
    pspScalingAdjustment,
    pspCooldown,
    pspMetricAggregationType,
    pspMinAdjustmentMagnitude,

    -- * Destructuring the response
    PutScalingPolicyResponse (..),
    mkPutScalingPolicyResponse,

    -- ** Response lenses
    psprsPolicyARN,
    psprsAlarms,
    psprsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutScalingPolicy' smart constructor.
data PutScalingPolicy = PutScalingPolicy'
  { -- | Available for backward compatibility. Use @MinAdjustmentMagnitude@ instead.
    minAdjustmentStep :: Lude.Maybe Lude.Int,
    -- | The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics. If not provided, the default is to use the value from the default cooldown period for the Auto Scaling group.
    --
    -- Valid only if the policy type is @TargetTrackingScaling@ or @StepScaling@ .
    estimatedInstanceWarmup :: Lude.Maybe Lude.Int,
    -- | The name of the policy.
    policyName :: Lude.Text,
    -- | Indicates whether the scaling policy is enabled or disabled. The default is enabled. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enable-disable-scaling-policy.html Disabling a scaling policy for an Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
    enabled :: Lude.Maybe Lude.Bool,
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
    policyType :: Lude.Maybe Lude.Text,
    -- | A set of adjustments that enable you to scale based on the size of the alarm breach.
    --
    -- Required if the policy type is @StepScaling@ . (Not used with any other policy type.)
    stepAdjustments :: Lude.Maybe [StepAdjustment],
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
    targetTrackingConfiguration :: Lude.Maybe TargetTrackingConfiguration,
    -- | Specifies how the scaling adjustment is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
    --
    -- Required if the policy type is @StepScaling@ or @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types> in the /Amazon EC2 Auto Scaling User Guide/ .
    adjustmentType :: Lude.Maybe Lude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Lude.Text,
    -- | The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity. For exact capacity, you must specify a positive value.
    --
    -- Required if the policy type is @SimpleScaling@ . (Not used with any other policy type.)
    scalingAdjustment :: Lude.Maybe Lude.Int,
    -- | The duration of the policy's cooldown period, in seconds. When a cooldown period is specified here, it overrides the default cooldown period defined for the Auto Scaling group.
    --
    -- Valid only if the policy type is @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
    cooldown :: Lude.Maybe Lude.Int,
    -- | The aggregation type for the CloudWatch metrics. The valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ .
    --
    -- Valid only if the policy type is @StepScaling@ .
    metricAggregationType :: Lude.Maybe Lude.Text,
    -- | The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ . For example, suppose that you create a step scaling policy to scale out an Auto Scaling group by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the group has 4 instances and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Amazon EC2 Auto Scaling scales out the group by 2 instances.
    --
    -- Valid only if the policy type is @StepScaling@ or @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types> in the /Amazon EC2 Auto Scaling User Guide/ .
    minAdjustmentMagnitude :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutScalingPolicy' with the minimum fields required to make a request.
--
-- * 'minAdjustmentStep' - Available for backward compatibility. Use @MinAdjustmentMagnitude@ instead.
-- * 'estimatedInstanceWarmup' - The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics. If not provided, the default is to use the value from the default cooldown period for the Auto Scaling group.
--
-- Valid only if the policy type is @TargetTrackingScaling@ or @StepScaling@ .
-- * 'policyName' - The name of the policy.
-- * 'enabled' - Indicates whether the scaling policy is enabled or disabled. The default is enabled. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enable-disable-scaling-policy.html Disabling a scaling policy for an Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'policyType' - One of the following policy types:
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
-- * 'stepAdjustments' - A set of adjustments that enable you to scale based on the size of the alarm breach.
--
-- Required if the policy type is @StepScaling@ . (Not used with any other policy type.)
-- * 'targetTrackingConfiguration' - A target tracking scaling policy. Includes support for predefined or customized metrics.
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
-- * 'adjustmentType' - Specifies how the scaling adjustment is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
--
-- Required if the policy type is @StepScaling@ or @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'scalingAdjustment' - The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity. For exact capacity, you must specify a positive value.
--
-- Required if the policy type is @SimpleScaling@ . (Not used with any other policy type.)
-- * 'cooldown' - The duration of the policy's cooldown period, in seconds. When a cooldown period is specified here, it overrides the default cooldown period defined for the Auto Scaling group.
--
-- Valid only if the policy type is @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'metricAggregationType' - The aggregation type for the CloudWatch metrics. The valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ .
--
-- Valid only if the policy type is @StepScaling@ .
-- * 'minAdjustmentMagnitude' - The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ . For example, suppose that you create a step scaling policy to scale out an Auto Scaling group by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the group has 4 instances and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Amazon EC2 Auto Scaling scales out the group by 2 instances.
--
-- Valid only if the policy type is @StepScaling@ or @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types> in the /Amazon EC2 Auto Scaling User Guide/ .
mkPutScalingPolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'autoScalingGroupName'
  Lude.Text ->
  PutScalingPolicy
mkPutScalingPolicy pPolicyName_ pAutoScalingGroupName_ =
  PutScalingPolicy'
    { minAdjustmentStep = Lude.Nothing,
      estimatedInstanceWarmup = Lude.Nothing,
      policyName = pPolicyName_,
      enabled = Lude.Nothing,
      policyType = Lude.Nothing,
      stepAdjustments = Lude.Nothing,
      targetTrackingConfiguration = Lude.Nothing,
      adjustmentType = Lude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_,
      scalingAdjustment = Lude.Nothing,
      cooldown = Lude.Nothing,
      metricAggregationType = Lude.Nothing,
      minAdjustmentMagnitude = Lude.Nothing
    }

-- | Available for backward compatibility. Use @MinAdjustmentMagnitude@ instead.
--
-- /Note:/ Consider using 'minAdjustmentStep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspMinAdjustmentStep :: Lens.Lens' PutScalingPolicy (Lude.Maybe Lude.Int)
pspMinAdjustmentStep = Lens.lens (minAdjustmentStep :: PutScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {minAdjustmentStep = a} :: PutScalingPolicy)
{-# DEPRECATED pspMinAdjustmentStep "Use generic-lens or generic-optics with 'minAdjustmentStep' instead." #-}

-- | The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics. If not provided, the default is to use the value from the default cooldown period for the Auto Scaling group.
--
-- Valid only if the policy type is @TargetTrackingScaling@ or @StepScaling@ .
--
-- /Note:/ Consider using 'estimatedInstanceWarmup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspEstimatedInstanceWarmup :: Lens.Lens' PutScalingPolicy (Lude.Maybe Lude.Int)
pspEstimatedInstanceWarmup = Lens.lens (estimatedInstanceWarmup :: PutScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {estimatedInstanceWarmup = a} :: PutScalingPolicy)
{-# DEPRECATED pspEstimatedInstanceWarmup "Use generic-lens or generic-optics with 'estimatedInstanceWarmup' instead." #-}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspPolicyName :: Lens.Lens' PutScalingPolicy Lude.Text
pspPolicyName = Lens.lens (policyName :: PutScalingPolicy -> Lude.Text) (\s a -> s {policyName = a} :: PutScalingPolicy)
{-# DEPRECATED pspPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | Indicates whether the scaling policy is enabled or disabled. The default is enabled. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enable-disable-scaling-policy.html Disabling a scaling policy for an Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspEnabled :: Lens.Lens' PutScalingPolicy (Lude.Maybe Lude.Bool)
pspEnabled = Lens.lens (enabled :: PutScalingPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: PutScalingPolicy)
{-# DEPRECATED pspEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

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
pspPolicyType :: Lens.Lens' PutScalingPolicy (Lude.Maybe Lude.Text)
pspPolicyType = Lens.lens (policyType :: PutScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyType = a} :: PutScalingPolicy)
{-# DEPRECATED pspPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

-- | A set of adjustments that enable you to scale based on the size of the alarm breach.
--
-- Required if the policy type is @StepScaling@ . (Not used with any other policy type.)
--
-- /Note:/ Consider using 'stepAdjustments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspStepAdjustments :: Lens.Lens' PutScalingPolicy (Lude.Maybe [StepAdjustment])
pspStepAdjustments = Lens.lens (stepAdjustments :: PutScalingPolicy -> Lude.Maybe [StepAdjustment]) (\s a -> s {stepAdjustments = a} :: PutScalingPolicy)
{-# DEPRECATED pspStepAdjustments "Use generic-lens or generic-optics with 'stepAdjustments' instead." #-}

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
pspTargetTrackingConfiguration :: Lens.Lens' PutScalingPolicy (Lude.Maybe TargetTrackingConfiguration)
pspTargetTrackingConfiguration = Lens.lens (targetTrackingConfiguration :: PutScalingPolicy -> Lude.Maybe TargetTrackingConfiguration) (\s a -> s {targetTrackingConfiguration = a} :: PutScalingPolicy)
{-# DEPRECATED pspTargetTrackingConfiguration "Use generic-lens or generic-optics with 'targetTrackingConfiguration' instead." #-}

-- | Specifies how the scaling adjustment is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
--
-- Required if the policy type is @StepScaling@ or @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'adjustmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspAdjustmentType :: Lens.Lens' PutScalingPolicy (Lude.Maybe Lude.Text)
pspAdjustmentType = Lens.lens (adjustmentType :: PutScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {adjustmentType = a} :: PutScalingPolicy)
{-# DEPRECATED pspAdjustmentType "Use generic-lens or generic-optics with 'adjustmentType' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspAutoScalingGroupName :: Lens.Lens' PutScalingPolicy Lude.Text
pspAutoScalingGroupName = Lens.lens (autoScalingGroupName :: PutScalingPolicy -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: PutScalingPolicy)
{-# DEPRECATED pspAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity. For exact capacity, you must specify a positive value.
--
-- Required if the policy type is @SimpleScaling@ . (Not used with any other policy type.)
--
-- /Note:/ Consider using 'scalingAdjustment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspScalingAdjustment :: Lens.Lens' PutScalingPolicy (Lude.Maybe Lude.Int)
pspScalingAdjustment = Lens.lens (scalingAdjustment :: PutScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {scalingAdjustment = a} :: PutScalingPolicy)
{-# DEPRECATED pspScalingAdjustment "Use generic-lens or generic-optics with 'scalingAdjustment' instead." #-}

-- | The duration of the policy's cooldown period, in seconds. When a cooldown period is specified here, it overrides the default cooldown period defined for the Auto Scaling group.
--
-- Valid only if the policy type is @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'cooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspCooldown :: Lens.Lens' PutScalingPolicy (Lude.Maybe Lude.Int)
pspCooldown = Lens.lens (cooldown :: PutScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {cooldown = a} :: PutScalingPolicy)
{-# DEPRECATED pspCooldown "Use generic-lens or generic-optics with 'cooldown' instead." #-}

-- | The aggregation type for the CloudWatch metrics. The valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ .
--
-- Valid only if the policy type is @StepScaling@ .
--
-- /Note:/ Consider using 'metricAggregationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspMetricAggregationType :: Lens.Lens' PutScalingPolicy (Lude.Maybe Lude.Text)
pspMetricAggregationType = Lens.lens (metricAggregationType :: PutScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {metricAggregationType = a} :: PutScalingPolicy)
{-# DEPRECATED pspMetricAggregationType "Use generic-lens or generic-optics with 'metricAggregationType' instead." #-}

-- | The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ . For example, suppose that you create a step scaling policy to scale out an Auto Scaling group by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the group has 4 instances and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Amazon EC2 Auto Scaling scales out the group by 2 instances.
--
-- Valid only if the policy type is @StepScaling@ or @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'minAdjustmentMagnitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspMinAdjustmentMagnitude :: Lens.Lens' PutScalingPolicy (Lude.Maybe Lude.Int)
pspMinAdjustmentMagnitude = Lens.lens (minAdjustmentMagnitude :: PutScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {minAdjustmentMagnitude = a} :: PutScalingPolicy)
{-# DEPRECATED pspMinAdjustmentMagnitude "Use generic-lens or generic-optics with 'minAdjustmentMagnitude' instead." #-}

instance Lude.AWSRequest PutScalingPolicy where
  type Rs PutScalingPolicy = PutScalingPolicyResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "PutScalingPolicyResult"
      ( \s h x ->
          PutScalingPolicyResponse'
            Lude.<$> (x Lude..@? "PolicyARN")
            Lude.<*> ( x Lude..@? "Alarms" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutScalingPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutScalingPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutScalingPolicy where
  toQuery PutScalingPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PutScalingPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "MinAdjustmentStep" Lude.=: minAdjustmentStep,
        "EstimatedInstanceWarmup" Lude.=: estimatedInstanceWarmup,
        "PolicyName" Lude.=: policyName,
        "Enabled" Lude.=: enabled,
        "PolicyType" Lude.=: policyType,
        "StepAdjustments"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> stepAdjustments),
        "TargetTrackingConfiguration" Lude.=: targetTrackingConfiguration,
        "AdjustmentType" Lude.=: adjustmentType,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "ScalingAdjustment" Lude.=: scalingAdjustment,
        "Cooldown" Lude.=: cooldown,
        "MetricAggregationType" Lude.=: metricAggregationType,
        "MinAdjustmentMagnitude" Lude.=: minAdjustmentMagnitude
      ]

-- | Contains the output of PutScalingPolicy.
--
-- /See:/ 'mkPutScalingPolicyResponse' smart constructor.
data PutScalingPolicyResponse = PutScalingPolicyResponse'
  { -- | The Amazon Resource Name (ARN) of the policy.
    policyARN :: Lude.Maybe Lude.Text,
    -- | The CloudWatch alarms created for the target tracking scaling policy.
    alarms :: Lude.Maybe [Alarm],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutScalingPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policyARN' - The Amazon Resource Name (ARN) of the policy.
-- * 'alarms' - The CloudWatch alarms created for the target tracking scaling policy.
-- * 'responseStatus' - The response status code.
mkPutScalingPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutScalingPolicyResponse
mkPutScalingPolicyResponse pResponseStatus_ =
  PutScalingPolicyResponse'
    { policyARN = Lude.Nothing,
      alarms = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the policy.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psprsPolicyARN :: Lens.Lens' PutScalingPolicyResponse (Lude.Maybe Lude.Text)
psprsPolicyARN = Lens.lens (policyARN :: PutScalingPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyARN = a} :: PutScalingPolicyResponse)
{-# DEPRECATED psprsPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

-- | The CloudWatch alarms created for the target tracking scaling policy.
--
-- /Note:/ Consider using 'alarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psprsAlarms :: Lens.Lens' PutScalingPolicyResponse (Lude.Maybe [Alarm])
psprsAlarms = Lens.lens (alarms :: PutScalingPolicyResponse -> Lude.Maybe [Alarm]) (\s a -> s {alarms = a} :: PutScalingPolicyResponse)
{-# DEPRECATED psprsAlarms "Use generic-lens or generic-optics with 'alarms' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psprsResponseStatus :: Lens.Lens' PutScalingPolicyResponse Lude.Int
psprsResponseStatus = Lens.lens (responseStatus :: PutScalingPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutScalingPolicyResponse)
{-# DEPRECATED psprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
