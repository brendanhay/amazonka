{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.ScalingPolicy
  ( ScalingPolicy (..),

    -- * Smart constructor
    mkScalingPolicy,

    -- * Lenses
    spMinAdjustmentStep,
    spEstimatedInstanceWarmup,
    spPolicyName,
    spEnabled,
    spPolicyType,
    spStepAdjustments,
    spTargetTrackingConfiguration,
    spAdjustmentType,
    spAutoScalingGroupName,
    spScalingAdjustment,
    spCooldown,
    spPolicyARN,
    spAlarms,
    spMetricAggregationType,
    spMinAdjustmentMagnitude,
  )
where

import Network.AWS.AutoScaling.Types.Alarm
import Network.AWS.AutoScaling.Types.StepAdjustment
import Network.AWS.AutoScaling.Types.TargetTrackingConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a scaling policy.
--
-- /See:/ 'mkScalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
  { -- | Available for backward compatibility. Use @MinAdjustmentMagnitude@ instead.
    minAdjustmentStep :: Lude.Maybe Lude.Int,
    -- | The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics.
    estimatedInstanceWarmup :: Lude.Maybe Lude.Int,
    -- | The name of the scaling policy.
    policyName :: Lude.Maybe Lude.Text,
    -- | Indicates whether the policy is enabled (@true@ ) or disabled (@false@ ).
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
    --
    --
    -- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-target-tracking.html Target tracking scaling policies> and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html Step and simple scaling policies> in the /Amazon EC2 Auto Scaling User Guide/ .
    policyType :: Lude.Maybe Lude.Text,
    -- | A set of adjustments that enable you to scale based on the size of the alarm breach.
    stepAdjustments :: Lude.Maybe [StepAdjustment],
    -- | A target tracking scaling policy.
    targetTrackingConfiguration :: Lude.Maybe TargetTrackingConfiguration,
    -- | Specifies how the scaling adjustment is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
    adjustmentType :: Lude.Maybe Lude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Lude.Maybe Lude.Text,
    -- | The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity.
    scalingAdjustment :: Lude.Maybe Lude.Int,
    -- | The duration of the policy's cooldown period, in seconds.
    cooldown :: Lude.Maybe Lude.Int,
    -- | The Amazon Resource Name (ARN) of the policy.
    policyARN :: Lude.Maybe Lude.Text,
    -- | The CloudWatch alarms related to the policy.
    alarms :: Lude.Maybe [Alarm],
    -- | The aggregation type for the CloudWatch metrics. The valid values are @Minimum@ , @Maximum@ , and @Average@ .
    metricAggregationType :: Lude.Maybe Lude.Text,
    -- | The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ .
    minAdjustmentMagnitude :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScalingPolicy' with the minimum fields required to make a request.
--
-- * 'minAdjustmentStep' - Available for backward compatibility. Use @MinAdjustmentMagnitude@ instead.
-- * 'estimatedInstanceWarmup' - The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics.
-- * 'policyName' - The name of the scaling policy.
-- * 'enabled' - Indicates whether the policy is enabled (@true@ ) or disabled (@false@ ).
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
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-target-tracking.html Target tracking scaling policies> and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html Step and simple scaling policies> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'stepAdjustments' - A set of adjustments that enable you to scale based on the size of the alarm breach.
-- * 'targetTrackingConfiguration' - A target tracking scaling policy.
-- * 'adjustmentType' - Specifies how the scaling adjustment is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'scalingAdjustment' - The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity.
-- * 'cooldown' - The duration of the policy's cooldown period, in seconds.
-- * 'policyARN' - The Amazon Resource Name (ARN) of the policy.
-- * 'alarms' - The CloudWatch alarms related to the policy.
-- * 'metricAggregationType' - The aggregation type for the CloudWatch metrics. The valid values are @Minimum@ , @Maximum@ , and @Average@ .
-- * 'minAdjustmentMagnitude' - The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ .
mkScalingPolicy ::
  ScalingPolicy
mkScalingPolicy =
  ScalingPolicy'
    { minAdjustmentStep = Lude.Nothing,
      estimatedInstanceWarmup = Lude.Nothing,
      policyName = Lude.Nothing,
      enabled = Lude.Nothing,
      policyType = Lude.Nothing,
      stepAdjustments = Lude.Nothing,
      targetTrackingConfiguration = Lude.Nothing,
      adjustmentType = Lude.Nothing,
      autoScalingGroupName = Lude.Nothing,
      scalingAdjustment = Lude.Nothing,
      cooldown = Lude.Nothing,
      policyARN = Lude.Nothing,
      alarms = Lude.Nothing,
      metricAggregationType = Lude.Nothing,
      minAdjustmentMagnitude = Lude.Nothing
    }

-- | Available for backward compatibility. Use @MinAdjustmentMagnitude@ instead.
--
-- /Note:/ Consider using 'minAdjustmentStep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spMinAdjustmentStep :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Int)
spMinAdjustmentStep = Lens.lens (minAdjustmentStep :: ScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {minAdjustmentStep = a} :: ScalingPolicy)
{-# DEPRECATED spMinAdjustmentStep "Use generic-lens or generic-optics with 'minAdjustmentStep' instead." #-}

-- | The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics.
--
-- /Note:/ Consider using 'estimatedInstanceWarmup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spEstimatedInstanceWarmup :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Int)
spEstimatedInstanceWarmup = Lens.lens (estimatedInstanceWarmup :: ScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {estimatedInstanceWarmup = a} :: ScalingPolicy)
{-# DEPRECATED spEstimatedInstanceWarmup "Use generic-lens or generic-optics with 'estimatedInstanceWarmup' instead." #-}

-- | The name of the scaling policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spPolicyName :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Text)
spPolicyName = Lens.lens (policyName :: ScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: ScalingPolicy)
{-# DEPRECATED spPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | Indicates whether the policy is enabled (@true@ ) or disabled (@false@ ).
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spEnabled :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Bool)
spEnabled = Lens.lens (enabled :: ScalingPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: ScalingPolicy)
{-# DEPRECATED spEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

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
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-target-tracking.html Target tracking scaling policies> and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html Step and simple scaling policies> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spPolicyType :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Text)
spPolicyType = Lens.lens (policyType :: ScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyType = a} :: ScalingPolicy)
{-# DEPRECATED spPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

-- | A set of adjustments that enable you to scale based on the size of the alarm breach.
--
-- /Note:/ Consider using 'stepAdjustments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spStepAdjustments :: Lens.Lens' ScalingPolicy (Lude.Maybe [StepAdjustment])
spStepAdjustments = Lens.lens (stepAdjustments :: ScalingPolicy -> Lude.Maybe [StepAdjustment]) (\s a -> s {stepAdjustments = a} :: ScalingPolicy)
{-# DEPRECATED spStepAdjustments "Use generic-lens or generic-optics with 'stepAdjustments' instead." #-}

-- | A target tracking scaling policy.
--
-- /Note:/ Consider using 'targetTrackingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spTargetTrackingConfiguration :: Lens.Lens' ScalingPolicy (Lude.Maybe TargetTrackingConfiguration)
spTargetTrackingConfiguration = Lens.lens (targetTrackingConfiguration :: ScalingPolicy -> Lude.Maybe TargetTrackingConfiguration) (\s a -> s {targetTrackingConfiguration = a} :: ScalingPolicy)
{-# DEPRECATED spTargetTrackingConfiguration "Use generic-lens or generic-optics with 'targetTrackingConfiguration' instead." #-}

-- | Specifies how the scaling adjustment is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
--
-- /Note:/ Consider using 'adjustmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spAdjustmentType :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Text)
spAdjustmentType = Lens.lens (adjustmentType :: ScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {adjustmentType = a} :: ScalingPolicy)
{-# DEPRECATED spAdjustmentType "Use generic-lens or generic-optics with 'adjustmentType' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spAutoScalingGroupName :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Text)
spAutoScalingGroupName = Lens.lens (autoScalingGroupName :: ScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingGroupName = a} :: ScalingPolicy)
{-# DEPRECATED spAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity.
--
-- /Note:/ Consider using 'scalingAdjustment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spScalingAdjustment :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Int)
spScalingAdjustment = Lens.lens (scalingAdjustment :: ScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {scalingAdjustment = a} :: ScalingPolicy)
{-# DEPRECATED spScalingAdjustment "Use generic-lens or generic-optics with 'scalingAdjustment' instead." #-}

-- | The duration of the policy's cooldown period, in seconds.
--
-- /Note:/ Consider using 'cooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spCooldown :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Int)
spCooldown = Lens.lens (cooldown :: ScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {cooldown = a} :: ScalingPolicy)
{-# DEPRECATED spCooldown "Use generic-lens or generic-optics with 'cooldown' instead." #-}

-- | The Amazon Resource Name (ARN) of the policy.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spPolicyARN :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Text)
spPolicyARN = Lens.lens (policyARN :: ScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyARN = a} :: ScalingPolicy)
{-# DEPRECATED spPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

-- | The CloudWatch alarms related to the policy.
--
-- /Note:/ Consider using 'alarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spAlarms :: Lens.Lens' ScalingPolicy (Lude.Maybe [Alarm])
spAlarms = Lens.lens (alarms :: ScalingPolicy -> Lude.Maybe [Alarm]) (\s a -> s {alarms = a} :: ScalingPolicy)
{-# DEPRECATED spAlarms "Use generic-lens or generic-optics with 'alarms' instead." #-}

-- | The aggregation type for the CloudWatch metrics. The valid values are @Minimum@ , @Maximum@ , and @Average@ .
--
-- /Note:/ Consider using 'metricAggregationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spMetricAggregationType :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Text)
spMetricAggregationType = Lens.lens (metricAggregationType :: ScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {metricAggregationType = a} :: ScalingPolicy)
{-# DEPRECATED spMetricAggregationType "Use generic-lens or generic-optics with 'metricAggregationType' instead." #-}

-- | The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ .
--
-- /Note:/ Consider using 'minAdjustmentMagnitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spMinAdjustmentMagnitude :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Int)
spMinAdjustmentMagnitude = Lens.lens (minAdjustmentMagnitude :: ScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {minAdjustmentMagnitude = a} :: ScalingPolicy)
{-# DEPRECATED spMinAdjustmentMagnitude "Use generic-lens or generic-optics with 'minAdjustmentMagnitude' instead." #-}

instance Lude.FromXML ScalingPolicy where
  parseXML x =
    ScalingPolicy'
      Lude.<$> (x Lude..@? "MinAdjustmentStep")
      Lude.<*> (x Lude..@? "EstimatedInstanceWarmup")
      Lude.<*> (x Lude..@? "PolicyName")
      Lude.<*> (x Lude..@? "Enabled")
      Lude.<*> (x Lude..@? "PolicyType")
      Lude.<*> ( x Lude..@? "StepAdjustments" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "TargetTrackingConfiguration")
      Lude.<*> (x Lude..@? "AdjustmentType")
      Lude.<*> (x Lude..@? "AutoScalingGroupName")
      Lude.<*> (x Lude..@? "ScalingAdjustment")
      Lude.<*> (x Lude..@? "Cooldown")
      Lude.<*> (x Lude..@? "PolicyARN")
      Lude.<*> ( x Lude..@? "Alarms" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "MetricAggregationType")
      Lude.<*> (x Lude..@? "MinAdjustmentMagnitude")
