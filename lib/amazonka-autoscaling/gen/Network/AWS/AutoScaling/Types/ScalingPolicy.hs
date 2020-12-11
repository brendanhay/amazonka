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
    sMinAdjustmentStep,
    sEstimatedInstanceWarmup,
    sPolicyName,
    sEnabled,
    sPolicyType,
    sStepAdjustments,
    sTargetTrackingConfiguration,
    sAdjustmentType,
    sAutoScalingGroupName,
    sScalingAdjustment,
    sCooldown,
    sPolicyARN,
    sAlarms,
    sMetricAggregationType,
    sMinAdjustmentMagnitude,
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
  { minAdjustmentStep ::
      Lude.Maybe Lude.Int,
    estimatedInstanceWarmup :: Lude.Maybe Lude.Int,
    policyName :: Lude.Maybe Lude.Text,
    enabled :: Lude.Maybe Lude.Bool,
    policyType :: Lude.Maybe Lude.Text,
    stepAdjustments :: Lude.Maybe [StepAdjustment],
    targetTrackingConfiguration ::
      Lude.Maybe TargetTrackingConfiguration,
    adjustmentType :: Lude.Maybe Lude.Text,
    autoScalingGroupName :: Lude.Maybe Lude.Text,
    scalingAdjustment :: Lude.Maybe Lude.Int,
    cooldown :: Lude.Maybe Lude.Int,
    policyARN :: Lude.Maybe Lude.Text,
    alarms :: Lude.Maybe [Alarm],
    metricAggregationType :: Lude.Maybe Lude.Text,
    minAdjustmentMagnitude :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScalingPolicy' with the minimum fields required to make a request.
--
-- * 'adjustmentType' - Specifies how the scaling adjustment is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
-- * 'alarms' - The CloudWatch alarms related to the policy.
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'cooldown' - The duration of the policy's cooldown period, in seconds.
-- * 'enabled' - Indicates whether the policy is enabled (@true@ ) or disabled (@false@ ).
-- * 'estimatedInstanceWarmup' - The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics.
-- * 'metricAggregationType' - The aggregation type for the CloudWatch metrics. The valid values are @Minimum@ , @Maximum@ , and @Average@ .
-- * 'minAdjustmentMagnitude' - The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ .
-- * 'minAdjustmentStep' - Available for backward compatibility. Use @MinAdjustmentMagnitude@ instead.
-- * 'policyARN' - The Amazon Resource Name (ARN) of the policy.
-- * 'policyName' - The name of the scaling policy.
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
-- * 'scalingAdjustment' - The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity.
-- * 'stepAdjustments' - A set of adjustments that enable you to scale based on the size of the alarm breach.
-- * 'targetTrackingConfiguration' - A target tracking scaling policy.
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
sMinAdjustmentStep :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Int)
sMinAdjustmentStep = Lens.lens (minAdjustmentStep :: ScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {minAdjustmentStep = a} :: ScalingPolicy)
{-# DEPRECATED sMinAdjustmentStep "Use generic-lens or generic-optics with 'minAdjustmentStep' instead." #-}

-- | The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics.
--
-- /Note:/ Consider using 'estimatedInstanceWarmup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEstimatedInstanceWarmup :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Int)
sEstimatedInstanceWarmup = Lens.lens (estimatedInstanceWarmup :: ScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {estimatedInstanceWarmup = a} :: ScalingPolicy)
{-# DEPRECATED sEstimatedInstanceWarmup "Use generic-lens or generic-optics with 'estimatedInstanceWarmup' instead." #-}

-- | The name of the scaling policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPolicyName :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Text)
sPolicyName = Lens.lens (policyName :: ScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: ScalingPolicy)
{-# DEPRECATED sPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | Indicates whether the policy is enabled (@true@ ) or disabled (@false@ ).
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEnabled :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Bool)
sEnabled = Lens.lens (enabled :: ScalingPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: ScalingPolicy)
{-# DEPRECATED sEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

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
sPolicyType :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Text)
sPolicyType = Lens.lens (policyType :: ScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyType = a} :: ScalingPolicy)
{-# DEPRECATED sPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

-- | A set of adjustments that enable you to scale based on the size of the alarm breach.
--
-- /Note:/ Consider using 'stepAdjustments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStepAdjustments :: Lens.Lens' ScalingPolicy (Lude.Maybe [StepAdjustment])
sStepAdjustments = Lens.lens (stepAdjustments :: ScalingPolicy -> Lude.Maybe [StepAdjustment]) (\s a -> s {stepAdjustments = a} :: ScalingPolicy)
{-# DEPRECATED sStepAdjustments "Use generic-lens or generic-optics with 'stepAdjustments' instead." #-}

-- | A target tracking scaling policy.
--
-- /Note:/ Consider using 'targetTrackingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTargetTrackingConfiguration :: Lens.Lens' ScalingPolicy (Lude.Maybe TargetTrackingConfiguration)
sTargetTrackingConfiguration = Lens.lens (targetTrackingConfiguration :: ScalingPolicy -> Lude.Maybe TargetTrackingConfiguration) (\s a -> s {targetTrackingConfiguration = a} :: ScalingPolicy)
{-# DEPRECATED sTargetTrackingConfiguration "Use generic-lens or generic-optics with 'targetTrackingConfiguration' instead." #-}

-- | Specifies how the scaling adjustment is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
--
-- /Note:/ Consider using 'adjustmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAdjustmentType :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Text)
sAdjustmentType = Lens.lens (adjustmentType :: ScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {adjustmentType = a} :: ScalingPolicy)
{-# DEPRECATED sAdjustmentType "Use generic-lens or generic-optics with 'adjustmentType' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAutoScalingGroupName :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Text)
sAutoScalingGroupName = Lens.lens (autoScalingGroupName :: ScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingGroupName = a} :: ScalingPolicy)
{-# DEPRECATED sAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity.
--
-- /Note:/ Consider using 'scalingAdjustment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sScalingAdjustment :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Int)
sScalingAdjustment = Lens.lens (scalingAdjustment :: ScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {scalingAdjustment = a} :: ScalingPolicy)
{-# DEPRECATED sScalingAdjustment "Use generic-lens or generic-optics with 'scalingAdjustment' instead." #-}

-- | The duration of the policy's cooldown period, in seconds.
--
-- /Note:/ Consider using 'cooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCooldown :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Int)
sCooldown = Lens.lens (cooldown :: ScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {cooldown = a} :: ScalingPolicy)
{-# DEPRECATED sCooldown "Use generic-lens or generic-optics with 'cooldown' instead." #-}

-- | The Amazon Resource Name (ARN) of the policy.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPolicyARN :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Text)
sPolicyARN = Lens.lens (policyARN :: ScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyARN = a} :: ScalingPolicy)
{-# DEPRECATED sPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

-- | The CloudWatch alarms related to the policy.
--
-- /Note:/ Consider using 'alarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAlarms :: Lens.Lens' ScalingPolicy (Lude.Maybe [Alarm])
sAlarms = Lens.lens (alarms :: ScalingPolicy -> Lude.Maybe [Alarm]) (\s a -> s {alarms = a} :: ScalingPolicy)
{-# DEPRECATED sAlarms "Use generic-lens or generic-optics with 'alarms' instead." #-}

-- | The aggregation type for the CloudWatch metrics. The valid values are @Minimum@ , @Maximum@ , and @Average@ .
--
-- /Note:/ Consider using 'metricAggregationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMetricAggregationType :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Text)
sMetricAggregationType = Lens.lens (metricAggregationType :: ScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {metricAggregationType = a} :: ScalingPolicy)
{-# DEPRECATED sMetricAggregationType "Use generic-lens or generic-optics with 'metricAggregationType' instead." #-}

-- | The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ .
--
-- /Note:/ Consider using 'minAdjustmentMagnitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMinAdjustmentMagnitude :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Int)
sMinAdjustmentMagnitude = Lens.lens (minAdjustmentMagnitude :: ScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {minAdjustmentMagnitude = a} :: ScalingPolicy)
{-# DEPRECATED sMinAdjustmentMagnitude "Use generic-lens or generic-optics with 'minAdjustmentMagnitude' instead." #-}

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
