{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ScalingPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.ScalingPolicy where

import Network.AWS.AutoScaling.Types.Alarm
import Network.AWS.AutoScaling.Types.StepAdjustment
import Network.AWS.AutoScaling.Types.TargetTrackingConfiguration
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a scaling policy.
--
-- /See:/ 'newScalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
  { -- | The name of the scaling policy.
    policyName :: Core.Maybe Core.Text,
    -- | A set of adjustments that enable you to scale based on the size of the
    -- alarm breach.
    stepAdjustments :: Core.Maybe [StepAdjustment],
    -- | A target tracking scaling policy.
    targetTrackingConfiguration :: Core.Maybe TargetTrackingConfiguration,
    -- | The aggregation type for the CloudWatch metrics. The valid values are
    -- @Minimum@, @Maximum@, and @Average@.
    metricAggregationType :: Core.Maybe Core.Text,
    -- | One of the following policy types:
    --
    -- -   @TargetTrackingScaling@
    --
    -- -   @StepScaling@
    --
    -- -   @SimpleScaling@ (default)
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-target-tracking.html Target tracking scaling policies>
    -- and
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html Step and simple scaling policies>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    policyType :: Core.Maybe Core.Text,
    -- | The duration of the policy\'s cooldown period, in seconds.
    cooldown :: Core.Maybe Core.Int,
    -- | Indicates whether the policy is enabled (@true@) or disabled (@false@).
    enabled :: Core.Maybe Core.Bool,
    -- | The amount by which to scale, based on the specified adjustment type. A
    -- positive value adds to the current capacity while a negative number
    -- removes from the current capacity.
    scalingAdjustment :: Core.Maybe Core.Int,
    -- | Specifies how the scaling adjustment is interpreted (for example, an
    -- absolute number or a percentage). The valid values are
    -- @ChangeInCapacity@, @ExactCapacity@, and @PercentChangeInCapacity@.
    adjustmentType :: Core.Maybe Core.Text,
    -- | Available for backward compatibility. Use @MinAdjustmentMagnitude@
    -- instead.
    minAdjustmentStep :: Core.Maybe Core.Int,
    -- | The estimated time, in seconds, until a newly launched instance can
    -- contribute to the CloudWatch metrics.
    estimatedInstanceWarmup :: Core.Maybe Core.Int,
    -- | The minimum value to scale by when the adjustment type is
    -- @PercentChangeInCapacity@.
    minAdjustmentMagnitude :: Core.Maybe Core.Int,
    -- | The CloudWatch alarms related to the policy.
    alarms :: Core.Maybe [Alarm],
    -- | The Amazon Resource Name (ARN) of the policy.
    policyARN :: Core.Maybe Core.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScalingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'scalingPolicy_policyName' - The name of the scaling policy.
--
-- 'stepAdjustments', 'scalingPolicy_stepAdjustments' - A set of adjustments that enable you to scale based on the size of the
-- alarm breach.
--
-- 'targetTrackingConfiguration', 'scalingPolicy_targetTrackingConfiguration' - A target tracking scaling policy.
--
-- 'metricAggregationType', 'scalingPolicy_metricAggregationType' - The aggregation type for the CloudWatch metrics. The valid values are
-- @Minimum@, @Maximum@, and @Average@.
--
-- 'policyType', 'scalingPolicy_policyType' - One of the following policy types:
--
-- -   @TargetTrackingScaling@
--
-- -   @StepScaling@
--
-- -   @SimpleScaling@ (default)
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-target-tracking.html Target tracking scaling policies>
-- and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html Step and simple scaling policies>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'cooldown', 'scalingPolicy_cooldown' - The duration of the policy\'s cooldown period, in seconds.
--
-- 'enabled', 'scalingPolicy_enabled' - Indicates whether the policy is enabled (@true@) or disabled (@false@).
--
-- 'scalingAdjustment', 'scalingPolicy_scalingAdjustment' - The amount by which to scale, based on the specified adjustment type. A
-- positive value adds to the current capacity while a negative number
-- removes from the current capacity.
--
-- 'adjustmentType', 'scalingPolicy_adjustmentType' - Specifies how the scaling adjustment is interpreted (for example, an
-- absolute number or a percentage). The valid values are
-- @ChangeInCapacity@, @ExactCapacity@, and @PercentChangeInCapacity@.
--
-- 'minAdjustmentStep', 'scalingPolicy_minAdjustmentStep' - Available for backward compatibility. Use @MinAdjustmentMagnitude@
-- instead.
--
-- 'estimatedInstanceWarmup', 'scalingPolicy_estimatedInstanceWarmup' - The estimated time, in seconds, until a newly launched instance can
-- contribute to the CloudWatch metrics.
--
-- 'minAdjustmentMagnitude', 'scalingPolicy_minAdjustmentMagnitude' - The minimum value to scale by when the adjustment type is
-- @PercentChangeInCapacity@.
--
-- 'alarms', 'scalingPolicy_alarms' - The CloudWatch alarms related to the policy.
--
-- 'policyARN', 'scalingPolicy_policyARN' - The Amazon Resource Name (ARN) of the policy.
--
-- 'autoScalingGroupName', 'scalingPolicy_autoScalingGroupName' - The name of the Auto Scaling group.
newScalingPolicy ::
  ScalingPolicy
newScalingPolicy =
  ScalingPolicy'
    { policyName = Core.Nothing,
      stepAdjustments = Core.Nothing,
      targetTrackingConfiguration = Core.Nothing,
      metricAggregationType = Core.Nothing,
      policyType = Core.Nothing,
      cooldown = Core.Nothing,
      enabled = Core.Nothing,
      scalingAdjustment = Core.Nothing,
      adjustmentType = Core.Nothing,
      minAdjustmentStep = Core.Nothing,
      estimatedInstanceWarmup = Core.Nothing,
      minAdjustmentMagnitude = Core.Nothing,
      alarms = Core.Nothing,
      policyARN = Core.Nothing,
      autoScalingGroupName = Core.Nothing
    }

-- | The name of the scaling policy.
scalingPolicy_policyName :: Lens.Lens' ScalingPolicy (Core.Maybe Core.Text)
scalingPolicy_policyName = Lens.lens (\ScalingPolicy' {policyName} -> policyName) (\s@ScalingPolicy' {} a -> s {policyName = a} :: ScalingPolicy)

-- | A set of adjustments that enable you to scale based on the size of the
-- alarm breach.
scalingPolicy_stepAdjustments :: Lens.Lens' ScalingPolicy (Core.Maybe [StepAdjustment])
scalingPolicy_stepAdjustments = Lens.lens (\ScalingPolicy' {stepAdjustments} -> stepAdjustments) (\s@ScalingPolicy' {} a -> s {stepAdjustments = a} :: ScalingPolicy) Core.. Lens.mapping Lens._Coerce

-- | A target tracking scaling policy.
scalingPolicy_targetTrackingConfiguration :: Lens.Lens' ScalingPolicy (Core.Maybe TargetTrackingConfiguration)
scalingPolicy_targetTrackingConfiguration = Lens.lens (\ScalingPolicy' {targetTrackingConfiguration} -> targetTrackingConfiguration) (\s@ScalingPolicy' {} a -> s {targetTrackingConfiguration = a} :: ScalingPolicy)

-- | The aggregation type for the CloudWatch metrics. The valid values are
-- @Minimum@, @Maximum@, and @Average@.
scalingPolicy_metricAggregationType :: Lens.Lens' ScalingPolicy (Core.Maybe Core.Text)
scalingPolicy_metricAggregationType = Lens.lens (\ScalingPolicy' {metricAggregationType} -> metricAggregationType) (\s@ScalingPolicy' {} a -> s {metricAggregationType = a} :: ScalingPolicy)

-- | One of the following policy types:
--
-- -   @TargetTrackingScaling@
--
-- -   @StepScaling@
--
-- -   @SimpleScaling@ (default)
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-target-tracking.html Target tracking scaling policies>
-- and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html Step and simple scaling policies>
-- in the /Amazon EC2 Auto Scaling User Guide/.
scalingPolicy_policyType :: Lens.Lens' ScalingPolicy (Core.Maybe Core.Text)
scalingPolicy_policyType = Lens.lens (\ScalingPolicy' {policyType} -> policyType) (\s@ScalingPolicy' {} a -> s {policyType = a} :: ScalingPolicy)

-- | The duration of the policy\'s cooldown period, in seconds.
scalingPolicy_cooldown :: Lens.Lens' ScalingPolicy (Core.Maybe Core.Int)
scalingPolicy_cooldown = Lens.lens (\ScalingPolicy' {cooldown} -> cooldown) (\s@ScalingPolicy' {} a -> s {cooldown = a} :: ScalingPolicy)

-- | Indicates whether the policy is enabled (@true@) or disabled (@false@).
scalingPolicy_enabled :: Lens.Lens' ScalingPolicy (Core.Maybe Core.Bool)
scalingPolicy_enabled = Lens.lens (\ScalingPolicy' {enabled} -> enabled) (\s@ScalingPolicy' {} a -> s {enabled = a} :: ScalingPolicy)

-- | The amount by which to scale, based on the specified adjustment type. A
-- positive value adds to the current capacity while a negative number
-- removes from the current capacity.
scalingPolicy_scalingAdjustment :: Lens.Lens' ScalingPolicy (Core.Maybe Core.Int)
scalingPolicy_scalingAdjustment = Lens.lens (\ScalingPolicy' {scalingAdjustment} -> scalingAdjustment) (\s@ScalingPolicy' {} a -> s {scalingAdjustment = a} :: ScalingPolicy)

-- | Specifies how the scaling adjustment is interpreted (for example, an
-- absolute number or a percentage). The valid values are
-- @ChangeInCapacity@, @ExactCapacity@, and @PercentChangeInCapacity@.
scalingPolicy_adjustmentType :: Lens.Lens' ScalingPolicy (Core.Maybe Core.Text)
scalingPolicy_adjustmentType = Lens.lens (\ScalingPolicy' {adjustmentType} -> adjustmentType) (\s@ScalingPolicy' {} a -> s {adjustmentType = a} :: ScalingPolicy)

-- | Available for backward compatibility. Use @MinAdjustmentMagnitude@
-- instead.
scalingPolicy_minAdjustmentStep :: Lens.Lens' ScalingPolicy (Core.Maybe Core.Int)
scalingPolicy_minAdjustmentStep = Lens.lens (\ScalingPolicy' {minAdjustmentStep} -> minAdjustmentStep) (\s@ScalingPolicy' {} a -> s {minAdjustmentStep = a} :: ScalingPolicy)

-- | The estimated time, in seconds, until a newly launched instance can
-- contribute to the CloudWatch metrics.
scalingPolicy_estimatedInstanceWarmup :: Lens.Lens' ScalingPolicy (Core.Maybe Core.Int)
scalingPolicy_estimatedInstanceWarmup = Lens.lens (\ScalingPolicy' {estimatedInstanceWarmup} -> estimatedInstanceWarmup) (\s@ScalingPolicy' {} a -> s {estimatedInstanceWarmup = a} :: ScalingPolicy)

-- | The minimum value to scale by when the adjustment type is
-- @PercentChangeInCapacity@.
scalingPolicy_minAdjustmentMagnitude :: Lens.Lens' ScalingPolicy (Core.Maybe Core.Int)
scalingPolicy_minAdjustmentMagnitude = Lens.lens (\ScalingPolicy' {minAdjustmentMagnitude} -> minAdjustmentMagnitude) (\s@ScalingPolicy' {} a -> s {minAdjustmentMagnitude = a} :: ScalingPolicy)

-- | The CloudWatch alarms related to the policy.
scalingPolicy_alarms :: Lens.Lens' ScalingPolicy (Core.Maybe [Alarm])
scalingPolicy_alarms = Lens.lens (\ScalingPolicy' {alarms} -> alarms) (\s@ScalingPolicy' {} a -> s {alarms = a} :: ScalingPolicy) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the policy.
scalingPolicy_policyARN :: Lens.Lens' ScalingPolicy (Core.Maybe Core.Text)
scalingPolicy_policyARN = Lens.lens (\ScalingPolicy' {policyARN} -> policyARN) (\s@ScalingPolicy' {} a -> s {policyARN = a} :: ScalingPolicy)

-- | The name of the Auto Scaling group.
scalingPolicy_autoScalingGroupName :: Lens.Lens' ScalingPolicy (Core.Maybe Core.Text)
scalingPolicy_autoScalingGroupName = Lens.lens (\ScalingPolicy' {autoScalingGroupName} -> autoScalingGroupName) (\s@ScalingPolicy' {} a -> s {autoScalingGroupName = a} :: ScalingPolicy)

instance Core.FromXML ScalingPolicy where
  parseXML x =
    ScalingPolicy'
      Core.<$> (x Core..@? "PolicyName")
      Core.<*> ( x Core..@? "StepAdjustments" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "TargetTrackingConfiguration")
      Core.<*> (x Core..@? "MetricAggregationType")
      Core.<*> (x Core..@? "PolicyType")
      Core.<*> (x Core..@? "Cooldown")
      Core.<*> (x Core..@? "Enabled")
      Core.<*> (x Core..@? "ScalingAdjustment")
      Core.<*> (x Core..@? "AdjustmentType")
      Core.<*> (x Core..@? "MinAdjustmentStep")
      Core.<*> (x Core..@? "EstimatedInstanceWarmup")
      Core.<*> (x Core..@? "MinAdjustmentMagnitude")
      Core.<*> ( x Core..@? "Alarms" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "PolicyARN")
      Core.<*> (x Core..@? "AutoScalingGroupName")

instance Core.Hashable ScalingPolicy

instance Core.NFData ScalingPolicy
