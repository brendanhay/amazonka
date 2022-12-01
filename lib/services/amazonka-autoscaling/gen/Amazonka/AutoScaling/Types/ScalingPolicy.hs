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
-- Module      : Amazonka.AutoScaling.Types.ScalingPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.ScalingPolicy where

import Amazonka.AutoScaling.Types.Alarm
import Amazonka.AutoScaling.Types.PredictiveScalingConfiguration
import Amazonka.AutoScaling.Types.StepAdjustment
import Amazonka.AutoScaling.Types.TargetTrackingConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a scaling policy.
--
-- /See:/ 'newScalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
  { -- | The name of the scaling policy.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The CloudWatch alarms related to the policy.
    alarms :: Prelude.Maybe [Alarm],
    -- | The aggregation type for the CloudWatch metrics. The valid values are
    -- @Minimum@, @Maximum@, and @Average@.
    metricAggregationType :: Prelude.Maybe Prelude.Text,
    -- | One of the following policy types:
    --
    -- -   @TargetTrackingScaling@
    --
    -- -   @StepScaling@
    --
    -- -   @SimpleScaling@ (default)
    --
    -- -   @PredictiveScaling@
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-target-tracking.html Target tracking scaling policies>
    -- and
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html Step and simple scaling policies>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    policyType :: Prelude.Maybe Prelude.Text,
    -- | The duration of the policy\'s cooldown period, in seconds.
    cooldown :: Prelude.Maybe Prelude.Int,
    -- | Specifies how the scaling adjustment is interpreted (for example, an
    -- absolute number or a percentage). The valid values are
    -- @ChangeInCapacity@, @ExactCapacity@, and @PercentChangeInCapacity@.
    adjustmentType :: Prelude.Maybe Prelude.Text,
    -- | The estimated time, in seconds, until a newly launched instance can
    -- contribute to the CloudWatch metrics.
    estimatedInstanceWarmup :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the policy is enabled (@true@) or disabled (@false@).
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the policy.
    policyARN :: Prelude.Maybe Prelude.Text,
    -- | A target tracking scaling policy.
    targetTrackingConfiguration :: Prelude.Maybe TargetTrackingConfiguration,
    -- | Available for backward compatibility. Use @MinAdjustmentMagnitude@
    -- instead.
    minAdjustmentStep :: Prelude.Maybe Prelude.Int,
    -- | The amount by which to scale, based on the specified adjustment type. A
    -- positive value adds to the current capacity while a negative number
    -- removes from the current capacity.
    scalingAdjustment :: Prelude.Maybe Prelude.Int,
    -- | A set of adjustments that enable you to scale based on the size of the
    -- alarm breach.
    stepAdjustments :: Prelude.Maybe [StepAdjustment],
    -- | A predictive scaling policy.
    predictiveScalingConfiguration :: Prelude.Maybe PredictiveScalingConfiguration,
    -- | The minimum value to scale by when the adjustment type is
    -- @PercentChangeInCapacity@.
    minAdjustmentMagnitude :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'alarms', 'scalingPolicy_alarms' - The CloudWatch alarms related to the policy.
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
-- -   @PredictiveScaling@
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-target-tracking.html Target tracking scaling policies>
-- and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html Step and simple scaling policies>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'cooldown', 'scalingPolicy_cooldown' - The duration of the policy\'s cooldown period, in seconds.
--
-- 'adjustmentType', 'scalingPolicy_adjustmentType' - Specifies how the scaling adjustment is interpreted (for example, an
-- absolute number or a percentage). The valid values are
-- @ChangeInCapacity@, @ExactCapacity@, and @PercentChangeInCapacity@.
--
-- 'estimatedInstanceWarmup', 'scalingPolicy_estimatedInstanceWarmup' - The estimated time, in seconds, until a newly launched instance can
-- contribute to the CloudWatch metrics.
--
-- 'enabled', 'scalingPolicy_enabled' - Indicates whether the policy is enabled (@true@) or disabled (@false@).
--
-- 'autoScalingGroupName', 'scalingPolicy_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'policyARN', 'scalingPolicy_policyARN' - The Amazon Resource Name (ARN) of the policy.
--
-- 'targetTrackingConfiguration', 'scalingPolicy_targetTrackingConfiguration' - A target tracking scaling policy.
--
-- 'minAdjustmentStep', 'scalingPolicy_minAdjustmentStep' - Available for backward compatibility. Use @MinAdjustmentMagnitude@
-- instead.
--
-- 'scalingAdjustment', 'scalingPolicy_scalingAdjustment' - The amount by which to scale, based on the specified adjustment type. A
-- positive value adds to the current capacity while a negative number
-- removes from the current capacity.
--
-- 'stepAdjustments', 'scalingPolicy_stepAdjustments' - A set of adjustments that enable you to scale based on the size of the
-- alarm breach.
--
-- 'predictiveScalingConfiguration', 'scalingPolicy_predictiveScalingConfiguration' - A predictive scaling policy.
--
-- 'minAdjustmentMagnitude', 'scalingPolicy_minAdjustmentMagnitude' - The minimum value to scale by when the adjustment type is
-- @PercentChangeInCapacity@.
newScalingPolicy ::
  ScalingPolicy
newScalingPolicy =
  ScalingPolicy'
    { policyName = Prelude.Nothing,
      alarms = Prelude.Nothing,
      metricAggregationType = Prelude.Nothing,
      policyType = Prelude.Nothing,
      cooldown = Prelude.Nothing,
      adjustmentType = Prelude.Nothing,
      estimatedInstanceWarmup = Prelude.Nothing,
      enabled = Prelude.Nothing,
      autoScalingGroupName = Prelude.Nothing,
      policyARN = Prelude.Nothing,
      targetTrackingConfiguration = Prelude.Nothing,
      minAdjustmentStep = Prelude.Nothing,
      scalingAdjustment = Prelude.Nothing,
      stepAdjustments = Prelude.Nothing,
      predictiveScalingConfiguration = Prelude.Nothing,
      minAdjustmentMagnitude = Prelude.Nothing
    }

-- | The name of the scaling policy.
scalingPolicy_policyName :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Text)
scalingPolicy_policyName = Lens.lens (\ScalingPolicy' {policyName} -> policyName) (\s@ScalingPolicy' {} a -> s {policyName = a} :: ScalingPolicy)

-- | The CloudWatch alarms related to the policy.
scalingPolicy_alarms :: Lens.Lens' ScalingPolicy (Prelude.Maybe [Alarm])
scalingPolicy_alarms = Lens.lens (\ScalingPolicy' {alarms} -> alarms) (\s@ScalingPolicy' {} a -> s {alarms = a} :: ScalingPolicy) Prelude.. Lens.mapping Lens.coerced

-- | The aggregation type for the CloudWatch metrics. The valid values are
-- @Minimum@, @Maximum@, and @Average@.
scalingPolicy_metricAggregationType :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Text)
scalingPolicy_metricAggregationType = Lens.lens (\ScalingPolicy' {metricAggregationType} -> metricAggregationType) (\s@ScalingPolicy' {} a -> s {metricAggregationType = a} :: ScalingPolicy)

-- | One of the following policy types:
--
-- -   @TargetTrackingScaling@
--
-- -   @StepScaling@
--
-- -   @SimpleScaling@ (default)
--
-- -   @PredictiveScaling@
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-target-tracking.html Target tracking scaling policies>
-- and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html Step and simple scaling policies>
-- in the /Amazon EC2 Auto Scaling User Guide/.
scalingPolicy_policyType :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Text)
scalingPolicy_policyType = Lens.lens (\ScalingPolicy' {policyType} -> policyType) (\s@ScalingPolicy' {} a -> s {policyType = a} :: ScalingPolicy)

-- | The duration of the policy\'s cooldown period, in seconds.
scalingPolicy_cooldown :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Int)
scalingPolicy_cooldown = Lens.lens (\ScalingPolicy' {cooldown} -> cooldown) (\s@ScalingPolicy' {} a -> s {cooldown = a} :: ScalingPolicy)

-- | Specifies how the scaling adjustment is interpreted (for example, an
-- absolute number or a percentage). The valid values are
-- @ChangeInCapacity@, @ExactCapacity@, and @PercentChangeInCapacity@.
scalingPolicy_adjustmentType :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Text)
scalingPolicy_adjustmentType = Lens.lens (\ScalingPolicy' {adjustmentType} -> adjustmentType) (\s@ScalingPolicy' {} a -> s {adjustmentType = a} :: ScalingPolicy)

-- | The estimated time, in seconds, until a newly launched instance can
-- contribute to the CloudWatch metrics.
scalingPolicy_estimatedInstanceWarmup :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Int)
scalingPolicy_estimatedInstanceWarmup = Lens.lens (\ScalingPolicy' {estimatedInstanceWarmup} -> estimatedInstanceWarmup) (\s@ScalingPolicy' {} a -> s {estimatedInstanceWarmup = a} :: ScalingPolicy)

-- | Indicates whether the policy is enabled (@true@) or disabled (@false@).
scalingPolicy_enabled :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Bool)
scalingPolicy_enabled = Lens.lens (\ScalingPolicy' {enabled} -> enabled) (\s@ScalingPolicy' {} a -> s {enabled = a} :: ScalingPolicy)

-- | The name of the Auto Scaling group.
scalingPolicy_autoScalingGroupName :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Text)
scalingPolicy_autoScalingGroupName = Lens.lens (\ScalingPolicy' {autoScalingGroupName} -> autoScalingGroupName) (\s@ScalingPolicy' {} a -> s {autoScalingGroupName = a} :: ScalingPolicy)

-- | The Amazon Resource Name (ARN) of the policy.
scalingPolicy_policyARN :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Text)
scalingPolicy_policyARN = Lens.lens (\ScalingPolicy' {policyARN} -> policyARN) (\s@ScalingPolicy' {} a -> s {policyARN = a} :: ScalingPolicy)

-- | A target tracking scaling policy.
scalingPolicy_targetTrackingConfiguration :: Lens.Lens' ScalingPolicy (Prelude.Maybe TargetTrackingConfiguration)
scalingPolicy_targetTrackingConfiguration = Lens.lens (\ScalingPolicy' {targetTrackingConfiguration} -> targetTrackingConfiguration) (\s@ScalingPolicy' {} a -> s {targetTrackingConfiguration = a} :: ScalingPolicy)

-- | Available for backward compatibility. Use @MinAdjustmentMagnitude@
-- instead.
scalingPolicy_minAdjustmentStep :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Int)
scalingPolicy_minAdjustmentStep = Lens.lens (\ScalingPolicy' {minAdjustmentStep} -> minAdjustmentStep) (\s@ScalingPolicy' {} a -> s {minAdjustmentStep = a} :: ScalingPolicy)

-- | The amount by which to scale, based on the specified adjustment type. A
-- positive value adds to the current capacity while a negative number
-- removes from the current capacity.
scalingPolicy_scalingAdjustment :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Int)
scalingPolicy_scalingAdjustment = Lens.lens (\ScalingPolicy' {scalingAdjustment} -> scalingAdjustment) (\s@ScalingPolicy' {} a -> s {scalingAdjustment = a} :: ScalingPolicy)

-- | A set of adjustments that enable you to scale based on the size of the
-- alarm breach.
scalingPolicy_stepAdjustments :: Lens.Lens' ScalingPolicy (Prelude.Maybe [StepAdjustment])
scalingPolicy_stepAdjustments = Lens.lens (\ScalingPolicy' {stepAdjustments} -> stepAdjustments) (\s@ScalingPolicy' {} a -> s {stepAdjustments = a} :: ScalingPolicy) Prelude.. Lens.mapping Lens.coerced

-- | A predictive scaling policy.
scalingPolicy_predictiveScalingConfiguration :: Lens.Lens' ScalingPolicy (Prelude.Maybe PredictiveScalingConfiguration)
scalingPolicy_predictiveScalingConfiguration = Lens.lens (\ScalingPolicy' {predictiveScalingConfiguration} -> predictiveScalingConfiguration) (\s@ScalingPolicy' {} a -> s {predictiveScalingConfiguration = a} :: ScalingPolicy)

-- | The minimum value to scale by when the adjustment type is
-- @PercentChangeInCapacity@.
scalingPolicy_minAdjustmentMagnitude :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Int)
scalingPolicy_minAdjustmentMagnitude = Lens.lens (\ScalingPolicy' {minAdjustmentMagnitude} -> minAdjustmentMagnitude) (\s@ScalingPolicy' {} a -> s {minAdjustmentMagnitude = a} :: ScalingPolicy)

instance Core.FromXML ScalingPolicy where
  parseXML x =
    ScalingPolicy'
      Prelude.<$> (x Core..@? "PolicyName")
      Prelude.<*> ( x Core..@? "Alarms" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "MetricAggregationType")
      Prelude.<*> (x Core..@? "PolicyType")
      Prelude.<*> (x Core..@? "Cooldown")
      Prelude.<*> (x Core..@? "AdjustmentType")
      Prelude.<*> (x Core..@? "EstimatedInstanceWarmup")
      Prelude.<*> (x Core..@? "Enabled")
      Prelude.<*> (x Core..@? "AutoScalingGroupName")
      Prelude.<*> (x Core..@? "PolicyARN")
      Prelude.<*> (x Core..@? "TargetTrackingConfiguration")
      Prelude.<*> (x Core..@? "MinAdjustmentStep")
      Prelude.<*> (x Core..@? "ScalingAdjustment")
      Prelude.<*> ( x Core..@? "StepAdjustments" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "PredictiveScalingConfiguration")
      Prelude.<*> (x Core..@? "MinAdjustmentMagnitude")

instance Prelude.Hashable ScalingPolicy where
  hashWithSalt _salt ScalingPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` alarms
      `Prelude.hashWithSalt` metricAggregationType
      `Prelude.hashWithSalt` policyType
      `Prelude.hashWithSalt` cooldown
      `Prelude.hashWithSalt` adjustmentType
      `Prelude.hashWithSalt` estimatedInstanceWarmup
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` policyARN
      `Prelude.hashWithSalt` targetTrackingConfiguration
      `Prelude.hashWithSalt` minAdjustmentStep
      `Prelude.hashWithSalt` scalingAdjustment
      `Prelude.hashWithSalt` stepAdjustments
      `Prelude.hashWithSalt` predictiveScalingConfiguration
      `Prelude.hashWithSalt` minAdjustmentMagnitude

instance Prelude.NFData ScalingPolicy where
  rnf ScalingPolicy' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf alarms
      `Prelude.seq` Prelude.rnf metricAggregationType
      `Prelude.seq` Prelude.rnf policyType
      `Prelude.seq` Prelude.rnf cooldown
      `Prelude.seq` Prelude.rnf adjustmentType
      `Prelude.seq` Prelude.rnf estimatedInstanceWarmup
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf policyARN
      `Prelude.seq` Prelude.rnf targetTrackingConfiguration
      `Prelude.seq` Prelude.rnf minAdjustmentStep
      `Prelude.seq` Prelude.rnf scalingAdjustment
      `Prelude.seq` Prelude.rnf stepAdjustments
      `Prelude.seq` Prelude.rnf
        predictiveScalingConfiguration
      `Prelude.seq` Prelude.rnf minAdjustmentMagnitude
