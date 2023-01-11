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
-- Module      : Amazonka.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration where

import Amazonka.ApplicationAutoScaling.Types.AdjustmentType
import Amazonka.ApplicationAutoScaling.Types.MetricAggregationType
import Amazonka.ApplicationAutoScaling.Types.StepAdjustment
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a step scaling policy configuration to use with Application
-- Auto Scaling.
--
-- /See:/ 'newStepScalingPolicyConfiguration' smart constructor.
data StepScalingPolicyConfiguration = StepScalingPolicyConfiguration'
  { -- | Specifies how the @ScalingAdjustment@ value in a
    -- <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepAdjustment.html StepAdjustment>
    -- is interpreted (for example, an absolute number or a percentage). The
    -- valid values are @ChangeInCapacity@, @ExactCapacity@, and
    -- @PercentChangeInCapacity@.
    --
    -- @AdjustmentType@ is required if you are adding a new step scaling policy
    -- configuration.
    adjustmentType :: Prelude.Maybe AdjustmentType,
    -- | The amount of time, in seconds, to wait for a previous scaling activity
    -- to take effect.
    --
    -- With scale-out policies, the intention is to continuously (but not
    -- excessively) scale out. After Application Auto Scaling successfully
    -- scales out using a step scaling policy, it starts to calculate the
    -- cooldown time. The scaling policy won\'t increase the desired capacity
    -- again unless either a larger scale out is triggered or the cooldown
    -- period ends. While the cooldown period is in effect, capacity added by
    -- the initiating scale-out activity is calculated as part of the desired
    -- capacity for the next scale-out activity. For example, when an alarm
    -- triggers a step scaling policy to increase the capacity by 2, the
    -- scaling activity completes successfully, and a cooldown period starts.
    -- If the alarm triggers again during the cooldown period but at a more
    -- aggressive step adjustment of 3, the previous increase of 2 is
    -- considered part of the current capacity. Therefore, only 1 is added to
    -- the capacity.
    --
    -- With scale-in policies, the intention is to scale in conservatively to
    -- protect your application’s availability, so scale-in activities are
    -- blocked until the cooldown period has expired. However, if another alarm
    -- triggers a scale-out activity during the cooldown period after a
    -- scale-in activity, Application Auto Scaling scales out the target
    -- immediately. In this case, the cooldown period for the scale-in activity
    -- stops and doesn\'t complete.
    --
    -- Application Auto Scaling provides a default value of 600 for Amazon
    -- ElastiCache replication groups and a default value of 300 for the
    -- following scalable targets:
    --
    -- -   AppStream 2.0 fleets
    --
    -- -   Aurora DB clusters
    --
    -- -   ECS services
    --
    -- -   EMR clusters
    --
    -- -   Neptune clusters
    --
    -- -   SageMaker endpoint variants
    --
    -- -   Spot Fleets
    --
    -- -   Custom resources
    --
    -- For all other scalable targets, the default value is 0:
    --
    -- -   Amazon Comprehend document classification and entity recognizer
    --     endpoints
    --
    -- -   DynamoDB tables and global secondary indexes
    --
    -- -   Amazon Keyspaces tables
    --
    -- -   Lambda provisioned concurrency
    --
    -- -   Amazon MSK broker storage
    cooldown :: Prelude.Maybe Prelude.Int,
    -- | The aggregation type for the CloudWatch metrics. Valid values are
    -- @Minimum@, @Maximum@, and @Average@. If the aggregation type is null,
    -- the value is treated as @Average@.
    metricAggregationType :: Prelude.Maybe MetricAggregationType,
    -- | The minimum value to scale by when the adjustment type is
    -- @PercentChangeInCapacity@. For example, suppose that you create a step
    -- scaling policy to scale out an Amazon ECS service by 25 percent and you
    -- specify a @MinAdjustmentMagnitude@ of 2. If the service has 4 tasks and
    -- the scaling policy is performed, 25 percent of 4 is 1. However, because
    -- you specified a @MinAdjustmentMagnitude@ of 2, Application Auto Scaling
    -- scales out the service by 2 tasks.
    minAdjustmentMagnitude :: Prelude.Maybe Prelude.Int,
    -- | A set of adjustments that enable you to scale based on the size of the
    -- alarm breach.
    --
    -- At least one step adjustment is required if you are adding a new step
    -- scaling policy configuration.
    stepAdjustments :: Prelude.Maybe [StepAdjustment]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StepScalingPolicyConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adjustmentType', 'stepScalingPolicyConfiguration_adjustmentType' - Specifies how the @ScalingAdjustment@ value in a
-- <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepAdjustment.html StepAdjustment>
-- is interpreted (for example, an absolute number or a percentage). The
-- valid values are @ChangeInCapacity@, @ExactCapacity@, and
-- @PercentChangeInCapacity@.
--
-- @AdjustmentType@ is required if you are adding a new step scaling policy
-- configuration.
--
-- 'cooldown', 'stepScalingPolicyConfiguration_cooldown' - The amount of time, in seconds, to wait for a previous scaling activity
-- to take effect.
--
-- With scale-out policies, the intention is to continuously (but not
-- excessively) scale out. After Application Auto Scaling successfully
-- scales out using a step scaling policy, it starts to calculate the
-- cooldown time. The scaling policy won\'t increase the desired capacity
-- again unless either a larger scale out is triggered or the cooldown
-- period ends. While the cooldown period is in effect, capacity added by
-- the initiating scale-out activity is calculated as part of the desired
-- capacity for the next scale-out activity. For example, when an alarm
-- triggers a step scaling policy to increase the capacity by 2, the
-- scaling activity completes successfully, and a cooldown period starts.
-- If the alarm triggers again during the cooldown period but at a more
-- aggressive step adjustment of 3, the previous increase of 2 is
-- considered part of the current capacity. Therefore, only 1 is added to
-- the capacity.
--
-- With scale-in policies, the intention is to scale in conservatively to
-- protect your application’s availability, so scale-in activities are
-- blocked until the cooldown period has expired. However, if another alarm
-- triggers a scale-out activity during the cooldown period after a
-- scale-in activity, Application Auto Scaling scales out the target
-- immediately. In this case, the cooldown period for the scale-in activity
-- stops and doesn\'t complete.
--
-- Application Auto Scaling provides a default value of 600 for Amazon
-- ElastiCache replication groups and a default value of 300 for the
-- following scalable targets:
--
-- -   AppStream 2.0 fleets
--
-- -   Aurora DB clusters
--
-- -   ECS services
--
-- -   EMR clusters
--
-- -   Neptune clusters
--
-- -   SageMaker endpoint variants
--
-- -   Spot Fleets
--
-- -   Custom resources
--
-- For all other scalable targets, the default value is 0:
--
-- -   Amazon Comprehend document classification and entity recognizer
--     endpoints
--
-- -   DynamoDB tables and global secondary indexes
--
-- -   Amazon Keyspaces tables
--
-- -   Lambda provisioned concurrency
--
-- -   Amazon MSK broker storage
--
-- 'metricAggregationType', 'stepScalingPolicyConfiguration_metricAggregationType' - The aggregation type for the CloudWatch metrics. Valid values are
-- @Minimum@, @Maximum@, and @Average@. If the aggregation type is null,
-- the value is treated as @Average@.
--
-- 'minAdjustmentMagnitude', 'stepScalingPolicyConfiguration_minAdjustmentMagnitude' - The minimum value to scale by when the adjustment type is
-- @PercentChangeInCapacity@. For example, suppose that you create a step
-- scaling policy to scale out an Amazon ECS service by 25 percent and you
-- specify a @MinAdjustmentMagnitude@ of 2. If the service has 4 tasks and
-- the scaling policy is performed, 25 percent of 4 is 1. However, because
-- you specified a @MinAdjustmentMagnitude@ of 2, Application Auto Scaling
-- scales out the service by 2 tasks.
--
-- 'stepAdjustments', 'stepScalingPolicyConfiguration_stepAdjustments' - A set of adjustments that enable you to scale based on the size of the
-- alarm breach.
--
-- At least one step adjustment is required if you are adding a new step
-- scaling policy configuration.
newStepScalingPolicyConfiguration ::
  StepScalingPolicyConfiguration
newStepScalingPolicyConfiguration =
  StepScalingPolicyConfiguration'
    { adjustmentType =
        Prelude.Nothing,
      cooldown = Prelude.Nothing,
      metricAggregationType = Prelude.Nothing,
      minAdjustmentMagnitude = Prelude.Nothing,
      stepAdjustments = Prelude.Nothing
    }

-- | Specifies how the @ScalingAdjustment@ value in a
-- <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepAdjustment.html StepAdjustment>
-- is interpreted (for example, an absolute number or a percentage). The
-- valid values are @ChangeInCapacity@, @ExactCapacity@, and
-- @PercentChangeInCapacity@.
--
-- @AdjustmentType@ is required if you are adding a new step scaling policy
-- configuration.
stepScalingPolicyConfiguration_adjustmentType :: Lens.Lens' StepScalingPolicyConfiguration (Prelude.Maybe AdjustmentType)
stepScalingPolicyConfiguration_adjustmentType = Lens.lens (\StepScalingPolicyConfiguration' {adjustmentType} -> adjustmentType) (\s@StepScalingPolicyConfiguration' {} a -> s {adjustmentType = a} :: StepScalingPolicyConfiguration)

-- | The amount of time, in seconds, to wait for a previous scaling activity
-- to take effect.
--
-- With scale-out policies, the intention is to continuously (but not
-- excessively) scale out. After Application Auto Scaling successfully
-- scales out using a step scaling policy, it starts to calculate the
-- cooldown time. The scaling policy won\'t increase the desired capacity
-- again unless either a larger scale out is triggered or the cooldown
-- period ends. While the cooldown period is in effect, capacity added by
-- the initiating scale-out activity is calculated as part of the desired
-- capacity for the next scale-out activity. For example, when an alarm
-- triggers a step scaling policy to increase the capacity by 2, the
-- scaling activity completes successfully, and a cooldown period starts.
-- If the alarm triggers again during the cooldown period but at a more
-- aggressive step adjustment of 3, the previous increase of 2 is
-- considered part of the current capacity. Therefore, only 1 is added to
-- the capacity.
--
-- With scale-in policies, the intention is to scale in conservatively to
-- protect your application’s availability, so scale-in activities are
-- blocked until the cooldown period has expired. However, if another alarm
-- triggers a scale-out activity during the cooldown period after a
-- scale-in activity, Application Auto Scaling scales out the target
-- immediately. In this case, the cooldown period for the scale-in activity
-- stops and doesn\'t complete.
--
-- Application Auto Scaling provides a default value of 600 for Amazon
-- ElastiCache replication groups and a default value of 300 for the
-- following scalable targets:
--
-- -   AppStream 2.0 fleets
--
-- -   Aurora DB clusters
--
-- -   ECS services
--
-- -   EMR clusters
--
-- -   Neptune clusters
--
-- -   SageMaker endpoint variants
--
-- -   Spot Fleets
--
-- -   Custom resources
--
-- For all other scalable targets, the default value is 0:
--
-- -   Amazon Comprehend document classification and entity recognizer
--     endpoints
--
-- -   DynamoDB tables and global secondary indexes
--
-- -   Amazon Keyspaces tables
--
-- -   Lambda provisioned concurrency
--
-- -   Amazon MSK broker storage
stepScalingPolicyConfiguration_cooldown :: Lens.Lens' StepScalingPolicyConfiguration (Prelude.Maybe Prelude.Int)
stepScalingPolicyConfiguration_cooldown = Lens.lens (\StepScalingPolicyConfiguration' {cooldown} -> cooldown) (\s@StepScalingPolicyConfiguration' {} a -> s {cooldown = a} :: StepScalingPolicyConfiguration)

-- | The aggregation type for the CloudWatch metrics. Valid values are
-- @Minimum@, @Maximum@, and @Average@. If the aggregation type is null,
-- the value is treated as @Average@.
stepScalingPolicyConfiguration_metricAggregationType :: Lens.Lens' StepScalingPolicyConfiguration (Prelude.Maybe MetricAggregationType)
stepScalingPolicyConfiguration_metricAggregationType = Lens.lens (\StepScalingPolicyConfiguration' {metricAggregationType} -> metricAggregationType) (\s@StepScalingPolicyConfiguration' {} a -> s {metricAggregationType = a} :: StepScalingPolicyConfiguration)

-- | The minimum value to scale by when the adjustment type is
-- @PercentChangeInCapacity@. For example, suppose that you create a step
-- scaling policy to scale out an Amazon ECS service by 25 percent and you
-- specify a @MinAdjustmentMagnitude@ of 2. If the service has 4 tasks and
-- the scaling policy is performed, 25 percent of 4 is 1. However, because
-- you specified a @MinAdjustmentMagnitude@ of 2, Application Auto Scaling
-- scales out the service by 2 tasks.
stepScalingPolicyConfiguration_minAdjustmentMagnitude :: Lens.Lens' StepScalingPolicyConfiguration (Prelude.Maybe Prelude.Int)
stepScalingPolicyConfiguration_minAdjustmentMagnitude = Lens.lens (\StepScalingPolicyConfiguration' {minAdjustmentMagnitude} -> minAdjustmentMagnitude) (\s@StepScalingPolicyConfiguration' {} a -> s {minAdjustmentMagnitude = a} :: StepScalingPolicyConfiguration)

-- | A set of adjustments that enable you to scale based on the size of the
-- alarm breach.
--
-- At least one step adjustment is required if you are adding a new step
-- scaling policy configuration.
stepScalingPolicyConfiguration_stepAdjustments :: Lens.Lens' StepScalingPolicyConfiguration (Prelude.Maybe [StepAdjustment])
stepScalingPolicyConfiguration_stepAdjustments = Lens.lens (\StepScalingPolicyConfiguration' {stepAdjustments} -> stepAdjustments) (\s@StepScalingPolicyConfiguration' {} a -> s {stepAdjustments = a} :: StepScalingPolicyConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON StepScalingPolicyConfiguration where
  parseJSON =
    Data.withObject
      "StepScalingPolicyConfiguration"
      ( \x ->
          StepScalingPolicyConfiguration'
            Prelude.<$> (x Data..:? "AdjustmentType")
            Prelude.<*> (x Data..:? "Cooldown")
            Prelude.<*> (x Data..:? "MetricAggregationType")
            Prelude.<*> (x Data..:? "MinAdjustmentMagnitude")
            Prelude.<*> ( x Data..:? "StepAdjustments"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    StepScalingPolicyConfiguration
  where
  hashWithSalt
    _salt
    StepScalingPolicyConfiguration' {..} =
      _salt `Prelude.hashWithSalt` adjustmentType
        `Prelude.hashWithSalt` cooldown
        `Prelude.hashWithSalt` metricAggregationType
        `Prelude.hashWithSalt` minAdjustmentMagnitude
        `Prelude.hashWithSalt` stepAdjustments

instance
  Prelude.NFData
    StepScalingPolicyConfiguration
  where
  rnf StepScalingPolicyConfiguration' {..} =
    Prelude.rnf adjustmentType
      `Prelude.seq` Prelude.rnf cooldown
      `Prelude.seq` Prelude.rnf metricAggregationType
      `Prelude.seq` Prelude.rnf minAdjustmentMagnitude
      `Prelude.seq` Prelude.rnf stepAdjustments

instance Data.ToJSON StepScalingPolicyConfiguration where
  toJSON StepScalingPolicyConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdjustmentType" Data..=)
              Prelude.<$> adjustmentType,
            ("Cooldown" Data..=) Prelude.<$> cooldown,
            ("MetricAggregationType" Data..=)
              Prelude.<$> metricAggregationType,
            ("MinAdjustmentMagnitude" Data..=)
              Prelude.<$> minAdjustmentMagnitude,
            ("StepAdjustments" Data..=)
              Prelude.<$> stepAdjustments
          ]
      )
