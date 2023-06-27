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
-- Module      : Amazonka.ApplicationAutoScaling.Types.StepAdjustment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.StepAdjustment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a step adjustment for a
-- <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepScalingPolicyConfiguration.html StepScalingPolicyConfiguration>.
-- Describes an adjustment based on the difference between the value of the
-- aggregated CloudWatch metric and the breach threshold that you\'ve
-- defined for the alarm.
--
-- For the following examples, suppose that you have an alarm with a breach
-- threshold of 50:
--
-- -   To initiate the adjustment when the metric is greater than or equal
--     to 50 and less than 60, specify a lower bound of @0@ and an upper
--     bound of @10@.
--
-- -   To initiate the adjustment when the metric is greater than 40 and
--     less than or equal to 50, specify a lower bound of @-10@ and an
--     upper bound of @0@.
--
-- There are a few rules for the step adjustments for your step policy:
--
-- -   The ranges of your step adjustments can\'t overlap or have a gap.
--
-- -   At most one step adjustment can have a null lower bound. If one step
--     adjustment has a negative lower bound, then there must be a step
--     adjustment with a null lower bound.
--
-- -   At most one step adjustment can have a null upper bound. If one step
--     adjustment has a positive upper bound, then there must be a step
--     adjustment with a null upper bound.
--
-- -   The upper and lower bound can\'t be null in the same step
--     adjustment.
--
-- /See:/ 'newStepAdjustment' smart constructor.
data StepAdjustment = StepAdjustment'
  { -- | The lower bound for the difference between the alarm threshold and the
    -- CloudWatch metric. If the metric value is above the breach threshold,
    -- the lower bound is inclusive (the metric must be greater than or equal
    -- to the threshold plus the lower bound). Otherwise, it\'s exclusive (the
    -- metric must be greater than the threshold plus the lower bound). A null
    -- value indicates negative infinity.
    metricIntervalLowerBound :: Prelude.Maybe Prelude.Double,
    -- | The upper bound for the difference between the alarm threshold and the
    -- CloudWatch metric. If the metric value is above the breach threshold,
    -- the upper bound is exclusive (the metric must be less than the threshold
    -- plus the upper bound). Otherwise, it\'s inclusive (the metric must be
    -- less than or equal to the threshold plus the upper bound). A null value
    -- indicates positive infinity.
    --
    -- The upper bound must be greater than the lower bound.
    metricIntervalUpperBound :: Prelude.Maybe Prelude.Double,
    -- | The amount by which to scale, based on the specified adjustment type. A
    -- positive value adds to the current capacity while a negative number
    -- removes from the current capacity. For exact capacity, you must specify
    -- a non-negative value.
    scalingAdjustment :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StepAdjustment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricIntervalLowerBound', 'stepAdjustment_metricIntervalLowerBound' - The lower bound for the difference between the alarm threshold and the
-- CloudWatch metric. If the metric value is above the breach threshold,
-- the lower bound is inclusive (the metric must be greater than or equal
-- to the threshold plus the lower bound). Otherwise, it\'s exclusive (the
-- metric must be greater than the threshold plus the lower bound). A null
-- value indicates negative infinity.
--
-- 'metricIntervalUpperBound', 'stepAdjustment_metricIntervalUpperBound' - The upper bound for the difference between the alarm threshold and the
-- CloudWatch metric. If the metric value is above the breach threshold,
-- the upper bound is exclusive (the metric must be less than the threshold
-- plus the upper bound). Otherwise, it\'s inclusive (the metric must be
-- less than or equal to the threshold plus the upper bound). A null value
-- indicates positive infinity.
--
-- The upper bound must be greater than the lower bound.
--
-- 'scalingAdjustment', 'stepAdjustment_scalingAdjustment' - The amount by which to scale, based on the specified adjustment type. A
-- positive value adds to the current capacity while a negative number
-- removes from the current capacity. For exact capacity, you must specify
-- a non-negative value.
newStepAdjustment ::
  -- | 'scalingAdjustment'
  Prelude.Int ->
  StepAdjustment
newStepAdjustment pScalingAdjustment_ =
  StepAdjustment'
    { metricIntervalLowerBound =
        Prelude.Nothing,
      metricIntervalUpperBound = Prelude.Nothing,
      scalingAdjustment = pScalingAdjustment_
    }

-- | The lower bound for the difference between the alarm threshold and the
-- CloudWatch metric. If the metric value is above the breach threshold,
-- the lower bound is inclusive (the metric must be greater than or equal
-- to the threshold plus the lower bound). Otherwise, it\'s exclusive (the
-- metric must be greater than the threshold plus the lower bound). A null
-- value indicates negative infinity.
stepAdjustment_metricIntervalLowerBound :: Lens.Lens' StepAdjustment (Prelude.Maybe Prelude.Double)
stepAdjustment_metricIntervalLowerBound = Lens.lens (\StepAdjustment' {metricIntervalLowerBound} -> metricIntervalLowerBound) (\s@StepAdjustment' {} a -> s {metricIntervalLowerBound = a} :: StepAdjustment)

-- | The upper bound for the difference between the alarm threshold and the
-- CloudWatch metric. If the metric value is above the breach threshold,
-- the upper bound is exclusive (the metric must be less than the threshold
-- plus the upper bound). Otherwise, it\'s inclusive (the metric must be
-- less than or equal to the threshold plus the upper bound). A null value
-- indicates positive infinity.
--
-- The upper bound must be greater than the lower bound.
stepAdjustment_metricIntervalUpperBound :: Lens.Lens' StepAdjustment (Prelude.Maybe Prelude.Double)
stepAdjustment_metricIntervalUpperBound = Lens.lens (\StepAdjustment' {metricIntervalUpperBound} -> metricIntervalUpperBound) (\s@StepAdjustment' {} a -> s {metricIntervalUpperBound = a} :: StepAdjustment)

-- | The amount by which to scale, based on the specified adjustment type. A
-- positive value adds to the current capacity while a negative number
-- removes from the current capacity. For exact capacity, you must specify
-- a non-negative value.
stepAdjustment_scalingAdjustment :: Lens.Lens' StepAdjustment Prelude.Int
stepAdjustment_scalingAdjustment = Lens.lens (\StepAdjustment' {scalingAdjustment} -> scalingAdjustment) (\s@StepAdjustment' {} a -> s {scalingAdjustment = a} :: StepAdjustment)

instance Data.FromJSON StepAdjustment where
  parseJSON =
    Data.withObject
      "StepAdjustment"
      ( \x ->
          StepAdjustment'
            Prelude.<$> (x Data..:? "MetricIntervalLowerBound")
            Prelude.<*> (x Data..:? "MetricIntervalUpperBound")
            Prelude.<*> (x Data..: "ScalingAdjustment")
      )

instance Prelude.Hashable StepAdjustment where
  hashWithSalt _salt StepAdjustment' {..} =
    _salt
      `Prelude.hashWithSalt` metricIntervalLowerBound
      `Prelude.hashWithSalt` metricIntervalUpperBound
      `Prelude.hashWithSalt` scalingAdjustment

instance Prelude.NFData StepAdjustment where
  rnf StepAdjustment' {..} =
    Prelude.rnf metricIntervalLowerBound
      `Prelude.seq` Prelude.rnf metricIntervalUpperBound
      `Prelude.seq` Prelude.rnf scalingAdjustment

instance Data.ToJSON StepAdjustment where
  toJSON StepAdjustment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MetricIntervalLowerBound" Data..=)
              Prelude.<$> metricIntervalLowerBound,
            ("MetricIntervalUpperBound" Data..=)
              Prelude.<$> metricIntervalUpperBound,
            Prelude.Just
              ("ScalingAdjustment" Data..= scalingAdjustment)
          ]
      )
