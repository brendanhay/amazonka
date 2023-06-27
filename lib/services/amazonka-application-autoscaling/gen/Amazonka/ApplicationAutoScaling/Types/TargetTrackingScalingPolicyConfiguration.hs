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
-- Module      : Amazonka.ApplicationAutoScaling.Types.TargetTrackingScalingPolicyConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.TargetTrackingScalingPolicyConfiguration where

import Amazonka.ApplicationAutoScaling.Types.CustomizedMetricSpecification
import Amazonka.ApplicationAutoScaling.Types.PredefinedMetricSpecification
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a target tracking scaling policy configuration to use with
-- Application Auto Scaling.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html Target tracking scaling policies>
-- in the /Application Auto Scaling User Guide/.
--
-- /See:/ 'newTargetTrackingScalingPolicyConfiguration' smart constructor.
data TargetTrackingScalingPolicyConfiguration = TargetTrackingScalingPolicyConfiguration'
  { -- | A customized metric. You can specify either a predefined metric or a
    -- customized metric.
    customizedMetricSpecification :: Prelude.Maybe CustomizedMetricSpecification,
    -- | Indicates whether scale in by the target tracking scaling policy is
    -- disabled. If the value is @true@, scale in is disabled and the target
    -- tracking scaling policy won\'t remove capacity from the scalable target.
    -- Otherwise, scale in is enabled and the target tracking scaling policy
    -- can remove capacity from the scalable target. The default value is
    -- @false@.
    disableScaleIn :: Prelude.Maybe Prelude.Bool,
    -- | A predefined metric. You can specify either a predefined metric or a
    -- customized metric.
    predefinedMetricSpecification :: Prelude.Maybe PredefinedMetricSpecification,
    -- | The amount of time, in seconds, after a scale-in activity completes
    -- before another scale-in activity can start. For more information and for
    -- default values, see
    -- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html#target-tracking-cooldown Define cooldown periods>
    -- in the /Application Auto Scaling User Guide/.
    scaleInCooldown :: Prelude.Maybe Prelude.Int,
    -- | The amount of time, in seconds, to wait for a previous scale-out
    -- activity to take effect. For more information and for default values,
    -- see
    -- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html#target-tracking-cooldown Define cooldown periods>
    -- in the /Application Auto Scaling User Guide/.
    scaleOutCooldown :: Prelude.Maybe Prelude.Int,
    -- | The target value for the metric. Although this property accepts numbers
    -- of type Double, it won\'t accept values that are either too small or too
    -- large. Values must be in the range of -2^360 to 2^360. The value must be
    -- a valid number based on the choice of metric. For example, if the metric
    -- is CPU utilization, then the target value is a percent value that
    -- represents how much of the CPU can be used before scaling out.
    --
    -- If the scaling policy specifies the @ALBRequestCountPerTarget@
    -- predefined metric, specify the target utilization as the optimal average
    -- request count per target during any one-minute interval.
    targetValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetTrackingScalingPolicyConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customizedMetricSpecification', 'targetTrackingScalingPolicyConfiguration_customizedMetricSpecification' - A customized metric. You can specify either a predefined metric or a
-- customized metric.
--
-- 'disableScaleIn', 'targetTrackingScalingPolicyConfiguration_disableScaleIn' - Indicates whether scale in by the target tracking scaling policy is
-- disabled. If the value is @true@, scale in is disabled and the target
-- tracking scaling policy won\'t remove capacity from the scalable target.
-- Otherwise, scale in is enabled and the target tracking scaling policy
-- can remove capacity from the scalable target. The default value is
-- @false@.
--
-- 'predefinedMetricSpecification', 'targetTrackingScalingPolicyConfiguration_predefinedMetricSpecification' - A predefined metric. You can specify either a predefined metric or a
-- customized metric.
--
-- 'scaleInCooldown', 'targetTrackingScalingPolicyConfiguration_scaleInCooldown' - The amount of time, in seconds, after a scale-in activity completes
-- before another scale-in activity can start. For more information and for
-- default values, see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html#target-tracking-cooldown Define cooldown periods>
-- in the /Application Auto Scaling User Guide/.
--
-- 'scaleOutCooldown', 'targetTrackingScalingPolicyConfiguration_scaleOutCooldown' - The amount of time, in seconds, to wait for a previous scale-out
-- activity to take effect. For more information and for default values,
-- see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html#target-tracking-cooldown Define cooldown periods>
-- in the /Application Auto Scaling User Guide/.
--
-- 'targetValue', 'targetTrackingScalingPolicyConfiguration_targetValue' - The target value for the metric. Although this property accepts numbers
-- of type Double, it won\'t accept values that are either too small or too
-- large. Values must be in the range of -2^360 to 2^360. The value must be
-- a valid number based on the choice of metric. For example, if the metric
-- is CPU utilization, then the target value is a percent value that
-- represents how much of the CPU can be used before scaling out.
--
-- If the scaling policy specifies the @ALBRequestCountPerTarget@
-- predefined metric, specify the target utilization as the optimal average
-- request count per target during any one-minute interval.
newTargetTrackingScalingPolicyConfiguration ::
  -- | 'targetValue'
  Prelude.Double ->
  TargetTrackingScalingPolicyConfiguration
newTargetTrackingScalingPolicyConfiguration
  pTargetValue_ =
    TargetTrackingScalingPolicyConfiguration'
      { customizedMetricSpecification =
          Prelude.Nothing,
        disableScaleIn = Prelude.Nothing,
        predefinedMetricSpecification =
          Prelude.Nothing,
        scaleInCooldown = Prelude.Nothing,
        scaleOutCooldown =
          Prelude.Nothing,
        targetValue = pTargetValue_
      }

-- | A customized metric. You can specify either a predefined metric or a
-- customized metric.
targetTrackingScalingPolicyConfiguration_customizedMetricSpecification :: Lens.Lens' TargetTrackingScalingPolicyConfiguration (Prelude.Maybe CustomizedMetricSpecification)
targetTrackingScalingPolicyConfiguration_customizedMetricSpecification = Lens.lens (\TargetTrackingScalingPolicyConfiguration' {customizedMetricSpecification} -> customizedMetricSpecification) (\s@TargetTrackingScalingPolicyConfiguration' {} a -> s {customizedMetricSpecification = a} :: TargetTrackingScalingPolicyConfiguration)

-- | Indicates whether scale in by the target tracking scaling policy is
-- disabled. If the value is @true@, scale in is disabled and the target
-- tracking scaling policy won\'t remove capacity from the scalable target.
-- Otherwise, scale in is enabled and the target tracking scaling policy
-- can remove capacity from the scalable target. The default value is
-- @false@.
targetTrackingScalingPolicyConfiguration_disableScaleIn :: Lens.Lens' TargetTrackingScalingPolicyConfiguration (Prelude.Maybe Prelude.Bool)
targetTrackingScalingPolicyConfiguration_disableScaleIn = Lens.lens (\TargetTrackingScalingPolicyConfiguration' {disableScaleIn} -> disableScaleIn) (\s@TargetTrackingScalingPolicyConfiguration' {} a -> s {disableScaleIn = a} :: TargetTrackingScalingPolicyConfiguration)

-- | A predefined metric. You can specify either a predefined metric or a
-- customized metric.
targetTrackingScalingPolicyConfiguration_predefinedMetricSpecification :: Lens.Lens' TargetTrackingScalingPolicyConfiguration (Prelude.Maybe PredefinedMetricSpecification)
targetTrackingScalingPolicyConfiguration_predefinedMetricSpecification = Lens.lens (\TargetTrackingScalingPolicyConfiguration' {predefinedMetricSpecification} -> predefinedMetricSpecification) (\s@TargetTrackingScalingPolicyConfiguration' {} a -> s {predefinedMetricSpecification = a} :: TargetTrackingScalingPolicyConfiguration)

-- | The amount of time, in seconds, after a scale-in activity completes
-- before another scale-in activity can start. For more information and for
-- default values, see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html#target-tracking-cooldown Define cooldown periods>
-- in the /Application Auto Scaling User Guide/.
targetTrackingScalingPolicyConfiguration_scaleInCooldown :: Lens.Lens' TargetTrackingScalingPolicyConfiguration (Prelude.Maybe Prelude.Int)
targetTrackingScalingPolicyConfiguration_scaleInCooldown = Lens.lens (\TargetTrackingScalingPolicyConfiguration' {scaleInCooldown} -> scaleInCooldown) (\s@TargetTrackingScalingPolicyConfiguration' {} a -> s {scaleInCooldown = a} :: TargetTrackingScalingPolicyConfiguration)

-- | The amount of time, in seconds, to wait for a previous scale-out
-- activity to take effect. For more information and for default values,
-- see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html#target-tracking-cooldown Define cooldown periods>
-- in the /Application Auto Scaling User Guide/.
targetTrackingScalingPolicyConfiguration_scaleOutCooldown :: Lens.Lens' TargetTrackingScalingPolicyConfiguration (Prelude.Maybe Prelude.Int)
targetTrackingScalingPolicyConfiguration_scaleOutCooldown = Lens.lens (\TargetTrackingScalingPolicyConfiguration' {scaleOutCooldown} -> scaleOutCooldown) (\s@TargetTrackingScalingPolicyConfiguration' {} a -> s {scaleOutCooldown = a} :: TargetTrackingScalingPolicyConfiguration)

-- | The target value for the metric. Although this property accepts numbers
-- of type Double, it won\'t accept values that are either too small or too
-- large. Values must be in the range of -2^360 to 2^360. The value must be
-- a valid number based on the choice of metric. For example, if the metric
-- is CPU utilization, then the target value is a percent value that
-- represents how much of the CPU can be used before scaling out.
--
-- If the scaling policy specifies the @ALBRequestCountPerTarget@
-- predefined metric, specify the target utilization as the optimal average
-- request count per target during any one-minute interval.
targetTrackingScalingPolicyConfiguration_targetValue :: Lens.Lens' TargetTrackingScalingPolicyConfiguration Prelude.Double
targetTrackingScalingPolicyConfiguration_targetValue = Lens.lens (\TargetTrackingScalingPolicyConfiguration' {targetValue} -> targetValue) (\s@TargetTrackingScalingPolicyConfiguration' {} a -> s {targetValue = a} :: TargetTrackingScalingPolicyConfiguration)

instance
  Data.FromJSON
    TargetTrackingScalingPolicyConfiguration
  where
  parseJSON =
    Data.withObject
      "TargetTrackingScalingPolicyConfiguration"
      ( \x ->
          TargetTrackingScalingPolicyConfiguration'
            Prelude.<$> (x Data..:? "CustomizedMetricSpecification")
            Prelude.<*> (x Data..:? "DisableScaleIn")
            Prelude.<*> (x Data..:? "PredefinedMetricSpecification")
            Prelude.<*> (x Data..:? "ScaleInCooldown")
            Prelude.<*> (x Data..:? "ScaleOutCooldown")
            Prelude.<*> (x Data..: "TargetValue")
      )

instance
  Prelude.Hashable
    TargetTrackingScalingPolicyConfiguration
  where
  hashWithSalt
    _salt
    TargetTrackingScalingPolicyConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` customizedMetricSpecification
        `Prelude.hashWithSalt` disableScaleIn
        `Prelude.hashWithSalt` predefinedMetricSpecification
        `Prelude.hashWithSalt` scaleInCooldown
        `Prelude.hashWithSalt` scaleOutCooldown
        `Prelude.hashWithSalt` targetValue

instance
  Prelude.NFData
    TargetTrackingScalingPolicyConfiguration
  where
  rnf TargetTrackingScalingPolicyConfiguration' {..} =
    Prelude.rnf customizedMetricSpecification
      `Prelude.seq` Prelude.rnf disableScaleIn
      `Prelude.seq` Prelude.rnf predefinedMetricSpecification
      `Prelude.seq` Prelude.rnf scaleInCooldown
      `Prelude.seq` Prelude.rnf scaleOutCooldown
      `Prelude.seq` Prelude.rnf targetValue

instance
  Data.ToJSON
    TargetTrackingScalingPolicyConfiguration
  where
  toJSON TargetTrackingScalingPolicyConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomizedMetricSpecification" Data..=)
              Prelude.<$> customizedMetricSpecification,
            ("DisableScaleIn" Data..=)
              Prelude.<$> disableScaleIn,
            ("PredefinedMetricSpecification" Data..=)
              Prelude.<$> predefinedMetricSpecification,
            ("ScaleInCooldown" Data..=)
              Prelude.<$> scaleInCooldown,
            ("ScaleOutCooldown" Data..=)
              Prelude.<$> scaleOutCooldown,
            Prelude.Just ("TargetValue" Data..= targetValue)
          ]
      )
