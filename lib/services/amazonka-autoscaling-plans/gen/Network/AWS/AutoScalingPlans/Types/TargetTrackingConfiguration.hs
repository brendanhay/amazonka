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
-- Module      : Amazonka.AutoScalingPlans.Types.TargetTrackingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScalingPlans.Types.TargetTrackingConfiguration where

import Amazonka.AutoScalingPlans.Types.CustomizedScalingMetricSpecification
import Amazonka.AutoScalingPlans.Types.PredefinedScalingMetricSpecification
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a target tracking configuration to use with AWS Auto Scaling.
-- Used with ScalingInstruction and ScalingPolicy.
--
-- /See:/ 'newTargetTrackingConfiguration' smart constructor.
data TargetTrackingConfiguration = TargetTrackingConfiguration'
  { -- | The estimated time, in seconds, until a newly launched instance can
    -- contribute to the CloudWatch metrics. This value is used only if the
    -- resource is an Auto Scaling group.
    estimatedInstanceWarmup :: Prelude.Maybe Prelude.Int,
    -- | A predefined metric. You can specify either a predefined metric or a
    -- customized metric.
    predefinedScalingMetricSpecification :: Prelude.Maybe PredefinedScalingMetricSpecification,
    -- | The amount of time, in seconds, after a scale-in activity completes
    -- before another scale-in activity can start. This property is not used if
    -- the scalable resource is an Auto Scaling group.
    --
    -- With the /scale-in cooldown period/, the intention is to scale in
    -- conservatively to protect your application’s availability, so scale-in
    -- activities are blocked until the cooldown period has expired. However,
    -- if another alarm triggers a scale-out activity during the scale-in
    -- cooldown period, Auto Scaling scales out the target immediately. In this
    -- case, the scale-in cooldown period stops and doesn\'t complete.
    scaleInCooldown :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether scale in by the target tracking scaling policy is
    -- disabled. If the value is @true@, scale in is disabled and the target
    -- tracking scaling policy doesn\'t remove capacity from the scalable
    -- resource. Otherwise, scale in is enabled and the target tracking scaling
    -- policy can remove capacity from the scalable resource.
    --
    -- The default value is @false@.
    disableScaleIn :: Prelude.Maybe Prelude.Bool,
    -- | A customized metric. You can specify either a predefined metric or a
    -- customized metric.
    customizedScalingMetricSpecification :: Prelude.Maybe CustomizedScalingMetricSpecification,
    -- | The amount of time, in seconds, to wait for a previous scale-out
    -- activity to take effect. This property is not used if the scalable
    -- resource is an Auto Scaling group.
    --
    -- With the /scale-out cooldown period/, the intention is to continuously
    -- (but not excessively) scale out. After Auto Scaling successfully scales
    -- out using a target tracking scaling policy, it starts to calculate the
    -- cooldown time. The scaling policy won\'t increase the desired capacity
    -- again unless either a larger scale out is triggered or the cooldown
    -- period ends.
    scaleOutCooldown :: Prelude.Maybe Prelude.Int,
    -- | The target value for the metric. Although this property accepts numbers
    -- of type Double, it won\'t accept values that are either too small or too
    -- large. Values must be in the range of -2^360 to 2^360.
    targetValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetTrackingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedInstanceWarmup', 'targetTrackingConfiguration_estimatedInstanceWarmup' - The estimated time, in seconds, until a newly launched instance can
-- contribute to the CloudWatch metrics. This value is used only if the
-- resource is an Auto Scaling group.
--
-- 'predefinedScalingMetricSpecification', 'targetTrackingConfiguration_predefinedScalingMetricSpecification' - A predefined metric. You can specify either a predefined metric or a
-- customized metric.
--
-- 'scaleInCooldown', 'targetTrackingConfiguration_scaleInCooldown' - The amount of time, in seconds, after a scale-in activity completes
-- before another scale-in activity can start. This property is not used if
-- the scalable resource is an Auto Scaling group.
--
-- With the /scale-in cooldown period/, the intention is to scale in
-- conservatively to protect your application’s availability, so scale-in
-- activities are blocked until the cooldown period has expired. However,
-- if another alarm triggers a scale-out activity during the scale-in
-- cooldown period, Auto Scaling scales out the target immediately. In this
-- case, the scale-in cooldown period stops and doesn\'t complete.
--
-- 'disableScaleIn', 'targetTrackingConfiguration_disableScaleIn' - Indicates whether scale in by the target tracking scaling policy is
-- disabled. If the value is @true@, scale in is disabled and the target
-- tracking scaling policy doesn\'t remove capacity from the scalable
-- resource. Otherwise, scale in is enabled and the target tracking scaling
-- policy can remove capacity from the scalable resource.
--
-- The default value is @false@.
--
-- 'customizedScalingMetricSpecification', 'targetTrackingConfiguration_customizedScalingMetricSpecification' - A customized metric. You can specify either a predefined metric or a
-- customized metric.
--
-- 'scaleOutCooldown', 'targetTrackingConfiguration_scaleOutCooldown' - The amount of time, in seconds, to wait for a previous scale-out
-- activity to take effect. This property is not used if the scalable
-- resource is an Auto Scaling group.
--
-- With the /scale-out cooldown period/, the intention is to continuously
-- (but not excessively) scale out. After Auto Scaling successfully scales
-- out using a target tracking scaling policy, it starts to calculate the
-- cooldown time. The scaling policy won\'t increase the desired capacity
-- again unless either a larger scale out is triggered or the cooldown
-- period ends.
--
-- 'targetValue', 'targetTrackingConfiguration_targetValue' - The target value for the metric. Although this property accepts numbers
-- of type Double, it won\'t accept values that are either too small or too
-- large. Values must be in the range of -2^360 to 2^360.
newTargetTrackingConfiguration ::
  -- | 'targetValue'
  Prelude.Double ->
  TargetTrackingConfiguration
newTargetTrackingConfiguration pTargetValue_ =
  TargetTrackingConfiguration'
    { estimatedInstanceWarmup =
        Prelude.Nothing,
      predefinedScalingMetricSpecification =
        Prelude.Nothing,
      scaleInCooldown = Prelude.Nothing,
      disableScaleIn = Prelude.Nothing,
      customizedScalingMetricSpecification =
        Prelude.Nothing,
      scaleOutCooldown = Prelude.Nothing,
      targetValue = pTargetValue_
    }

-- | The estimated time, in seconds, until a newly launched instance can
-- contribute to the CloudWatch metrics. This value is used only if the
-- resource is an Auto Scaling group.
targetTrackingConfiguration_estimatedInstanceWarmup :: Lens.Lens' TargetTrackingConfiguration (Prelude.Maybe Prelude.Int)
targetTrackingConfiguration_estimatedInstanceWarmup = Lens.lens (\TargetTrackingConfiguration' {estimatedInstanceWarmup} -> estimatedInstanceWarmup) (\s@TargetTrackingConfiguration' {} a -> s {estimatedInstanceWarmup = a} :: TargetTrackingConfiguration)

-- | A predefined metric. You can specify either a predefined metric or a
-- customized metric.
targetTrackingConfiguration_predefinedScalingMetricSpecification :: Lens.Lens' TargetTrackingConfiguration (Prelude.Maybe PredefinedScalingMetricSpecification)
targetTrackingConfiguration_predefinedScalingMetricSpecification = Lens.lens (\TargetTrackingConfiguration' {predefinedScalingMetricSpecification} -> predefinedScalingMetricSpecification) (\s@TargetTrackingConfiguration' {} a -> s {predefinedScalingMetricSpecification = a} :: TargetTrackingConfiguration)

-- | The amount of time, in seconds, after a scale-in activity completes
-- before another scale-in activity can start. This property is not used if
-- the scalable resource is an Auto Scaling group.
--
-- With the /scale-in cooldown period/, the intention is to scale in
-- conservatively to protect your application’s availability, so scale-in
-- activities are blocked until the cooldown period has expired. However,
-- if another alarm triggers a scale-out activity during the scale-in
-- cooldown period, Auto Scaling scales out the target immediately. In this
-- case, the scale-in cooldown period stops and doesn\'t complete.
targetTrackingConfiguration_scaleInCooldown :: Lens.Lens' TargetTrackingConfiguration (Prelude.Maybe Prelude.Int)
targetTrackingConfiguration_scaleInCooldown = Lens.lens (\TargetTrackingConfiguration' {scaleInCooldown} -> scaleInCooldown) (\s@TargetTrackingConfiguration' {} a -> s {scaleInCooldown = a} :: TargetTrackingConfiguration)

-- | Indicates whether scale in by the target tracking scaling policy is
-- disabled. If the value is @true@, scale in is disabled and the target
-- tracking scaling policy doesn\'t remove capacity from the scalable
-- resource. Otherwise, scale in is enabled and the target tracking scaling
-- policy can remove capacity from the scalable resource.
--
-- The default value is @false@.
targetTrackingConfiguration_disableScaleIn :: Lens.Lens' TargetTrackingConfiguration (Prelude.Maybe Prelude.Bool)
targetTrackingConfiguration_disableScaleIn = Lens.lens (\TargetTrackingConfiguration' {disableScaleIn} -> disableScaleIn) (\s@TargetTrackingConfiguration' {} a -> s {disableScaleIn = a} :: TargetTrackingConfiguration)

-- | A customized metric. You can specify either a predefined metric or a
-- customized metric.
targetTrackingConfiguration_customizedScalingMetricSpecification :: Lens.Lens' TargetTrackingConfiguration (Prelude.Maybe CustomizedScalingMetricSpecification)
targetTrackingConfiguration_customizedScalingMetricSpecification = Lens.lens (\TargetTrackingConfiguration' {customizedScalingMetricSpecification} -> customizedScalingMetricSpecification) (\s@TargetTrackingConfiguration' {} a -> s {customizedScalingMetricSpecification = a} :: TargetTrackingConfiguration)

-- | The amount of time, in seconds, to wait for a previous scale-out
-- activity to take effect. This property is not used if the scalable
-- resource is an Auto Scaling group.
--
-- With the /scale-out cooldown period/, the intention is to continuously
-- (but not excessively) scale out. After Auto Scaling successfully scales
-- out using a target tracking scaling policy, it starts to calculate the
-- cooldown time. The scaling policy won\'t increase the desired capacity
-- again unless either a larger scale out is triggered or the cooldown
-- period ends.
targetTrackingConfiguration_scaleOutCooldown :: Lens.Lens' TargetTrackingConfiguration (Prelude.Maybe Prelude.Int)
targetTrackingConfiguration_scaleOutCooldown = Lens.lens (\TargetTrackingConfiguration' {scaleOutCooldown} -> scaleOutCooldown) (\s@TargetTrackingConfiguration' {} a -> s {scaleOutCooldown = a} :: TargetTrackingConfiguration)

-- | The target value for the metric. Although this property accepts numbers
-- of type Double, it won\'t accept values that are either too small or too
-- large. Values must be in the range of -2^360 to 2^360.
targetTrackingConfiguration_targetValue :: Lens.Lens' TargetTrackingConfiguration Prelude.Double
targetTrackingConfiguration_targetValue = Lens.lens (\TargetTrackingConfiguration' {targetValue} -> targetValue) (\s@TargetTrackingConfiguration' {} a -> s {targetValue = a} :: TargetTrackingConfiguration)

instance Core.FromJSON TargetTrackingConfiguration where
  parseJSON =
    Core.withObject
      "TargetTrackingConfiguration"
      ( \x ->
          TargetTrackingConfiguration'
            Prelude.<$> (x Core..:? "EstimatedInstanceWarmup")
            Prelude.<*> (x Core..:? "PredefinedScalingMetricSpecification")
            Prelude.<*> (x Core..:? "ScaleInCooldown")
            Prelude.<*> (x Core..:? "DisableScaleIn")
            Prelude.<*> (x Core..:? "CustomizedScalingMetricSpecification")
            Prelude.<*> (x Core..:? "ScaleOutCooldown")
            Prelude.<*> (x Core..: "TargetValue")
      )

instance Prelude.Hashable TargetTrackingConfiguration

instance Prelude.NFData TargetTrackingConfiguration

instance Core.ToJSON TargetTrackingConfiguration where
  toJSON TargetTrackingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EstimatedInstanceWarmup" Core..=)
              Prelude.<$> estimatedInstanceWarmup,
            ("PredefinedScalingMetricSpecification" Core..=)
              Prelude.<$> predefinedScalingMetricSpecification,
            ("ScaleInCooldown" Core..=)
              Prelude.<$> scaleInCooldown,
            ("DisableScaleIn" Core..=)
              Prelude.<$> disableScaleIn,
            ("CustomizedScalingMetricSpecification" Core..=)
              Prelude.<$> customizedScalingMetricSpecification,
            ("ScaleOutCooldown" Core..=)
              Prelude.<$> scaleOutCooldown,
            Prelude.Just ("TargetValue" Core..= targetValue)
          ]
      )
