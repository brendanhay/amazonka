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
-- Module      : Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration where

import Network.AWS.AutoScalingPlans.Types.CustomizedScalingMetricSpecification
import Network.AWS.AutoScalingPlans.Types.PredefinedScalingMetricSpecification
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a target tracking configuration to use with AWS Auto Scaling.
-- Used with ScalingInstruction and ScalingPolicy.
--
-- /See:/ 'newTargetTrackingConfiguration' smart constructor.
data TargetTrackingConfiguration = TargetTrackingConfiguration'
  { -- | Indicates whether scale in by the target tracking scaling policy is
    -- disabled. If the value is @true@, scale in is disabled and the target
    -- tracking scaling policy doesn\'t remove capacity from the scalable
    -- resource. Otherwise, scale in is enabled and the target tracking scaling
    -- policy can remove capacity from the scalable resource.
    --
    -- The default value is @false@.
    disableScaleIn :: Core.Maybe Core.Bool,
    -- | A customized metric. You can specify either a predefined metric or a
    -- customized metric.
    customizedScalingMetricSpecification :: Core.Maybe CustomizedScalingMetricSpecification,
    -- | A predefined metric. You can specify either a predefined metric or a
    -- customized metric.
    predefinedScalingMetricSpecification :: Core.Maybe PredefinedScalingMetricSpecification,
    -- | The estimated time, in seconds, until a newly launched instance can
    -- contribute to the CloudWatch metrics. This value is used only if the
    -- resource is an Auto Scaling group.
    estimatedInstanceWarmup :: Core.Maybe Core.Int,
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
    scaleOutCooldown :: Core.Maybe Core.Int,
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
    scaleInCooldown :: Core.Maybe Core.Int,
    -- | The target value for the metric. Although this property accepts numbers
    -- of type Double, it won\'t accept values that are either too small or too
    -- large. Values must be in the range of -2^360 to 2^360.
    targetValue :: Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TargetTrackingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'predefinedScalingMetricSpecification', 'targetTrackingConfiguration_predefinedScalingMetricSpecification' - A predefined metric. You can specify either a predefined metric or a
-- customized metric.
--
-- 'estimatedInstanceWarmup', 'targetTrackingConfiguration_estimatedInstanceWarmup' - The estimated time, in seconds, until a newly launched instance can
-- contribute to the CloudWatch metrics. This value is used only if the
-- resource is an Auto Scaling group.
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
-- 'targetValue', 'targetTrackingConfiguration_targetValue' - The target value for the metric. Although this property accepts numbers
-- of type Double, it won\'t accept values that are either too small or too
-- large. Values must be in the range of -2^360 to 2^360.
newTargetTrackingConfiguration ::
  -- | 'targetValue'
  Core.Double ->
  TargetTrackingConfiguration
newTargetTrackingConfiguration pTargetValue_ =
  TargetTrackingConfiguration'
    { disableScaleIn =
        Core.Nothing,
      customizedScalingMetricSpecification =
        Core.Nothing,
      predefinedScalingMetricSpecification =
        Core.Nothing,
      estimatedInstanceWarmup = Core.Nothing,
      scaleOutCooldown = Core.Nothing,
      scaleInCooldown = Core.Nothing,
      targetValue = pTargetValue_
    }

-- | Indicates whether scale in by the target tracking scaling policy is
-- disabled. If the value is @true@, scale in is disabled and the target
-- tracking scaling policy doesn\'t remove capacity from the scalable
-- resource. Otherwise, scale in is enabled and the target tracking scaling
-- policy can remove capacity from the scalable resource.
--
-- The default value is @false@.
targetTrackingConfiguration_disableScaleIn :: Lens.Lens' TargetTrackingConfiguration (Core.Maybe Core.Bool)
targetTrackingConfiguration_disableScaleIn = Lens.lens (\TargetTrackingConfiguration' {disableScaleIn} -> disableScaleIn) (\s@TargetTrackingConfiguration' {} a -> s {disableScaleIn = a} :: TargetTrackingConfiguration)

-- | A customized metric. You can specify either a predefined metric or a
-- customized metric.
targetTrackingConfiguration_customizedScalingMetricSpecification :: Lens.Lens' TargetTrackingConfiguration (Core.Maybe CustomizedScalingMetricSpecification)
targetTrackingConfiguration_customizedScalingMetricSpecification = Lens.lens (\TargetTrackingConfiguration' {customizedScalingMetricSpecification} -> customizedScalingMetricSpecification) (\s@TargetTrackingConfiguration' {} a -> s {customizedScalingMetricSpecification = a} :: TargetTrackingConfiguration)

-- | A predefined metric. You can specify either a predefined metric or a
-- customized metric.
targetTrackingConfiguration_predefinedScalingMetricSpecification :: Lens.Lens' TargetTrackingConfiguration (Core.Maybe PredefinedScalingMetricSpecification)
targetTrackingConfiguration_predefinedScalingMetricSpecification = Lens.lens (\TargetTrackingConfiguration' {predefinedScalingMetricSpecification} -> predefinedScalingMetricSpecification) (\s@TargetTrackingConfiguration' {} a -> s {predefinedScalingMetricSpecification = a} :: TargetTrackingConfiguration)

-- | The estimated time, in seconds, until a newly launched instance can
-- contribute to the CloudWatch metrics. This value is used only if the
-- resource is an Auto Scaling group.
targetTrackingConfiguration_estimatedInstanceWarmup :: Lens.Lens' TargetTrackingConfiguration (Core.Maybe Core.Int)
targetTrackingConfiguration_estimatedInstanceWarmup = Lens.lens (\TargetTrackingConfiguration' {estimatedInstanceWarmup} -> estimatedInstanceWarmup) (\s@TargetTrackingConfiguration' {} a -> s {estimatedInstanceWarmup = a} :: TargetTrackingConfiguration)

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
targetTrackingConfiguration_scaleOutCooldown :: Lens.Lens' TargetTrackingConfiguration (Core.Maybe Core.Int)
targetTrackingConfiguration_scaleOutCooldown = Lens.lens (\TargetTrackingConfiguration' {scaleOutCooldown} -> scaleOutCooldown) (\s@TargetTrackingConfiguration' {} a -> s {scaleOutCooldown = a} :: TargetTrackingConfiguration)

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
targetTrackingConfiguration_scaleInCooldown :: Lens.Lens' TargetTrackingConfiguration (Core.Maybe Core.Int)
targetTrackingConfiguration_scaleInCooldown = Lens.lens (\TargetTrackingConfiguration' {scaleInCooldown} -> scaleInCooldown) (\s@TargetTrackingConfiguration' {} a -> s {scaleInCooldown = a} :: TargetTrackingConfiguration)

-- | The target value for the metric. Although this property accepts numbers
-- of type Double, it won\'t accept values that are either too small or too
-- large. Values must be in the range of -2^360 to 2^360.
targetTrackingConfiguration_targetValue :: Lens.Lens' TargetTrackingConfiguration Core.Double
targetTrackingConfiguration_targetValue = Lens.lens (\TargetTrackingConfiguration' {targetValue} -> targetValue) (\s@TargetTrackingConfiguration' {} a -> s {targetValue = a} :: TargetTrackingConfiguration)

instance Core.FromJSON TargetTrackingConfiguration where
  parseJSON =
    Core.withObject
      "TargetTrackingConfiguration"
      ( \x ->
          TargetTrackingConfiguration'
            Core.<$> (x Core..:? "DisableScaleIn")
            Core.<*> (x Core..:? "CustomizedScalingMetricSpecification")
            Core.<*> (x Core..:? "PredefinedScalingMetricSpecification")
            Core.<*> (x Core..:? "EstimatedInstanceWarmup")
            Core.<*> (x Core..:? "ScaleOutCooldown")
            Core.<*> (x Core..:? "ScaleInCooldown")
            Core.<*> (x Core..: "TargetValue")
      )

instance Core.Hashable TargetTrackingConfiguration

instance Core.NFData TargetTrackingConfiguration

instance Core.ToJSON TargetTrackingConfiguration where
  toJSON TargetTrackingConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DisableScaleIn" Core..=) Core.<$> disableScaleIn,
            ("CustomizedScalingMetricSpecification" Core..=)
              Core.<$> customizedScalingMetricSpecification,
            ("PredefinedScalingMetricSpecification" Core..=)
              Core.<$> predefinedScalingMetricSpecification,
            ("EstimatedInstanceWarmup" Core..=)
              Core.<$> estimatedInstanceWarmup,
            ("ScaleOutCooldown" Core..=)
              Core.<$> scaleOutCooldown,
            ("ScaleInCooldown" Core..=) Core.<$> scaleInCooldown,
            Core.Just ("TargetValue" Core..= targetValue)
          ]
      )
