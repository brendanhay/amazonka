{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the properties of a target tracking scaling policy.
--
-- /See:/ 'newAutoScalingTargetTrackingScalingPolicyConfigurationDescription' smart constructor.
data AutoScalingTargetTrackingScalingPolicyConfigurationDescription = AutoScalingTargetTrackingScalingPolicyConfigurationDescription'
  { -- | Indicates whether scale in by the target tracking policy is disabled. If
    -- the value is true, scale in is disabled and the target tracking policy
    -- won\'t remove capacity from the scalable resource. Otherwise, scale in
    -- is enabled and the target tracking policy can remove capacity from the
    -- scalable resource. The default value is false.
    disableScaleIn :: Prelude.Maybe Prelude.Bool,
    -- | The amount of time, in seconds, after a scale out activity completes
    -- before another scale out activity can start. While the cooldown period
    -- is in effect, the capacity that has been added by the previous scale out
    -- event that initiated the cooldown is calculated as part of the desired
    -- capacity for the next scale out. You should continuously (but not
    -- excessively) scale out.
    scaleOutCooldown :: Prelude.Maybe Prelude.Int,
    -- | The amount of time, in seconds, after a scale in activity completes
    -- before another scale in activity can start. The cooldown period is used
    -- to block subsequent scale in requests until it has expired. You should
    -- scale in conservatively to protect your application\'s availability.
    -- However, if another alarm triggers a scale out policy during the
    -- cooldown period after a scale-in, application auto scaling scales out
    -- your scalable target immediately.
    scaleInCooldown :: Prelude.Maybe Prelude.Int,
    -- | The target value for the metric. The range is 8.515920e-109 to
    -- 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
    targetValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingTargetTrackingScalingPolicyConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disableScaleIn', 'autoScalingTargetTrackingScalingPolicyConfigurationDescription_disableScaleIn' - Indicates whether scale in by the target tracking policy is disabled. If
-- the value is true, scale in is disabled and the target tracking policy
-- won\'t remove capacity from the scalable resource. Otherwise, scale in
-- is enabled and the target tracking policy can remove capacity from the
-- scalable resource. The default value is false.
--
-- 'scaleOutCooldown', 'autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleOutCooldown' - The amount of time, in seconds, after a scale out activity completes
-- before another scale out activity can start. While the cooldown period
-- is in effect, the capacity that has been added by the previous scale out
-- event that initiated the cooldown is calculated as part of the desired
-- capacity for the next scale out. You should continuously (but not
-- excessively) scale out.
--
-- 'scaleInCooldown', 'autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleInCooldown' - The amount of time, in seconds, after a scale in activity completes
-- before another scale in activity can start. The cooldown period is used
-- to block subsequent scale in requests until it has expired. You should
-- scale in conservatively to protect your application\'s availability.
-- However, if another alarm triggers a scale out policy during the
-- cooldown period after a scale-in, application auto scaling scales out
-- your scalable target immediately.
--
-- 'targetValue', 'autoScalingTargetTrackingScalingPolicyConfigurationDescription_targetValue' - The target value for the metric. The range is 8.515920e-109 to
-- 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
newAutoScalingTargetTrackingScalingPolicyConfigurationDescription ::
  -- | 'targetValue'
  Prelude.Double ->
  AutoScalingTargetTrackingScalingPolicyConfigurationDescription
newAutoScalingTargetTrackingScalingPolicyConfigurationDescription
  pTargetValue_ =
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription'
      { disableScaleIn =
          Prelude.Nothing,
        scaleOutCooldown =
          Prelude.Nothing,
        scaleInCooldown =
          Prelude.Nothing,
        targetValue =
          pTargetValue_
      }

-- | Indicates whether scale in by the target tracking policy is disabled. If
-- the value is true, scale in is disabled and the target tracking policy
-- won\'t remove capacity from the scalable resource. Otherwise, scale in
-- is enabled and the target tracking policy can remove capacity from the
-- scalable resource. The default value is false.
autoScalingTargetTrackingScalingPolicyConfigurationDescription_disableScaleIn :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Prelude.Maybe Prelude.Bool)
autoScalingTargetTrackingScalingPolicyConfigurationDescription_disableScaleIn = Lens.lens (\AutoScalingTargetTrackingScalingPolicyConfigurationDescription' {disableScaleIn} -> disableScaleIn) (\s@AutoScalingTargetTrackingScalingPolicyConfigurationDescription' {} a -> s {disableScaleIn = a} :: AutoScalingTargetTrackingScalingPolicyConfigurationDescription)

-- | The amount of time, in seconds, after a scale out activity completes
-- before another scale out activity can start. While the cooldown period
-- is in effect, the capacity that has been added by the previous scale out
-- event that initiated the cooldown is calculated as part of the desired
-- capacity for the next scale out. You should continuously (but not
-- excessively) scale out.
autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleOutCooldown :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Prelude.Maybe Prelude.Int)
autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleOutCooldown = Lens.lens (\AutoScalingTargetTrackingScalingPolicyConfigurationDescription' {scaleOutCooldown} -> scaleOutCooldown) (\s@AutoScalingTargetTrackingScalingPolicyConfigurationDescription' {} a -> s {scaleOutCooldown = a} :: AutoScalingTargetTrackingScalingPolicyConfigurationDescription)

-- | The amount of time, in seconds, after a scale in activity completes
-- before another scale in activity can start. The cooldown period is used
-- to block subsequent scale in requests until it has expired. You should
-- scale in conservatively to protect your application\'s availability.
-- However, if another alarm triggers a scale out policy during the
-- cooldown period after a scale-in, application auto scaling scales out
-- your scalable target immediately.
autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleInCooldown :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Prelude.Maybe Prelude.Int)
autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleInCooldown = Lens.lens (\AutoScalingTargetTrackingScalingPolicyConfigurationDescription' {scaleInCooldown} -> scaleInCooldown) (\s@AutoScalingTargetTrackingScalingPolicyConfigurationDescription' {} a -> s {scaleInCooldown = a} :: AutoScalingTargetTrackingScalingPolicyConfigurationDescription)

-- | The target value for the metric. The range is 8.515920e-109 to
-- 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
autoScalingTargetTrackingScalingPolicyConfigurationDescription_targetValue :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription Prelude.Double
autoScalingTargetTrackingScalingPolicyConfigurationDescription_targetValue = Lens.lens (\AutoScalingTargetTrackingScalingPolicyConfigurationDescription' {targetValue} -> targetValue) (\s@AutoScalingTargetTrackingScalingPolicyConfigurationDescription' {} a -> s {targetValue = a} :: AutoScalingTargetTrackingScalingPolicyConfigurationDescription)

instance
  Prelude.FromJSON
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription
  where
  parseJSON =
    Prelude.withObject
      "AutoScalingTargetTrackingScalingPolicyConfigurationDescription"
      ( \x ->
          AutoScalingTargetTrackingScalingPolicyConfigurationDescription'
            Prelude.<$> (x Prelude..:? "DisableScaleIn")
              Prelude.<*> (x Prelude..:? "ScaleOutCooldown")
              Prelude.<*> (x Prelude..:? "ScaleInCooldown")
              Prelude.<*> (x Prelude..: "TargetValue")
      )

instance
  Prelude.Hashable
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription

instance
  Prelude.NFData
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription
