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
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the settings of a target tracking scaling policy that will be
-- modified.
--
-- /See:/ 'newAutoScalingTargetTrackingScalingPolicyConfigurationUpdate' smart constructor.
data AutoScalingTargetTrackingScalingPolicyConfigurationUpdate = AutoScalingTargetTrackingScalingPolicyConfigurationUpdate'
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
-- Create a value of 'AutoScalingTargetTrackingScalingPolicyConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disableScaleIn', 'autoScalingTargetTrackingScalingPolicyConfigurationUpdate_disableScaleIn' - Indicates whether scale in by the target tracking policy is disabled. If
-- the value is true, scale in is disabled and the target tracking policy
-- won\'t remove capacity from the scalable resource. Otherwise, scale in
-- is enabled and the target tracking policy can remove capacity from the
-- scalable resource. The default value is false.
--
-- 'scaleOutCooldown', 'autoScalingTargetTrackingScalingPolicyConfigurationUpdate_scaleOutCooldown' - The amount of time, in seconds, after a scale out activity completes
-- before another scale out activity can start. While the cooldown period
-- is in effect, the capacity that has been added by the previous scale out
-- event that initiated the cooldown is calculated as part of the desired
-- capacity for the next scale out. You should continuously (but not
-- excessively) scale out.
--
-- 'scaleInCooldown', 'autoScalingTargetTrackingScalingPolicyConfigurationUpdate_scaleInCooldown' - The amount of time, in seconds, after a scale in activity completes
-- before another scale in activity can start. The cooldown period is used
-- to block subsequent scale in requests until it has expired. You should
-- scale in conservatively to protect your application\'s availability.
-- However, if another alarm triggers a scale out policy during the
-- cooldown period after a scale-in, application auto scaling scales out
-- your scalable target immediately.
--
-- 'targetValue', 'autoScalingTargetTrackingScalingPolicyConfigurationUpdate_targetValue' - The target value for the metric. The range is 8.515920e-109 to
-- 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
newAutoScalingTargetTrackingScalingPolicyConfigurationUpdate ::
  -- | 'targetValue'
  Prelude.Double ->
  AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
newAutoScalingTargetTrackingScalingPolicyConfigurationUpdate
  pTargetValue_ =
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate'
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
autoScalingTargetTrackingScalingPolicyConfigurationUpdate_disableScaleIn :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationUpdate (Prelude.Maybe Prelude.Bool)
autoScalingTargetTrackingScalingPolicyConfigurationUpdate_disableScaleIn = Lens.lens (\AutoScalingTargetTrackingScalingPolicyConfigurationUpdate' {disableScaleIn} -> disableScaleIn) (\s@AutoScalingTargetTrackingScalingPolicyConfigurationUpdate' {} a -> s {disableScaleIn = a} :: AutoScalingTargetTrackingScalingPolicyConfigurationUpdate)

-- | The amount of time, in seconds, after a scale out activity completes
-- before another scale out activity can start. While the cooldown period
-- is in effect, the capacity that has been added by the previous scale out
-- event that initiated the cooldown is calculated as part of the desired
-- capacity for the next scale out. You should continuously (but not
-- excessively) scale out.
autoScalingTargetTrackingScalingPolicyConfigurationUpdate_scaleOutCooldown :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationUpdate (Prelude.Maybe Prelude.Int)
autoScalingTargetTrackingScalingPolicyConfigurationUpdate_scaleOutCooldown = Lens.lens (\AutoScalingTargetTrackingScalingPolicyConfigurationUpdate' {scaleOutCooldown} -> scaleOutCooldown) (\s@AutoScalingTargetTrackingScalingPolicyConfigurationUpdate' {} a -> s {scaleOutCooldown = a} :: AutoScalingTargetTrackingScalingPolicyConfigurationUpdate)

-- | The amount of time, in seconds, after a scale in activity completes
-- before another scale in activity can start. The cooldown period is used
-- to block subsequent scale in requests until it has expired. You should
-- scale in conservatively to protect your application\'s availability.
-- However, if another alarm triggers a scale out policy during the
-- cooldown period after a scale-in, application auto scaling scales out
-- your scalable target immediately.
autoScalingTargetTrackingScalingPolicyConfigurationUpdate_scaleInCooldown :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationUpdate (Prelude.Maybe Prelude.Int)
autoScalingTargetTrackingScalingPolicyConfigurationUpdate_scaleInCooldown = Lens.lens (\AutoScalingTargetTrackingScalingPolicyConfigurationUpdate' {scaleInCooldown} -> scaleInCooldown) (\s@AutoScalingTargetTrackingScalingPolicyConfigurationUpdate' {} a -> s {scaleInCooldown = a} :: AutoScalingTargetTrackingScalingPolicyConfigurationUpdate)

-- | The target value for the metric. The range is 8.515920e-109 to
-- 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
autoScalingTargetTrackingScalingPolicyConfigurationUpdate_targetValue :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationUpdate Prelude.Double
autoScalingTargetTrackingScalingPolicyConfigurationUpdate_targetValue = Lens.lens (\AutoScalingTargetTrackingScalingPolicyConfigurationUpdate' {targetValue} -> targetValue) (\s@AutoScalingTargetTrackingScalingPolicyConfigurationUpdate' {} a -> s {targetValue = a} :: AutoScalingTargetTrackingScalingPolicyConfigurationUpdate)

instance
  Prelude.Hashable
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate

instance
  Prelude.NFData
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate

instance
  Prelude.ToJSON
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
  where
  toJSON
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ ("DisableScaleIn" Prelude..=)
                Prelude.<$> disableScaleIn,
              ("ScaleOutCooldown" Prelude..=)
                Prelude.<$> scaleOutCooldown,
              ("ScaleInCooldown" Prelude..=)
                Prelude.<$> scaleInCooldown,
              Prelude.Just ("TargetValue" Prelude..= targetValue)
            ]
        )
