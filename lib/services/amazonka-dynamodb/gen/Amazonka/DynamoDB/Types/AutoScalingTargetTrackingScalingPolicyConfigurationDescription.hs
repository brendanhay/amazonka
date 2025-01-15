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
-- Module      : Amazonka.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

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
    -- | The amount of time, in seconds, after a scale in activity completes
    -- before another scale in activity can start. The cooldown period is used
    -- to block subsequent scale in requests until it has expired. You should
    -- scale in conservatively to protect your application\'s availability.
    -- However, if another alarm triggers a scale out policy during the
    -- cooldown period after a scale-in, application auto scaling scales out
    -- your scalable target immediately.
    scaleInCooldown :: Prelude.Maybe Prelude.Int,
    -- | The amount of time, in seconds, after a scale out activity completes
    -- before another scale out activity can start. While the cooldown period
    -- is in effect, the capacity that has been added by the previous scale out
    -- event that initiated the cooldown is calculated as part of the desired
    -- capacity for the next scale out. You should continuously (but not
    -- excessively) scale out.
    scaleOutCooldown :: Prelude.Maybe Prelude.Int,
    -- | The target value for the metric. The range is 8.515920e-109 to
    -- 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
    targetValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'scaleInCooldown', 'autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleInCooldown' - The amount of time, in seconds, after a scale in activity completes
-- before another scale in activity can start. The cooldown period is used
-- to block subsequent scale in requests until it has expired. You should
-- scale in conservatively to protect your application\'s availability.
-- However, if another alarm triggers a scale out policy during the
-- cooldown period after a scale-in, application auto scaling scales out
-- your scalable target immediately.
--
-- 'scaleOutCooldown', 'autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleOutCooldown' - The amount of time, in seconds, after a scale out activity completes
-- before another scale out activity can start. While the cooldown period
-- is in effect, the capacity that has been added by the previous scale out
-- event that initiated the cooldown is calculated as part of the desired
-- capacity for the next scale out. You should continuously (but not
-- excessively) scale out.
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
        scaleInCooldown =
          Prelude.Nothing,
        scaleOutCooldown =
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

-- | The amount of time, in seconds, after a scale in activity completes
-- before another scale in activity can start. The cooldown period is used
-- to block subsequent scale in requests until it has expired. You should
-- scale in conservatively to protect your application\'s availability.
-- However, if another alarm triggers a scale out policy during the
-- cooldown period after a scale-in, application auto scaling scales out
-- your scalable target immediately.
autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleInCooldown :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Prelude.Maybe Prelude.Int)
autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleInCooldown = Lens.lens (\AutoScalingTargetTrackingScalingPolicyConfigurationDescription' {scaleInCooldown} -> scaleInCooldown) (\s@AutoScalingTargetTrackingScalingPolicyConfigurationDescription' {} a -> s {scaleInCooldown = a} :: AutoScalingTargetTrackingScalingPolicyConfigurationDescription)

-- | The amount of time, in seconds, after a scale out activity completes
-- before another scale out activity can start. While the cooldown period
-- is in effect, the capacity that has been added by the previous scale out
-- event that initiated the cooldown is calculated as part of the desired
-- capacity for the next scale out. You should continuously (but not
-- excessively) scale out.
autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleOutCooldown :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Prelude.Maybe Prelude.Int)
autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleOutCooldown = Lens.lens (\AutoScalingTargetTrackingScalingPolicyConfigurationDescription' {scaleOutCooldown} -> scaleOutCooldown) (\s@AutoScalingTargetTrackingScalingPolicyConfigurationDescription' {} a -> s {scaleOutCooldown = a} :: AutoScalingTargetTrackingScalingPolicyConfigurationDescription)

-- | The target value for the metric. The range is 8.515920e-109 to
-- 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
autoScalingTargetTrackingScalingPolicyConfigurationDescription_targetValue :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription Prelude.Double
autoScalingTargetTrackingScalingPolicyConfigurationDescription_targetValue = Lens.lens (\AutoScalingTargetTrackingScalingPolicyConfigurationDescription' {targetValue} -> targetValue) (\s@AutoScalingTargetTrackingScalingPolicyConfigurationDescription' {} a -> s {targetValue = a} :: AutoScalingTargetTrackingScalingPolicyConfigurationDescription)

instance
  Data.FromJSON
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription
  where
  parseJSON =
    Data.withObject
      "AutoScalingTargetTrackingScalingPolicyConfigurationDescription"
      ( \x ->
          AutoScalingTargetTrackingScalingPolicyConfigurationDescription'
            Prelude.<$> (x Data..:? "DisableScaleIn")
            Prelude.<*> (x Data..:? "ScaleInCooldown")
            Prelude.<*> (x Data..:? "ScaleOutCooldown")
            Prelude.<*> (x Data..: "TargetValue")
      )

instance
  Prelude.Hashable
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription
  where
  hashWithSalt
    _salt
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription' {..} =
      _salt
        `Prelude.hashWithSalt` disableScaleIn
        `Prelude.hashWithSalt` scaleInCooldown
        `Prelude.hashWithSalt` scaleOutCooldown
        `Prelude.hashWithSalt` targetValue

instance
  Prelude.NFData
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription
  where
  rnf
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription' {..} =
      Prelude.rnf disableScaleIn `Prelude.seq`
        Prelude.rnf scaleInCooldown `Prelude.seq`
          Prelude.rnf scaleOutCooldown `Prelude.seq`
            Prelude.rnf targetValue
