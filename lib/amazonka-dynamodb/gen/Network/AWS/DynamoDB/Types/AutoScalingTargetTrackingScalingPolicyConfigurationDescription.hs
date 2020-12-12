{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
  ( AutoScalingTargetTrackingScalingPolicyConfigurationDescription (..),

    -- * Smart constructor
    mkAutoScalingTargetTrackingScalingPolicyConfigurationDescription,

    -- * Lenses
    asttspcdScaleInCooldown,
    asttspcdDisableScaleIn,
    asttspcdScaleOutCooldown,
    asttspcdTargetValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the properties of a target tracking scaling policy.
--
-- /See:/ 'mkAutoScalingTargetTrackingScalingPolicyConfigurationDescription' smart constructor.
data AutoScalingTargetTrackingScalingPolicyConfigurationDescription = AutoScalingTargetTrackingScalingPolicyConfigurationDescription'
  { scaleInCooldown ::
      Lude.Maybe
        Lude.Int,
    disableScaleIn ::
      Lude.Maybe
        Lude.Bool,
    scaleOutCooldown ::
      Lude.Maybe
        Lude.Int,
    targetValue ::
      Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'AutoScalingTargetTrackingScalingPolicyConfigurationDescription' with the minimum fields required to make a request.
--
-- * 'disableScaleIn' - Indicates whether scale in by the target tracking policy is disabled. If the value is true, scale in is disabled and the target tracking policy won't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking policy can remove capacity from the scalable resource. The default value is false.
-- * 'scaleInCooldown' - The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. The cooldown period is used to block subsequent scale in requests until it has expired. You should scale in conservatively to protect your application's availability. However, if another alarm triggers a scale out policy during the cooldown period after a scale-in, application auto scaling scales out your scalable target immediately.
-- * 'scaleOutCooldown' - The amount of time, in seconds, after a scale out activity completes before another scale out activity can start. While the cooldown period is in effect, the capacity that has been added by the previous scale out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. You should continuously (but not excessively) scale out.
-- * 'targetValue' - The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
mkAutoScalingTargetTrackingScalingPolicyConfigurationDescription ::
  -- | 'targetValue'
  Lude.Double ->
  AutoScalingTargetTrackingScalingPolicyConfigurationDescription
mkAutoScalingTargetTrackingScalingPolicyConfigurationDescription
  pTargetValue_ =
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription'
      { scaleInCooldown =
          Lude.Nothing,
        disableScaleIn = Lude.Nothing,
        scaleOutCooldown = Lude.Nothing,
        targetValue = pTargetValue_
      }

-- | The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. The cooldown period is used to block subsequent scale in requests until it has expired. You should scale in conservatively to protect your application's availability. However, if another alarm triggers a scale out policy during the cooldown period after a scale-in, application auto scaling scales out your scalable target immediately.
--
-- /Note:/ Consider using 'scaleInCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asttspcdScaleInCooldown :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Lude.Maybe Lude.Int)
asttspcdScaleInCooldown = Lens.lens (scaleInCooldown :: AutoScalingTargetTrackingScalingPolicyConfigurationDescription -> Lude.Maybe Lude.Int) (\s a -> s {scaleInCooldown = a} :: AutoScalingTargetTrackingScalingPolicyConfigurationDescription)
{-# DEPRECATED asttspcdScaleInCooldown "Use generic-lens or generic-optics with 'scaleInCooldown' instead." #-}

-- | Indicates whether scale in by the target tracking policy is disabled. If the value is true, scale in is disabled and the target tracking policy won't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking policy can remove capacity from the scalable resource. The default value is false.
--
-- /Note:/ Consider using 'disableScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asttspcdDisableScaleIn :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Lude.Maybe Lude.Bool)
asttspcdDisableScaleIn = Lens.lens (disableScaleIn :: AutoScalingTargetTrackingScalingPolicyConfigurationDescription -> Lude.Maybe Lude.Bool) (\s a -> s {disableScaleIn = a} :: AutoScalingTargetTrackingScalingPolicyConfigurationDescription)
{-# DEPRECATED asttspcdDisableScaleIn "Use generic-lens or generic-optics with 'disableScaleIn' instead." #-}

-- | The amount of time, in seconds, after a scale out activity completes before another scale out activity can start. While the cooldown period is in effect, the capacity that has been added by the previous scale out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. You should continuously (but not excessively) scale out.
--
-- /Note:/ Consider using 'scaleOutCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asttspcdScaleOutCooldown :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Lude.Maybe Lude.Int)
asttspcdScaleOutCooldown = Lens.lens (scaleOutCooldown :: AutoScalingTargetTrackingScalingPolicyConfigurationDescription -> Lude.Maybe Lude.Int) (\s a -> s {scaleOutCooldown = a} :: AutoScalingTargetTrackingScalingPolicyConfigurationDescription)
{-# DEPRECATED asttspcdScaleOutCooldown "Use generic-lens or generic-optics with 'scaleOutCooldown' instead." #-}

-- | The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
--
-- /Note:/ Consider using 'targetValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asttspcdTargetValue :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription Lude.Double
asttspcdTargetValue = Lens.lens (targetValue :: AutoScalingTargetTrackingScalingPolicyConfigurationDescription -> Lude.Double) (\s a -> s {targetValue = a} :: AutoScalingTargetTrackingScalingPolicyConfigurationDescription)
{-# DEPRECATED asttspcdTargetValue "Use generic-lens or generic-optics with 'targetValue' instead." #-}

instance
  Lude.FromJSON
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription
  where
  parseJSON =
    Lude.withObject
      "AutoScalingTargetTrackingScalingPolicyConfigurationDescription"
      ( \x ->
          AutoScalingTargetTrackingScalingPolicyConfigurationDescription'
            Lude.<$> (x Lude..:? "ScaleInCooldown")
              Lude.<*> (x Lude..:? "DisableScaleIn")
              Lude.<*> (x Lude..:? "ScaleOutCooldown")
              Lude.<*> (x Lude..: "TargetValue")
      )
