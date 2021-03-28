{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
  ( AutoScalingTargetTrackingScalingPolicyConfigurationDescription (..)
  -- * Smart constructor
  , mkAutoScalingTargetTrackingScalingPolicyConfigurationDescription
  -- * Lenses
  , asttspcdTargetValue
  , asttspcdDisableScaleIn
  , asttspcdScaleInCooldown
  , asttspcdScaleOutCooldown
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the properties of a target tracking scaling policy.
--
-- /See:/ 'mkAutoScalingTargetTrackingScalingPolicyConfigurationDescription' smart constructor.
data AutoScalingTargetTrackingScalingPolicyConfigurationDescription = AutoScalingTargetTrackingScalingPolicyConfigurationDescription'
  { targetValue :: Core.Double
    -- ^ The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
  , disableScaleIn :: Core.Maybe Core.Bool
    -- ^ Indicates whether scale in by the target tracking policy is disabled. If the value is true, scale in is disabled and the target tracking policy won't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking policy can remove capacity from the scalable resource. The default value is false.
  , scaleInCooldown :: Core.Maybe Core.Int
    -- ^ The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. The cooldown period is used to block subsequent scale in requests until it has expired. You should scale in conservatively to protect your application's availability. However, if another alarm triggers a scale out policy during the cooldown period after a scale-in, application auto scaling scales out your scalable target immediately. 
  , scaleOutCooldown :: Core.Maybe Core.Int
    -- ^ The amount of time, in seconds, after a scale out activity completes before another scale out activity can start. While the cooldown period is in effect, the capacity that has been added by the previous scale out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. You should continuously (but not excessively) scale out.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoScalingTargetTrackingScalingPolicyConfigurationDescription' value with any optional fields omitted.
mkAutoScalingTargetTrackingScalingPolicyConfigurationDescription
    :: Core.Double -- ^ 'targetValue'
    -> AutoScalingTargetTrackingScalingPolicyConfigurationDescription
mkAutoScalingTargetTrackingScalingPolicyConfigurationDescription
  targetValue
  = AutoScalingTargetTrackingScalingPolicyConfigurationDescription'{targetValue,
                                                                    disableScaleIn = Core.Nothing,
                                                                    scaleInCooldown = Core.Nothing,
                                                                    scaleOutCooldown = Core.Nothing}

-- | The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
--
-- /Note:/ Consider using 'targetValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asttspcdTargetValue :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription Core.Double
asttspcdTargetValue = Lens.field @"targetValue"
{-# INLINEABLE asttspcdTargetValue #-}
{-# DEPRECATED targetValue "Use generic-lens or generic-optics with 'targetValue' instead"  #-}

-- | Indicates whether scale in by the target tracking policy is disabled. If the value is true, scale in is disabled and the target tracking policy won't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking policy can remove capacity from the scalable resource. The default value is false.
--
-- /Note:/ Consider using 'disableScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asttspcdDisableScaleIn :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Core.Maybe Core.Bool)
asttspcdDisableScaleIn = Lens.field @"disableScaleIn"
{-# INLINEABLE asttspcdDisableScaleIn #-}
{-# DEPRECATED disableScaleIn "Use generic-lens or generic-optics with 'disableScaleIn' instead"  #-}

-- | The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. The cooldown period is used to block subsequent scale in requests until it has expired. You should scale in conservatively to protect your application's availability. However, if another alarm triggers a scale out policy during the cooldown period after a scale-in, application auto scaling scales out your scalable target immediately. 
--
-- /Note:/ Consider using 'scaleInCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asttspcdScaleInCooldown :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Core.Maybe Core.Int)
asttspcdScaleInCooldown = Lens.field @"scaleInCooldown"
{-# INLINEABLE asttspcdScaleInCooldown #-}
{-# DEPRECATED scaleInCooldown "Use generic-lens or generic-optics with 'scaleInCooldown' instead"  #-}

-- | The amount of time, in seconds, after a scale out activity completes before another scale out activity can start. While the cooldown period is in effect, the capacity that has been added by the previous scale out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. You should continuously (but not excessively) scale out.
--
-- /Note:/ Consider using 'scaleOutCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asttspcdScaleOutCooldown :: Lens.Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Core.Maybe Core.Int)
asttspcdScaleOutCooldown = Lens.field @"scaleOutCooldown"
{-# INLINEABLE asttspcdScaleOutCooldown #-}
{-# DEPRECATED scaleOutCooldown "Use generic-lens or generic-optics with 'scaleOutCooldown' instead"  #-}

instance Core.FromJSON
           AutoScalingTargetTrackingScalingPolicyConfigurationDescription
         where
        parseJSON
          = Core.withObject
              "AutoScalingTargetTrackingScalingPolicyConfigurationDescription"
              Core.$
              \ x ->
                AutoScalingTargetTrackingScalingPolicyConfigurationDescription'
                  Core.<$>
                  (x Core..: "TargetValue") Core.<*> x Core..:? "DisableScaleIn"
                    Core.<*> x Core..:? "ScaleInCooldown"
                    Core.<*> x Core..:? "ScaleOutCooldown"
