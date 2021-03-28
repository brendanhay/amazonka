{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.SuspendedState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.SuspendedState
  ( SuspendedState (..)
  -- * Smart constructor
  , mkSuspendedState
  -- * Lenses
  , ssDynamicScalingInSuspended
  , ssDynamicScalingOutSuspended
  , ssScheduledScalingSuspended
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies whether the scaling activities for a scalable target are in a suspended state. 
--
-- /See:/ 'mkSuspendedState' smart constructor.
data SuspendedState = SuspendedState'
  { dynamicScalingInSuspended :: Core.Maybe Core.Bool
    -- ^ Whether scale in by a target tracking scaling policy or a step scaling policy is suspended. Set the value to @true@ if you don't want Application Auto Scaling to remove capacity when a scaling policy is triggered. The default is @false@ . 
  , dynamicScalingOutSuspended :: Core.Maybe Core.Bool
    -- ^ Whether scale out by a target tracking scaling policy or a step scaling policy is suspended. Set the value to @true@ if you don't want Application Auto Scaling to add capacity when a scaling policy is triggered. The default is @false@ . 
  , scheduledScalingSuspended :: Core.Maybe Core.Bool
    -- ^ Whether scheduled scaling is suspended. Set the value to @true@ if you don't want Application Auto Scaling to add or remove capacity by initiating scheduled actions. The default is @false@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SuspendedState' value with any optional fields omitted.
mkSuspendedState
    :: SuspendedState
mkSuspendedState
  = SuspendedState'{dynamicScalingInSuspended = Core.Nothing,
                    dynamicScalingOutSuspended = Core.Nothing,
                    scheduledScalingSuspended = Core.Nothing}

-- | Whether scale in by a target tracking scaling policy or a step scaling policy is suspended. Set the value to @true@ if you don't want Application Auto Scaling to remove capacity when a scaling policy is triggered. The default is @false@ . 
--
-- /Note:/ Consider using 'dynamicScalingInSuspended' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDynamicScalingInSuspended :: Lens.Lens' SuspendedState (Core.Maybe Core.Bool)
ssDynamicScalingInSuspended = Lens.field @"dynamicScalingInSuspended"
{-# INLINEABLE ssDynamicScalingInSuspended #-}
{-# DEPRECATED dynamicScalingInSuspended "Use generic-lens or generic-optics with 'dynamicScalingInSuspended' instead"  #-}

-- | Whether scale out by a target tracking scaling policy or a step scaling policy is suspended. Set the value to @true@ if you don't want Application Auto Scaling to add capacity when a scaling policy is triggered. The default is @false@ . 
--
-- /Note:/ Consider using 'dynamicScalingOutSuspended' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDynamicScalingOutSuspended :: Lens.Lens' SuspendedState (Core.Maybe Core.Bool)
ssDynamicScalingOutSuspended = Lens.field @"dynamicScalingOutSuspended"
{-# INLINEABLE ssDynamicScalingOutSuspended #-}
{-# DEPRECATED dynamicScalingOutSuspended "Use generic-lens or generic-optics with 'dynamicScalingOutSuspended' instead"  #-}

-- | Whether scheduled scaling is suspended. Set the value to @true@ if you don't want Application Auto Scaling to add or remove capacity by initiating scheduled actions. The default is @false@ . 
--
-- /Note:/ Consider using 'scheduledScalingSuspended' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssScheduledScalingSuspended :: Lens.Lens' SuspendedState (Core.Maybe Core.Bool)
ssScheduledScalingSuspended = Lens.field @"scheduledScalingSuspended"
{-# INLINEABLE ssScheduledScalingSuspended #-}
{-# DEPRECATED scheduledScalingSuspended "Use generic-lens or generic-optics with 'scheduledScalingSuspended' instead"  #-}

instance Core.FromJSON SuspendedState where
        toJSON SuspendedState{..}
          = Core.object
              (Core.catMaybes
                 [("DynamicScalingInSuspended" Core..=) Core.<$>
                    dynamicScalingInSuspended,
                  ("DynamicScalingOutSuspended" Core..=) Core.<$>
                    dynamicScalingOutSuspended,
                  ("ScheduledScalingSuspended" Core..=) Core.<$>
                    scheduledScalingSuspended])

instance Core.FromJSON SuspendedState where
        parseJSON
          = Core.withObject "SuspendedState" Core.$
              \ x ->
                SuspendedState' Core.<$>
                  (x Core..:? "DynamicScalingInSuspended") Core.<*>
                    x Core..:? "DynamicScalingOutSuspended"
                    Core.<*> x Core..:? "ScheduledScalingSuspended"
