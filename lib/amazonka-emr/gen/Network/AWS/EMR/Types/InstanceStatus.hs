{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceStatus
  ( InstanceStatus (..)
  -- * Smart constructor
  , mkInstanceStatus
  -- * Lenses
  , isState
  , isStateChangeReason
  , isTimeline
  ) where

import qualified Network.AWS.EMR.Types.InstanceState as Types
import qualified Network.AWS.EMR.Types.InstanceStateChangeReason as Types
import qualified Network.AWS.EMR.Types.InstanceTimeline as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The instance status details.
--
-- /See:/ 'mkInstanceStatus' smart constructor.
data InstanceStatus = InstanceStatus'
  { state :: Core.Maybe Types.InstanceState
    -- ^ The current state of the instance.
  , stateChangeReason :: Core.Maybe Types.InstanceStateChangeReason
    -- ^ The details of the status change reason for the instance.
  , timeline :: Core.Maybe Types.InstanceTimeline
    -- ^ The timeline of the instance status over time.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceStatus' value with any optional fields omitted.
mkInstanceStatus
    :: InstanceStatus
mkInstanceStatus
  = InstanceStatus'{state = Core.Nothing,
                    stateChangeReason = Core.Nothing, timeline = Core.Nothing}

-- | The current state of the instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isState :: Lens.Lens' InstanceStatus (Core.Maybe Types.InstanceState)
isState = Lens.field @"state"
{-# INLINEABLE isState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The details of the status change reason for the instance.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isStateChangeReason :: Lens.Lens' InstanceStatus (Core.Maybe Types.InstanceStateChangeReason)
isStateChangeReason = Lens.field @"stateChangeReason"
{-# INLINEABLE isStateChangeReason #-}
{-# DEPRECATED stateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead"  #-}

-- | The timeline of the instance status over time.
--
-- /Note:/ Consider using 'timeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isTimeline :: Lens.Lens' InstanceStatus (Core.Maybe Types.InstanceTimeline)
isTimeline = Lens.field @"timeline"
{-# INLINEABLE isTimeline #-}
{-# DEPRECATED timeline "Use generic-lens or generic-optics with 'timeline' instead"  #-}

instance Core.FromJSON InstanceStatus where
        parseJSON
          = Core.withObject "InstanceStatus" Core.$
              \ x ->
                InstanceStatus' Core.<$>
                  (x Core..:? "State") Core.<*> x Core..:? "StateChangeReason"
                    Core.<*> x Core..:? "Timeline"
