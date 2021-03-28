{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceGroupStatus
  ( InstanceGroupStatus (..)
  -- * Smart constructor
  , mkInstanceGroupStatus
  -- * Lenses
  , igsState
  , igsStateChangeReason
  , igsTimeline
  ) where

import qualified Network.AWS.EMR.Types.InstanceGroupState as Types
import qualified Network.AWS.EMR.Types.InstanceGroupStateChangeReason as Types
import qualified Network.AWS.EMR.Types.InstanceGroupTimeline as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of the instance group status.
--
-- /See:/ 'mkInstanceGroupStatus' smart constructor.
data InstanceGroupStatus = InstanceGroupStatus'
  { state :: Core.Maybe Types.InstanceGroupState
    -- ^ The current state of the instance group.
  , stateChangeReason :: Core.Maybe Types.InstanceGroupStateChangeReason
    -- ^ The status change reason details for the instance group.
  , timeline :: Core.Maybe Types.InstanceGroupTimeline
    -- ^ The timeline of the instance group status over time.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceGroupStatus' value with any optional fields omitted.
mkInstanceGroupStatus
    :: InstanceGroupStatus
mkInstanceGroupStatus
  = InstanceGroupStatus'{state = Core.Nothing,
                         stateChangeReason = Core.Nothing, timeline = Core.Nothing}

-- | The current state of the instance group.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igsState :: Lens.Lens' InstanceGroupStatus (Core.Maybe Types.InstanceGroupState)
igsState = Lens.field @"state"
{-# INLINEABLE igsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The status change reason details for the instance group.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igsStateChangeReason :: Lens.Lens' InstanceGroupStatus (Core.Maybe Types.InstanceGroupStateChangeReason)
igsStateChangeReason = Lens.field @"stateChangeReason"
{-# INLINEABLE igsStateChangeReason #-}
{-# DEPRECATED stateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead"  #-}

-- | The timeline of the instance group status over time.
--
-- /Note:/ Consider using 'timeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igsTimeline :: Lens.Lens' InstanceGroupStatus (Core.Maybe Types.InstanceGroupTimeline)
igsTimeline = Lens.field @"timeline"
{-# INLINEABLE igsTimeline #-}
{-# DEPRECATED timeline "Use generic-lens or generic-optics with 'timeline' instead"  #-}

instance Core.FromJSON InstanceGroupStatus where
        parseJSON
          = Core.withObject "InstanceGroupStatus" Core.$
              \ x ->
                InstanceGroupStatus' Core.<$>
                  (x Core..:? "State") Core.<*> x Core..:? "StateChangeReason"
                    Core.<*> x Core..:? "Timeline"
