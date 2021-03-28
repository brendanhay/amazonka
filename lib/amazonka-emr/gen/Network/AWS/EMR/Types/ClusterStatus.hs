{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ClusterStatus
  ( ClusterStatus (..)
  -- * Smart constructor
  , mkClusterStatus
  -- * Lenses
  , csState
  , csStateChangeReason
  , csTimeline
  ) where

import qualified Network.AWS.EMR.Types.ClusterState as Types
import qualified Network.AWS.EMR.Types.ClusterStateChangeReason as Types
import qualified Network.AWS.EMR.Types.ClusterTimeline as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The detailed status of the cluster.
--
-- /See:/ 'mkClusterStatus' smart constructor.
data ClusterStatus = ClusterStatus'
  { state :: Core.Maybe Types.ClusterState
    -- ^ The current state of the cluster.
  , stateChangeReason :: Core.Maybe Types.ClusterStateChangeReason
    -- ^ The reason for the cluster status change.
  , timeline :: Core.Maybe Types.ClusterTimeline
    -- ^ A timeline that represents the status of a cluster over the lifetime of the cluster.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ClusterStatus' value with any optional fields omitted.
mkClusterStatus
    :: ClusterStatus
mkClusterStatus
  = ClusterStatus'{state = Core.Nothing,
                   stateChangeReason = Core.Nothing, timeline = Core.Nothing}

-- | The current state of the cluster.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csState :: Lens.Lens' ClusterStatus (Core.Maybe Types.ClusterState)
csState = Lens.field @"state"
{-# INLINEABLE csState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The reason for the cluster status change.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStateChangeReason :: Lens.Lens' ClusterStatus (Core.Maybe Types.ClusterStateChangeReason)
csStateChangeReason = Lens.field @"stateChangeReason"
{-# INLINEABLE csStateChangeReason #-}
{-# DEPRECATED stateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead"  #-}

-- | A timeline that represents the status of a cluster over the lifetime of the cluster.
--
-- /Note:/ Consider using 'timeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTimeline :: Lens.Lens' ClusterStatus (Core.Maybe Types.ClusterTimeline)
csTimeline = Lens.field @"timeline"
{-# INLINEABLE csTimeline #-}
{-# DEPRECATED timeline "Use generic-lens or generic-optics with 'timeline' instead"  #-}

instance Core.FromJSON ClusterStatus where
        parseJSON
          = Core.withObject "ClusterStatus" Core.$
              \ x ->
                ClusterStatus' Core.<$>
                  (x Core..:? "State") Core.<*> x Core..:? "StateChangeReason"
                    Core.<*> x Core..:? "Timeline"
