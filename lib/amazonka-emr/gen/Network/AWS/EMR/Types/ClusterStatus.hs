{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ClusterStatus
  ( ClusterStatus (..),

    -- * Smart constructor
    mkClusterStatus,

    -- * Lenses
    csState,
    csStateChangeReason,
    csTimeline,
  )
where

import Network.AWS.EMR.Types.ClusterState
import Network.AWS.EMR.Types.ClusterStateChangeReason
import Network.AWS.EMR.Types.ClusterTimeline
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The detailed status of the cluster.
--
-- /See:/ 'mkClusterStatus' smart constructor.
data ClusterStatus = ClusterStatus'
  { state ::
      Lude.Maybe ClusterState,
    stateChangeReason :: Lude.Maybe ClusterStateChangeReason,
    timeline :: Lude.Maybe ClusterTimeline
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterStatus' with the minimum fields required to make a request.
--
-- * 'state' - The current state of the cluster.
-- * 'stateChangeReason' - The reason for the cluster status change.
-- * 'timeline' - A timeline that represents the status of a cluster over the lifetime of the cluster.
mkClusterStatus ::
  ClusterStatus
mkClusterStatus =
  ClusterStatus'
    { state = Lude.Nothing,
      stateChangeReason = Lude.Nothing,
      timeline = Lude.Nothing
    }

-- | The current state of the cluster.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csState :: Lens.Lens' ClusterStatus (Lude.Maybe ClusterState)
csState = Lens.lens (state :: ClusterStatus -> Lude.Maybe ClusterState) (\s a -> s {state = a} :: ClusterStatus)
{-# DEPRECATED csState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The reason for the cluster status change.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStateChangeReason :: Lens.Lens' ClusterStatus (Lude.Maybe ClusterStateChangeReason)
csStateChangeReason = Lens.lens (stateChangeReason :: ClusterStatus -> Lude.Maybe ClusterStateChangeReason) (\s a -> s {stateChangeReason = a} :: ClusterStatus)
{-# DEPRECATED csStateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead." #-}

-- | A timeline that represents the status of a cluster over the lifetime of the cluster.
--
-- /Note:/ Consider using 'timeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTimeline :: Lens.Lens' ClusterStatus (Lude.Maybe ClusterTimeline)
csTimeline = Lens.lens (timeline :: ClusterStatus -> Lude.Maybe ClusterTimeline) (\s a -> s {timeline = a} :: ClusterStatus)
{-# DEPRECATED csTimeline "Use generic-lens or generic-optics with 'timeline' instead." #-}

instance Lude.FromJSON ClusterStatus where
  parseJSON =
    Lude.withObject
      "ClusterStatus"
      ( \x ->
          ClusterStatus'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "StateChangeReason")
            Lude.<*> (x Lude..:? "Timeline")
      )
