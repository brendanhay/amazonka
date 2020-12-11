-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupStatus
  ( InstanceGroupStatus (..),

    -- * Smart constructor
    mkInstanceGroupStatus,

    -- * Lenses
    igsState,
    igsStateChangeReason,
    igsTimeline,
  )
where

import Network.AWS.EMR.Types.InstanceGroupState
import Network.AWS.EMR.Types.InstanceGroupStateChangeReason
import Network.AWS.EMR.Types.InstanceGroupTimeline
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of the instance group status.
--
-- /See:/ 'mkInstanceGroupStatus' smart constructor.
data InstanceGroupStatus = InstanceGroupStatus'
  { state ::
      Lude.Maybe InstanceGroupState,
    stateChangeReason ::
      Lude.Maybe InstanceGroupStateChangeReason,
    timeline :: Lude.Maybe InstanceGroupTimeline
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceGroupStatus' with the minimum fields required to make a request.
--
-- * 'state' - The current state of the instance group.
-- * 'stateChangeReason' - The status change reason details for the instance group.
-- * 'timeline' - The timeline of the instance group status over time.
mkInstanceGroupStatus ::
  InstanceGroupStatus
mkInstanceGroupStatus =
  InstanceGroupStatus'
    { state = Lude.Nothing,
      stateChangeReason = Lude.Nothing,
      timeline = Lude.Nothing
    }

-- | The current state of the instance group.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igsState :: Lens.Lens' InstanceGroupStatus (Lude.Maybe InstanceGroupState)
igsState = Lens.lens (state :: InstanceGroupStatus -> Lude.Maybe InstanceGroupState) (\s a -> s {state = a} :: InstanceGroupStatus)
{-# DEPRECATED igsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The status change reason details for the instance group.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igsStateChangeReason :: Lens.Lens' InstanceGroupStatus (Lude.Maybe InstanceGroupStateChangeReason)
igsStateChangeReason = Lens.lens (stateChangeReason :: InstanceGroupStatus -> Lude.Maybe InstanceGroupStateChangeReason) (\s a -> s {stateChangeReason = a} :: InstanceGroupStatus)
{-# DEPRECATED igsStateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead." #-}

-- | The timeline of the instance group status over time.
--
-- /Note:/ Consider using 'timeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igsTimeline :: Lens.Lens' InstanceGroupStatus (Lude.Maybe InstanceGroupTimeline)
igsTimeline = Lens.lens (timeline :: InstanceGroupStatus -> Lude.Maybe InstanceGroupTimeline) (\s a -> s {timeline = a} :: InstanceGroupStatus)
{-# DEPRECATED igsTimeline "Use generic-lens or generic-optics with 'timeline' instead." #-}

instance Lude.FromJSON InstanceGroupStatus where
  parseJSON =
    Lude.withObject
      "InstanceGroupStatus"
      ( \x ->
          InstanceGroupStatus'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "StateChangeReason")
            Lude.<*> (x Lude..:? "Timeline")
      )
