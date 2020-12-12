{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceStatus
  ( InstanceStatus (..),

    -- * Smart constructor
    mkInstanceStatus,

    -- * Lenses
    isState,
    isStateChangeReason,
    isTimeline,
  )
where

import Network.AWS.EMR.Types.InstanceState
import Network.AWS.EMR.Types.InstanceStateChangeReason
import Network.AWS.EMR.Types.InstanceTimeline
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The instance status details.
--
-- /See:/ 'mkInstanceStatus' smart constructor.
data InstanceStatus = InstanceStatus'
  { state ::
      Lude.Maybe InstanceState,
    stateChangeReason :: Lude.Maybe InstanceStateChangeReason,
    timeline :: Lude.Maybe InstanceTimeline
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceStatus' with the minimum fields required to make a request.
--
-- * 'state' - The current state of the instance.
-- * 'stateChangeReason' - The details of the status change reason for the instance.
-- * 'timeline' - The timeline of the instance status over time.
mkInstanceStatus ::
  InstanceStatus
mkInstanceStatus =
  InstanceStatus'
    { state = Lude.Nothing,
      stateChangeReason = Lude.Nothing,
      timeline = Lude.Nothing
    }

-- | The current state of the instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isState :: Lens.Lens' InstanceStatus (Lude.Maybe InstanceState)
isState = Lens.lens (state :: InstanceStatus -> Lude.Maybe InstanceState) (\s a -> s {state = a} :: InstanceStatus)
{-# DEPRECATED isState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The details of the status change reason for the instance.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isStateChangeReason :: Lens.Lens' InstanceStatus (Lude.Maybe InstanceStateChangeReason)
isStateChangeReason = Lens.lens (stateChangeReason :: InstanceStatus -> Lude.Maybe InstanceStateChangeReason) (\s a -> s {stateChangeReason = a} :: InstanceStatus)
{-# DEPRECATED isStateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead." #-}

-- | The timeline of the instance status over time.
--
-- /Note:/ Consider using 'timeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isTimeline :: Lens.Lens' InstanceStatus (Lude.Maybe InstanceTimeline)
isTimeline = Lens.lens (timeline :: InstanceStatus -> Lude.Maybe InstanceTimeline) (\s a -> s {timeline = a} :: InstanceStatus)
{-# DEPRECATED isTimeline "Use generic-lens or generic-optics with 'timeline' instead." #-}

instance Lude.FromJSON InstanceStatus where
  parseJSON =
    Lude.withObject
      "InstanceStatus"
      ( \x ->
          InstanceStatus'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "StateChangeReason")
            Lude.<*> (x Lude..:? "Timeline")
      )
