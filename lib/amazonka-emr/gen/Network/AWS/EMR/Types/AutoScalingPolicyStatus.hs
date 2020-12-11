-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicyStatus
  ( AutoScalingPolicyStatus (..),

    -- * Smart constructor
    mkAutoScalingPolicyStatus,

    -- * Lenses
    aspsState,
    aspsStateChangeReason,
  )
where

import Network.AWS.EMR.Types.AutoScalingPolicyState
import Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReason
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status of an automatic scaling policy.
--
-- /See:/ 'mkAutoScalingPolicyStatus' smart constructor.
data AutoScalingPolicyStatus = AutoScalingPolicyStatus'
  { state ::
      Lude.Maybe AutoScalingPolicyState,
    stateChangeReason ::
      Lude.Maybe
        AutoScalingPolicyStateChangeReason
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoScalingPolicyStatus' with the minimum fields required to make a request.
--
-- * 'state' - Indicates the status of the automatic scaling policy.
-- * 'stateChangeReason' - The reason for a change in status.
mkAutoScalingPolicyStatus ::
  AutoScalingPolicyStatus
mkAutoScalingPolicyStatus =
  AutoScalingPolicyStatus'
    { state = Lude.Nothing,
      stateChangeReason = Lude.Nothing
    }

-- | Indicates the status of the automatic scaling policy.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspsState :: Lens.Lens' AutoScalingPolicyStatus (Lude.Maybe AutoScalingPolicyState)
aspsState = Lens.lens (state :: AutoScalingPolicyStatus -> Lude.Maybe AutoScalingPolicyState) (\s a -> s {state = a} :: AutoScalingPolicyStatus)
{-# DEPRECATED aspsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The reason for a change in status.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspsStateChangeReason :: Lens.Lens' AutoScalingPolicyStatus (Lude.Maybe AutoScalingPolicyStateChangeReason)
aspsStateChangeReason = Lens.lens (stateChangeReason :: AutoScalingPolicyStatus -> Lude.Maybe AutoScalingPolicyStateChangeReason) (\s a -> s {stateChangeReason = a} :: AutoScalingPolicyStatus)
{-# DEPRECATED aspsStateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead." #-}

instance Lude.FromJSON AutoScalingPolicyStatus where
  parseJSON =
    Lude.withObject
      "AutoScalingPolicyStatus"
      ( \x ->
          AutoScalingPolicyStatus'
            Lude.<$> (x Lude..:? "State") Lude.<*> (x Lude..:? "StateChangeReason")
      )
