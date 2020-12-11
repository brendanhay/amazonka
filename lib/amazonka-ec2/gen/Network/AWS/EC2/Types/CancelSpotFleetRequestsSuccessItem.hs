-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelSpotFleetRequestsSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelSpotFleetRequestsSuccessItem
  ( CancelSpotFleetRequestsSuccessItem (..),

    -- * Smart constructor
    mkCancelSpotFleetRequestsSuccessItem,

    -- * Lenses
    csfrsiCurrentSpotFleetRequestState,
    csfrsiSpotFleetRequestId,
    csfrsiPreviousSpotFleetRequestState,
  )
where

import Network.AWS.EC2.Types.BatchState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Spot Fleet request that was successfully canceled.
--
-- /See:/ 'mkCancelSpotFleetRequestsSuccessItem' smart constructor.
data CancelSpotFleetRequestsSuccessItem = CancelSpotFleetRequestsSuccessItem'
  { currentSpotFleetRequestState ::
      Lude.Maybe BatchState,
    spotFleetRequestId ::
      Lude.Maybe Lude.Text,
    previousSpotFleetRequestState ::
      Lude.Maybe BatchState
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelSpotFleetRequestsSuccessItem' with the minimum fields required to make a request.
--
-- * 'currentSpotFleetRequestState' - The current state of the Spot Fleet request.
-- * 'previousSpotFleetRequestState' - The previous state of the Spot Fleet request.
-- * 'spotFleetRequestId' - The ID of the Spot Fleet request.
mkCancelSpotFleetRequestsSuccessItem ::
  CancelSpotFleetRequestsSuccessItem
mkCancelSpotFleetRequestsSuccessItem =
  CancelSpotFleetRequestsSuccessItem'
    { currentSpotFleetRequestState =
        Lude.Nothing,
      spotFleetRequestId = Lude.Nothing,
      previousSpotFleetRequestState = Lude.Nothing
    }

-- | The current state of the Spot Fleet request.
--
-- /Note:/ Consider using 'currentSpotFleetRequestState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrsiCurrentSpotFleetRequestState :: Lens.Lens' CancelSpotFleetRequestsSuccessItem (Lude.Maybe BatchState)
csfrsiCurrentSpotFleetRequestState = Lens.lens (currentSpotFleetRequestState :: CancelSpotFleetRequestsSuccessItem -> Lude.Maybe BatchState) (\s a -> s {currentSpotFleetRequestState = a} :: CancelSpotFleetRequestsSuccessItem)
{-# DEPRECATED csfrsiCurrentSpotFleetRequestState "Use generic-lens or generic-optics with 'currentSpotFleetRequestState' instead." #-}

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrsiSpotFleetRequestId :: Lens.Lens' CancelSpotFleetRequestsSuccessItem (Lude.Maybe Lude.Text)
csfrsiSpotFleetRequestId = Lens.lens (spotFleetRequestId :: CancelSpotFleetRequestsSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {spotFleetRequestId = a} :: CancelSpotFleetRequestsSuccessItem)
{-# DEPRECATED csfrsiSpotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead." #-}

-- | The previous state of the Spot Fleet request.
--
-- /Note:/ Consider using 'previousSpotFleetRequestState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrsiPreviousSpotFleetRequestState :: Lens.Lens' CancelSpotFleetRequestsSuccessItem (Lude.Maybe BatchState)
csfrsiPreviousSpotFleetRequestState = Lens.lens (previousSpotFleetRequestState :: CancelSpotFleetRequestsSuccessItem -> Lude.Maybe BatchState) (\s a -> s {previousSpotFleetRequestState = a} :: CancelSpotFleetRequestsSuccessItem)
{-# DEPRECATED csfrsiPreviousSpotFleetRequestState "Use generic-lens or generic-optics with 'previousSpotFleetRequestState' instead." #-}

instance Lude.FromXML CancelSpotFleetRequestsSuccessItem where
  parseXML x =
    CancelSpotFleetRequestsSuccessItem'
      Lude.<$> (x Lude..@? "currentSpotFleetRequestState")
      Lude.<*> (x Lude..@? "spotFleetRequestId")
      Lude.<*> (x Lude..@? "previousSpotFleetRequestState")
