{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelledSpotInstanceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelledSpotInstanceRequest
  ( CancelledSpotInstanceRequest (..),

    -- * Smart constructor
    mkCancelledSpotInstanceRequest,

    -- * Lenses
    csirState,
    csirSpotInstanceRequestId,
  )
where

import Network.AWS.EC2.Types.CancelSpotInstanceRequestState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a request to cancel a Spot Instance.
--
-- /See:/ 'mkCancelledSpotInstanceRequest' smart constructor.
data CancelledSpotInstanceRequest = CancelledSpotInstanceRequest'
  { -- | The state of the Spot Instance request.
    state :: Lude.Maybe CancelSpotInstanceRequestState,
    -- | The ID of the Spot Instance request.
    spotInstanceRequestId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelledSpotInstanceRequest' with the minimum fields required to make a request.
--
-- * 'state' - The state of the Spot Instance request.
-- * 'spotInstanceRequestId' - The ID of the Spot Instance request.
mkCancelledSpotInstanceRequest ::
  CancelledSpotInstanceRequest
mkCancelledSpotInstanceRequest =
  CancelledSpotInstanceRequest'
    { state = Lude.Nothing,
      spotInstanceRequestId = Lude.Nothing
    }

-- | The state of the Spot Instance request.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirState :: Lens.Lens' CancelledSpotInstanceRequest (Lude.Maybe CancelSpotInstanceRequestState)
csirState = Lens.lens (state :: CancelledSpotInstanceRequest -> Lude.Maybe CancelSpotInstanceRequestState) (\s a -> s {state = a} :: CancelledSpotInstanceRequest)
{-# DEPRECATED csirState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the Spot Instance request.
--
-- /Note:/ Consider using 'spotInstanceRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirSpotInstanceRequestId :: Lens.Lens' CancelledSpotInstanceRequest (Lude.Maybe Lude.Text)
csirSpotInstanceRequestId = Lens.lens (spotInstanceRequestId :: CancelledSpotInstanceRequest -> Lude.Maybe Lude.Text) (\s a -> s {spotInstanceRequestId = a} :: CancelledSpotInstanceRequest)
{-# DEPRECATED csirSpotInstanceRequestId "Use generic-lens or generic-optics with 'spotInstanceRequestId' instead." #-}

instance Lude.FromXML CancelledSpotInstanceRequest where
  parseXML x =
    CancelledSpotInstanceRequest'
      Lude.<$> (x Lude..@? "state") Lude.<*> (x Lude..@? "spotInstanceRequestId")
