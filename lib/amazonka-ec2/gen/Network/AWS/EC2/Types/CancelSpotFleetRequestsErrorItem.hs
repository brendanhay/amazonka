{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelSpotFleetRequestsErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelSpotFleetRequestsErrorItem
  ( CancelSpotFleetRequestsErrorItem (..),

    -- * Smart constructor
    mkCancelSpotFleetRequestsErrorItem,

    -- * Lenses
    csfreiError,
    csfreiSpotFleetRequestId,
  )
where

import Network.AWS.EC2.Types.CancelSpotFleetRequestsError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Spot Fleet request that was not successfully canceled.
--
-- /See:/ 'mkCancelSpotFleetRequestsErrorItem' smart constructor.
data CancelSpotFleetRequestsErrorItem = CancelSpotFleetRequestsErrorItem'
  { -- | The error.
    error :: Lude.Maybe CancelSpotFleetRequestsError,
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelSpotFleetRequestsErrorItem' with the minimum fields required to make a request.
--
-- * 'error' - The error.
-- * 'spotFleetRequestId' - The ID of the Spot Fleet request.
mkCancelSpotFleetRequestsErrorItem ::
  CancelSpotFleetRequestsErrorItem
mkCancelSpotFleetRequestsErrorItem =
  CancelSpotFleetRequestsErrorItem'
    { error = Lude.Nothing,
      spotFleetRequestId = Lude.Nothing
    }

-- | The error.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfreiError :: Lens.Lens' CancelSpotFleetRequestsErrorItem (Lude.Maybe CancelSpotFleetRequestsError)
csfreiError = Lens.lens (error :: CancelSpotFleetRequestsErrorItem -> Lude.Maybe CancelSpotFleetRequestsError) (\s a -> s {error = a} :: CancelSpotFleetRequestsErrorItem)
{-# DEPRECATED csfreiError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfreiSpotFleetRequestId :: Lens.Lens' CancelSpotFleetRequestsErrorItem (Lude.Maybe Lude.Text)
csfreiSpotFleetRequestId = Lens.lens (spotFleetRequestId :: CancelSpotFleetRequestsErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {spotFleetRequestId = a} :: CancelSpotFleetRequestsErrorItem)
{-# DEPRECATED csfreiSpotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead." #-}

instance Lude.FromXML CancelSpotFleetRequestsErrorItem where
  parseXML x =
    CancelSpotFleetRequestsErrorItem'
      Lude.<$> (x Lude..@? "error") Lude.<*> (x Lude..@? "spotFleetRequestId")
