{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RouteTableAssociationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RouteTableAssociationState
  ( RouteTableAssociationState (..),

    -- * Smart constructor
    mkRouteTableAssociationState,

    -- * Lenses
    rtasState,
    rtasStatusMessage,
  )
where

import Network.AWS.EC2.Types.RouteTableAssociationStateCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the state of an association between a route table and a subnet or gateway.
--
-- /See:/ 'mkRouteTableAssociationState' smart constructor.
data RouteTableAssociationState = RouteTableAssociationState'
  { state ::
      Lude.Maybe
        RouteTableAssociationStateCode,
    statusMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RouteTableAssociationState' with the minimum fields required to make a request.
--
-- * 'state' - The state of the association.
-- * 'statusMessage' - The status message, if applicable.
mkRouteTableAssociationState ::
  RouteTableAssociationState
mkRouteTableAssociationState =
  RouteTableAssociationState'
    { state = Lude.Nothing,
      statusMessage = Lude.Nothing
    }

-- | The state of the association.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtasState :: Lens.Lens' RouteTableAssociationState (Lude.Maybe RouteTableAssociationStateCode)
rtasState = Lens.lens (state :: RouteTableAssociationState -> Lude.Maybe RouteTableAssociationStateCode) (\s a -> s {state = a} :: RouteTableAssociationState)
{-# DEPRECATED rtasState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The status message, if applicable.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtasStatusMessage :: Lens.Lens' RouteTableAssociationState (Lude.Maybe Lude.Text)
rtasStatusMessage = Lens.lens (statusMessage :: RouteTableAssociationState -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: RouteTableAssociationState)
{-# DEPRECATED rtasStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

instance Lude.FromXML RouteTableAssociationState where
  parseXML x =
    RouteTableAssociationState'
      Lude.<$> (x Lude..@? "state") Lude.<*> (x Lude..@? "statusMessage")
