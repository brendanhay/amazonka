-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation
  ( TransitGatewayAttachmentAssociation (..),

    -- * Smart constructor
    mkTransitGatewayAttachmentAssociation,

    -- * Lenses
    tgaaState,
    tgaaTransitGatewayRouteTableId,
  )
where

import Network.AWS.EC2.Types.TransitGatewayAssociationState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an association.
--
-- /See:/ 'mkTransitGatewayAttachmentAssociation' smart constructor.
data TransitGatewayAttachmentAssociation = TransitGatewayAttachmentAssociation'
  { state ::
      Lude.Maybe
        TransitGatewayAssociationState,
    transitGatewayRouteTableId ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayAttachmentAssociation' with the minimum fields required to make a request.
--
-- * 'state' - The state of the association.
-- * 'transitGatewayRouteTableId' - The ID of the route table for the transit gateway.
mkTransitGatewayAttachmentAssociation ::
  TransitGatewayAttachmentAssociation
mkTransitGatewayAttachmentAssociation =
  TransitGatewayAttachmentAssociation'
    { state = Lude.Nothing,
      transitGatewayRouteTableId = Lude.Nothing
    }

-- | The state of the association.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaaState :: Lens.Lens' TransitGatewayAttachmentAssociation (Lude.Maybe TransitGatewayAssociationState)
tgaaState = Lens.lens (state :: TransitGatewayAttachmentAssociation -> Lude.Maybe TransitGatewayAssociationState) (\s a -> s {state = a} :: TransitGatewayAttachmentAssociation)
{-# DEPRECATED tgaaState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the route table for the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaaTransitGatewayRouteTableId :: Lens.Lens' TransitGatewayAttachmentAssociation (Lude.Maybe Lude.Text)
tgaaTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: TransitGatewayAttachmentAssociation -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: TransitGatewayAttachmentAssociation)
{-# DEPRECATED tgaaTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

instance Lude.FromXML TransitGatewayAttachmentAssociation where
  parseXML x =
    TransitGatewayAttachmentAssociation'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "transitGatewayRouteTableId")
