-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteTableAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteTableAssociation
  ( TransitGatewayRouteTableAssociation (..),

    -- * Smart constructor
    mkTransitGatewayRouteTableAssociation,

    -- * Lenses
    tgrtaState,
    tgrtaResourceId,
    tgrtaResourceType,
    tgrtaTransitGatewayAttachmentId,
  )
where

import Network.AWS.EC2.Types.TransitGatewayAssociationState
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an association between a route table and a resource attachment.
--
-- /See:/ 'mkTransitGatewayRouteTableAssociation' smart constructor.
data TransitGatewayRouteTableAssociation = TransitGatewayRouteTableAssociation'
  { state ::
      Lude.Maybe
        TransitGatewayAssociationState,
    resourceId ::
      Lude.Maybe
        Lude.Text,
    resourceType ::
      Lude.Maybe
        TransitGatewayAttachmentResourceType,
    transitGatewayAttachmentId ::
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

-- | Creates a value of 'TransitGatewayRouteTableAssociation' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource.
-- * 'resourceType' - The resource type. Note that the @tgw-peering@ resource type has been deprecated.
-- * 'state' - The state of the association.
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
mkTransitGatewayRouteTableAssociation ::
  TransitGatewayRouteTableAssociation
mkTransitGatewayRouteTableAssociation =
  TransitGatewayRouteTableAssociation'
    { state = Lude.Nothing,
      resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      transitGatewayAttachmentId = Lude.Nothing
    }

-- | The state of the association.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtaState :: Lens.Lens' TransitGatewayRouteTableAssociation (Lude.Maybe TransitGatewayAssociationState)
tgrtaState = Lens.lens (state :: TransitGatewayRouteTableAssociation -> Lude.Maybe TransitGatewayAssociationState) (\s a -> s {state = a} :: TransitGatewayRouteTableAssociation)
{-# DEPRECATED tgrtaState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtaResourceId :: Lens.Lens' TransitGatewayRouteTableAssociation (Lude.Maybe Lude.Text)
tgrtaResourceId = Lens.lens (resourceId :: TransitGatewayRouteTableAssociation -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: TransitGatewayRouteTableAssociation)
{-# DEPRECATED tgrtaResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtaResourceType :: Lens.Lens' TransitGatewayRouteTableAssociation (Lude.Maybe TransitGatewayAttachmentResourceType)
tgrtaResourceType = Lens.lens (resourceType :: TransitGatewayRouteTableAssociation -> Lude.Maybe TransitGatewayAttachmentResourceType) (\s a -> s {resourceType = a} :: TransitGatewayRouteTableAssociation)
{-# DEPRECATED tgrtaResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtaTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayRouteTableAssociation (Lude.Maybe Lude.Text)
tgrtaTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: TransitGatewayRouteTableAssociation -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: TransitGatewayRouteTableAssociation)
{-# DEPRECATED tgrtaTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Lude.FromXML TransitGatewayRouteTableAssociation where
  parseXML x =
    TransitGatewayRouteTableAssociation'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "resourceId")
      Lude.<*> (x Lude..@? "resourceType")
      Lude.<*> (x Lude..@? "transitGatewayAttachmentId")
