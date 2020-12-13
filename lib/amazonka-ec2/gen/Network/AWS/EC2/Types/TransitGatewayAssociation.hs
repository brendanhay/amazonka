{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAssociation
  ( TransitGatewayAssociation (..),

    -- * Smart constructor
    mkTransitGatewayAssociation,

    -- * Lenses
    tgafState,
    tgafResourceId,
    tgafResourceType,
    tgafTransitGatewayRouteTableId,
    tgafTransitGatewayAttachmentId,
  )
where

import Network.AWS.EC2.Types.TransitGatewayAssociationState
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an association between a resource attachment and a transit gateway route table.
--
-- /See:/ 'mkTransitGatewayAssociation' smart constructor.
data TransitGatewayAssociation = TransitGatewayAssociation'
  { -- | The state of the association.
    state :: Lude.Maybe TransitGatewayAssociationState,
    -- | The ID of the resource.
    resourceId :: Lude.Maybe Lude.Text,
    -- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
    resourceType :: Lude.Maybe TransitGatewayAttachmentResourceType,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Lude.Maybe Lude.Text,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayAssociation' with the minimum fields required to make a request.
--
-- * 'state' - The state of the association.
-- * 'resourceId' - The ID of the resource.
-- * 'resourceType' - The resource type. Note that the @tgw-peering@ resource type has been deprecated.
-- * 'transitGatewayRouteTableId' - The ID of the transit gateway route table.
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
mkTransitGatewayAssociation ::
  TransitGatewayAssociation
mkTransitGatewayAssociation =
  TransitGatewayAssociation'
    { state = Lude.Nothing,
      resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      transitGatewayRouteTableId = Lude.Nothing,
      transitGatewayAttachmentId = Lude.Nothing
    }

-- | The state of the association.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgafState :: Lens.Lens' TransitGatewayAssociation (Lude.Maybe TransitGatewayAssociationState)
tgafState = Lens.lens (state :: TransitGatewayAssociation -> Lude.Maybe TransitGatewayAssociationState) (\s a -> s {state = a} :: TransitGatewayAssociation)
{-# DEPRECATED tgafState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgafResourceId :: Lens.Lens' TransitGatewayAssociation (Lude.Maybe Lude.Text)
tgafResourceId = Lens.lens (resourceId :: TransitGatewayAssociation -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: TransitGatewayAssociation)
{-# DEPRECATED tgafResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgafResourceType :: Lens.Lens' TransitGatewayAssociation (Lude.Maybe TransitGatewayAttachmentResourceType)
tgafResourceType = Lens.lens (resourceType :: TransitGatewayAssociation -> Lude.Maybe TransitGatewayAttachmentResourceType) (\s a -> s {resourceType = a} :: TransitGatewayAssociation)
{-# DEPRECATED tgafResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgafTransitGatewayRouteTableId :: Lens.Lens' TransitGatewayAssociation (Lude.Maybe Lude.Text)
tgafTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: TransitGatewayAssociation -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: TransitGatewayAssociation)
{-# DEPRECATED tgafTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgafTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayAssociation (Lude.Maybe Lude.Text)
tgafTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: TransitGatewayAssociation -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: TransitGatewayAssociation)
{-# DEPRECATED tgafTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Lude.FromXML TransitGatewayAssociation where
  parseXML x =
    TransitGatewayAssociation'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "resourceId")
      Lude.<*> (x Lude..@? "resourceType")
      Lude.<*> (x Lude..@? "transitGatewayRouteTableId")
      Lude.<*> (x Lude..@? "transitGatewayAttachmentId")
