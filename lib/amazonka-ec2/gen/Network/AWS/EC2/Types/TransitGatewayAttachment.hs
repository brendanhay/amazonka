-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachment
  ( TransitGatewayAttachment (..),

    -- * Smart constructor
    mkTransitGatewayAttachment,

    -- * Lenses
    tgaCreationTime,
    tgaState,
    tgaResourceId,
    tgaResourceType,
    tgaTransitGatewayOwnerId,
    tgaTransitGatewayId,
    tgaTransitGatewayAttachmentId,
    tgaResourceOwnerId,
    tgaTags,
    tgaAssociation,
  )
where

import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.EC2.Types.TransitGatewayAttachmentState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an attachment between a resource and a transit gateway.
--
-- /See:/ 'mkTransitGatewayAttachment' smart constructor.
data TransitGatewayAttachment = TransitGatewayAttachment'
  { creationTime ::
      Lude.Maybe Lude.ISO8601,
    state ::
      Lude.Maybe TransitGatewayAttachmentState,
    resourceId :: Lude.Maybe Lude.Text,
    resourceType ::
      Lude.Maybe
        TransitGatewayAttachmentResourceType,
    transitGatewayOwnerId ::
      Lude.Maybe Lude.Text,
    transitGatewayId :: Lude.Maybe Lude.Text,
    transitGatewayAttachmentId ::
      Lude.Maybe Lude.Text,
    resourceOwnerId :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    association ::
      Lude.Maybe
        TransitGatewayAttachmentAssociation
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayAttachment' with the minimum fields required to make a request.
--
-- * 'association' - The association.
-- * 'creationTime' - The creation time.
-- * 'resourceId' - The ID of the resource.
-- * 'resourceOwnerId' - The ID of the AWS account that owns the resource.
-- * 'resourceType' - The resource type. Note that the @tgw-peering@ resource type has been deprecated.
-- * 'state' - The attachment state. Note that the @initiating@ state has been deprecated.
-- * 'tags' - The tags for the attachment.
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
-- * 'transitGatewayId' - The ID of the transit gateway.
-- * 'transitGatewayOwnerId' - The ID of the AWS account that owns the transit gateway.
mkTransitGatewayAttachment ::
  TransitGatewayAttachment
mkTransitGatewayAttachment =
  TransitGatewayAttachment'
    { creationTime = Lude.Nothing,
      state = Lude.Nothing,
      resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      transitGatewayOwnerId = Lude.Nothing,
      transitGatewayId = Lude.Nothing,
      transitGatewayAttachmentId = Lude.Nothing,
      resourceOwnerId = Lude.Nothing,
      tags = Lude.Nothing,
      association = Lude.Nothing
    }

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaCreationTime :: Lens.Lens' TransitGatewayAttachment (Lude.Maybe Lude.ISO8601)
tgaCreationTime = Lens.lens (creationTime :: TransitGatewayAttachment -> Lude.Maybe Lude.ISO8601) (\s a -> s {creationTime = a} :: TransitGatewayAttachment)
{-# DEPRECATED tgaCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The attachment state. Note that the @initiating@ state has been deprecated.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaState :: Lens.Lens' TransitGatewayAttachment (Lude.Maybe TransitGatewayAttachmentState)
tgaState = Lens.lens (state :: TransitGatewayAttachment -> Lude.Maybe TransitGatewayAttachmentState) (\s a -> s {state = a} :: TransitGatewayAttachment)
{-# DEPRECATED tgaState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaResourceId :: Lens.Lens' TransitGatewayAttachment (Lude.Maybe Lude.Text)
tgaResourceId = Lens.lens (resourceId :: TransitGatewayAttachment -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: TransitGatewayAttachment)
{-# DEPRECATED tgaResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaResourceType :: Lens.Lens' TransitGatewayAttachment (Lude.Maybe TransitGatewayAttachmentResourceType)
tgaResourceType = Lens.lens (resourceType :: TransitGatewayAttachment -> Lude.Maybe TransitGatewayAttachmentResourceType) (\s a -> s {resourceType = a} :: TransitGatewayAttachment)
{-# DEPRECATED tgaResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the AWS account that owns the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaTransitGatewayOwnerId :: Lens.Lens' TransitGatewayAttachment (Lude.Maybe Lude.Text)
tgaTransitGatewayOwnerId = Lens.lens (transitGatewayOwnerId :: TransitGatewayAttachment -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayOwnerId = a} :: TransitGatewayAttachment)
{-# DEPRECATED tgaTransitGatewayOwnerId "Use generic-lens or generic-optics with 'transitGatewayOwnerId' instead." #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaTransitGatewayId :: Lens.Lens' TransitGatewayAttachment (Lude.Maybe Lude.Text)
tgaTransitGatewayId = Lens.lens (transitGatewayId :: TransitGatewayAttachment -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayId = a} :: TransitGatewayAttachment)
{-# DEPRECATED tgaTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayAttachment (Lude.Maybe Lude.Text)
tgaTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: TransitGatewayAttachment -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: TransitGatewayAttachment)
{-# DEPRECATED tgaTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | The ID of the AWS account that owns the resource.
--
-- /Note:/ Consider using 'resourceOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaResourceOwnerId :: Lens.Lens' TransitGatewayAttachment (Lude.Maybe Lude.Text)
tgaResourceOwnerId = Lens.lens (resourceOwnerId :: TransitGatewayAttachment -> Lude.Maybe Lude.Text) (\s a -> s {resourceOwnerId = a} :: TransitGatewayAttachment)
{-# DEPRECATED tgaResourceOwnerId "Use generic-lens or generic-optics with 'resourceOwnerId' instead." #-}

-- | The tags for the attachment.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaTags :: Lens.Lens' TransitGatewayAttachment (Lude.Maybe [Tag])
tgaTags = Lens.lens (tags :: TransitGatewayAttachment -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: TransitGatewayAttachment)
{-# DEPRECATED tgaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The association.
--
-- /Note:/ Consider using 'association' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaAssociation :: Lens.Lens' TransitGatewayAttachment (Lude.Maybe TransitGatewayAttachmentAssociation)
tgaAssociation = Lens.lens (association :: TransitGatewayAttachment -> Lude.Maybe TransitGatewayAttachmentAssociation) (\s a -> s {association = a} :: TransitGatewayAttachment)
{-# DEPRECATED tgaAssociation "Use generic-lens or generic-optics with 'association' instead." #-}

instance Lude.FromXML TransitGatewayAttachment where
  parseXML x =
    TransitGatewayAttachment'
      Lude.<$> (x Lude..@? "creationTime")
      Lude.<*> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "resourceId")
      Lude.<*> (x Lude..@? "resourceType")
      Lude.<*> (x Lude..@? "transitGatewayOwnerId")
      Lude.<*> (x Lude..@? "transitGatewayId")
      Lude.<*> (x Lude..@? "transitGatewayAttachmentId")
      Lude.<*> (x Lude..@? "resourceOwnerId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "association")
