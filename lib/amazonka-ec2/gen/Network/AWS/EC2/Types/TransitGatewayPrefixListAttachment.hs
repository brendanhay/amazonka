{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayPrefixListAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPrefixListAttachment
  ( TransitGatewayPrefixListAttachment (..),

    -- * Smart constructor
    mkTransitGatewayPrefixListAttachment,

    -- * Lenses
    tgplaResourceId,
    tgplaResourceType,
    tgplaTransitGatewayAttachmentId,
  )
where

import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a transit gateway prefix list attachment.
--
-- /See:/ 'mkTransitGatewayPrefixListAttachment' smart constructor.
data TransitGatewayPrefixListAttachment = TransitGatewayPrefixListAttachment'
  { resourceId ::
      Lude.Maybe Lude.Text,
    resourceType ::
      Lude.Maybe
        TransitGatewayAttachmentResourceType,
    transitGatewayAttachmentId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayPrefixListAttachment' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource.
-- * 'resourceType' - The resource type. Note that the @tgw-peering@ resource type has been deprecated.
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
mkTransitGatewayPrefixListAttachment ::
  TransitGatewayPrefixListAttachment
mkTransitGatewayPrefixListAttachment =
  TransitGatewayPrefixListAttachment'
    { resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      transitGatewayAttachmentId = Lude.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplaResourceId :: Lens.Lens' TransitGatewayPrefixListAttachment (Lude.Maybe Lude.Text)
tgplaResourceId = Lens.lens (resourceId :: TransitGatewayPrefixListAttachment -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: TransitGatewayPrefixListAttachment)
{-# DEPRECATED tgplaResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplaResourceType :: Lens.Lens' TransitGatewayPrefixListAttachment (Lude.Maybe TransitGatewayAttachmentResourceType)
tgplaResourceType = Lens.lens (resourceType :: TransitGatewayPrefixListAttachment -> Lude.Maybe TransitGatewayAttachmentResourceType) (\s a -> s {resourceType = a} :: TransitGatewayPrefixListAttachment)
{-# DEPRECATED tgplaResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplaTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayPrefixListAttachment (Lude.Maybe Lude.Text)
tgplaTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: TransitGatewayPrefixListAttachment -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: TransitGatewayPrefixListAttachment)
{-# DEPRECATED tgplaTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Lude.FromXML TransitGatewayPrefixListAttachment where
  parseXML x =
    TransitGatewayPrefixListAttachment'
      Lude.<$> (x Lude..@? "resourceId")
      Lude.<*> (x Lude..@? "resourceType")
      Lude.<*> (x Lude..@? "transitGatewayAttachmentId")
