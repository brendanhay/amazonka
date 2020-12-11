-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteAttachment
  ( TransitGatewayRouteAttachment (..),

    -- * Smart constructor
    mkTransitGatewayRouteAttachment,

    -- * Lenses
    tgraResourceId,
    tgraResourceType,
    tgraTransitGatewayAttachmentId,
  )
where

import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a route attachment.
--
-- /See:/ 'mkTransitGatewayRouteAttachment' smart constructor.
data TransitGatewayRouteAttachment = TransitGatewayRouteAttachment'
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

-- | Creates a value of 'TransitGatewayRouteAttachment' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource.
-- * 'resourceType' - The resource type. Note that the @tgw-peering@ resource type has been deprecated.
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
mkTransitGatewayRouteAttachment ::
  TransitGatewayRouteAttachment
mkTransitGatewayRouteAttachment =
  TransitGatewayRouteAttachment'
    { resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      transitGatewayAttachmentId = Lude.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgraResourceId :: Lens.Lens' TransitGatewayRouteAttachment (Lude.Maybe Lude.Text)
tgraResourceId = Lens.lens (resourceId :: TransitGatewayRouteAttachment -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: TransitGatewayRouteAttachment)
{-# DEPRECATED tgraResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgraResourceType :: Lens.Lens' TransitGatewayRouteAttachment (Lude.Maybe TransitGatewayAttachmentResourceType)
tgraResourceType = Lens.lens (resourceType :: TransitGatewayRouteAttachment -> Lude.Maybe TransitGatewayAttachmentResourceType) (\s a -> s {resourceType = a} :: TransitGatewayRouteAttachment)
{-# DEPRECATED tgraResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgraTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayRouteAttachment (Lude.Maybe Lude.Text)
tgraTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: TransitGatewayRouteAttachment -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: TransitGatewayRouteAttachment)
{-# DEPRECATED tgraTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Lude.FromXML TransitGatewayRouteAttachment where
  parseXML x =
    TransitGatewayRouteAttachment'
      Lude.<$> (x Lude..@? "resourceId")
      Lude.<*> (x Lude..@? "resourceType")
      Lude.<*> (x Lude..@? "transitGatewayAttachmentId")
