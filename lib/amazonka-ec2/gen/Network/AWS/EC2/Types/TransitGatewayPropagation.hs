{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayPropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPropagation
  ( TransitGatewayPropagation (..),

    -- * Smart constructor
    mkTransitGatewayPropagation,

    -- * Lenses
    tgpState,
    tgpResourceId,
    tgpResourceType,
    tgpTransitGatewayRouteTableId,
    tgpTransitGatewayAttachmentId,
  )
where

import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.EC2.Types.TransitGatewayPropagationState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes route propagation.
--
-- /See:/ 'mkTransitGatewayPropagation' smart constructor.
data TransitGatewayPropagation = TransitGatewayPropagation'
  { state ::
      Lude.Maybe
        TransitGatewayPropagationState,
    resourceId :: Lude.Maybe Lude.Text,
    resourceType ::
      Lude.Maybe
        TransitGatewayAttachmentResourceType,
    transitGatewayRouteTableId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'TransitGatewayPropagation' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource.
-- * 'resourceType' - The resource type. Note that the @tgw-peering@ resource type has been deprecated.
-- * 'state' - The state.
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
-- * 'transitGatewayRouteTableId' - The ID of the transit gateway route table.
mkTransitGatewayPropagation ::
  TransitGatewayPropagation
mkTransitGatewayPropagation =
  TransitGatewayPropagation'
    { state = Lude.Nothing,
      resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      transitGatewayRouteTableId = Lude.Nothing,
      transitGatewayAttachmentId = Lude.Nothing
    }

-- | The state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpState :: Lens.Lens' TransitGatewayPropagation (Lude.Maybe TransitGatewayPropagationState)
tgpState = Lens.lens (state :: TransitGatewayPropagation -> Lude.Maybe TransitGatewayPropagationState) (\s a -> s {state = a} :: TransitGatewayPropagation)
{-# DEPRECATED tgpState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpResourceId :: Lens.Lens' TransitGatewayPropagation (Lude.Maybe Lude.Text)
tgpResourceId = Lens.lens (resourceId :: TransitGatewayPropagation -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: TransitGatewayPropagation)
{-# DEPRECATED tgpResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpResourceType :: Lens.Lens' TransitGatewayPropagation (Lude.Maybe TransitGatewayAttachmentResourceType)
tgpResourceType = Lens.lens (resourceType :: TransitGatewayPropagation -> Lude.Maybe TransitGatewayAttachmentResourceType) (\s a -> s {resourceType = a} :: TransitGatewayPropagation)
{-# DEPRECATED tgpResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpTransitGatewayRouteTableId :: Lens.Lens' TransitGatewayPropagation (Lude.Maybe Lude.Text)
tgpTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: TransitGatewayPropagation -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: TransitGatewayPropagation)
{-# DEPRECATED tgpTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayPropagation (Lude.Maybe Lude.Text)
tgpTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: TransitGatewayPropagation -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: TransitGatewayPropagation)
{-# DEPRECATED tgpTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Lude.FromXML TransitGatewayPropagation where
  parseXML x =
    TransitGatewayPropagation'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "resourceId")
      Lude.<*> (x Lude..@? "resourceType")
      Lude.<*> (x Lude..@? "transitGatewayRouteTableId")
      Lude.<*> (x Lude..@? "transitGatewayAttachmentId")
