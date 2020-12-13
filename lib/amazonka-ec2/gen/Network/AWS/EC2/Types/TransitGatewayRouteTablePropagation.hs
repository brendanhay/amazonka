{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteTablePropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteTablePropagation
  ( TransitGatewayRouteTablePropagation (..),

    -- * Smart constructor
    mkTransitGatewayRouteTablePropagation,

    -- * Lenses
    tgrtpState,
    tgrtpResourceId,
    tgrtpResourceType,
    tgrtpTransitGatewayAttachmentId,
  )
where

import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.EC2.Types.TransitGatewayPropagationState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a route table propagation.
--
-- /See:/ 'mkTransitGatewayRouteTablePropagation' smart constructor.
data TransitGatewayRouteTablePropagation = TransitGatewayRouteTablePropagation'
  { -- | The state of the resource.
    state :: Lude.Maybe TransitGatewayPropagationState,
    -- | The ID of the resource.
    resourceId :: Lude.Maybe Lude.Text,
    -- | The type of resource. Note that the @tgw-peering@ resource type has been deprecated.
    resourceType :: Lude.Maybe TransitGatewayAttachmentResourceType,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayRouteTablePropagation' with the minimum fields required to make a request.
--
-- * 'state' - The state of the resource.
-- * 'resourceId' - The ID of the resource.
-- * 'resourceType' - The type of resource. Note that the @tgw-peering@ resource type has been deprecated.
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
mkTransitGatewayRouteTablePropagation ::
  TransitGatewayRouteTablePropagation
mkTransitGatewayRouteTablePropagation =
  TransitGatewayRouteTablePropagation'
    { state = Lude.Nothing,
      resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      transitGatewayAttachmentId = Lude.Nothing
    }

-- | The state of the resource.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtpState :: Lens.Lens' TransitGatewayRouteTablePropagation (Lude.Maybe TransitGatewayPropagationState)
tgrtpState = Lens.lens (state :: TransitGatewayRouteTablePropagation -> Lude.Maybe TransitGatewayPropagationState) (\s a -> s {state = a} :: TransitGatewayRouteTablePropagation)
{-# DEPRECATED tgrtpState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtpResourceId :: Lens.Lens' TransitGatewayRouteTablePropagation (Lude.Maybe Lude.Text)
tgrtpResourceId = Lens.lens (resourceId :: TransitGatewayRouteTablePropagation -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: TransitGatewayRouteTablePropagation)
{-# DEPRECATED tgrtpResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of resource. Note that the @tgw-peering@ resource type has been deprecated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtpResourceType :: Lens.Lens' TransitGatewayRouteTablePropagation (Lude.Maybe TransitGatewayAttachmentResourceType)
tgrtpResourceType = Lens.lens (resourceType :: TransitGatewayRouteTablePropagation -> Lude.Maybe TransitGatewayAttachmentResourceType) (\s a -> s {resourceType = a} :: TransitGatewayRouteTablePropagation)
{-# DEPRECATED tgrtpResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtpTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayRouteTablePropagation (Lude.Maybe Lude.Text)
tgrtpTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: TransitGatewayRouteTablePropagation -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: TransitGatewayRouteTablePropagation)
{-# DEPRECATED tgrtpTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Lude.FromXML TransitGatewayRouteTablePropagation where
  parseXML x =
    TransitGatewayRouteTablePropagation'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "resourceId")
      Lude.<*> (x Lude..@? "resourceType")
      Lude.<*> (x Lude..@? "transitGatewayAttachmentId")
