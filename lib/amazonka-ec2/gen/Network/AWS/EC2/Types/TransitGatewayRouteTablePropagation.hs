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
    tgrtpResourceId,
    tgrtpResourceType,
    tgrtpState,
    tgrtpTransitGatewayAttachmentId,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType as Types
import qualified Network.AWS.EC2.Types.TransitGatewayPropagationState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a route table propagation.
--
-- /See:/ 'mkTransitGatewayRouteTablePropagation' smart constructor.
data TransitGatewayRouteTablePropagation = TransitGatewayRouteTablePropagation'
  { -- | The ID of the resource.
    resourceId :: Core.Maybe Types.String,
    -- | The type of resource. Note that the @tgw-peering@ resource type has been deprecated.
    resourceType :: Core.Maybe Types.TransitGatewayAttachmentResourceType,
    -- | The state of the resource.
    state :: Core.Maybe Types.TransitGatewayPropagationState,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayRouteTablePropagation' value with any optional fields omitted.
mkTransitGatewayRouteTablePropagation ::
  TransitGatewayRouteTablePropagation
mkTransitGatewayRouteTablePropagation =
  TransitGatewayRouteTablePropagation'
    { resourceId = Core.Nothing,
      resourceType = Core.Nothing,
      state = Core.Nothing,
      transitGatewayAttachmentId = Core.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtpResourceId :: Lens.Lens' TransitGatewayRouteTablePropagation (Core.Maybe Types.String)
tgrtpResourceId = Lens.field @"resourceId"
{-# DEPRECATED tgrtpResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of resource. Note that the @tgw-peering@ resource type has been deprecated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtpResourceType :: Lens.Lens' TransitGatewayRouteTablePropagation (Core.Maybe Types.TransitGatewayAttachmentResourceType)
tgrtpResourceType = Lens.field @"resourceType"
{-# DEPRECATED tgrtpResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The state of the resource.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtpState :: Lens.Lens' TransitGatewayRouteTablePropagation (Core.Maybe Types.TransitGatewayPropagationState)
tgrtpState = Lens.field @"state"
{-# DEPRECATED tgrtpState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtpTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayRouteTablePropagation (Core.Maybe Types.String)
tgrtpTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED tgrtpTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Core.FromXML TransitGatewayRouteTablePropagation where
  parseXML x =
    TransitGatewayRouteTablePropagation'
      Core.<$> (x Core..@? "resourceId")
      Core.<*> (x Core..@? "resourceType")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "transitGatewayAttachmentId")
