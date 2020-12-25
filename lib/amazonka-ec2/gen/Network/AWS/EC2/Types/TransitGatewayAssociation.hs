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
    tResourceId,
    tResourceType,
    tState,
    tTransitGatewayAttachmentId,
    tTransitGatewayRouteTableId,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAssociationState as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentId as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType as Types
import qualified Network.AWS.EC2.Types.TransitGatewayRouteTableId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an association between a resource attachment and a transit gateway route table.
--
-- /See:/ 'mkTransitGatewayAssociation' smart constructor.
data TransitGatewayAssociation = TransitGatewayAssociation'
  { -- | The ID of the resource.
    resourceId :: Core.Maybe Types.String,
    -- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
    resourceType :: Core.Maybe Types.TransitGatewayAttachmentResourceType,
    -- | The state of the association.
    state :: Core.Maybe Types.TransitGatewayAssociationState,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Core.Maybe Types.TransitGatewayAttachmentId,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Core.Maybe Types.TransitGatewayRouteTableId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayAssociation' value with any optional fields omitted.
mkTransitGatewayAssociation ::
  TransitGatewayAssociation
mkTransitGatewayAssociation =
  TransitGatewayAssociation'
    { resourceId = Core.Nothing,
      resourceType = Core.Nothing,
      state = Core.Nothing,
      transitGatewayAttachmentId = Core.Nothing,
      transitGatewayRouteTableId = Core.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tResourceId :: Lens.Lens' TransitGatewayAssociation (Core.Maybe Types.String)
tResourceId = Lens.field @"resourceId"
{-# DEPRECATED tResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tResourceType :: Lens.Lens' TransitGatewayAssociation (Core.Maybe Types.TransitGatewayAttachmentResourceType)
tResourceType = Lens.field @"resourceType"
{-# DEPRECATED tResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The state of the association.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tState :: Lens.Lens' TransitGatewayAssociation (Core.Maybe Types.TransitGatewayAssociationState)
tState = Lens.field @"state"
{-# DEPRECATED tState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayAssociation (Core.Maybe Types.TransitGatewayAttachmentId)
tTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED tTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTransitGatewayRouteTableId :: Lens.Lens' TransitGatewayAssociation (Core.Maybe Types.TransitGatewayRouteTableId)
tTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED tTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

instance Core.FromXML TransitGatewayAssociation where
  parseXML x =
    TransitGatewayAssociation'
      Core.<$> (x Core..@? "resourceId")
      Core.<*> (x Core..@? "resourceType")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "transitGatewayAttachmentId")
      Core.<*> (x Core..@? "transitGatewayRouteTableId")
