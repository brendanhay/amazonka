{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteTableAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayRouteTableAssociation
  ( TransitGatewayRouteTableAssociation (..)
  -- * Smart constructor
  , mkTransitGatewayRouteTableAssociation
  -- * Lenses
  , tgrtaResourceId
  , tgrtaResourceType
  , tgrtaState
  , tgrtaTransitGatewayAttachmentId
  ) where

import qualified Network.AWS.EC2.Types.TransitGatewayAssociationState as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an association between a route table and a resource attachment.
--
-- /See:/ 'mkTransitGatewayRouteTableAssociation' smart constructor.
data TransitGatewayRouteTableAssociation = TransitGatewayRouteTableAssociation'
  { resourceId :: Core.Maybe Core.Text
    -- ^ The ID of the resource.
  , resourceType :: Core.Maybe Types.TransitGatewayAttachmentResourceType
    -- ^ The resource type. Note that the @tgw-peering@ resource type has been deprecated.
  , state :: Core.Maybe Types.TransitGatewayAssociationState
    -- ^ The state of the association.
  , transitGatewayAttachmentId :: Core.Maybe Core.Text
    -- ^ The ID of the attachment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayRouteTableAssociation' value with any optional fields omitted.
mkTransitGatewayRouteTableAssociation
    :: TransitGatewayRouteTableAssociation
mkTransitGatewayRouteTableAssociation
  = TransitGatewayRouteTableAssociation'{resourceId = Core.Nothing,
                                         resourceType = Core.Nothing, state = Core.Nothing,
                                         transitGatewayAttachmentId = Core.Nothing}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtaResourceId :: Lens.Lens' TransitGatewayRouteTableAssociation (Core.Maybe Core.Text)
tgrtaResourceId = Lens.field @"resourceId"
{-# INLINEABLE tgrtaResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtaResourceType :: Lens.Lens' TransitGatewayRouteTableAssociation (Core.Maybe Types.TransitGatewayAttachmentResourceType)
tgrtaResourceType = Lens.field @"resourceType"
{-# INLINEABLE tgrtaResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The state of the association.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtaState :: Lens.Lens' TransitGatewayRouteTableAssociation (Core.Maybe Types.TransitGatewayAssociationState)
tgrtaState = Lens.field @"state"
{-# INLINEABLE tgrtaState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtaTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayRouteTableAssociation (Core.Maybe Core.Text)
tgrtaTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE tgrtaTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

instance Core.FromXML TransitGatewayRouteTableAssociation where
        parseXML x
          = TransitGatewayRouteTableAssociation' Core.<$>
              (x Core..@? "resourceId") Core.<*> x Core..@? "resourceType"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "transitGatewayAttachmentId"
