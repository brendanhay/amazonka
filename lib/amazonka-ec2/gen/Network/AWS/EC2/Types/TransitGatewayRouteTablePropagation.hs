{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteTablePropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayRouteTablePropagation
  ( TransitGatewayRouteTablePropagation (..)
  -- * Smart constructor
  , mkTransitGatewayRouteTablePropagation
  -- * Lenses
  , tgrtpResourceId
  , tgrtpResourceType
  , tgrtpState
  , tgrtpTransitGatewayAttachmentId
  ) where

import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType as Types
import qualified Network.AWS.EC2.Types.TransitGatewayPropagationState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a route table propagation.
--
-- /See:/ 'mkTransitGatewayRouteTablePropagation' smart constructor.
data TransitGatewayRouteTablePropagation = TransitGatewayRouteTablePropagation'
  { resourceId :: Core.Maybe Core.Text
    -- ^ The ID of the resource.
  , resourceType :: Core.Maybe Types.TransitGatewayAttachmentResourceType
    -- ^ The type of resource. Note that the @tgw-peering@ resource type has been deprecated.
  , state :: Core.Maybe Types.TransitGatewayPropagationState
    -- ^ The state of the resource.
  , transitGatewayAttachmentId :: Core.Maybe Core.Text
    -- ^ The ID of the attachment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayRouteTablePropagation' value with any optional fields omitted.
mkTransitGatewayRouteTablePropagation
    :: TransitGatewayRouteTablePropagation
mkTransitGatewayRouteTablePropagation
  = TransitGatewayRouteTablePropagation'{resourceId = Core.Nothing,
                                         resourceType = Core.Nothing, state = Core.Nothing,
                                         transitGatewayAttachmentId = Core.Nothing}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtpResourceId :: Lens.Lens' TransitGatewayRouteTablePropagation (Core.Maybe Core.Text)
tgrtpResourceId = Lens.field @"resourceId"
{-# INLINEABLE tgrtpResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The type of resource. Note that the @tgw-peering@ resource type has been deprecated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtpResourceType :: Lens.Lens' TransitGatewayRouteTablePropagation (Core.Maybe Types.TransitGatewayAttachmentResourceType)
tgrtpResourceType = Lens.field @"resourceType"
{-# INLINEABLE tgrtpResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The state of the resource.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtpState :: Lens.Lens' TransitGatewayRouteTablePropagation (Core.Maybe Types.TransitGatewayPropagationState)
tgrtpState = Lens.field @"state"
{-# INLINEABLE tgrtpState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtpTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayRouteTablePropagation (Core.Maybe Core.Text)
tgrtpTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE tgrtpTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

instance Core.FromXML TransitGatewayRouteTablePropagation where
        parseXML x
          = TransitGatewayRouteTablePropagation' Core.<$>
              (x Core..@? "resourceId") Core.<*> x Core..@? "resourceType"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "transitGatewayAttachmentId"
