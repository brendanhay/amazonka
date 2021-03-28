{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayPropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayPropagation
  ( TransitGatewayPropagation (..)
  -- * Smart constructor
  , mkTransitGatewayPropagation
  -- * Lenses
  , tgpResourceId
  , tgpResourceType
  , tgpState
  , tgpTransitGatewayAttachmentId
  , tgpTransitGatewayRouteTableId
  ) where

import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentId as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType as Types
import qualified Network.AWS.EC2.Types.TransitGatewayPropagationState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes route propagation.
--
-- /See:/ 'mkTransitGatewayPropagation' smart constructor.
data TransitGatewayPropagation = TransitGatewayPropagation'
  { resourceId :: Core.Maybe Core.Text
    -- ^ The ID of the resource.
  , resourceType :: Core.Maybe Types.TransitGatewayAttachmentResourceType
    -- ^ The resource type. Note that the @tgw-peering@ resource type has been deprecated.
  , state :: Core.Maybe Types.TransitGatewayPropagationState
    -- ^ The state.
  , transitGatewayAttachmentId :: Core.Maybe Types.TransitGatewayAttachmentId
    -- ^ The ID of the attachment.
  , transitGatewayRouteTableId :: Core.Maybe Core.Text
    -- ^ The ID of the transit gateway route table.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayPropagation' value with any optional fields omitted.
mkTransitGatewayPropagation
    :: TransitGatewayPropagation
mkTransitGatewayPropagation
  = TransitGatewayPropagation'{resourceId = Core.Nothing,
                               resourceType = Core.Nothing, state = Core.Nothing,
                               transitGatewayAttachmentId = Core.Nothing,
                               transitGatewayRouteTableId = Core.Nothing}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpResourceId :: Lens.Lens' TransitGatewayPropagation (Core.Maybe Core.Text)
tgpResourceId = Lens.field @"resourceId"
{-# INLINEABLE tgpResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpResourceType :: Lens.Lens' TransitGatewayPropagation (Core.Maybe Types.TransitGatewayAttachmentResourceType)
tgpResourceType = Lens.field @"resourceType"
{-# INLINEABLE tgpResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpState :: Lens.Lens' TransitGatewayPropagation (Core.Maybe Types.TransitGatewayPropagationState)
tgpState = Lens.field @"state"
{-# INLINEABLE tgpState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayPropagation (Core.Maybe Types.TransitGatewayAttachmentId)
tgpTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE tgpTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpTransitGatewayRouteTableId :: Lens.Lens' TransitGatewayPropagation (Core.Maybe Core.Text)
tgpTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# INLINEABLE tgpTransitGatewayRouteTableId #-}
{-# DEPRECATED transitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead"  #-}

instance Core.FromXML TransitGatewayPropagation where
        parseXML x
          = TransitGatewayPropagation' Core.<$>
              (x Core..@? "resourceId") Core.<*> x Core..@? "resourceType"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "transitGatewayAttachmentId"
                Core.<*> x Core..@? "transitGatewayRouteTableId"
