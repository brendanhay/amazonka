{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReplaceTransitGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the specified route in the specified transit gateway route table.
module Network.AWS.EC2.ReplaceTransitGatewayRoute
    (
    -- * Creating a request
      ReplaceTransitGatewayRoute (..)
    , mkReplaceTransitGatewayRoute
    -- ** Request lenses
    , rtgrDestinationCidrBlock
    , rtgrTransitGatewayRouteTableId
    , rtgrBlackhole
    , rtgrDryRun
    , rtgrTransitGatewayAttachmentId

    -- * Destructuring the response
    , ReplaceTransitGatewayRouteResponse (..)
    , mkReplaceTransitGatewayRouteResponse
    -- ** Response lenses
    , rtgrrrsRoute
    , rtgrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkReplaceTransitGatewayRoute' smart constructor.
data ReplaceTransitGatewayRoute = ReplaceTransitGatewayRoute'
  { destinationCidrBlock :: Core.Text
    -- ^ The CIDR range used for the destination match. Routing decisions are based on the most specific match.
  , transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId
    -- ^ The ID of the route table.
  , blackhole :: Core.Maybe Core.Bool
    -- ^ Indicates whether traffic matching this route is to be dropped.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , transitGatewayAttachmentId :: Core.Maybe Types.TransitGatewayAttachmentId
    -- ^ The ID of the attachment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceTransitGatewayRoute' value with any optional fields omitted.
mkReplaceTransitGatewayRoute
    :: Core.Text -- ^ 'destinationCidrBlock'
    -> Types.TransitGatewayRouteTableId -- ^ 'transitGatewayRouteTableId'
    -> ReplaceTransitGatewayRoute
mkReplaceTransitGatewayRoute destinationCidrBlock
  transitGatewayRouteTableId
  = ReplaceTransitGatewayRoute'{destinationCidrBlock,
                                transitGatewayRouteTableId, blackhole = Core.Nothing,
                                dryRun = Core.Nothing, transitGatewayAttachmentId = Core.Nothing}

-- | The CIDR range used for the destination match. Routing decisions are based on the most specific match.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrDestinationCidrBlock :: Lens.Lens' ReplaceTransitGatewayRoute Core.Text
rtgrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE rtgrDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrTransitGatewayRouteTableId :: Lens.Lens' ReplaceTransitGatewayRoute Types.TransitGatewayRouteTableId
rtgrTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# INLINEABLE rtgrTransitGatewayRouteTableId #-}
{-# DEPRECATED transitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead"  #-}

-- | Indicates whether traffic matching this route is to be dropped.
--
-- /Note:/ Consider using 'blackhole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrBlackhole :: Lens.Lens' ReplaceTransitGatewayRoute (Core.Maybe Core.Bool)
rtgrBlackhole = Lens.field @"blackhole"
{-# INLINEABLE rtgrBlackhole #-}
{-# DEPRECATED blackhole "Use generic-lens or generic-optics with 'blackhole' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrDryRun :: Lens.Lens' ReplaceTransitGatewayRoute (Core.Maybe Core.Bool)
rtgrDryRun = Lens.field @"dryRun"
{-# INLINEABLE rtgrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrTransitGatewayAttachmentId :: Lens.Lens' ReplaceTransitGatewayRoute (Core.Maybe Types.TransitGatewayAttachmentId)
rtgrTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE rtgrTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

instance Core.ToQuery ReplaceTransitGatewayRoute where
        toQuery ReplaceTransitGatewayRoute{..}
          = Core.toQueryPair "Action"
              ("ReplaceTransitGatewayRoute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "DestinationCidrBlock" destinationCidrBlock
              Core.<>
              Core.toQueryPair "TransitGatewayRouteTableId"
                transitGatewayRouteTableId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Blackhole") blackhole
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "TransitGatewayAttachmentId")
                transitGatewayAttachmentId

instance Core.ToHeaders ReplaceTransitGatewayRoute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ReplaceTransitGatewayRoute where
        type Rs ReplaceTransitGatewayRoute =
             ReplaceTransitGatewayRouteResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ReplaceTransitGatewayRouteResponse' Core.<$>
                   (x Core..@? "route") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkReplaceTransitGatewayRouteResponse' smart constructor.
data ReplaceTransitGatewayRouteResponse = ReplaceTransitGatewayRouteResponse'
  { route :: Core.Maybe Types.TransitGatewayRoute
    -- ^ Information about the modified route.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceTransitGatewayRouteResponse' value with any optional fields omitted.
mkReplaceTransitGatewayRouteResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ReplaceTransitGatewayRouteResponse
mkReplaceTransitGatewayRouteResponse responseStatus
  = ReplaceTransitGatewayRouteResponse'{route = Core.Nothing,
                                        responseStatus}

-- | Information about the modified route.
--
-- /Note:/ Consider using 'route' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrrrsRoute :: Lens.Lens' ReplaceTransitGatewayRouteResponse (Core.Maybe Types.TransitGatewayRoute)
rtgrrrsRoute = Lens.field @"route"
{-# INLINEABLE rtgrrrsRoute #-}
{-# DEPRECATED route "Use generic-lens or generic-optics with 'route' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrrrsResponseStatus :: Lens.Lens' ReplaceTransitGatewayRouteResponse Core.Int
rtgrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtgrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
