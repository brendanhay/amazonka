{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ReplaceTransitGatewayRoute (..),
    mkReplaceTransitGatewayRoute,

    -- ** Request lenses
    rtgrDestinationCidrBlock,
    rtgrTransitGatewayRouteTableId,
    rtgrBlackhole,
    rtgrDryRun,
    rtgrTransitGatewayAttachmentId,

    -- * Destructuring the response
    ReplaceTransitGatewayRouteResponse (..),
    mkReplaceTransitGatewayRouteResponse,

    -- ** Response lenses
    rtgrrrsRoute,
    rtgrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkReplaceTransitGatewayRoute' smart constructor.
data ReplaceTransitGatewayRoute = ReplaceTransitGatewayRoute'
  { -- | The CIDR range used for the destination match. Routing decisions are based on the most specific match.
    destinationCidrBlock :: Types.String,
    -- | The ID of the route table.
    transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId,
    -- | Indicates whether traffic matching this route is to be dropped.
    blackhole :: Core.Maybe Core.Bool,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Core.Maybe Types.TransitGatewayAttachmentId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceTransitGatewayRoute' value with any optional fields omitted.
mkReplaceTransitGatewayRoute ::
  -- | 'destinationCidrBlock'
  Types.String ->
  -- | 'transitGatewayRouteTableId'
  Types.TransitGatewayRouteTableId ->
  ReplaceTransitGatewayRoute
mkReplaceTransitGatewayRoute
  destinationCidrBlock
  transitGatewayRouteTableId =
    ReplaceTransitGatewayRoute'
      { destinationCidrBlock,
        transitGatewayRouteTableId,
        blackhole = Core.Nothing,
        dryRun = Core.Nothing,
        transitGatewayAttachmentId = Core.Nothing
      }

-- | The CIDR range used for the destination match. Routing decisions are based on the most specific match.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrDestinationCidrBlock :: Lens.Lens' ReplaceTransitGatewayRoute Types.String
rtgrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# DEPRECATED rtgrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrTransitGatewayRouteTableId :: Lens.Lens' ReplaceTransitGatewayRoute Types.TransitGatewayRouteTableId
rtgrTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED rtgrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | Indicates whether traffic matching this route is to be dropped.
--
-- /Note:/ Consider using 'blackhole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrBlackhole :: Lens.Lens' ReplaceTransitGatewayRoute (Core.Maybe Core.Bool)
rtgrBlackhole = Lens.field @"blackhole"
{-# DEPRECATED rtgrBlackhole "Use generic-lens or generic-optics with 'blackhole' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrDryRun :: Lens.Lens' ReplaceTransitGatewayRoute (Core.Maybe Core.Bool)
rtgrDryRun = Lens.field @"dryRun"
{-# DEPRECATED rtgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrTransitGatewayAttachmentId :: Lens.Lens' ReplaceTransitGatewayRoute (Core.Maybe Types.TransitGatewayAttachmentId)
rtgrTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED rtgrTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Core.AWSRequest ReplaceTransitGatewayRoute where
  type
    Rs ReplaceTransitGatewayRoute =
      ReplaceTransitGatewayRouteResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ReplaceTransitGatewayRoute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DestinationCidrBlock" destinationCidrBlock)
                Core.<> ( Core.toQueryValue
                            "TransitGatewayRouteTableId"
                            transitGatewayRouteTableId
                        )
                Core.<> (Core.toQueryValue "Blackhole" Core.<$> blackhole)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> ( Core.toQueryValue "TransitGatewayAttachmentId"
                            Core.<$> transitGatewayAttachmentId
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ReplaceTransitGatewayRouteResponse'
            Core.<$> (x Core..@? "route") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkReplaceTransitGatewayRouteResponse' smart constructor.
data ReplaceTransitGatewayRouteResponse = ReplaceTransitGatewayRouteResponse'
  { -- | Information about the modified route.
    route :: Core.Maybe Types.TransitGatewayRoute,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceTransitGatewayRouteResponse' value with any optional fields omitted.
mkReplaceTransitGatewayRouteResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ReplaceTransitGatewayRouteResponse
mkReplaceTransitGatewayRouteResponse responseStatus =
  ReplaceTransitGatewayRouteResponse'
    { route = Core.Nothing,
      responseStatus
    }

-- | Information about the modified route.
--
-- /Note:/ Consider using 'route' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrrrsRoute :: Lens.Lens' ReplaceTransitGatewayRouteResponse (Core.Maybe Types.TransitGatewayRoute)
rtgrrrsRoute = Lens.field @"route"
{-# DEPRECATED rtgrrrsRoute "Use generic-lens or generic-optics with 'route' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgrrrsResponseStatus :: Lens.Lens' ReplaceTransitGatewayRouteResponse Core.Int
rtgrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtgrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
