{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTransitGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a static route for the specified transit gateway route table.
module Network.AWS.EC2.CreateTransitGatewayRoute
  ( -- * Creating a request
    CreateTransitGatewayRoute (..),
    mkCreateTransitGatewayRoute,

    -- ** Request lenses
    ctgrDestinationCidrBlock,
    ctgrTransitGatewayRouteTableId,
    ctgrBlackhole,
    ctgrDryRun,
    ctgrTransitGatewayAttachmentId,

    -- * Destructuring the response
    CreateTransitGatewayRouteResponse (..),
    mkCreateTransitGatewayRouteResponse,

    -- ** Response lenses
    ctgrrrsRoute,
    ctgrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTransitGatewayRoute' smart constructor.
data CreateTransitGatewayRoute = CreateTransitGatewayRoute'
  { -- | The CIDR range used for destination matches. Routing decisions are based on the most specific match.
    destinationCidrBlock :: Types.String,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId,
    -- | Indicates whether to drop traffic that matches this route.
    blackhole :: Core.Maybe Core.Bool,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Core.Maybe Types.TransitGatewayAttachmentId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTransitGatewayRoute' value with any optional fields omitted.
mkCreateTransitGatewayRoute ::
  -- | 'destinationCidrBlock'
  Types.String ->
  -- | 'transitGatewayRouteTableId'
  Types.TransitGatewayRouteTableId ->
  CreateTransitGatewayRoute
mkCreateTransitGatewayRoute
  destinationCidrBlock
  transitGatewayRouteTableId =
    CreateTransitGatewayRoute'
      { destinationCidrBlock,
        transitGatewayRouteTableId,
        blackhole = Core.Nothing,
        dryRun = Core.Nothing,
        transitGatewayAttachmentId = Core.Nothing
      }

-- | The CIDR range used for destination matches. Routing decisions are based on the most specific match.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrDestinationCidrBlock :: Lens.Lens' CreateTransitGatewayRoute Types.String
ctgrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# DEPRECATED ctgrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrTransitGatewayRouteTableId :: Lens.Lens' CreateTransitGatewayRoute Types.TransitGatewayRouteTableId
ctgrTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED ctgrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | Indicates whether to drop traffic that matches this route.
--
-- /Note:/ Consider using 'blackhole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrBlackhole :: Lens.Lens' CreateTransitGatewayRoute (Core.Maybe Core.Bool)
ctgrBlackhole = Lens.field @"blackhole"
{-# DEPRECATED ctgrBlackhole "Use generic-lens or generic-optics with 'blackhole' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrDryRun :: Lens.Lens' CreateTransitGatewayRoute (Core.Maybe Core.Bool)
ctgrDryRun = Lens.field @"dryRun"
{-# DEPRECATED ctgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrTransitGatewayAttachmentId :: Lens.Lens' CreateTransitGatewayRoute (Core.Maybe Types.TransitGatewayAttachmentId)
ctgrTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED ctgrTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Core.AWSRequest CreateTransitGatewayRoute where
  type
    Rs CreateTransitGatewayRoute =
      CreateTransitGatewayRouteResponse
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
            ( Core.pure ("Action", "CreateTransitGatewayRoute")
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
          CreateTransitGatewayRouteResponse'
            Core.<$> (x Core..@? "route") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTransitGatewayRouteResponse' smart constructor.
data CreateTransitGatewayRouteResponse = CreateTransitGatewayRouteResponse'
  { -- | Information about the route.
    route :: Core.Maybe Types.TransitGatewayRoute,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTransitGatewayRouteResponse' value with any optional fields omitted.
mkCreateTransitGatewayRouteResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTransitGatewayRouteResponse
mkCreateTransitGatewayRouteResponse responseStatus =
  CreateTransitGatewayRouteResponse'
    { route = Core.Nothing,
      responseStatus
    }

-- | Information about the route.
--
-- /Note:/ Consider using 'route' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrrsRoute :: Lens.Lens' CreateTransitGatewayRouteResponse (Core.Maybe Types.TransitGatewayRoute)
ctgrrrsRoute = Lens.field @"route"
{-# DEPRECATED ctgrrrsRoute "Use generic-lens or generic-optics with 'route' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrrsResponseStatus :: Lens.Lens' CreateTransitGatewayRouteResponse Core.Int
ctgrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctgrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
