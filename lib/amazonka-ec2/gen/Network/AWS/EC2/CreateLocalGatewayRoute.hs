{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateLocalGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a static route for the specified local gateway route table.
module Network.AWS.EC2.CreateLocalGatewayRoute
  ( -- * Creating a request
    CreateLocalGatewayRoute (..),
    mkCreateLocalGatewayRoute,

    -- ** Request lenses
    clgrDestinationCidrBlock,
    clgrLocalGatewayRouteTableId,
    clgrLocalGatewayVirtualInterfaceGroupId,
    clgrDryRun,

    -- * Destructuring the response
    CreateLocalGatewayRouteResponse (..),
    mkCreateLocalGatewayRouteResponse,

    -- ** Response lenses
    clgrrrsRoute,
    clgrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLocalGatewayRoute' smart constructor.
data CreateLocalGatewayRoute = CreateLocalGatewayRoute'
  { -- | The CIDR range used for destination matches. Routing decisions are based on the most specific match.
    destinationCidrBlock :: Types.String,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Types.LocalGatewayRouteTableId,
    -- | The ID of the virtual interface group.
    localGatewayVirtualInterfaceGroupId :: Types.LocalGatewayVirtualInterfaceGroupId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLocalGatewayRoute' value with any optional fields omitted.
mkCreateLocalGatewayRoute ::
  -- | 'destinationCidrBlock'
  Types.String ->
  -- | 'localGatewayRouteTableId'
  Types.LocalGatewayRouteTableId ->
  -- | 'localGatewayVirtualInterfaceGroupId'
  Types.LocalGatewayVirtualInterfaceGroupId ->
  CreateLocalGatewayRoute
mkCreateLocalGatewayRoute
  destinationCidrBlock
  localGatewayRouteTableId
  localGatewayVirtualInterfaceGroupId =
    CreateLocalGatewayRoute'
      { destinationCidrBlock,
        localGatewayRouteTableId,
        localGatewayVirtualInterfaceGroupId,
        dryRun = Core.Nothing
      }

-- | The CIDR range used for destination matches. Routing decisions are based on the most specific match.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrDestinationCidrBlock :: Lens.Lens' CreateLocalGatewayRoute Types.String
clgrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# DEPRECATED clgrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrLocalGatewayRouteTableId :: Lens.Lens' CreateLocalGatewayRoute Types.LocalGatewayRouteTableId
clgrLocalGatewayRouteTableId = Lens.field @"localGatewayRouteTableId"
{-# DEPRECATED clgrLocalGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead." #-}

-- | The ID of the virtual interface group.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrLocalGatewayVirtualInterfaceGroupId :: Lens.Lens' CreateLocalGatewayRoute Types.LocalGatewayVirtualInterfaceGroupId
clgrLocalGatewayVirtualInterfaceGroupId = Lens.field @"localGatewayVirtualInterfaceGroupId"
{-# DEPRECATED clgrLocalGatewayVirtualInterfaceGroupId "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceGroupId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrDryRun :: Lens.Lens' CreateLocalGatewayRoute (Core.Maybe Core.Bool)
clgrDryRun = Lens.field @"dryRun"
{-# DEPRECATED clgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest CreateLocalGatewayRoute where
  type Rs CreateLocalGatewayRoute = CreateLocalGatewayRouteResponse
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
            ( Core.pure ("Action", "CreateLocalGatewayRoute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DestinationCidrBlock" destinationCidrBlock)
                Core.<> ( Core.toQueryValue
                            "LocalGatewayRouteTableId"
                            localGatewayRouteTableId
                        )
                Core.<> ( Core.toQueryValue
                            "LocalGatewayVirtualInterfaceGroupId"
                            localGatewayVirtualInterfaceGroupId
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateLocalGatewayRouteResponse'
            Core.<$> (x Core..@? "route") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateLocalGatewayRouteResponse' smart constructor.
data CreateLocalGatewayRouteResponse = CreateLocalGatewayRouteResponse'
  { -- | Information about the route.
    route :: Core.Maybe Types.LocalGatewayRoute,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLocalGatewayRouteResponse' value with any optional fields omitted.
mkCreateLocalGatewayRouteResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateLocalGatewayRouteResponse
mkCreateLocalGatewayRouteResponse responseStatus =
  CreateLocalGatewayRouteResponse'
    { route = Core.Nothing,
      responseStatus
    }

-- | Information about the route.
--
-- /Note:/ Consider using 'route' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrrrsRoute :: Lens.Lens' CreateLocalGatewayRouteResponse (Core.Maybe Types.LocalGatewayRoute)
clgrrrsRoute = Lens.field @"route"
{-# DEPRECATED clgrrrsRoute "Use generic-lens or generic-optics with 'route' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrrrsResponseStatus :: Lens.Lens' CreateLocalGatewayRouteResponse Core.Int
clgrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED clgrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
