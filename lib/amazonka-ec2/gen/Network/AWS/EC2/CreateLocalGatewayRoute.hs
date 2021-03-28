{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateLocalGatewayRoute (..)
    , mkCreateLocalGatewayRoute
    -- ** Request lenses
    , clgrDestinationCidrBlock
    , clgrLocalGatewayRouteTableId
    , clgrLocalGatewayVirtualInterfaceGroupId
    , clgrDryRun

    -- * Destructuring the response
    , CreateLocalGatewayRouteResponse (..)
    , mkCreateLocalGatewayRouteResponse
    -- ** Response lenses
    , clgrrrsRoute
    , clgrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLocalGatewayRoute' smart constructor.
data CreateLocalGatewayRoute = CreateLocalGatewayRoute'
  { destinationCidrBlock :: Core.Text
    -- ^ The CIDR range used for destination matches. Routing decisions are based on the most specific match.
  , localGatewayRouteTableId :: Types.LocalGatewayRouteTableId
    -- ^ The ID of the local gateway route table.
  , localGatewayVirtualInterfaceGroupId :: Types.LocalGatewayVirtualInterfaceGroupId
    -- ^ The ID of the virtual interface group.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLocalGatewayRoute' value with any optional fields omitted.
mkCreateLocalGatewayRoute
    :: Core.Text -- ^ 'destinationCidrBlock'
    -> Types.LocalGatewayRouteTableId -- ^ 'localGatewayRouteTableId'
    -> Types.LocalGatewayVirtualInterfaceGroupId -- ^ 'localGatewayVirtualInterfaceGroupId'
    -> CreateLocalGatewayRoute
mkCreateLocalGatewayRoute destinationCidrBlock
  localGatewayRouteTableId localGatewayVirtualInterfaceGroupId
  = CreateLocalGatewayRoute'{destinationCidrBlock,
                             localGatewayRouteTableId, localGatewayVirtualInterfaceGroupId,
                             dryRun = Core.Nothing}

-- | The CIDR range used for destination matches. Routing decisions are based on the most specific match.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrDestinationCidrBlock :: Lens.Lens' CreateLocalGatewayRoute Core.Text
clgrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE clgrDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrLocalGatewayRouteTableId :: Lens.Lens' CreateLocalGatewayRoute Types.LocalGatewayRouteTableId
clgrLocalGatewayRouteTableId = Lens.field @"localGatewayRouteTableId"
{-# INLINEABLE clgrLocalGatewayRouteTableId #-}
{-# DEPRECATED localGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead"  #-}

-- | The ID of the virtual interface group.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrLocalGatewayVirtualInterfaceGroupId :: Lens.Lens' CreateLocalGatewayRoute Types.LocalGatewayVirtualInterfaceGroupId
clgrLocalGatewayVirtualInterfaceGroupId = Lens.field @"localGatewayVirtualInterfaceGroupId"
{-# INLINEABLE clgrLocalGatewayVirtualInterfaceGroupId #-}
{-# DEPRECATED localGatewayVirtualInterfaceGroupId "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceGroupId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrDryRun :: Lens.Lens' CreateLocalGatewayRoute (Core.Maybe Core.Bool)
clgrDryRun = Lens.field @"dryRun"
{-# INLINEABLE clgrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery CreateLocalGatewayRoute where
        toQuery CreateLocalGatewayRoute{..}
          = Core.toQueryPair "Action"
              ("CreateLocalGatewayRoute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "DestinationCidrBlock" destinationCidrBlock
              Core.<>
              Core.toQueryPair "LocalGatewayRouteTableId"
                localGatewayRouteTableId
              Core.<>
              Core.toQueryPair "LocalGatewayVirtualInterfaceGroupId"
                localGatewayVirtualInterfaceGroupId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders CreateLocalGatewayRoute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateLocalGatewayRoute where
        type Rs CreateLocalGatewayRoute = CreateLocalGatewayRouteResponse
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
                 CreateLocalGatewayRouteResponse' Core.<$>
                   (x Core..@? "route") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateLocalGatewayRouteResponse' smart constructor.
data CreateLocalGatewayRouteResponse = CreateLocalGatewayRouteResponse'
  { route :: Core.Maybe Types.LocalGatewayRoute
    -- ^ Information about the route.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLocalGatewayRouteResponse' value with any optional fields omitted.
mkCreateLocalGatewayRouteResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateLocalGatewayRouteResponse
mkCreateLocalGatewayRouteResponse responseStatus
  = CreateLocalGatewayRouteResponse'{route = Core.Nothing,
                                     responseStatus}

-- | Information about the route.
--
-- /Note:/ Consider using 'route' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrrrsRoute :: Lens.Lens' CreateLocalGatewayRouteResponse (Core.Maybe Types.LocalGatewayRoute)
clgrrrsRoute = Lens.field @"route"
{-# INLINEABLE clgrrrsRoute #-}
{-# DEPRECATED route "Use generic-lens or generic-optics with 'route' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrrrsResponseStatus :: Lens.Lens' CreateLocalGatewayRouteResponse Core.Int
clgrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE clgrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
