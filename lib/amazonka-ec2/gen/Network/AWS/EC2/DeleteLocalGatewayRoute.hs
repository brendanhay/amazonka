{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteLocalGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route from the specified local gateway route table.
module Network.AWS.EC2.DeleteLocalGatewayRoute
    (
    -- * Creating a request
      DeleteLocalGatewayRoute (..)
    , mkDeleteLocalGatewayRoute
    -- ** Request lenses
    , dlgrDestinationCidrBlock
    , dlgrLocalGatewayRouteTableId
    , dlgrDryRun

    -- * Destructuring the response
    , DeleteLocalGatewayRouteResponse (..)
    , mkDeleteLocalGatewayRouteResponse
    -- ** Response lenses
    , dlgrrrsRoute
    , dlgrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLocalGatewayRoute' smart constructor.
data DeleteLocalGatewayRoute = DeleteLocalGatewayRoute'
  { destinationCidrBlock :: Core.Text
    -- ^ The CIDR range for the route. This must match the CIDR for the route exactly.
  , localGatewayRouteTableId :: Types.LocalGatewayRouteTableId
    -- ^ The ID of the local gateway route table.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLocalGatewayRoute' value with any optional fields omitted.
mkDeleteLocalGatewayRoute
    :: Core.Text -- ^ 'destinationCidrBlock'
    -> Types.LocalGatewayRouteTableId -- ^ 'localGatewayRouteTableId'
    -> DeleteLocalGatewayRoute
mkDeleteLocalGatewayRoute destinationCidrBlock
  localGatewayRouteTableId
  = DeleteLocalGatewayRoute'{destinationCidrBlock,
                             localGatewayRouteTableId, dryRun = Core.Nothing}

-- | The CIDR range for the route. This must match the CIDR for the route exactly.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrDestinationCidrBlock :: Lens.Lens' DeleteLocalGatewayRoute Core.Text
dlgrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE dlgrDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrLocalGatewayRouteTableId :: Lens.Lens' DeleteLocalGatewayRoute Types.LocalGatewayRouteTableId
dlgrLocalGatewayRouteTableId = Lens.field @"localGatewayRouteTableId"
{-# INLINEABLE dlgrLocalGatewayRouteTableId #-}
{-# DEPRECATED localGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrDryRun :: Lens.Lens' DeleteLocalGatewayRoute (Core.Maybe Core.Bool)
dlgrDryRun = Lens.field @"dryRun"
{-# INLINEABLE dlgrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteLocalGatewayRoute where
        toQuery DeleteLocalGatewayRoute{..}
          = Core.toQueryPair "Action"
              ("DeleteLocalGatewayRoute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "DestinationCidrBlock" destinationCidrBlock
              Core.<>
              Core.toQueryPair "LocalGatewayRouteTableId"
                localGatewayRouteTableId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteLocalGatewayRoute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteLocalGatewayRoute where
        type Rs DeleteLocalGatewayRoute = DeleteLocalGatewayRouteResponse
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
                 DeleteLocalGatewayRouteResponse' Core.<$>
                   (x Core..@? "route") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteLocalGatewayRouteResponse' smart constructor.
data DeleteLocalGatewayRouteResponse = DeleteLocalGatewayRouteResponse'
  { route :: Core.Maybe Types.LocalGatewayRoute
    -- ^ Information about the route.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLocalGatewayRouteResponse' value with any optional fields omitted.
mkDeleteLocalGatewayRouteResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteLocalGatewayRouteResponse
mkDeleteLocalGatewayRouteResponse responseStatus
  = DeleteLocalGatewayRouteResponse'{route = Core.Nothing,
                                     responseStatus}

-- | Information about the route.
--
-- /Note:/ Consider using 'route' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrrrsRoute :: Lens.Lens' DeleteLocalGatewayRouteResponse (Core.Maybe Types.LocalGatewayRoute)
dlgrrrsRoute = Lens.field @"route"
{-# INLINEABLE dlgrrrsRoute #-}
{-# DEPRECATED route "Use generic-lens or generic-optics with 'route' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrrrsResponseStatus :: Lens.Lens' DeleteLocalGatewayRouteResponse Core.Int
dlgrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlgrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
