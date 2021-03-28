{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route from the specified transit gateway route table.
module Network.AWS.EC2.DeleteTransitGatewayRoute
    (
    -- * Creating a request
      DeleteTransitGatewayRoute (..)
    , mkDeleteTransitGatewayRoute
    -- ** Request lenses
    , dtgrTransitGatewayRouteTableId
    , dtgrDestinationCidrBlock
    , dtgrDryRun

    -- * Destructuring the response
    , DeleteTransitGatewayRouteResponse (..)
    , mkDeleteTransitGatewayRouteResponse
    -- ** Response lenses
    , dtgrrrsRoute
    , dtgrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTransitGatewayRoute' smart constructor.
data DeleteTransitGatewayRoute = DeleteTransitGatewayRoute'
  { transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId
    -- ^ The ID of the transit gateway route table.
  , destinationCidrBlock :: Core.Text
    -- ^ The CIDR range for the route. This must match the CIDR for the route exactly.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTransitGatewayRoute' value with any optional fields omitted.
mkDeleteTransitGatewayRoute
    :: Types.TransitGatewayRouteTableId -- ^ 'transitGatewayRouteTableId'
    -> Core.Text -- ^ 'destinationCidrBlock'
    -> DeleteTransitGatewayRoute
mkDeleteTransitGatewayRoute transitGatewayRouteTableId
  destinationCidrBlock
  = DeleteTransitGatewayRoute'{transitGatewayRouteTableId,
                               destinationCidrBlock, dryRun = Core.Nothing}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrTransitGatewayRouteTableId :: Lens.Lens' DeleteTransitGatewayRoute Types.TransitGatewayRouteTableId
dtgrTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# INLINEABLE dtgrTransitGatewayRouteTableId #-}
{-# DEPRECATED transitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead"  #-}

-- | The CIDR range for the route. This must match the CIDR for the route exactly.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrDestinationCidrBlock :: Lens.Lens' DeleteTransitGatewayRoute Core.Text
dtgrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE dtgrDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrDryRun :: Lens.Lens' DeleteTransitGatewayRoute (Core.Maybe Core.Bool)
dtgrDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtgrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteTransitGatewayRoute where
        toQuery DeleteTransitGatewayRoute{..}
          = Core.toQueryPair "Action"
              ("DeleteTransitGatewayRoute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayRouteTableId"
                transitGatewayRouteTableId
              Core.<>
              Core.toQueryPair "DestinationCidrBlock" destinationCidrBlock
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteTransitGatewayRoute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteTransitGatewayRoute where
        type Rs DeleteTransitGatewayRoute =
             DeleteTransitGatewayRouteResponse
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
                 DeleteTransitGatewayRouteResponse' Core.<$>
                   (x Core..@? "route") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTransitGatewayRouteResponse' smart constructor.
data DeleteTransitGatewayRouteResponse = DeleteTransitGatewayRouteResponse'
  { route :: Core.Maybe Types.TransitGatewayRoute
    -- ^ Information about the route.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTransitGatewayRouteResponse' value with any optional fields omitted.
mkDeleteTransitGatewayRouteResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTransitGatewayRouteResponse
mkDeleteTransitGatewayRouteResponse responseStatus
  = DeleteTransitGatewayRouteResponse'{route = Core.Nothing,
                                       responseStatus}

-- | Information about the route.
--
-- /Note:/ Consider using 'route' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrrsRoute :: Lens.Lens' DeleteTransitGatewayRouteResponse (Core.Maybe Types.TransitGatewayRoute)
dtgrrrsRoute = Lens.field @"route"
{-# INLINEABLE dtgrrrsRoute #-}
{-# DEPRECATED route "Use generic-lens or generic-optics with 'route' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrrsResponseStatus :: Lens.Lens' DeleteTransitGatewayRouteResponse Core.Int
dtgrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
