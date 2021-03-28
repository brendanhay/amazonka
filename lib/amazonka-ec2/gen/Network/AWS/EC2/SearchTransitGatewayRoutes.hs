{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.SearchTransitGatewayRoutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for routes in the specified transit gateway route table.
module Network.AWS.EC2.SearchTransitGatewayRoutes
    (
    -- * Creating a request
      SearchTransitGatewayRoutes (..)
    , mkSearchTransitGatewayRoutes
    -- ** Request lenses
    , stgrTransitGatewayRouteTableId
    , stgrFilters
    , stgrDryRun
    , stgrMaxResults

    -- * Destructuring the response
    , SearchTransitGatewayRoutesResponse (..)
    , mkSearchTransitGatewayRoutesResponse
    -- ** Response lenses
    , stgrrrsAdditionalRoutesAvailable
    , stgrrrsRoutes
    , stgrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSearchTransitGatewayRoutes' smart constructor.
data SearchTransitGatewayRoutes = SearchTransitGatewayRoutes'
  { transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId
    -- ^ The ID of the transit gateway route table.
  , filters :: [Types.Filter]
    -- ^ One or more filters. The possible values are:
--
--
--     * @attachment.transit-gateway-attachment-id@ - The id of the transit gateway attachment.
--
--
--     * @attachment.resource-id@ - The resource id of the transit gateway attachment.
--
--
--     * @attachment.resource-type@ - The attachment resource type. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .
--
--
--     * @prefix-list-id@ - The ID of the prefix list.
--
--
--     * @route-search.exact-match@ - The exact match of the specified filter.
--
--
--     * @route-search.longest-prefix-match@ - The longest prefix that matches the route.
--
--
--     * @route-search.subnet-of-match@ - The routes with a subnet that match the specified CIDR filter.
--
--
--     * @route-search.supernet-of-match@ - The routes with a CIDR that encompass the CIDR filter. For example, if you have 10.0.1.0/29 and 10.0.1.0/31 routes in your route table and you specify supernet-of-match as 10.0.1.0/30, then the result returns 10.0.1.0/29.
--
--
--     * @state@ - The state of the route (@active@ | @blackhole@ ).
--
--
--     * @type@ - The type of route (@propagated@ | @static@ ).
--
--
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of routes to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchTransitGatewayRoutes' value with any optional fields omitted.
mkSearchTransitGatewayRoutes
    :: Types.TransitGatewayRouteTableId -- ^ 'transitGatewayRouteTableId'
    -> SearchTransitGatewayRoutes
mkSearchTransitGatewayRoutes transitGatewayRouteTableId
  = SearchTransitGatewayRoutes'{transitGatewayRouteTableId,
                                filters = Core.mempty, dryRun = Core.Nothing,
                                maxResults = Core.Nothing}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgrTransitGatewayRouteTableId :: Lens.Lens' SearchTransitGatewayRoutes Types.TransitGatewayRouteTableId
stgrTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# INLINEABLE stgrTransitGatewayRouteTableId #-}
{-# DEPRECATED transitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead"  #-}

-- | One or more filters. The possible values are:
--
--
--     * @attachment.transit-gateway-attachment-id@ - The id of the transit gateway attachment.
--
--
--     * @attachment.resource-id@ - The resource id of the transit gateway attachment.
--
--
--     * @attachment.resource-type@ - The attachment resource type. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .
--
--
--     * @prefix-list-id@ - The ID of the prefix list.
--
--
--     * @route-search.exact-match@ - The exact match of the specified filter.
--
--
--     * @route-search.longest-prefix-match@ - The longest prefix that matches the route.
--
--
--     * @route-search.subnet-of-match@ - The routes with a subnet that match the specified CIDR filter.
--
--
--     * @route-search.supernet-of-match@ - The routes with a CIDR that encompass the CIDR filter. For example, if you have 10.0.1.0/29 and 10.0.1.0/31 routes in your route table and you specify supernet-of-match as 10.0.1.0/30, then the result returns 10.0.1.0/29.
--
--
--     * @state@ - The state of the route (@active@ | @blackhole@ ).
--
--
--     * @type@ - The type of route (@propagated@ | @static@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgrFilters :: Lens.Lens' SearchTransitGatewayRoutes [Types.Filter]
stgrFilters = Lens.field @"filters"
{-# INLINEABLE stgrFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgrDryRun :: Lens.Lens' SearchTransitGatewayRoutes (Core.Maybe Core.Bool)
stgrDryRun = Lens.field @"dryRun"
{-# INLINEABLE stgrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The maximum number of routes to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgrMaxResults :: Lens.Lens' SearchTransitGatewayRoutes (Core.Maybe Core.Natural)
stgrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE stgrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

instance Core.ToQuery SearchTransitGatewayRoutes where
        toQuery SearchTransitGatewayRoutes{..}
          = Core.toQueryPair "Action"
              ("SearchTransitGatewayRoutes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayRouteTableId"
                transitGatewayRouteTableId
              Core.<> Core.toQueryList "Filter" filters
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults

instance Core.ToHeaders SearchTransitGatewayRoutes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SearchTransitGatewayRoutes where
        type Rs SearchTransitGatewayRoutes =
             SearchTransitGatewayRoutesResponse
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
                 SearchTransitGatewayRoutesResponse' Core.<$>
                   (x Core..@? "additionalRoutesAvailable") Core.<*>
                     x Core..@? "routeSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSearchTransitGatewayRoutesResponse' smart constructor.
data SearchTransitGatewayRoutesResponse = SearchTransitGatewayRoutesResponse'
  { additionalRoutesAvailable :: Core.Maybe Core.Bool
    -- ^ Indicates whether there are additional routes available.
  , routes :: Core.Maybe [Types.TransitGatewayRoute]
    -- ^ Information about the routes.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchTransitGatewayRoutesResponse' value with any optional fields omitted.
mkSearchTransitGatewayRoutesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SearchTransitGatewayRoutesResponse
mkSearchTransitGatewayRoutesResponse responseStatus
  = SearchTransitGatewayRoutesResponse'{additionalRoutesAvailable =
                                          Core.Nothing,
                                        routes = Core.Nothing, responseStatus}

-- | Indicates whether there are additional routes available.
--
-- /Note:/ Consider using 'additionalRoutesAvailable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgrrrsAdditionalRoutesAvailable :: Lens.Lens' SearchTransitGatewayRoutesResponse (Core.Maybe Core.Bool)
stgrrrsAdditionalRoutesAvailable = Lens.field @"additionalRoutesAvailable"
{-# INLINEABLE stgrrrsAdditionalRoutesAvailable #-}
{-# DEPRECATED additionalRoutesAvailable "Use generic-lens or generic-optics with 'additionalRoutesAvailable' instead"  #-}

-- | Information about the routes.
--
-- /Note:/ Consider using 'routes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgrrrsRoutes :: Lens.Lens' SearchTransitGatewayRoutesResponse (Core.Maybe [Types.TransitGatewayRoute])
stgrrrsRoutes = Lens.field @"routes"
{-# INLINEABLE stgrrrsRoutes #-}
{-# DEPRECATED routes "Use generic-lens or generic-optics with 'routes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgrrrsResponseStatus :: Lens.Lens' SearchTransitGatewayRoutesResponse Core.Int
stgrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE stgrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
