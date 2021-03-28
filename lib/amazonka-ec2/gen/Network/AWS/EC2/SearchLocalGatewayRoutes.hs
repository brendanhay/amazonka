{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.SearchLocalGatewayRoutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for routes in the specified local gateway route table.
--
-- This operation returns paginated results.
module Network.AWS.EC2.SearchLocalGatewayRoutes
    (
    -- * Creating a request
      SearchLocalGatewayRoutes (..)
    , mkSearchLocalGatewayRoutes
    -- ** Request lenses
    , slgrLocalGatewayRouteTableId
    , slgrFilters
    , slgrDryRun
    , slgrMaxResults
    , slgrNextToken

    -- * Destructuring the response
    , SearchLocalGatewayRoutesResponse (..)
    , mkSearchLocalGatewayRoutesResponse
    -- ** Response lenses
    , slgrrrsNextToken
    , slgrrrsRoutes
    , slgrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSearchLocalGatewayRoutes' smart constructor.
data SearchLocalGatewayRoutes = SearchLocalGatewayRoutes'
  { localGatewayRouteTableId :: Types.LocalGatewayRoutetableId
    -- ^ The ID of the local gateway route table.
  , filters :: [Types.Filter]
    -- ^ One or more filters.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchLocalGatewayRoutes' value with any optional fields omitted.
mkSearchLocalGatewayRoutes
    :: Types.LocalGatewayRoutetableId -- ^ 'localGatewayRouteTableId'
    -> SearchLocalGatewayRoutes
mkSearchLocalGatewayRoutes localGatewayRouteTableId
  = SearchLocalGatewayRoutes'{localGatewayRouteTableId,
                              filters = Core.mempty, dryRun = Core.Nothing,
                              maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrLocalGatewayRouteTableId :: Lens.Lens' SearchLocalGatewayRoutes Types.LocalGatewayRoutetableId
slgrLocalGatewayRouteTableId = Lens.field @"localGatewayRouteTableId"
{-# INLINEABLE slgrLocalGatewayRouteTableId #-}
{-# DEPRECATED localGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead"  #-}

-- | One or more filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrFilters :: Lens.Lens' SearchLocalGatewayRoutes [Types.Filter]
slgrFilters = Lens.field @"filters"
{-# INLINEABLE slgrFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrDryRun :: Lens.Lens' SearchLocalGatewayRoutes (Core.Maybe Core.Bool)
slgrDryRun = Lens.field @"dryRun"
{-# INLINEABLE slgrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrMaxResults :: Lens.Lens' SearchLocalGatewayRoutes (Core.Maybe Core.Int)
slgrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE slgrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrNextToken :: Lens.Lens' SearchLocalGatewayRoutes (Core.Maybe Core.Text)
slgrNextToken = Lens.field @"nextToken"
{-# INLINEABLE slgrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery SearchLocalGatewayRoutes where
        toQuery SearchLocalGatewayRoutes{..}
          = Core.toQueryPair "Action"
              ("SearchLocalGatewayRoutes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "LocalGatewayRouteTableId"
                localGatewayRouteTableId
              Core.<> Core.toQueryList "Filter" filters
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders SearchLocalGatewayRoutes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SearchLocalGatewayRoutes where
        type Rs SearchLocalGatewayRoutes = SearchLocalGatewayRoutesResponse
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
                 SearchLocalGatewayRoutesResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "routeSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager SearchLocalGatewayRoutes where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"routes" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkSearchLocalGatewayRoutesResponse' smart constructor.
data SearchLocalGatewayRoutesResponse = SearchLocalGatewayRoutesResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , routes :: Core.Maybe [Types.LocalGatewayRoute]
    -- ^ Information about the routes.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchLocalGatewayRoutesResponse' value with any optional fields omitted.
mkSearchLocalGatewayRoutesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SearchLocalGatewayRoutesResponse
mkSearchLocalGatewayRoutesResponse responseStatus
  = SearchLocalGatewayRoutesResponse'{nextToken = Core.Nothing,
                                      routes = Core.Nothing, responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrrrsNextToken :: Lens.Lens' SearchLocalGatewayRoutesResponse (Core.Maybe Core.Text)
slgrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE slgrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the routes.
--
-- /Note:/ Consider using 'routes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrrrsRoutes :: Lens.Lens' SearchLocalGatewayRoutesResponse (Core.Maybe [Types.LocalGatewayRoute])
slgrrrsRoutes = Lens.field @"routes"
{-# INLINEABLE slgrrrsRoutes #-}
{-# DEPRECATED routes "Use generic-lens or generic-optics with 'routes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrrrsResponseStatus :: Lens.Lens' SearchLocalGatewayRoutesResponse Core.Int
slgrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE slgrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
