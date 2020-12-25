{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SearchLocalGatewayRoutes (..),
    mkSearchLocalGatewayRoutes,

    -- ** Request lenses
    slgrLocalGatewayRouteTableId,
    slgrFilters,
    slgrDryRun,
    slgrMaxResults,
    slgrNextToken,

    -- * Destructuring the response
    SearchLocalGatewayRoutesResponse (..),
    mkSearchLocalGatewayRoutesResponse,

    -- ** Response lenses
    slgrrrsNextToken,
    slgrrrsRoutes,
    slgrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSearchLocalGatewayRoutes' smart constructor.
data SearchLocalGatewayRoutes = SearchLocalGatewayRoutes'
  { -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Types.LocalGatewayRoutetableId,
    -- | One or more filters.
    filters :: [Types.Filter],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Int,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchLocalGatewayRoutes' value with any optional fields omitted.
mkSearchLocalGatewayRoutes ::
  -- | 'localGatewayRouteTableId'
  Types.LocalGatewayRoutetableId ->
  SearchLocalGatewayRoutes
mkSearchLocalGatewayRoutes localGatewayRouteTableId =
  SearchLocalGatewayRoutes'
    { localGatewayRouteTableId,
      filters = Core.mempty,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrLocalGatewayRouteTableId :: Lens.Lens' SearchLocalGatewayRoutes Types.LocalGatewayRoutetableId
slgrLocalGatewayRouteTableId = Lens.field @"localGatewayRouteTableId"
{-# DEPRECATED slgrLocalGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead." #-}

-- | One or more filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrFilters :: Lens.Lens' SearchLocalGatewayRoutes [Types.Filter]
slgrFilters = Lens.field @"filters"
{-# DEPRECATED slgrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrDryRun :: Lens.Lens' SearchLocalGatewayRoutes (Core.Maybe Core.Bool)
slgrDryRun = Lens.field @"dryRun"
{-# DEPRECATED slgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrMaxResults :: Lens.Lens' SearchLocalGatewayRoutes (Core.Maybe Core.Int)
slgrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED slgrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrNextToken :: Lens.Lens' SearchLocalGatewayRoutes (Core.Maybe Types.String)
slgrNextToken = Lens.field @"nextToken"
{-# DEPRECATED slgrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest SearchLocalGatewayRoutes where
  type Rs SearchLocalGatewayRoutes = SearchLocalGatewayRoutesResponse
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
            ( Core.pure ("Action", "SearchLocalGatewayRoutes")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue
                            "LocalGatewayRouteTableId"
                            localGatewayRouteTableId
                        )
                Core.<> (Core.toQueryList "Filter" filters)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          SearchLocalGatewayRoutesResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> (x Core..@? "routeSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager SearchLocalGatewayRoutes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"routes" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkSearchLocalGatewayRoutesResponse' smart constructor.
data SearchLocalGatewayRoutesResponse = SearchLocalGatewayRoutesResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | Information about the routes.
    routes :: Core.Maybe [Types.LocalGatewayRoute],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchLocalGatewayRoutesResponse' value with any optional fields omitted.
mkSearchLocalGatewayRoutesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SearchLocalGatewayRoutesResponse
mkSearchLocalGatewayRoutesResponse responseStatus =
  SearchLocalGatewayRoutesResponse'
    { nextToken = Core.Nothing,
      routes = Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrrrsNextToken :: Lens.Lens' SearchLocalGatewayRoutesResponse (Core.Maybe Types.String)
slgrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED slgrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the routes.
--
-- /Note:/ Consider using 'routes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrrrsRoutes :: Lens.Lens' SearchLocalGatewayRoutesResponse (Core.Maybe [Types.LocalGatewayRoute])
slgrrrsRoutes = Lens.field @"routes"
{-# DEPRECATED slgrrrsRoutes "Use generic-lens or generic-optics with 'routes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrrrsResponseStatus :: Lens.Lens' SearchLocalGatewayRoutesResponse Core.Int
slgrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED slgrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
