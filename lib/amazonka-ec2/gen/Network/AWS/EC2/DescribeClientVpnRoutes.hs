{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeClientVpnRoutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the routes for the specified Client VPN endpoint.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVpnRoutes
    (
    -- * Creating a request
      DescribeClientVpnRoutes (..)
    , mkDescribeClientVpnRoutes
    -- ** Request lenses
    , dcvrClientVpnEndpointId
    , dcvrDryRun
    , dcvrFilters
    , dcvrMaxResults
    , dcvrNextToken

    -- * Destructuring the response
    , DescribeClientVpnRoutesResponse (..)
    , mkDescribeClientVpnRoutesResponse
    -- ** Response lenses
    , dcvrrrsNextToken
    , dcvrrrsRoutes
    , dcvrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeClientVpnRoutes' smart constructor.
data DescribeClientVpnRoutes = DescribeClientVpnRoutes'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN endpoint.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters. Filter names and values are case-sensitive.
--
--
--     * @destination-cidr@ - The CIDR of the route destination.
--
--
--     * @origin@ - How the route was associated with the Client VPN endpoint (@associate@ | @add-route@ ).
--
--
--     * @target-subnet@ - The ID of the subnet through which traffic is routed.
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to retrieve the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClientVpnRoutes' value with any optional fields omitted.
mkDescribeClientVpnRoutes
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> DescribeClientVpnRoutes
mkDescribeClientVpnRoutes clientVpnEndpointId
  = DescribeClientVpnRoutes'{clientVpnEndpointId,
                             dryRun = Core.Nothing, filters = Core.Nothing,
                             maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrClientVpnEndpointId :: Lens.Lens' DescribeClientVpnRoutes Types.ClientVpnEndpointId
dcvrClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE dcvrClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrDryRun :: Lens.Lens' DescribeClientVpnRoutes (Core.Maybe Core.Bool)
dcvrDryRun = Lens.field @"dryRun"
{-# INLINEABLE dcvrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters. Filter names and values are case-sensitive.
--
--
--     * @destination-cidr@ - The CIDR of the route destination.
--
--
--     * @origin@ - How the route was associated with the Client VPN endpoint (@associate@ | @add-route@ ).
--
--
--     * @target-subnet@ - The ID of the subnet through which traffic is routed.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrFilters :: Lens.Lens' DescribeClientVpnRoutes (Core.Maybe [Types.Filter])
dcvrFilters = Lens.field @"filters"
{-# INLINEABLE dcvrFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrMaxResults :: Lens.Lens' DescribeClientVpnRoutes (Core.Maybe Core.Natural)
dcvrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dcvrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrNextToken :: Lens.Lens' DescribeClientVpnRoutes (Core.Maybe Types.NextToken)
dcvrNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcvrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeClientVpnRoutes where
        toQuery DescribeClientVpnRoutes{..}
          = Core.toQueryPair "Action"
              ("DescribeClientVpnRoutes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeClientVpnRoutes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeClientVpnRoutes where
        type Rs DescribeClientVpnRoutes = DescribeClientVpnRoutesResponse
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
                 DescribeClientVpnRoutesResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "routes" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeClientVpnRoutes where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"routes" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeClientVpnRoutesResponse' smart constructor.
data DescribeClientVpnRoutesResponse = DescribeClientVpnRoutesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , routes :: Core.Maybe [Types.ClientVpnRoute]
    -- ^ Information about the Client VPN endpoint routes.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClientVpnRoutesResponse' value with any optional fields omitted.
mkDescribeClientVpnRoutesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeClientVpnRoutesResponse
mkDescribeClientVpnRoutesResponse responseStatus
  = DescribeClientVpnRoutesResponse'{nextToken = Core.Nothing,
                                     routes = Core.Nothing, responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrrrsNextToken :: Lens.Lens' DescribeClientVpnRoutesResponse (Core.Maybe Types.NextToken)
dcvrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcvrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the Client VPN endpoint routes.
--
-- /Note:/ Consider using 'routes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrrrsRoutes :: Lens.Lens' DescribeClientVpnRoutesResponse (Core.Maybe [Types.ClientVpnRoute])
dcvrrrsRoutes = Lens.field @"routes"
{-# INLINEABLE dcvrrrsRoutes #-}
{-# DEPRECATED routes "Use generic-lens or generic-optics with 'routes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrrrsResponseStatus :: Lens.Lens' DescribeClientVpnRoutesResponse Core.Int
dcvrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcvrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
