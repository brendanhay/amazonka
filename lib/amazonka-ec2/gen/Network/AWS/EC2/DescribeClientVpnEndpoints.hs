{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeClientVpnEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Client VPN endpoints in the account.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVpnEndpoints
    (
    -- * Creating a request
      DescribeClientVpnEndpoints (..)
    , mkDescribeClientVpnEndpoints
    -- ** Request lenses
    , dcveClientVpnEndpointIds
    , dcveDryRun
    , dcveFilters
    , dcveMaxResults
    , dcveNextToken

    -- * Destructuring the response
    , DescribeClientVpnEndpointsResponse (..)
    , mkDescribeClientVpnEndpointsResponse
    -- ** Response lenses
    , dcverrsClientVpnEndpoints
    , dcverrsNextToken
    , dcverrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeClientVpnEndpoints' smart constructor.
data DescribeClientVpnEndpoints = DescribeClientVpnEndpoints'
  { clientVpnEndpointIds :: Core.Maybe [Types.ClientVpnEndpointId]
    -- ^ The ID of the Client VPN endpoint.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters. Filter names and values are case-sensitive.
--
--
--     * @endpoint-id@ - The ID of the Client VPN endpoint.
--
--
--     * @transport-protocol@ - The transport protocol (@tcp@ | @udp@ ).
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to retrieve the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClientVpnEndpoints' value with any optional fields omitted.
mkDescribeClientVpnEndpoints
    :: DescribeClientVpnEndpoints
mkDescribeClientVpnEndpoints
  = DescribeClientVpnEndpoints'{clientVpnEndpointIds = Core.Nothing,
                                dryRun = Core.Nothing, filters = Core.Nothing,
                                maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVpnEndpointIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcveClientVpnEndpointIds :: Lens.Lens' DescribeClientVpnEndpoints (Core.Maybe [Types.ClientVpnEndpointId])
dcveClientVpnEndpointIds = Lens.field @"clientVpnEndpointIds"
{-# INLINEABLE dcveClientVpnEndpointIds #-}
{-# DEPRECATED clientVpnEndpointIds "Use generic-lens or generic-optics with 'clientVpnEndpointIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcveDryRun :: Lens.Lens' DescribeClientVpnEndpoints (Core.Maybe Core.Bool)
dcveDryRun = Lens.field @"dryRun"
{-# INLINEABLE dcveDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters. Filter names and values are case-sensitive.
--
--
--     * @endpoint-id@ - The ID of the Client VPN endpoint.
--
--
--     * @transport-protocol@ - The transport protocol (@tcp@ | @udp@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcveFilters :: Lens.Lens' DescribeClientVpnEndpoints (Core.Maybe [Types.Filter])
dcveFilters = Lens.field @"filters"
{-# INLINEABLE dcveFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcveMaxResults :: Lens.Lens' DescribeClientVpnEndpoints (Core.Maybe Core.Natural)
dcveMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dcveMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcveNextToken :: Lens.Lens' DescribeClientVpnEndpoints (Core.Maybe Types.NextToken)
dcveNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcveNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeClientVpnEndpoints where
        toQuery DescribeClientVpnEndpoints{..}
          = Core.toQueryPair "Action"
              ("DescribeClientVpnEndpoints" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "ClientVpnEndpointId")
                clientVpnEndpointIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeClientVpnEndpoints where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeClientVpnEndpoints where
        type Rs DescribeClientVpnEndpoints =
             DescribeClientVpnEndpointsResponse
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
                 DescribeClientVpnEndpointsResponse' Core.<$>
                   (x Core..@? "clientVpnEndpoint" Core..<@> Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeClientVpnEndpoints where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"clientVpnEndpoints" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeClientVpnEndpointsResponse' smart constructor.
data DescribeClientVpnEndpointsResponse = DescribeClientVpnEndpointsResponse'
  { clientVpnEndpoints :: Core.Maybe [Types.ClientVpnEndpoint]
    -- ^ Information about the Client VPN endpoints.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClientVpnEndpointsResponse' value with any optional fields omitted.
mkDescribeClientVpnEndpointsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeClientVpnEndpointsResponse
mkDescribeClientVpnEndpointsResponse responseStatus
  = DescribeClientVpnEndpointsResponse'{clientVpnEndpoints =
                                          Core.Nothing,
                                        nextToken = Core.Nothing, responseStatus}

-- | Information about the Client VPN endpoints.
--
-- /Note:/ Consider using 'clientVpnEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcverrsClientVpnEndpoints :: Lens.Lens' DescribeClientVpnEndpointsResponse (Core.Maybe [Types.ClientVpnEndpoint])
dcverrsClientVpnEndpoints = Lens.field @"clientVpnEndpoints"
{-# INLINEABLE dcverrsClientVpnEndpoints #-}
{-# DEPRECATED clientVpnEndpoints "Use generic-lens or generic-optics with 'clientVpnEndpoints' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcverrsNextToken :: Lens.Lens' DescribeClientVpnEndpointsResponse (Core.Maybe Types.NextToken)
dcverrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcverrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcverrsResponseStatus :: Lens.Lens' DescribeClientVpnEndpointsResponse Core.Int
dcverrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcverrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
