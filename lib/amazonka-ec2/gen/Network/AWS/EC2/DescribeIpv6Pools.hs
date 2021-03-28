{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeIpv6Pools
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your IPv6 address pools.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeIpv6Pools
    (
    -- * Creating a request
      DescribeIpv6Pools (..)
    , mkDescribeIpv6Pools
    -- ** Request lenses
    , dipDryRun
    , dipFilters
    , dipMaxResults
    , dipNextToken
    , dipPoolIds

    -- * Destructuring the response
    , DescribeIpv6PoolsResponse (..)
    , mkDescribeIpv6PoolsResponse
    -- ** Response lenses
    , diprrsIpv6Pools
    , diprrsNextToken
    , diprrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeIpv6Pools' smart constructor.
data DescribeIpv6Pools = DescribeIpv6Pools'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next page of results.
  , poolIds :: Core.Maybe [Types.Ipv6PoolEc2Id]
    -- ^ The IDs of the IPv6 address pools.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeIpv6Pools' value with any optional fields omitted.
mkDescribeIpv6Pools
    :: DescribeIpv6Pools
mkDescribeIpv6Pools
  = DescribeIpv6Pools'{dryRun = Core.Nothing, filters = Core.Nothing,
                       maxResults = Core.Nothing, nextToken = Core.Nothing,
                       poolIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipDryRun :: Lens.Lens' DescribeIpv6Pools (Core.Maybe Core.Bool)
dipDryRun = Lens.field @"dryRun"
{-# INLINEABLE dipDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipFilters :: Lens.Lens' DescribeIpv6Pools (Core.Maybe [Types.Filter])
dipFilters = Lens.field @"filters"
{-# INLINEABLE dipFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipMaxResults :: Lens.Lens' DescribeIpv6Pools (Core.Maybe Core.Natural)
dipMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dipMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipNextToken :: Lens.Lens' DescribeIpv6Pools (Core.Maybe Types.NextToken)
dipNextToken = Lens.field @"nextToken"
{-# INLINEABLE dipNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The IDs of the IPv6 address pools.
--
-- /Note:/ Consider using 'poolIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipPoolIds :: Lens.Lens' DescribeIpv6Pools (Core.Maybe [Types.Ipv6PoolEc2Id])
dipPoolIds = Lens.field @"poolIds"
{-# INLINEABLE dipPoolIds #-}
{-# DEPRECATED poolIds "Use generic-lens or generic-optics with 'poolIds' instead"  #-}

instance Core.ToQuery DescribeIpv6Pools where
        toQuery DescribeIpv6Pools{..}
          = Core.toQueryPair "Action" ("DescribeIpv6Pools" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<> Core.maybe Core.mempty (Core.toQueryList "PoolId") poolIds

instance Core.ToHeaders DescribeIpv6Pools where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeIpv6Pools where
        type Rs DescribeIpv6Pools = DescribeIpv6PoolsResponse
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
                 DescribeIpv6PoolsResponse' Core.<$>
                   (x Core..@? "ipv6PoolSet" Core..<@> Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeIpv6Pools where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"ipv6Pools" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeIpv6PoolsResponse' smart constructor.
data DescribeIpv6PoolsResponse = DescribeIpv6PoolsResponse'
  { ipv6Pools :: Core.Maybe [Types.Ipv6Pool]
    -- ^ Information about the IPv6 address pools.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeIpv6PoolsResponse' value with any optional fields omitted.
mkDescribeIpv6PoolsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeIpv6PoolsResponse
mkDescribeIpv6PoolsResponse responseStatus
  = DescribeIpv6PoolsResponse'{ipv6Pools = Core.Nothing,
                               nextToken = Core.Nothing, responseStatus}

-- | Information about the IPv6 address pools.
--
-- /Note:/ Consider using 'ipv6Pools' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprrsIpv6Pools :: Lens.Lens' DescribeIpv6PoolsResponse (Core.Maybe [Types.Ipv6Pool])
diprrsIpv6Pools = Lens.field @"ipv6Pools"
{-# INLINEABLE diprrsIpv6Pools #-}
{-# DEPRECATED ipv6Pools "Use generic-lens or generic-optics with 'ipv6Pools' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprrsNextToken :: Lens.Lens' DescribeIpv6PoolsResponse (Core.Maybe Types.NextToken)
diprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE diprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprrsResponseStatus :: Lens.Lens' DescribeIpv6PoolsResponse Core.Int
diprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE diprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
