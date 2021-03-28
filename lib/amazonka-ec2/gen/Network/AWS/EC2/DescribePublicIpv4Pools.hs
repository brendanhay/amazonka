{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribePublicIpv4Pools
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified IPv4 address pools.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribePublicIpv4Pools
    (
    -- * Creating a request
      DescribePublicIpv4Pools (..)
    , mkDescribePublicIpv4Pools
    -- ** Request lenses
    , dpipFilters
    , dpipMaxResults
    , dpipNextToken
    , dpipPoolIds

    -- * Destructuring the response
    , DescribePublicIpv4PoolsResponse (..)
    , mkDescribePublicIpv4PoolsResponse
    -- ** Response lenses
    , dpiprrsNextToken
    , dpiprrsPublicIpv4Pools
    , dpiprrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePublicIpv4Pools' smart constructor.
data DescribePublicIpv4Pools = DescribePublicIpv4Pools'
  { filters :: Core.Maybe [Types.Filter]
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
  , poolIds :: Core.Maybe [Types.Ipv4PoolEc2Id]
    -- ^ The IDs of the address pools.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePublicIpv4Pools' value with any optional fields omitted.
mkDescribePublicIpv4Pools
    :: DescribePublicIpv4Pools
mkDescribePublicIpv4Pools
  = DescribePublicIpv4Pools'{filters = Core.Nothing,
                             maxResults = Core.Nothing, nextToken = Core.Nothing,
                             poolIds = Core.Nothing}

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
dpipFilters :: Lens.Lens' DescribePublicIpv4Pools (Core.Maybe [Types.Filter])
dpipFilters = Lens.field @"filters"
{-# INLINEABLE dpipFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpipMaxResults :: Lens.Lens' DescribePublicIpv4Pools (Core.Maybe Core.Natural)
dpipMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dpipMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpipNextToken :: Lens.Lens' DescribePublicIpv4Pools (Core.Maybe Types.NextToken)
dpipNextToken = Lens.field @"nextToken"
{-# INLINEABLE dpipNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The IDs of the address pools.
--
-- /Note:/ Consider using 'poolIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpipPoolIds :: Lens.Lens' DescribePublicIpv4Pools (Core.Maybe [Types.Ipv4PoolEc2Id])
dpipPoolIds = Lens.field @"poolIds"
{-# INLINEABLE dpipPoolIds #-}
{-# DEPRECATED poolIds "Use generic-lens or generic-optics with 'poolIds' instead"  #-}

instance Core.ToQuery DescribePublicIpv4Pools where
        toQuery DescribePublicIpv4Pools{..}
          = Core.toQueryPair "Action"
              ("DescribePublicIpv4Pools" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<> Core.maybe Core.mempty (Core.toQueryList "PoolId") poolIds

instance Core.ToHeaders DescribePublicIpv4Pools where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribePublicIpv4Pools where
        type Rs DescribePublicIpv4Pools = DescribePublicIpv4PoolsResponse
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
                 DescribePublicIpv4PoolsResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "publicIpv4PoolSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribePublicIpv4Pools where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"publicIpv4Pools" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribePublicIpv4PoolsResponse' smart constructor.
data DescribePublicIpv4PoolsResponse = DescribePublicIpv4PoolsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , publicIpv4Pools :: Core.Maybe [Types.PublicIpv4Pool]
    -- ^ Information about the address pools.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePublicIpv4PoolsResponse' value with any optional fields omitted.
mkDescribePublicIpv4PoolsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribePublicIpv4PoolsResponse
mkDescribePublicIpv4PoolsResponse responseStatus
  = DescribePublicIpv4PoolsResponse'{nextToken = Core.Nothing,
                                     publicIpv4Pools = Core.Nothing, responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpiprrsNextToken :: Lens.Lens' DescribePublicIpv4PoolsResponse (Core.Maybe Core.Text)
dpiprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dpiprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the address pools.
--
-- /Note:/ Consider using 'publicIpv4Pools' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpiprrsPublicIpv4Pools :: Lens.Lens' DescribePublicIpv4PoolsResponse (Core.Maybe [Types.PublicIpv4Pool])
dpiprrsPublicIpv4Pools = Lens.field @"publicIpv4Pools"
{-# INLINEABLE dpiprrsPublicIpv4Pools #-}
{-# DEPRECATED publicIpv4Pools "Use generic-lens or generic-optics with 'publicIpv4Pools' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpiprrsResponseStatus :: Lens.Lens' DescribePublicIpv4PoolsResponse Core.Int
dpiprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpiprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
