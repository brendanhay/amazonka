{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeCoipPools
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified customer-owned address pools or all of your customer-owned address pools.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeCoipPools
  ( -- * Creating a request
    DescribeCoipPools (..),
    mkDescribeCoipPools,

    -- ** Request lenses
    dcpDryRun,
    dcpFilters,
    dcpMaxResults,
    dcpNextToken,
    dcpPoolIds,

    -- * Destructuring the response
    DescribeCoipPoolsResponse (..),
    mkDescribeCoipPoolsResponse,

    -- ** Response lenses
    dcprrsCoipPools,
    dcprrsNextToken,
    dcprrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCoipPools' smart constructor.
data DescribeCoipPools = DescribeCoipPools'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The filters. The following are the possible values:
    --
    --
    --     * @coip-pool.pool-id@
    --
    --
    --
    --     * @coip-pool.local-gateway-route-table-id@
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | The IDs of the address pools.
    poolIds :: Core.Maybe [Types.CoipPoolId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCoipPools' value with any optional fields omitted.
mkDescribeCoipPools ::
  DescribeCoipPools
mkDescribeCoipPools =
  DescribeCoipPools'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      poolIds = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpDryRun :: Lens.Lens' DescribeCoipPools (Core.Maybe Core.Bool)
dcpDryRun = Lens.field @"dryRun"
{-# DEPRECATED dcpDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The filters. The following are the possible values:
--
--
--     * @coip-pool.pool-id@
--
--
--
--     * @coip-pool.local-gateway-route-table-id@
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpFilters :: Lens.Lens' DescribeCoipPools (Core.Maybe [Types.Filter])
dcpFilters = Lens.field @"filters"
{-# DEPRECATED dcpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpMaxResults :: Lens.Lens' DescribeCoipPools (Core.Maybe Core.Natural)
dcpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dcpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpNextToken :: Lens.Lens' DescribeCoipPools (Core.Maybe Types.String)
dcpNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The IDs of the address pools.
--
-- /Note:/ Consider using 'poolIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpPoolIds :: Lens.Lens' DescribeCoipPools (Core.Maybe [Types.CoipPoolId])
dcpPoolIds = Lens.field @"poolIds"
{-# DEPRECATED dcpPoolIds "Use generic-lens or generic-optics with 'poolIds' instead." #-}

instance Core.AWSRequest DescribeCoipPools where
  type Rs DescribeCoipPools = DescribeCoipPoolsResponse
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
            ( Core.pure ("Action", "DescribeCoipPools")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> (Core.toQueryList "PoolId" Core.<$> poolIds)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeCoipPoolsResponse'
            Core.<$> (x Core..@? "coipPoolSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeCoipPools where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"coipPools" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeCoipPoolsResponse' smart constructor.
data DescribeCoipPoolsResponse = DescribeCoipPoolsResponse'
  { -- | Information about the address pools.
    coipPools :: Core.Maybe [Types.CoipPool],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCoipPoolsResponse' value with any optional fields omitted.
mkDescribeCoipPoolsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCoipPoolsResponse
mkDescribeCoipPoolsResponse responseStatus =
  DescribeCoipPoolsResponse'
    { coipPools = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the address pools.
--
-- /Note:/ Consider using 'coipPools' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsCoipPools :: Lens.Lens' DescribeCoipPoolsResponse (Core.Maybe [Types.CoipPool])
dcprrsCoipPools = Lens.field @"coipPools"
{-# DEPRECATED dcprrsCoipPools "Use generic-lens or generic-optics with 'coipPools' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsNextToken :: Lens.Lens' DescribeCoipPoolsResponse (Core.Maybe Types.String)
dcprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsResponseStatus :: Lens.Lens' DescribeCoipPoolsResponse Core.Int
dcprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
