{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTrafficMirrorFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Traffic Mirror filters.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTrafficMirrorFilters
  ( -- * Creating a request
    DescribeTrafficMirrorFilters (..),
    mkDescribeTrafficMirrorFilters,

    -- ** Request lenses
    dtmfDryRun,
    dtmfFilters,
    dtmfMaxResults,
    dtmfNextToken,
    dtmfTrafficMirrorFilterIds,

    -- * Destructuring the response
    DescribeTrafficMirrorFiltersResponse (..),
    mkDescribeTrafficMirrorFiltersResponse,

    -- ** Response lenses
    dtmfrrsNextToken,
    dtmfrrsTrafficMirrorFilters,
    dtmfrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTrafficMirrorFilters' smart constructor.
data DescribeTrafficMirrorFilters = DescribeTrafficMirrorFilters'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters. The possible values are:
    --
    --
    --     * @description@ : The Traffic Mirror filter description.
    --
    --
    --     * @traffic-mirror-filter-id@ : The ID of the Traffic Mirror filter.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterIds :: Core.Maybe [Types.TrafficMirrorFilterId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrafficMirrorFilters' value with any optional fields omitted.
mkDescribeTrafficMirrorFilters ::
  DescribeTrafficMirrorFilters
mkDescribeTrafficMirrorFilters =
  DescribeTrafficMirrorFilters'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      trafficMirrorFilterIds = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfDryRun :: Lens.Lens' DescribeTrafficMirrorFilters (Core.Maybe Core.Bool)
dtmfDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtmfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters. The possible values are:
--
--
--     * @description@ : The Traffic Mirror filter description.
--
--
--     * @traffic-mirror-filter-id@ : The ID of the Traffic Mirror filter.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfFilters :: Lens.Lens' DescribeTrafficMirrorFilters (Core.Maybe [Types.Filter])
dtmfFilters = Lens.field @"filters"
{-# DEPRECATED dtmfFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfMaxResults :: Lens.Lens' DescribeTrafficMirrorFilters (Core.Maybe Core.Natural)
dtmfMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dtmfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfNextToken :: Lens.Lens' DescribeTrafficMirrorFilters (Core.Maybe Types.NextToken)
dtmfNextToken = Lens.field @"nextToken"
{-# DEPRECATED dtmfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfTrafficMirrorFilterIds :: Lens.Lens' DescribeTrafficMirrorFilters (Core.Maybe [Types.TrafficMirrorFilterId])
dtmfTrafficMirrorFilterIds = Lens.field @"trafficMirrorFilterIds"
{-# DEPRECATED dtmfTrafficMirrorFilterIds "Use generic-lens or generic-optics with 'trafficMirrorFilterIds' instead." #-}

instance Core.AWSRequest DescribeTrafficMirrorFilters where
  type
    Rs DescribeTrafficMirrorFilters =
      DescribeTrafficMirrorFiltersResponse
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
            ( Core.pure ("Action", "DescribeTrafficMirrorFilters")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> ( Core.toQueryList "TrafficMirrorFilterId"
                            Core.<$> trafficMirrorFilterIds
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTrafficMirrorFiltersResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "trafficMirrorFilterSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeTrafficMirrorFilters where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"trafficMirrorFilters" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeTrafficMirrorFiltersResponse' smart constructor.
data DescribeTrafficMirrorFiltersResponse = DescribeTrafficMirrorFiltersResponse'
  { -- | The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | Information about one or more Traffic Mirror filters.
    trafficMirrorFilters :: Core.Maybe [Types.TrafficMirrorFilter],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrafficMirrorFiltersResponse' value with any optional fields omitted.
mkDescribeTrafficMirrorFiltersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTrafficMirrorFiltersResponse
mkDescribeTrafficMirrorFiltersResponse responseStatus =
  DescribeTrafficMirrorFiltersResponse'
    { nextToken = Core.Nothing,
      trafficMirrorFilters = Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrrsNextToken :: Lens.Lens' DescribeTrafficMirrorFiltersResponse (Core.Maybe Types.String)
dtmfrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dtmfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about one or more Traffic Mirror filters.
--
-- /Note:/ Consider using 'trafficMirrorFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrrsTrafficMirrorFilters :: Lens.Lens' DescribeTrafficMirrorFiltersResponse (Core.Maybe [Types.TrafficMirrorFilter])
dtmfrrsTrafficMirrorFilters = Lens.field @"trafficMirrorFilters"
{-# DEPRECATED dtmfrrsTrafficMirrorFilters "Use generic-lens or generic-optics with 'trafficMirrorFilters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrrsResponseStatus :: Lens.Lens' DescribeTrafficMirrorFiltersResponse Core.Int
dtmfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtmfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
