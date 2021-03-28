{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeTrafficMirrorFilters (..)
    , mkDescribeTrafficMirrorFilters
    -- ** Request lenses
    , dtmfDryRun
    , dtmfFilters
    , dtmfMaxResults
    , dtmfNextToken
    , dtmfTrafficMirrorFilterIds

    -- * Destructuring the response
    , DescribeTrafficMirrorFiltersResponse (..)
    , mkDescribeTrafficMirrorFiltersResponse
    -- ** Response lenses
    , dtmfrrsNextToken
    , dtmfrrsTrafficMirrorFilters
    , dtmfrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTrafficMirrorFilters' smart constructor.
data DescribeTrafficMirrorFilters = DescribeTrafficMirrorFilters'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters. The possible values are:
--
--
--     * @description@ : The Traffic Mirror filter description.
--
--
--     * @traffic-mirror-filter-id@ : The ID of the Traffic Mirror filter.
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next page of results.
  , trafficMirrorFilterIds :: Core.Maybe [Types.TrafficMirrorFilterId]
    -- ^ The ID of the Traffic Mirror filter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrafficMirrorFilters' value with any optional fields omitted.
mkDescribeTrafficMirrorFilters
    :: DescribeTrafficMirrorFilters
mkDescribeTrafficMirrorFilters
  = DescribeTrafficMirrorFilters'{dryRun = Core.Nothing,
                                  filters = Core.Nothing, maxResults = Core.Nothing,
                                  nextToken = Core.Nothing, trafficMirrorFilterIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfDryRun :: Lens.Lens' DescribeTrafficMirrorFilters (Core.Maybe Core.Bool)
dtmfDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtmfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

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
{-# INLINEABLE dtmfFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfMaxResults :: Lens.Lens' DescribeTrafficMirrorFilters (Core.Maybe Core.Natural)
dtmfMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dtmfMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfNextToken :: Lens.Lens' DescribeTrafficMirrorFilters (Core.Maybe Types.NextToken)
dtmfNextToken = Lens.field @"nextToken"
{-# INLINEABLE dtmfNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfTrafficMirrorFilterIds :: Lens.Lens' DescribeTrafficMirrorFilters (Core.Maybe [Types.TrafficMirrorFilterId])
dtmfTrafficMirrorFilterIds = Lens.field @"trafficMirrorFilterIds"
{-# INLINEABLE dtmfTrafficMirrorFilterIds #-}
{-# DEPRECATED trafficMirrorFilterIds "Use generic-lens or generic-optics with 'trafficMirrorFilterIds' instead"  #-}

instance Core.ToQuery DescribeTrafficMirrorFilters where
        toQuery DescribeTrafficMirrorFilters{..}
          = Core.toQueryPair "Action"
              ("DescribeTrafficMirrorFilters" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TrafficMirrorFilterId")
                trafficMirrorFilterIds

instance Core.ToHeaders DescribeTrafficMirrorFilters where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeTrafficMirrorFilters where
        type Rs DescribeTrafficMirrorFilters =
             DescribeTrafficMirrorFiltersResponse
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
                 DescribeTrafficMirrorFiltersResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "trafficMirrorFilterSet" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeTrafficMirrorFilters where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"trafficMirrorFilters" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeTrafficMirrorFiltersResponse' smart constructor.
data DescribeTrafficMirrorFiltersResponse = DescribeTrafficMirrorFiltersResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
  , trafficMirrorFilters :: Core.Maybe [Types.TrafficMirrorFilter]
    -- ^ Information about one or more Traffic Mirror filters.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrafficMirrorFiltersResponse' value with any optional fields omitted.
mkDescribeTrafficMirrorFiltersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTrafficMirrorFiltersResponse
mkDescribeTrafficMirrorFiltersResponse responseStatus
  = DescribeTrafficMirrorFiltersResponse'{nextToken = Core.Nothing,
                                          trafficMirrorFilters = Core.Nothing, responseStatus}

-- | The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrrsNextToken :: Lens.Lens' DescribeTrafficMirrorFiltersResponse (Core.Maybe Core.Text)
dtmfrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dtmfrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about one or more Traffic Mirror filters.
--
-- /Note:/ Consider using 'trafficMirrorFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrrsTrafficMirrorFilters :: Lens.Lens' DescribeTrafficMirrorFiltersResponse (Core.Maybe [Types.TrafficMirrorFilter])
dtmfrrsTrafficMirrorFilters = Lens.field @"trafficMirrorFilters"
{-# INLINEABLE dtmfrrsTrafficMirrorFilters #-}
{-# DEPRECATED trafficMirrorFilters "Use generic-lens or generic-optics with 'trafficMirrorFilters' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrrsResponseStatus :: Lens.Lens' DescribeTrafficMirrorFiltersResponse Core.Int
dtmfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtmfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
