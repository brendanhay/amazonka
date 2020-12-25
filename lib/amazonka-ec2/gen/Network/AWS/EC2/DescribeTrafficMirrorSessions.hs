{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTrafficMirrorSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Traffic Mirror sessions. By default, all Traffic Mirror sessions are described. Alternatively, you can filter the results.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTrafficMirrorSessions
  ( -- * Creating a request
    DescribeTrafficMirrorSessions (..),
    mkDescribeTrafficMirrorSessions,

    -- ** Request lenses
    dtmsDryRun,
    dtmsFilters,
    dtmsMaxResults,
    dtmsNextToken,
    dtmsTrafficMirrorSessionIds,

    -- * Destructuring the response
    DescribeTrafficMirrorSessionsResponse (..),
    mkDescribeTrafficMirrorSessionsResponse,

    -- ** Response lenses
    dtmsrrsNextToken,
    dtmsrrsTrafficMirrorSessions,
    dtmsrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTrafficMirrorSessions' smart constructor.
data DescribeTrafficMirrorSessions = DescribeTrafficMirrorSessions'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters. The possible values are:
    --
    --
    --     * @description@ : The Traffic Mirror session description.
    --
    --
    --     * @network-interface-id@ : The ID of the Traffic Mirror session network interface.
    --
    --
    --     * @owner-id@ : The ID of the account that owns the Traffic Mirror session.
    --
    --
    --     * @packet-length@ : The assigned number of packets to mirror.
    --
    --
    --     * @session-number@ : The assigned session number.
    --
    --
    --     * @traffic-mirror-filter-id@ : The ID of the Traffic Mirror filter.
    --
    --
    --     * @traffic-mirror-session-id@ : The ID of the Traffic Mirror session.
    --
    --
    --     * @traffic-mirror-target-id@ : The ID of the Traffic Mirror target.
    --
    --
    --     * @virtual-network-id@ : The virtual network ID of the Traffic Mirror session.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The ID of the Traffic Mirror session.
    trafficMirrorSessionIds :: Core.Maybe [Types.TrafficMirrorSessionId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrafficMirrorSessions' value with any optional fields omitted.
mkDescribeTrafficMirrorSessions ::
  DescribeTrafficMirrorSessions
mkDescribeTrafficMirrorSessions =
  DescribeTrafficMirrorSessions'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      trafficMirrorSessionIds = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsDryRun :: Lens.Lens' DescribeTrafficMirrorSessions (Core.Maybe Core.Bool)
dtmsDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtmsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters. The possible values are:
--
--
--     * @description@ : The Traffic Mirror session description.
--
--
--     * @network-interface-id@ : The ID of the Traffic Mirror session network interface.
--
--
--     * @owner-id@ : The ID of the account that owns the Traffic Mirror session.
--
--
--     * @packet-length@ : The assigned number of packets to mirror.
--
--
--     * @session-number@ : The assigned session number.
--
--
--     * @traffic-mirror-filter-id@ : The ID of the Traffic Mirror filter.
--
--
--     * @traffic-mirror-session-id@ : The ID of the Traffic Mirror session.
--
--
--     * @traffic-mirror-target-id@ : The ID of the Traffic Mirror target.
--
--
--     * @virtual-network-id@ : The virtual network ID of the Traffic Mirror session.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsFilters :: Lens.Lens' DescribeTrafficMirrorSessions (Core.Maybe [Types.Filter])
dtmsFilters = Lens.field @"filters"
{-# DEPRECATED dtmsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsMaxResults :: Lens.Lens' DescribeTrafficMirrorSessions (Core.Maybe Core.Natural)
dtmsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dtmsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsNextToken :: Lens.Lens' DescribeTrafficMirrorSessions (Core.Maybe Types.NextToken)
dtmsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dtmsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the Traffic Mirror session.
--
-- /Note:/ Consider using 'trafficMirrorSessionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsTrafficMirrorSessionIds :: Lens.Lens' DescribeTrafficMirrorSessions (Core.Maybe [Types.TrafficMirrorSessionId])
dtmsTrafficMirrorSessionIds = Lens.field @"trafficMirrorSessionIds"
{-# DEPRECATED dtmsTrafficMirrorSessionIds "Use generic-lens or generic-optics with 'trafficMirrorSessionIds' instead." #-}

instance Core.AWSRequest DescribeTrafficMirrorSessions where
  type
    Rs DescribeTrafficMirrorSessions =
      DescribeTrafficMirrorSessionsResponse
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
            ( Core.pure ("Action", "DescribeTrafficMirrorSessions")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> ( Core.toQueryList "TrafficMirrorSessionId"
                            Core.<$> trafficMirrorSessionIds
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTrafficMirrorSessionsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "trafficMirrorSessionSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeTrafficMirrorSessions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"trafficMirrorSessions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeTrafficMirrorSessionsResponse' smart constructor.
data DescribeTrafficMirrorSessionsResponse = DescribeTrafficMirrorSessionsResponse'
  { -- | The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | Describes one or more Traffic Mirror sessions. By default, all Traffic Mirror sessions are described. Alternatively, you can filter the results.
    trafficMirrorSessions :: Core.Maybe [Types.TrafficMirrorSession],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrafficMirrorSessionsResponse' value with any optional fields omitted.
mkDescribeTrafficMirrorSessionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTrafficMirrorSessionsResponse
mkDescribeTrafficMirrorSessionsResponse responseStatus =
  DescribeTrafficMirrorSessionsResponse'
    { nextToken = Core.Nothing,
      trafficMirrorSessions = Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsrrsNextToken :: Lens.Lens' DescribeTrafficMirrorSessionsResponse (Core.Maybe Types.String)
dtmsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dtmsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Describes one or more Traffic Mirror sessions. By default, all Traffic Mirror sessions are described. Alternatively, you can filter the results.
--
-- /Note:/ Consider using 'trafficMirrorSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsrrsTrafficMirrorSessions :: Lens.Lens' DescribeTrafficMirrorSessionsResponse (Core.Maybe [Types.TrafficMirrorSession])
dtmsrrsTrafficMirrorSessions = Lens.field @"trafficMirrorSessions"
{-# DEPRECATED dtmsrrsTrafficMirrorSessions "Use generic-lens or generic-optics with 'trafficMirrorSessions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmsrrsResponseStatus :: Lens.Lens' DescribeTrafficMirrorSessionsResponse Core.Int
dtmsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtmsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
