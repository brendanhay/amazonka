{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTrafficMirrorTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about one or more Traffic Mirror targets.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTrafficMirrorTargets
  ( -- * Creating a request
    DescribeTrafficMirrorTargets (..),
    mkDescribeTrafficMirrorTargets,

    -- ** Request lenses
    dtmtDryRun,
    dtmtFilters,
    dtmtMaxResults,
    dtmtNextToken,
    dtmtTrafficMirrorTargetIds,

    -- * Destructuring the response
    DescribeTrafficMirrorTargetsResponse (..),
    mkDescribeTrafficMirrorTargetsResponse,

    -- ** Response lenses
    dtmtrrsNextToken,
    dtmtrrsTrafficMirrorTargets,
    dtmtrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTrafficMirrorTargets' smart constructor.
data DescribeTrafficMirrorTargets = DescribeTrafficMirrorTargets'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters. The possible values are:
    --
    --
    --     * @description@ : The Traffic Mirror target description.
    --
    --
    --     * @network-interface-id@ : The ID of the Traffic Mirror session network interface.
    --
    --
    --     * @network-load-balancer-arn@ : The Amazon Resource Name (ARN) of the Network Load Balancer that is associated with the session.
    --
    --
    --     * @owner-id@ : The ID of the account that owns the Traffic Mirror session.
    --
    --
    --     * @traffic-mirror-target-id@ : The ID of the Traffic Mirror target.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The ID of the Traffic Mirror targets.
    trafficMirrorTargetIds :: Core.Maybe [Types.TrafficMirrorTargetId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrafficMirrorTargets' value with any optional fields omitted.
mkDescribeTrafficMirrorTargets ::
  DescribeTrafficMirrorTargets
mkDescribeTrafficMirrorTargets =
  DescribeTrafficMirrorTargets'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      trafficMirrorTargetIds = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtDryRun :: Lens.Lens' DescribeTrafficMirrorTargets (Core.Maybe Core.Bool)
dtmtDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtmtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters. The possible values are:
--
--
--     * @description@ : The Traffic Mirror target description.
--
--
--     * @network-interface-id@ : The ID of the Traffic Mirror session network interface.
--
--
--     * @network-load-balancer-arn@ : The Amazon Resource Name (ARN) of the Network Load Balancer that is associated with the session.
--
--
--     * @owner-id@ : The ID of the account that owns the Traffic Mirror session.
--
--
--     * @traffic-mirror-target-id@ : The ID of the Traffic Mirror target.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtFilters :: Lens.Lens' DescribeTrafficMirrorTargets (Core.Maybe [Types.Filter])
dtmtFilters = Lens.field @"filters"
{-# DEPRECATED dtmtFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtMaxResults :: Lens.Lens' DescribeTrafficMirrorTargets (Core.Maybe Core.Natural)
dtmtMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dtmtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtNextToken :: Lens.Lens' DescribeTrafficMirrorTargets (Core.Maybe Types.NextToken)
dtmtNextToken = Lens.field @"nextToken"
{-# DEPRECATED dtmtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the Traffic Mirror targets.
--
-- /Note:/ Consider using 'trafficMirrorTargetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtTrafficMirrorTargetIds :: Lens.Lens' DescribeTrafficMirrorTargets (Core.Maybe [Types.TrafficMirrorTargetId])
dtmtTrafficMirrorTargetIds = Lens.field @"trafficMirrorTargetIds"
{-# DEPRECATED dtmtTrafficMirrorTargetIds "Use generic-lens or generic-optics with 'trafficMirrorTargetIds' instead." #-}

instance Core.AWSRequest DescribeTrafficMirrorTargets where
  type
    Rs DescribeTrafficMirrorTargets =
      DescribeTrafficMirrorTargetsResponse
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
            ( Core.pure ("Action", "DescribeTrafficMirrorTargets")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> ( Core.toQueryList "TrafficMirrorTargetId"
                            Core.<$> trafficMirrorTargetIds
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTrafficMirrorTargetsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "trafficMirrorTargetSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeTrafficMirrorTargets where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"trafficMirrorTargets" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeTrafficMirrorTargetsResponse' smart constructor.
data DescribeTrafficMirrorTargetsResponse = DescribeTrafficMirrorTargetsResponse'
  { -- | The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about one or more Traffic Mirror targets.
    trafficMirrorTargets :: Core.Maybe [Types.TrafficMirrorTarget],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrafficMirrorTargetsResponse' value with any optional fields omitted.
mkDescribeTrafficMirrorTargetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTrafficMirrorTargetsResponse
mkDescribeTrafficMirrorTargetsResponse responseStatus =
  DescribeTrafficMirrorTargetsResponse'
    { nextToken = Core.Nothing,
      trafficMirrorTargets = Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtrrsNextToken :: Lens.Lens' DescribeTrafficMirrorTargetsResponse (Core.Maybe Types.NextToken)
dtmtrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dtmtrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about one or more Traffic Mirror targets.
--
-- /Note:/ Consider using 'trafficMirrorTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtrrsTrafficMirrorTargets :: Lens.Lens' DescribeTrafficMirrorTargetsResponse (Core.Maybe [Types.TrafficMirrorTarget])
dtmtrrsTrafficMirrorTargets = Lens.field @"trafficMirrorTargets"
{-# DEPRECATED dtmtrrsTrafficMirrorTargets "Use generic-lens or generic-optics with 'trafficMirrorTargets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmtrrsResponseStatus :: Lens.Lens' DescribeTrafficMirrorTargetsResponse Core.Int
dtmtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtmtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
