{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeTargetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified target groups or all of your target groups. By default, all target groups are described. Alternatively, you can specify one of the following to filter the results: the ARN of the load balancer, the names of one or more target groups, or the ARNs of one or more target groups.
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeTargetGroups
  ( -- * Creating a request
    DescribeTargetGroups (..),
    mkDescribeTargetGroups,

    -- ** Request lenses
    dtgLoadBalancerArn,
    dtgMarker,
    dtgNames,
    dtgPageSize,
    dtgTargetGroupArns,

    -- * Destructuring the response
    DescribeTargetGroupsResponse (..),
    mkDescribeTargetGroupsResponse,

    -- ** Response lenses
    drsNextMarker,
    drsTargetGroups,
    drsResponseStatus,
  )
where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTargetGroups' smart constructor.
data DescribeTargetGroups = DescribeTargetGroups'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Core.Maybe Types.LoadBalancerArn,
    -- | The marker for the next set of results. (You received this marker from a previous call.)
    marker :: Core.Maybe Types.Marker,
    -- | The names of the target groups.
    names :: Core.Maybe [Types.TargetGroupName],
    -- | The maximum number of results to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Names (ARN) of the target groups.
    targetGroupArns :: Core.Maybe [Types.TargetGroupArn]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTargetGroups' value with any optional fields omitted.
mkDescribeTargetGroups ::
  DescribeTargetGroups
mkDescribeTargetGroups =
  DescribeTargetGroups'
    { loadBalancerArn = Core.Nothing,
      marker = Core.Nothing,
      names = Core.Nothing,
      pageSize = Core.Nothing,
      targetGroupArns = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgLoadBalancerArn :: Lens.Lens' DescribeTargetGroups (Core.Maybe Types.LoadBalancerArn)
dtgLoadBalancerArn = Lens.field @"loadBalancerArn"
{-# DEPRECATED dtgLoadBalancerArn "Use generic-lens or generic-optics with 'loadBalancerArn' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgMarker :: Lens.Lens' DescribeTargetGroups (Core.Maybe Types.Marker)
dtgMarker = Lens.field @"marker"
{-# DEPRECATED dtgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The names of the target groups.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgNames :: Lens.Lens' DescribeTargetGroups (Core.Maybe [Types.TargetGroupName])
dtgNames = Lens.field @"names"
{-# DEPRECATED dtgNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The maximum number of results to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgPageSize :: Lens.Lens' DescribeTargetGroups (Core.Maybe Core.Natural)
dtgPageSize = Lens.field @"pageSize"
{-# DEPRECATED dtgPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The Amazon Resource Names (ARN) of the target groups.
--
-- /Note:/ Consider using 'targetGroupArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgTargetGroupArns :: Lens.Lens' DescribeTargetGroups (Core.Maybe [Types.TargetGroupArn])
dtgTargetGroupArns = Lens.field @"targetGroupArns"
{-# DEPRECATED dtgTargetGroupArns "Use generic-lens or generic-optics with 'targetGroupArns' instead." #-}

instance Core.AWSRequest DescribeTargetGroups where
  type Rs DescribeTargetGroups = DescribeTargetGroupsResponse
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
            ( Core.pure ("Action", "DescribeTargetGroups")
                Core.<> (Core.pure ("Version", "2015-12-01"))
                Core.<> (Core.toQueryValue "LoadBalancerArn" Core.<$> loadBalancerArn)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> ( Core.toQueryValue
                            "Names"
                            (Core.toQueryList "member" Core.<$> names)
                        )
                Core.<> (Core.toQueryValue "PageSize" Core.<$> pageSize)
                Core.<> ( Core.toQueryValue
                            "TargetGroupArns"
                            (Core.toQueryList "member" Core.<$> targetGroupArns)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeTargetGroupsResult"
      ( \s h x ->
          DescribeTargetGroupsResponse'
            Core.<$> (x Core..@? "NextMarker")
            Core.<*> (x Core..@? "TargetGroups" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeTargetGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"targetGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkDescribeTargetGroupsResponse' smart constructor.
data DescribeTargetGroupsResponse = DescribeTargetGroupsResponse'
  { -- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
    nextMarker :: Core.Maybe Types.NextMarker,
    -- | Information about the target groups.
    targetGroups :: Core.Maybe [Types.TargetGroup],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTargetGroupsResponse' value with any optional fields omitted.
mkDescribeTargetGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTargetGroupsResponse
mkDescribeTargetGroupsResponse responseStatus =
  DescribeTargetGroupsResponse'
    { nextMarker = Core.Nothing,
      targetGroups = Core.Nothing,
      responseStatus
    }

-- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextMarker :: Lens.Lens' DescribeTargetGroupsResponse (Core.Maybe Types.NextMarker)
drsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED drsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Information about the target groups.
--
-- /Note:/ Consider using 'targetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsTargetGroups :: Lens.Lens' DescribeTargetGroupsResponse (Core.Maybe [Types.TargetGroup])
drsTargetGroups = Lens.field @"targetGroups"
{-# DEPRECATED drsTargetGroups "Use generic-lens or generic-optics with 'targetGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeTargetGroupsResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
