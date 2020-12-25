{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeLoadBalancerTargetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the target groups for the specified Auto Scaling group.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeLoadBalancerTargetGroups
  ( -- * Creating a request
    DescribeLoadBalancerTargetGroups (..),
    mkDescribeLoadBalancerTargetGroups,

    -- ** Request lenses
    dlbtgsAutoScalingGroupName,
    dlbtgsMaxRecords,
    dlbtgsNextToken,

    -- * Destructuring the response
    DescribeLoadBalancerTargetGroupsResponse (..),
    mkDescribeLoadBalancerTargetGroupsResponse,

    -- ** Response lenses
    dlbtgrrsLoadBalancerTargetGroups,
    dlbtgrrsNextToken,
    dlbtgrrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLoadBalancerTargetGroups' smart constructor.
data DescribeLoadBalancerTargetGroups = DescribeLoadBalancerTargetGroups'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.ResourceName,
    -- | The maximum number of items to return with this call. The default value is @100@ and the maximum value is @100@ .
    maxRecords :: Core.Maybe Core.Int,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.XmlString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLoadBalancerTargetGroups' value with any optional fields omitted.
mkDescribeLoadBalancerTargetGroups ::
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  DescribeLoadBalancerTargetGroups
mkDescribeLoadBalancerTargetGroups autoScalingGroupName =
  DescribeLoadBalancerTargetGroups'
    { autoScalingGroupName,
      maxRecords = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgsAutoScalingGroupName :: Lens.Lens' DescribeLoadBalancerTargetGroups Types.ResourceName
dlbtgsAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED dlbtgsAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The maximum number of items to return with this call. The default value is @100@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgsMaxRecords :: Lens.Lens' DescribeLoadBalancerTargetGroups (Core.Maybe Core.Int)
dlbtgsMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dlbtgsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgsNextToken :: Lens.Lens' DescribeLoadBalancerTargetGroups (Core.Maybe Types.XmlString)
dlbtgsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dlbtgsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeLoadBalancerTargetGroups where
  type
    Rs DescribeLoadBalancerTargetGroups =
      DescribeLoadBalancerTargetGroupsResponse
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
            ( Core.pure ("Action", "DescribeLoadBalancerTargetGroups")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeLoadBalancerTargetGroupsResult"
      ( \s h x ->
          DescribeLoadBalancerTargetGroupsResponse'
            Core.<$> ( x Core..@? "LoadBalancerTargetGroups"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeLoadBalancerTargetGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"loadBalancerTargetGroups" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeLoadBalancerTargetGroupsResponse' smart constructor.
data DescribeLoadBalancerTargetGroupsResponse = DescribeLoadBalancerTargetGroupsResponse'
  { -- | Information about the target groups.
    loadBalancerTargetGroups :: Core.Maybe [Types.LoadBalancerTargetGroupState],
    -- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
    nextToken :: Core.Maybe Types.XmlString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLoadBalancerTargetGroupsResponse' value with any optional fields omitted.
mkDescribeLoadBalancerTargetGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLoadBalancerTargetGroupsResponse
mkDescribeLoadBalancerTargetGroupsResponse responseStatus =
  DescribeLoadBalancerTargetGroupsResponse'
    { loadBalancerTargetGroups =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the target groups.
--
-- /Note:/ Consider using 'loadBalancerTargetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgrrsLoadBalancerTargetGroups :: Lens.Lens' DescribeLoadBalancerTargetGroupsResponse (Core.Maybe [Types.LoadBalancerTargetGroupState])
dlbtgrrsLoadBalancerTargetGroups = Lens.field @"loadBalancerTargetGroups"
{-# DEPRECATED dlbtgrrsLoadBalancerTargetGroups "Use generic-lens or generic-optics with 'loadBalancerTargetGroups' instead." #-}

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgrrsNextToken :: Lens.Lens' DescribeLoadBalancerTargetGroupsResponse (Core.Maybe Types.XmlString)
dlbtgrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dlbtgrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgrrsResponseStatus :: Lens.Lens' DescribeLoadBalancerTargetGroupsResponse Core.Int
dlbtgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlbtgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
