{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeLoadBalancers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the load balancers for the specified Auto Scaling group.
--
-- This operation describes only Classic Load Balancers. If you have Application Load Balancers, Network Load Balancers, or Gateway Load Balancers, use the 'DescribeLoadBalancerTargetGroups' API instead.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeLoadBalancers
  ( -- * Creating a request
    DescribeLoadBalancers (..),
    mkDescribeLoadBalancers,

    -- ** Request lenses
    dlbAutoScalingGroupName,
    dlbMaxRecords,
    dlbNextToken,

    -- * Destructuring the response
    DescribeLoadBalancersResponse (..),
    mkDescribeLoadBalancersResponse,

    -- ** Response lenses
    dlbrrsLoadBalancers,
    dlbrrsNextToken,
    dlbrrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLoadBalancers' smart constructor.
data DescribeLoadBalancers = DescribeLoadBalancers'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.ResourceName,
    -- | The maximum number of items to return with this call. The default value is @100@ and the maximum value is @100@ .
    maxRecords :: Core.Maybe Core.Int,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.XmlString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLoadBalancers' value with any optional fields omitted.
mkDescribeLoadBalancers ::
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  DescribeLoadBalancers
mkDescribeLoadBalancers autoScalingGroupName =
  DescribeLoadBalancers'
    { autoScalingGroupName,
      maxRecords = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbAutoScalingGroupName :: Lens.Lens' DescribeLoadBalancers Types.ResourceName
dlbAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED dlbAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The maximum number of items to return with this call. The default value is @100@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbMaxRecords :: Lens.Lens' DescribeLoadBalancers (Core.Maybe Core.Int)
dlbMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dlbMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbNextToken :: Lens.Lens' DescribeLoadBalancers (Core.Maybe Types.XmlString)
dlbNextToken = Lens.field @"nextToken"
{-# DEPRECATED dlbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeLoadBalancers where
  type Rs DescribeLoadBalancers = DescribeLoadBalancersResponse
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
            ( Core.pure ("Action", "DescribeLoadBalancers")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeLoadBalancersResult"
      ( \s h x ->
          DescribeLoadBalancersResponse'
            Core.<$> (x Core..@? "LoadBalancers" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeLoadBalancers where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"loadBalancers" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeLoadBalancersResponse' smart constructor.
data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse'
  { -- | The load balancers.
    loadBalancers :: Core.Maybe [Types.LoadBalancerState],
    -- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
    nextToken :: Core.Maybe Types.XmlString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLoadBalancersResponse' value with any optional fields omitted.
mkDescribeLoadBalancersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLoadBalancersResponse
mkDescribeLoadBalancersResponse responseStatus =
  DescribeLoadBalancersResponse'
    { loadBalancers = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The load balancers.
--
-- /Note:/ Consider using 'loadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrrsLoadBalancers :: Lens.Lens' DescribeLoadBalancersResponse (Core.Maybe [Types.LoadBalancerState])
dlbrrsLoadBalancers = Lens.field @"loadBalancers"
{-# DEPRECATED dlbrrsLoadBalancers "Use generic-lens or generic-optics with 'loadBalancers' instead." #-}

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrrsNextToken :: Lens.Lens' DescribeLoadBalancersResponse (Core.Maybe Types.XmlString)
dlbrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dlbrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrrsResponseStatus :: Lens.Lens' DescribeLoadBalancersResponse Core.Int
dlbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
