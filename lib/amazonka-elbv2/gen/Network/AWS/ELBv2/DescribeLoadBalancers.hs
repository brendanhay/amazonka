{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeLoadBalancers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified load balancers or all of your load balancers.
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeLoadBalancers
  ( -- * Creating a request
    DescribeLoadBalancers (..),
    mkDescribeLoadBalancers,

    -- ** Request lenses
    dlbLoadBalancerArns,
    dlbMarker,
    dlbNames,
    dlbPageSize,

    -- * Destructuring the response
    DescribeLoadBalancersResponse (..),
    mkDescribeLoadBalancersResponse,

    -- ** Response lenses
    dlbrrsLoadBalancers,
    dlbrrsNextMarker,
    dlbrrsResponseStatus,
  )
where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLoadBalancers' smart constructor.
data DescribeLoadBalancers = DescribeLoadBalancers'
  { -- | The Amazon Resource Names (ARN) of the load balancers. You can specify up to 20 load balancers in a single call.
    loadBalancerArns :: Core.Maybe [Types.LoadBalancerArn],
    -- | The marker for the next set of results. (You received this marker from a previous call.)
    marker :: Core.Maybe Types.Marker,
    -- | The names of the load balancers.
    names :: Core.Maybe [Types.LoadBalancerName],
    -- | The maximum number of results to return with this call.
    pageSize :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLoadBalancers' value with any optional fields omitted.
mkDescribeLoadBalancers ::
  DescribeLoadBalancers
mkDescribeLoadBalancers =
  DescribeLoadBalancers'
    { loadBalancerArns = Core.Nothing,
      marker = Core.Nothing,
      names = Core.Nothing,
      pageSize = Core.Nothing
    }

-- | The Amazon Resource Names (ARN) of the load balancers. You can specify up to 20 load balancers in a single call.
--
-- /Note:/ Consider using 'loadBalancerArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbLoadBalancerArns :: Lens.Lens' DescribeLoadBalancers (Core.Maybe [Types.LoadBalancerArn])
dlbLoadBalancerArns = Lens.field @"loadBalancerArns"
{-# DEPRECATED dlbLoadBalancerArns "Use generic-lens or generic-optics with 'loadBalancerArns' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbMarker :: Lens.Lens' DescribeLoadBalancers (Core.Maybe Types.Marker)
dlbMarker = Lens.field @"marker"
{-# DEPRECATED dlbMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The names of the load balancers.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbNames :: Lens.Lens' DescribeLoadBalancers (Core.Maybe [Types.LoadBalancerName])
dlbNames = Lens.field @"names"
{-# DEPRECATED dlbNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The maximum number of results to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbPageSize :: Lens.Lens' DescribeLoadBalancers (Core.Maybe Core.Natural)
dlbPageSize = Lens.field @"pageSize"
{-# DEPRECATED dlbPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

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
                Core.<> (Core.pure ("Version", "2015-12-01"))
                Core.<> ( Core.toQueryValue
                            "LoadBalancerArns"
                            (Core.toQueryList "member" Core.<$> loadBalancerArns)
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> ( Core.toQueryValue
                            "Names"
                            (Core.toQueryList "member" Core.<$> names)
                        )
                Core.<> (Core.toQueryValue "PageSize" Core.<$> pageSize)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeLoadBalancersResult"
      ( \s h x ->
          DescribeLoadBalancersResponse'
            Core.<$> (x Core..@? "LoadBalancers" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeLoadBalancers where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"loadBalancers" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkDescribeLoadBalancersResponse' smart constructor.
data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse'
  { -- | Information about the load balancers.
    loadBalancers :: Core.Maybe [Types.LoadBalancer],
    -- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
    nextMarker :: Core.Maybe Types.Marker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeLoadBalancersResponse' value with any optional fields omitted.
mkDescribeLoadBalancersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLoadBalancersResponse
mkDescribeLoadBalancersResponse responseStatus =
  DescribeLoadBalancersResponse'
    { loadBalancers = Core.Nothing,
      nextMarker = Core.Nothing,
      responseStatus
    }

-- | Information about the load balancers.
--
-- /Note:/ Consider using 'loadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrrsLoadBalancers :: Lens.Lens' DescribeLoadBalancersResponse (Core.Maybe [Types.LoadBalancer])
dlbrrsLoadBalancers = Lens.field @"loadBalancers"
{-# DEPRECATED dlbrrsLoadBalancers "Use generic-lens or generic-optics with 'loadBalancers' instead." #-}

-- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrrsNextMarker :: Lens.Lens' DescribeLoadBalancersResponse (Core.Maybe Types.Marker)
dlbrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED dlbrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrrsResponseStatus :: Lens.Lens' DescribeLoadBalancersResponse Core.Int
dlbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
