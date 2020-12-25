{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeLoadBalancers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified the load balancers. If no load balancers are specified, the call describes all of your load balancers.
--
-- This operation returns paginated results.
module Network.AWS.ELB.DescribeLoadBalancers
  ( -- * Creating a request
    DescribeLoadBalancers (..),
    mkDescribeLoadBalancers,

    -- ** Request lenses
    dlbLoadBalancerNames,
    dlbMarker,
    dlbPageSize,

    -- * Destructuring the response
    DescribeLoadBalancersResponse (..),
    mkDescribeLoadBalancersResponse,

    -- ** Response lenses
    dlbrrsLoadBalancerDescriptions,
    dlbrrsNextMarker,
    dlbrrsResponseStatus,
  )
where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeLoadBalancers.
--
-- /See:/ 'mkDescribeLoadBalancers' smart constructor.
data DescribeLoadBalancers = DescribeLoadBalancers'
  { -- | The names of the load balancers.
    loadBalancerNames :: Core.Maybe [Types.AccessPointName],
    -- | The marker for the next set of results. (You received this marker from a previous call.)
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of results to return with this call (a number from 1 to 400). The default is 400.
    pageSize :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLoadBalancers' value with any optional fields omitted.
mkDescribeLoadBalancers ::
  DescribeLoadBalancers
mkDescribeLoadBalancers =
  DescribeLoadBalancers'
    { loadBalancerNames = Core.Nothing,
      marker = Core.Nothing,
      pageSize = Core.Nothing
    }

-- | The names of the load balancers.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbLoadBalancerNames :: Lens.Lens' DescribeLoadBalancers (Core.Maybe [Types.AccessPointName])
dlbLoadBalancerNames = Lens.field @"loadBalancerNames"
{-# DEPRECATED dlbLoadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbMarker :: Lens.Lens' DescribeLoadBalancers (Core.Maybe Types.Marker)
dlbMarker = Lens.field @"marker"
{-# DEPRECATED dlbMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of results to return with this call (a number from 1 to 400). The default is 400.
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
                Core.<> (Core.pure ("Version", "2012-06-01"))
                Core.<> ( Core.toQueryValue
                            "LoadBalancerNames"
                            (Core.toQueryList "member" Core.<$> loadBalancerNames)
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "PageSize" Core.<$> pageSize)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeLoadBalancersResult"
      ( \s h x ->
          DescribeLoadBalancersResponse'
            Core.<$> ( x Core..@? "LoadBalancerDescriptions"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeLoadBalancers where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"loadBalancerDescriptions" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | Contains the parameters for DescribeLoadBalancers.
--
-- /See:/ 'mkDescribeLoadBalancersResponse' smart constructor.
data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse'
  { -- | Information about the load balancers.
    loadBalancerDescriptions :: Core.Maybe [Types.LoadBalancerDescription],
    -- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
    nextMarker :: Core.Maybe Types.NextMarker,
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
    { loadBalancerDescriptions =
        Core.Nothing,
      nextMarker = Core.Nothing,
      responseStatus
    }

-- | Information about the load balancers.
--
-- /Note:/ Consider using 'loadBalancerDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrrsLoadBalancerDescriptions :: Lens.Lens' DescribeLoadBalancersResponse (Core.Maybe [Types.LoadBalancerDescription])
dlbrrsLoadBalancerDescriptions = Lens.field @"loadBalancerDescriptions"
{-# DEPRECATED dlbrrsLoadBalancerDescriptions "Use generic-lens or generic-optics with 'loadBalancerDescriptions' instead." #-}

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrrsNextMarker :: Lens.Lens' DescribeLoadBalancersResponse (Core.Maybe Types.NextMarker)
dlbrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED dlbrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrrsResponseStatus :: Lens.Lens' DescribeLoadBalancersResponse Core.Int
dlbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
