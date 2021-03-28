{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeListeners
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified listeners or the listeners for the specified Application Load Balancer, Network Load Balancer, or Gateway Load Balancer. You must specify either a load balancer or one or more listeners.
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeListeners
    (
    -- * Creating a request
      DescribeListeners (..)
    , mkDescribeListeners
    -- ** Request lenses
    , dlListenerArns
    , dlLoadBalancerArn
    , dlMarker
    , dlPageSize

    -- * Destructuring the response
    , DescribeListenersResponse (..)
    , mkDescribeListenersResponse
    -- ** Response lenses
    , dlrfrsListeners
    , dlrfrsNextMarker
    , dlrfrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeListeners' smart constructor.
data DescribeListeners = DescribeListeners'
  { listenerArns :: Core.Maybe [Types.ListenerArn]
    -- ^ The Amazon Resource Names (ARN) of the listeners.
  , loadBalancerArn :: Core.Maybe Types.LoadBalancerArn
    -- ^ The Amazon Resource Name (ARN) of the load balancer.
  , marker :: Core.Maybe Types.Marker
    -- ^ The marker for the next set of results. (You received this marker from a previous call.)
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with this call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeListeners' value with any optional fields omitted.
mkDescribeListeners
    :: DescribeListeners
mkDescribeListeners
  = DescribeListeners'{listenerArns = Core.Nothing,
                       loadBalancerArn = Core.Nothing, marker = Core.Nothing,
                       pageSize = Core.Nothing}

-- | The Amazon Resource Names (ARN) of the listeners.
--
-- /Note:/ Consider using 'listenerArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlListenerArns :: Lens.Lens' DescribeListeners (Core.Maybe [Types.ListenerArn])
dlListenerArns = Lens.field @"listenerArns"
{-# INLINEABLE dlListenerArns #-}
{-# DEPRECATED listenerArns "Use generic-lens or generic-optics with 'listenerArns' instead"  #-}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLoadBalancerArn :: Lens.Lens' DescribeListeners (Core.Maybe Types.LoadBalancerArn)
dlLoadBalancerArn = Lens.field @"loadBalancerArn"
{-# INLINEABLE dlLoadBalancerArn #-}
{-# DEPRECATED loadBalancerArn "Use generic-lens or generic-optics with 'loadBalancerArn' instead"  #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlMarker :: Lens.Lens' DescribeListeners (Core.Maybe Types.Marker)
dlMarker = Lens.field @"marker"
{-# INLINEABLE dlMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of results to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlPageSize :: Lens.Lens' DescribeListeners (Core.Maybe Core.Natural)
dlPageSize = Lens.field @"pageSize"
{-# INLINEABLE dlPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

instance Core.ToQuery DescribeListeners where
        toQuery DescribeListeners{..}
          = Core.toQueryPair "Action" ("DescribeListeners" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "ListenerArns"
                (Core.maybe Core.mempty (Core.toQueryList "member") listenerArns)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LoadBalancerArn")
                loadBalancerArn
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PageSize") pageSize

instance Core.ToHeaders DescribeListeners where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeListeners where
        type Rs DescribeListeners = DescribeListenersResponse
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
          = Response.receiveXMLWrapper "DescribeListenersResult"
              (\ s h x ->
                 DescribeListenersResponse' Core.<$>
                   (x Core..@? "Listeners" Core..<@> Core.parseXMLList "member")
                     Core.<*> x Core..@? "NextMarker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeListeners where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"listeners" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkDescribeListenersResponse' smart constructor.
data DescribeListenersResponse = DescribeListenersResponse'
  { listeners :: Core.Maybe [Types.Listener]
    -- ^ Information about the listeners.
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeListenersResponse' value with any optional fields omitted.
mkDescribeListenersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeListenersResponse
mkDescribeListenersResponse responseStatus
  = DescribeListenersResponse'{listeners = Core.Nothing,
                               nextMarker = Core.Nothing, responseStatus}

-- | Information about the listeners.
--
-- /Note:/ Consider using 'listeners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrfrsListeners :: Lens.Lens' DescribeListenersResponse (Core.Maybe [Types.Listener])
dlrfrsListeners = Lens.field @"listeners"
{-# INLINEABLE dlrfrsListeners #-}
{-# DEPRECATED listeners "Use generic-lens or generic-optics with 'listeners' instead"  #-}

-- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrfrsNextMarker :: Lens.Lens' DescribeListenersResponse (Core.Maybe Types.NextMarker)
dlrfrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE dlrfrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrfrsResponseStatus :: Lens.Lens' DescribeListenersResponse Core.Int
dlrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
