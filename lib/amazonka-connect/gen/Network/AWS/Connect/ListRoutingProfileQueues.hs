{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListRoutingProfileQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the queues associated with a routing profile.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListRoutingProfileQueues
    (
    -- * Creating a request
      ListRoutingProfileQueues (..)
    , mkListRoutingProfileQueues
    -- ** Request lenses
    , lrpqInstanceId
    , lrpqRoutingProfileId
    , lrpqMaxResults
    , lrpqNextToken

    -- * Destructuring the response
    , ListRoutingProfileQueuesResponse (..)
    , mkListRoutingProfileQueuesResponse
    -- ** Response lenses
    , lrpqrrsNextToken
    , lrpqrrsRoutingProfileQueueConfigSummaryList
    , lrpqrrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListRoutingProfileQueues' smart constructor.
data ListRoutingProfileQueues = ListRoutingProfileQueues'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , routingProfileId :: Types.RoutingProfileId
    -- ^ The identifier of the routing profile.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximimum number of results to return per page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRoutingProfileQueues' value with any optional fields omitted.
mkListRoutingProfileQueues
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.RoutingProfileId -- ^ 'routingProfileId'
    -> ListRoutingProfileQueues
mkListRoutingProfileQueues instanceId routingProfileId
  = ListRoutingProfileQueues'{instanceId, routingProfileId,
                              maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpqInstanceId :: Lens.Lens' ListRoutingProfileQueues Types.InstanceId
lrpqInstanceId = Lens.field @"instanceId"
{-# INLINEABLE lrpqInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpqRoutingProfileId :: Lens.Lens' ListRoutingProfileQueues Types.RoutingProfileId
lrpqRoutingProfileId = Lens.field @"routingProfileId"
{-# INLINEABLE lrpqRoutingProfileId #-}
{-# DEPRECATED routingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead"  #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpqMaxResults :: Lens.Lens' ListRoutingProfileQueues (Core.Maybe Core.Natural)
lrpqMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lrpqMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpqNextToken :: Lens.Lens' ListRoutingProfileQueues (Core.Maybe Types.NextToken)
lrpqNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrpqNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListRoutingProfileQueues where
        toQuery ListRoutingProfileQueues{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListRoutingProfileQueues where
        toHeaders ListRoutingProfileQueues{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListRoutingProfileQueues where
        type Rs ListRoutingProfileQueues = ListRoutingProfileQueuesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/routing-profiles/" Core.<> Core.toText instanceId Core.<> "/"
                             Core.<> Core.toText routingProfileId
                             Core.<> "/queues",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListRoutingProfileQueuesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "RoutingProfileQueueConfigSummaryList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListRoutingProfileQueues where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"routingProfileQueueConfigSummaryList" Core..
                   Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListRoutingProfileQueuesResponse' smart constructor.
data ListRoutingProfileQueuesResponse = ListRoutingProfileQueuesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If there are additional results, this is the token for the next set of results.
  , routingProfileQueueConfigSummaryList :: Core.Maybe [Types.RoutingProfileQueueConfigSummary]
    -- ^ Information about the routing profiles.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRoutingProfileQueuesResponse' value with any optional fields omitted.
mkListRoutingProfileQueuesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListRoutingProfileQueuesResponse
mkListRoutingProfileQueuesResponse responseStatus
  = ListRoutingProfileQueuesResponse'{nextToken = Core.Nothing,
                                      routingProfileQueueConfigSummaryList = Core.Nothing,
                                      responseStatus}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpqrrsNextToken :: Lens.Lens' ListRoutingProfileQueuesResponse (Core.Maybe Types.NextToken)
lrpqrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrpqrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the routing profiles.
--
-- /Note:/ Consider using 'routingProfileQueueConfigSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpqrrsRoutingProfileQueueConfigSummaryList :: Lens.Lens' ListRoutingProfileQueuesResponse (Core.Maybe [Types.RoutingProfileQueueConfigSummary])
lrpqrrsRoutingProfileQueueConfigSummaryList = Lens.field @"routingProfileQueueConfigSummaryList"
{-# INLINEABLE lrpqrrsRoutingProfileQueueConfigSummaryList #-}
{-# DEPRECATED routingProfileQueueConfigSummaryList "Use generic-lens or generic-optics with 'routingProfileQueueConfigSummaryList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpqrrsResponseStatus :: Lens.Lens' ListRoutingProfileQueuesResponse Core.Int
lrpqrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrpqrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
