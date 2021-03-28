{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the queues for the specified Amazon Connect instance.
--
-- For more information about queues, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-queues-standard-and-agent.html Queues: Standard and Agent> in the /Amazon Connect Administrator Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListQueues
    (
    -- * Creating a request
      ListQueues (..)
    , mkListQueues
    -- ** Request lenses
    , lqInstanceId
    , lqMaxResults
    , lqNextToken
    , lqQueueTypes

    -- * Destructuring the response
    , ListQueuesResponse (..)
    , mkListQueuesResponse
    -- ** Response lenses
    , lqrrsNextToken
    , lqrrsQueueSummaryList
    , lqrrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListQueues' smart constructor.
data ListQueues = ListQueues'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximimum number of results to return per page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
  , queueTypes :: Core.Maybe [Types.QueueType]
    -- ^ The type of queue.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListQueues' value with any optional fields omitted.
mkListQueues
    :: Types.InstanceId -- ^ 'instanceId'
    -> ListQueues
mkListQueues instanceId
  = ListQueues'{instanceId, maxResults = Core.Nothing,
                nextToken = Core.Nothing, queueTypes = Core.Nothing}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqInstanceId :: Lens.Lens' ListQueues Types.InstanceId
lqInstanceId = Lens.field @"instanceId"
{-# INLINEABLE lqInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqMaxResults :: Lens.Lens' ListQueues (Core.Maybe Core.Natural)
lqMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lqMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqNextToken :: Lens.Lens' ListQueues (Core.Maybe Types.NextToken)
lqNextToken = Lens.field @"nextToken"
{-# INLINEABLE lqNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The type of queue.
--
-- /Note:/ Consider using 'queueTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqQueueTypes :: Lens.Lens' ListQueues (Core.Maybe [Types.QueueType])
lqQueueTypes = Lens.field @"queueTypes"
{-# INLINEABLE lqQueueTypes #-}
{-# DEPRECATED queueTypes "Use generic-lens or generic-optics with 'queueTypes' instead"  #-}

instance Core.ToQuery ListQueues where
        toQuery ListQueues{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.toQueryPair "queueTypes"
                (Core.maybe Core.mempty (Core.toQueryList "member") queueTypes)

instance Core.ToHeaders ListQueues where
        toHeaders ListQueues{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListQueues where
        type Rs ListQueues = ListQueuesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/queues-summary/" Core.<> Core.toText instanceId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListQueuesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "QueueSummaryList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListQueues where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"queueSummaryList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListQueuesResponse' smart constructor.
data ListQueuesResponse = ListQueuesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If there are additional results, this is the token for the next set of results.
  , queueSummaryList :: Core.Maybe [Types.QueueSummary]
    -- ^ Information about the queues.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListQueuesResponse' value with any optional fields omitted.
mkListQueuesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListQueuesResponse
mkListQueuesResponse responseStatus
  = ListQueuesResponse'{nextToken = Core.Nothing,
                        queueSummaryList = Core.Nothing, responseStatus}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrsNextToken :: Lens.Lens' ListQueuesResponse (Core.Maybe Types.NextToken)
lqrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lqrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the queues.
--
-- /Note:/ Consider using 'queueSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrsQueueSummaryList :: Lens.Lens' ListQueuesResponse (Core.Maybe [Types.QueueSummary])
lqrrsQueueSummaryList = Lens.field @"queueSummaryList"
{-# INLINEABLE lqrrsQueueSummaryList #-}
{-# DEPRECATED queueSummaryList "Use generic-lens or generic-optics with 'queueSummaryList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrsResponseStatus :: Lens.Lens' ListQueuesResponse Core.Int
lqrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lqrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
