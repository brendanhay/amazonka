{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThingRegistrationTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List bulk thing provisioning tasks.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingRegistrationTasks
    (
    -- * Creating a request
      ListThingRegistrationTasks (..)
    , mkListThingRegistrationTasks
    -- ** Request lenses
    , ltrtMaxResults
    , ltrtNextToken
    , ltrtStatus

    -- * Destructuring the response
    , ListThingRegistrationTasksResponse (..)
    , mkListThingRegistrationTasksResponse
    -- ** Response lenses
    , ltrtrrsNextToken
    , ltrtrrsTaskIds
    , ltrtrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListThingRegistrationTasks' smart constructor.
data ListThingRegistrationTasks = ListThingRegistrationTasks'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return at one time.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
  , status :: Core.Maybe Types.TaskStatus
    -- ^ The status of the bulk thing provisioning task.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingRegistrationTasks' value with any optional fields omitted.
mkListThingRegistrationTasks
    :: ListThingRegistrationTasks
mkListThingRegistrationTasks
  = ListThingRegistrationTasks'{maxResults = Core.Nothing,
                                nextToken = Core.Nothing, status = Core.Nothing}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtMaxResults :: Lens.Lens' ListThingRegistrationTasks (Core.Maybe Core.Natural)
ltrtMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltrtMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtNextToken :: Lens.Lens' ListThingRegistrationTasks (Core.Maybe Types.NextToken)
ltrtNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltrtNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The status of the bulk thing provisioning task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtStatus :: Lens.Lens' ListThingRegistrationTasks (Core.Maybe Types.TaskStatus)
ltrtStatus = Lens.field @"status"
{-# INLINEABLE ltrtStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.ToQuery ListThingRegistrationTasks where
        toQuery ListThingRegistrationTasks{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "status") status

instance Core.ToHeaders ListThingRegistrationTasks where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListThingRegistrationTasks where
        type Rs ListThingRegistrationTasks =
             ListThingRegistrationTasksResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/thing-registration-tasks",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListThingRegistrationTasksResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "taskIds" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListThingRegistrationTasks where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"taskIds" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListThingRegistrationTasksResponse' smart constructor.
data ListThingRegistrationTasksResponse = ListThingRegistrationTasksResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to get the next set of results, or __null__ if there are no additional results.
  , taskIds :: Core.Maybe [Types.TaskId]
    -- ^ A list of bulk thing provisioning task IDs.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingRegistrationTasksResponse' value with any optional fields omitted.
mkListThingRegistrationTasksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListThingRegistrationTasksResponse
mkListThingRegistrationTasksResponse responseStatus
  = ListThingRegistrationTasksResponse'{nextToken = Core.Nothing,
                                        taskIds = Core.Nothing, responseStatus}

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrsNextToken :: Lens.Lens' ListThingRegistrationTasksResponse (Core.Maybe Types.NextToken)
ltrtrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltrtrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of bulk thing provisioning task IDs.
--
-- /Note:/ Consider using 'taskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrsTaskIds :: Lens.Lens' ListThingRegistrationTasksResponse (Core.Maybe [Types.TaskId])
ltrtrrsTaskIds = Lens.field @"taskIds"
{-# INLINEABLE ltrtrrsTaskIds #-}
{-# DEPRECATED taskIds "Use generic-lens or generic-optics with 'taskIds' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrsResponseStatus :: Lens.Lens' ListThingRegistrationTasksResponse Core.Int
ltrtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltrtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
