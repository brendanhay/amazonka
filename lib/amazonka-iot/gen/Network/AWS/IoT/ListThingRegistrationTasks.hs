{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListThingRegistrationTasks (..),
    mkListThingRegistrationTasks,

    -- ** Request lenses
    ltrtMaxResults,
    ltrtNextToken,
    ltrtStatus,

    -- * Destructuring the response
    ListThingRegistrationTasksResponse (..),
    mkListThingRegistrationTasksResponse,

    -- ** Response lenses
    ltrtrrsNextToken,
    ltrtrrsTaskIds,
    ltrtrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListThingRegistrationTasks' smart constructor.
data ListThingRegistrationTasks = ListThingRegistrationTasks'
  { -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The status of the bulk thing provisioning task.
    status :: Core.Maybe Types.TaskStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingRegistrationTasks' value with any optional fields omitted.
mkListThingRegistrationTasks ::
  ListThingRegistrationTasks
mkListThingRegistrationTasks =
  ListThingRegistrationTasks'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      status = Core.Nothing
    }

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtMaxResults :: Lens.Lens' ListThingRegistrationTasks (Core.Maybe Core.Natural)
ltrtMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltrtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtNextToken :: Lens.Lens' ListThingRegistrationTasks (Core.Maybe Types.NextToken)
ltrtNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltrtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The status of the bulk thing provisioning task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtStatus :: Lens.Lens' ListThingRegistrationTasks (Core.Maybe Types.TaskStatus)
ltrtStatus = Lens.field @"status"
{-# DEPRECATED ltrtStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.AWSRequest ListThingRegistrationTasks where
  type
    Rs ListThingRegistrationTasks =
      ListThingRegistrationTasksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/thing-registration-tasks",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "status" Core.<$> status),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingRegistrationTasksResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "taskIds")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListThingRegistrationTasks where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"taskIds" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListThingRegistrationTasksResponse' smart constructor.
data ListThingRegistrationTasksResponse = ListThingRegistrationTasksResponse'
  { -- | The token to use to get the next set of results, or __null__ if there are no additional results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of bulk thing provisioning task IDs.
    taskIds :: Core.Maybe [Types.TaskId],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingRegistrationTasksResponse' value with any optional fields omitted.
mkListThingRegistrationTasksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListThingRegistrationTasksResponse
mkListThingRegistrationTasksResponse responseStatus =
  ListThingRegistrationTasksResponse'
    { nextToken = Core.Nothing,
      taskIds = Core.Nothing,
      responseStatus
    }

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrsNextToken :: Lens.Lens' ListThingRegistrationTasksResponse (Core.Maybe Types.NextToken)
ltrtrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltrtrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of bulk thing provisioning task IDs.
--
-- /Note:/ Consider using 'taskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrsTaskIds :: Lens.Lens' ListThingRegistrationTasksResponse (Core.Maybe [Types.TaskId])
ltrtrrsTaskIds = Lens.field @"taskIds"
{-# DEPRECATED ltrtrrsTaskIds "Use generic-lens or generic-optics with 'taskIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrsResponseStatus :: Lens.Lens' ListThingRegistrationTasksResponse Core.Int
ltrtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltrtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
