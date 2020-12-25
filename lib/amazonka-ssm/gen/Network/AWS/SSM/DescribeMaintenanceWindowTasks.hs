{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tasks in a maintenance window.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowTasks
  ( -- * Creating a request
    DescribeMaintenanceWindowTasks (..),
    mkDescribeMaintenanceWindowTasks,

    -- ** Request lenses
    dWindowId,
    dFilters,
    dMaxResults,
    dNextToken,

    -- * Destructuring the response
    DescribeMaintenanceWindowTasksResponse (..),
    mkDescribeMaintenanceWindowTasksResponse,

    -- ** Response lenses
    dmwtrfrsNextToken,
    dmwtrfrsTasks,
    dmwtrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeMaintenanceWindowTasks' smart constructor.
data DescribeMaintenanceWindowTasks = DescribeMaintenanceWindowTasks'
  { -- | The ID of the maintenance window whose tasks should be retrieved.
    windowId :: Types.WindowId,
    -- | Optional filters used to narrow down the scope of the returned tasks. The supported filter keys are WindowTaskId, TaskArn, Priority, and TaskType.
    filters :: Core.Maybe [Types.MaintenanceWindowFilter],
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMaintenanceWindowTasks' value with any optional fields omitted.
mkDescribeMaintenanceWindowTasks ::
  -- | 'windowId'
  Types.WindowId ->
  DescribeMaintenanceWindowTasks
mkDescribeMaintenanceWindowTasks windowId =
  DescribeMaintenanceWindowTasks'
    { windowId,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the maintenance window whose tasks should be retrieved.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dWindowId :: Lens.Lens' DescribeMaintenanceWindowTasks Types.WindowId
dWindowId = Lens.field @"windowId"
{-# DEPRECATED dWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | Optional filters used to narrow down the scope of the returned tasks. The supported filter keys are WindowTaskId, TaskArn, Priority, and TaskType.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFilters :: Lens.Lens' DescribeMaintenanceWindowTasks (Core.Maybe [Types.MaintenanceWindowFilter])
dFilters = Lens.field @"filters"
{-# DEPRECATED dFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeMaintenanceWindowTasks (Core.Maybe Core.Natural)
dMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeMaintenanceWindowTasks (Core.Maybe Types.NextToken)
dNextToken = Lens.field @"nextToken"
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeMaintenanceWindowTasks where
  toJSON DescribeMaintenanceWindowTasks {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WindowId" Core..= windowId),
            ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeMaintenanceWindowTasks where
  type
    Rs DescribeMaintenanceWindowTasks =
      DescribeMaintenanceWindowTasksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonSSM.DescribeMaintenanceWindowTasks")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowTasksResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Tasks")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeMaintenanceWindowTasks where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"tasks" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeMaintenanceWindowTasksResponse' smart constructor.
data DescribeMaintenanceWindowTasksResponse = DescribeMaintenanceWindowTasksResponse'
  { -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about the tasks in the maintenance window.
    tasks :: Core.Maybe [Types.MaintenanceWindowTask],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMaintenanceWindowTasksResponse' value with any optional fields omitted.
mkDescribeMaintenanceWindowTasksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeMaintenanceWindowTasksResponse
mkDescribeMaintenanceWindowTasksResponse responseStatus =
  DescribeMaintenanceWindowTasksResponse'
    { nextToken = Core.Nothing,
      tasks = Core.Nothing,
      responseStatus
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtrfrsNextToken :: Lens.Lens' DescribeMaintenanceWindowTasksResponse (Core.Maybe Types.NextToken)
dmwtrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmwtrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the tasks in the maintenance window.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtrfrsTasks :: Lens.Lens' DescribeMaintenanceWindowTasksResponse (Core.Maybe [Types.MaintenanceWindowTask])
dmwtrfrsTasks = Lens.field @"tasks"
{-# DEPRECATED dmwtrfrsTasks "Use generic-lens or generic-optics with 'tasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtrfrsResponseStatus :: Lens.Lens' DescribeMaintenanceWindowTasksResponse Core.Int
dmwtrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmwtrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
