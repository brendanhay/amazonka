{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeExportTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified export tasks. You can list all your export tasks or filter the results based on task ID or task status.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeExportTasks
  ( -- * Creating a request
    DescribeExportTasks (..),
    mkDescribeExportTasks,

    -- ** Request lenses
    detLimit,
    detNextToken,
    detStatusCode,
    detTaskId,

    -- * Destructuring the response
    DescribeExportTasksResponse (..),
    mkDescribeExportTasksResponse,

    -- ** Response lenses
    detrrsExportTasks,
    detrrsNextToken,
    detrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { -- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
    limit :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken,
    -- | The status code of the export task. Specifying a status code filters the results to zero or more export tasks.
    statusCode :: Core.Maybe Types.ExportTaskStatusCode,
    -- | The ID of the export task. Specifying a task ID filters the results to zero or one export tasks.
    taskId :: Core.Maybe Types.ExportTaskId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeExportTasks' value with any optional fields omitted.
mkDescribeExportTasks ::
  DescribeExportTasks
mkDescribeExportTasks =
  DescribeExportTasks'
    { limit = Core.Nothing,
      nextToken = Core.Nothing,
      statusCode = Core.Nothing,
      taskId = Core.Nothing
    }

-- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detLimit :: Lens.Lens' DescribeExportTasks (Core.Maybe Core.Natural)
detLimit = Lens.field @"limit"
{-# DEPRECATED detLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detNextToken :: Lens.Lens' DescribeExportTasks (Core.Maybe Types.NextToken)
detNextToken = Lens.field @"nextToken"
{-# DEPRECATED detNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The status code of the export task. Specifying a status code filters the results to zero or more export tasks.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detStatusCode :: Lens.Lens' DescribeExportTasks (Core.Maybe Types.ExportTaskStatusCode)
detStatusCode = Lens.field @"statusCode"
{-# DEPRECATED detStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | The ID of the export task. Specifying a task ID filters the results to zero or one export tasks.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detTaskId :: Lens.Lens' DescribeExportTasks (Core.Maybe Types.ExportTaskId)
detTaskId = Lens.field @"taskId"
{-# DEPRECATED detTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Core.FromJSON DescribeExportTasks where
  toJSON DescribeExportTasks {..} =
    Core.object
      ( Core.catMaybes
          [ ("limit" Core..=) Core.<$> limit,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("statusCode" Core..=) Core.<$> statusCode,
            ("taskId" Core..=) Core.<$> taskId
          ]
      )

instance Core.AWSRequest DescribeExportTasks where
  type Rs DescribeExportTasks = DescribeExportTasksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.DescribeExportTasks")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExportTasksResponse'
            Core.<$> (x Core..:? "exportTasks")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeExportTasks where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"exportTasks" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { -- | The export tasks.
    exportTasks :: Core.Maybe [Types.ExportTask],
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeExportTasksResponse' value with any optional fields omitted.
mkDescribeExportTasksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeExportTasksResponse
mkDescribeExportTasksResponse responseStatus =
  DescribeExportTasksResponse'
    { exportTasks = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The export tasks.
--
-- /Note:/ Consider using 'exportTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsExportTasks :: Lens.Lens' DescribeExportTasksResponse (Core.Maybe [Types.ExportTask])
detrrsExportTasks = Lens.field @"exportTasks"
{-# DEPRECATED detrrsExportTasks "Use generic-lens or generic-optics with 'exportTasks' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsNextToken :: Lens.Lens' DescribeExportTasksResponse (Core.Maybe Types.NextToken)
detrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED detrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsResponseStatus :: Lens.Lens' DescribeExportTasksResponse Core.Int
detrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED detrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
