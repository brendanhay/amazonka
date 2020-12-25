{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListAuditTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Device Defender audits that have been performed during a given time period.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuditTasks
  ( -- * Creating a request
    ListAuditTasks (..),
    mkListAuditTasks,

    -- ** Request lenses
    latStartTime,
    latEndTime,
    latMaxResults,
    latNextToken,
    latTaskStatus,
    latTaskType,

    -- * Destructuring the response
    ListAuditTasksResponse (..),
    mkListAuditTasksResponse,

    -- ** Response lenses
    latrrsNextToken,
    latrrsTasks,
    latrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAuditTasks' smart constructor.
data ListAuditTasks = ListAuditTasks'
  { -- | The beginning of the time period. Audit information is retained for a limited time (90 days). Requesting a start time prior to what is retained results in an "InvalidRequestException".
    startTime :: Core.NominalDiffTime,
    -- | The end of the time period.
    endTime :: Core.NominalDiffTime,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A filter to limit the output to audits with the specified completion status: can be one of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
    taskStatus :: Core.Maybe Types.AuditTaskStatus,
    -- | A filter to limit the output to the specified type of audit: can be one of "ON_DEMAND_AUDIT_TASK" or "SCHEDULED__AUDIT_TASK".
    taskType :: Core.Maybe Types.AuditTaskType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAuditTasks' value with any optional fields omitted.
mkListAuditTasks ::
  -- | 'startTime'
  Core.NominalDiffTime ->
  -- | 'endTime'
  Core.NominalDiffTime ->
  ListAuditTasks
mkListAuditTasks startTime endTime =
  ListAuditTasks'
    { startTime,
      endTime,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      taskStatus = Core.Nothing,
      taskType = Core.Nothing
    }

-- | The beginning of the time period. Audit information is retained for a limited time (90 days). Requesting a start time prior to what is retained results in an "InvalidRequestException".
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latStartTime :: Lens.Lens' ListAuditTasks Core.NominalDiffTime
latStartTime = Lens.field @"startTime"
{-# DEPRECATED latStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The end of the time period.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latEndTime :: Lens.Lens' ListAuditTasks Core.NominalDiffTime
latEndTime = Lens.field @"endTime"
{-# DEPRECATED latEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The maximum number of results to return at one time. The default is 25.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latMaxResults :: Lens.Lens' ListAuditTasks (Core.Maybe Core.Natural)
latMaxResults = Lens.field @"maxResults"
{-# DEPRECATED latMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latNextToken :: Lens.Lens' ListAuditTasks (Core.Maybe Types.NextToken)
latNextToken = Lens.field @"nextToken"
{-# DEPRECATED latNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A filter to limit the output to audits with the specified completion status: can be one of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latTaskStatus :: Lens.Lens' ListAuditTasks (Core.Maybe Types.AuditTaskStatus)
latTaskStatus = Lens.field @"taskStatus"
{-# DEPRECATED latTaskStatus "Use generic-lens or generic-optics with 'taskStatus' instead." #-}

-- | A filter to limit the output to the specified type of audit: can be one of "ON_DEMAND_AUDIT_TASK" or "SCHEDULED__AUDIT_TASK".
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latTaskType :: Lens.Lens' ListAuditTasks (Core.Maybe Types.AuditTaskType)
latTaskType = Lens.field @"taskType"
{-# DEPRECATED latTaskType "Use generic-lens or generic-optics with 'taskType' instead." #-}

instance Core.AWSRequest ListAuditTasks where
  type Rs ListAuditTasks = ListAuditTasksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/audit/tasks",
        Core._rqQuery =
          Core.toQueryValue "startTime" startTime
            Core.<> (Core.toQueryValue "endTime" endTime)
            Core.<> (Core.toQueryValue "maxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "taskStatus" Core.<$> taskStatus)
            Core.<> (Core.toQueryValue "taskType" Core.<$> taskType),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAuditTasksResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "tasks")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListAuditTasks where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"tasks" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListAuditTasksResponse' smart constructor.
data ListAuditTasksResponse = ListAuditTasksResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The audits that were performed during the specified time period.
    tasks :: Core.Maybe [Types.AuditTaskMetadata],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAuditTasksResponse' value with any optional fields omitted.
mkListAuditTasksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAuditTasksResponse
mkListAuditTasksResponse responseStatus =
  ListAuditTasksResponse'
    { nextToken = Core.Nothing,
      tasks = Core.Nothing,
      responseStatus
    }

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrrsNextToken :: Lens.Lens' ListAuditTasksResponse (Core.Maybe Types.NextToken)
latrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED latrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The audits that were performed during the specified time period.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrrsTasks :: Lens.Lens' ListAuditTasksResponse (Core.Maybe [Types.AuditTaskMetadata])
latrrsTasks = Lens.field @"tasks"
{-# DEPRECATED latrrsTasks "Use generic-lens or generic-optics with 'tasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrrsResponseStatus :: Lens.Lens' ListAuditTasksResponse Core.Int
latrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED latrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
