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
    latTaskType,
    latStartTime,
    latNextToken,
    latEndTime,
    latMaxResults,
    latTaskStatus,

    -- * Destructuring the response
    ListAuditTasksResponse (..),
    mkListAuditTasksResponse,

    -- ** Response lenses
    latrsTasks,
    latrsNextToken,
    latrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAuditTasks' smart constructor.
data ListAuditTasks = ListAuditTasks'
  { -- | A filter to limit the output to the specified type of audit: can be one of "ON_DEMAND_AUDIT_TASK" or "SCHEDULED__AUDIT_TASK".
    taskType :: Lude.Maybe AuditTaskType,
    -- | The beginning of the time period. Audit information is retained for a limited time (90 days). Requesting a start time prior to what is retained results in an "InvalidRequestException".
    startTime :: Lude.Timestamp,
    -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The end of the time period.
    endTime :: Lude.Timestamp,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | A filter to limit the output to audits with the specified completion status: can be one of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
    taskStatus :: Lude.Maybe AuditTaskStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAuditTasks' with the minimum fields required to make a request.
--
-- * 'taskType' - A filter to limit the output to the specified type of audit: can be one of "ON_DEMAND_AUDIT_TASK" or "SCHEDULED__AUDIT_TASK".
-- * 'startTime' - The beginning of the time period. Audit information is retained for a limited time (90 days). Requesting a start time prior to what is retained results in an "InvalidRequestException".
-- * 'nextToken' - The token for the next set of results.
-- * 'endTime' - The end of the time period.
-- * 'maxResults' - The maximum number of results to return at one time. The default is 25.
-- * 'taskStatus' - A filter to limit the output to audits with the specified completion status: can be one of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
mkListAuditTasks ::
  -- | 'startTime'
  Lude.Timestamp ->
  -- | 'endTime'
  Lude.Timestamp ->
  ListAuditTasks
mkListAuditTasks pStartTime_ pEndTime_ =
  ListAuditTasks'
    { taskType = Lude.Nothing,
      startTime = pStartTime_,
      nextToken = Lude.Nothing,
      endTime = pEndTime_,
      maxResults = Lude.Nothing,
      taskStatus = Lude.Nothing
    }

-- | A filter to limit the output to the specified type of audit: can be one of "ON_DEMAND_AUDIT_TASK" or "SCHEDULED__AUDIT_TASK".
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latTaskType :: Lens.Lens' ListAuditTasks (Lude.Maybe AuditTaskType)
latTaskType = Lens.lens (taskType :: ListAuditTasks -> Lude.Maybe AuditTaskType) (\s a -> s {taskType = a} :: ListAuditTasks)
{-# DEPRECATED latTaskType "Use generic-lens or generic-optics with 'taskType' instead." #-}

-- | The beginning of the time period. Audit information is retained for a limited time (90 days). Requesting a start time prior to what is retained results in an "InvalidRequestException".
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latStartTime :: Lens.Lens' ListAuditTasks Lude.Timestamp
latStartTime = Lens.lens (startTime :: ListAuditTasks -> Lude.Timestamp) (\s a -> s {startTime = a} :: ListAuditTasks)
{-# DEPRECATED latStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latNextToken :: Lens.Lens' ListAuditTasks (Lude.Maybe Lude.Text)
latNextToken = Lens.lens (nextToken :: ListAuditTasks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAuditTasks)
{-# DEPRECATED latNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The end of the time period.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latEndTime :: Lens.Lens' ListAuditTasks Lude.Timestamp
latEndTime = Lens.lens (endTime :: ListAuditTasks -> Lude.Timestamp) (\s a -> s {endTime = a} :: ListAuditTasks)
{-# DEPRECATED latEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The maximum number of results to return at one time. The default is 25.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latMaxResults :: Lens.Lens' ListAuditTasks (Lude.Maybe Lude.Natural)
latMaxResults = Lens.lens (maxResults :: ListAuditTasks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAuditTasks)
{-# DEPRECATED latMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A filter to limit the output to audits with the specified completion status: can be one of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latTaskStatus :: Lens.Lens' ListAuditTasks (Lude.Maybe AuditTaskStatus)
latTaskStatus = Lens.lens (taskStatus :: ListAuditTasks -> Lude.Maybe AuditTaskStatus) (\s a -> s {taskStatus = a} :: ListAuditTasks)
{-# DEPRECATED latTaskStatus "Use generic-lens or generic-optics with 'taskStatus' instead." #-}

instance Page.AWSPager ListAuditTasks where
  page rq rs
    | Page.stop (rs Lens.^. latrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. latrsTasks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& latNextToken Lens..~ rs Lens.^. latrsNextToken

instance Lude.AWSRequest ListAuditTasks where
  type Rs ListAuditTasks = ListAuditTasksResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAuditTasksResponse'
            Lude.<$> (x Lude..?> "tasks" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAuditTasks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListAuditTasks where
  toPath = Lude.const "/audit/tasks"

instance Lude.ToQuery ListAuditTasks where
  toQuery ListAuditTasks' {..} =
    Lude.mconcat
      [ "taskType" Lude.=: taskType,
        "startTime" Lude.=: startTime,
        "nextToken" Lude.=: nextToken,
        "endTime" Lude.=: endTime,
        "maxResults" Lude.=: maxResults,
        "taskStatus" Lude.=: taskStatus
      ]

-- | /See:/ 'mkListAuditTasksResponse' smart constructor.
data ListAuditTasksResponse = ListAuditTasksResponse'
  { -- | The audits that were performed during the specified time period.
    tasks :: Lude.Maybe [AuditTaskMetadata],
    -- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAuditTasksResponse' with the minimum fields required to make a request.
--
-- * 'tasks' - The audits that were performed during the specified time period.
-- * 'nextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListAuditTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAuditTasksResponse
mkListAuditTasksResponse pResponseStatus_ =
  ListAuditTasksResponse'
    { tasks = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The audits that were performed during the specified time period.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrsTasks :: Lens.Lens' ListAuditTasksResponse (Lude.Maybe [AuditTaskMetadata])
latrsTasks = Lens.lens (tasks :: ListAuditTasksResponse -> Lude.Maybe [AuditTaskMetadata]) (\s a -> s {tasks = a} :: ListAuditTasksResponse)
{-# DEPRECATED latrsTasks "Use generic-lens or generic-optics with 'tasks' instead." #-}

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrsNextToken :: Lens.Lens' ListAuditTasksResponse (Lude.Maybe Lude.Text)
latrsNextToken = Lens.lens (nextToken :: ListAuditTasksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAuditTasksResponse)
{-# DEPRECATED latrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrsResponseStatus :: Lens.Lens' ListAuditTasksResponse Lude.Int
latrsResponseStatus = Lens.lens (responseStatus :: ListAuditTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAuditTasksResponse)
{-# DEPRECATED latrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
