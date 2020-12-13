{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListAuditMitigationActionsTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of audit mitigation action tasks that match the specified filters.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuditMitigationActionsTasks
  ( -- * Creating a request
    ListAuditMitigationActionsTasks (..),
    mkListAuditMitigationActionsTasks,

    -- ** Request lenses
    lamatStartTime,
    lamatAuditTaskId,
    lamatNextToken,
    lamatEndTime,
    lamatFindingId,
    lamatMaxResults,
    lamatTaskStatus,

    -- * Destructuring the response
    ListAuditMitigationActionsTasksResponse (..),
    mkListAuditMitigationActionsTasksResponse,

    -- ** Response lenses
    lamatrsTasks,
    lamatrsNextToken,
    lamatrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAuditMitigationActionsTasks' smart constructor.
data ListAuditMitigationActionsTasks = ListAuditMitigationActionsTasks'
  { -- | Specify this filter to limit results to tasks that began on or after a specific date and time.
    startTime :: Lude.Timestamp,
    -- | Specify this filter to limit results to tasks that were applied to results for a specific audit.
    auditTaskId :: Lude.Maybe Lude.Text,
    -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Specify this filter to limit results to tasks that were completed or canceled on or before a specific date and time.
    endTime :: Lude.Timestamp,
    -- | Specify this filter to limit results to tasks that were applied to a specific audit finding.
    findingId :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | Specify this filter to limit results to tasks that are in a specific state.
    taskStatus :: Lude.Maybe AuditMitigationActionsTaskStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAuditMitigationActionsTasks' with the minimum fields required to make a request.
--
-- * 'startTime' - Specify this filter to limit results to tasks that began on or after a specific date and time.
-- * 'auditTaskId' - Specify this filter to limit results to tasks that were applied to results for a specific audit.
-- * 'nextToken' - The token for the next set of results.
-- * 'endTime' - Specify this filter to limit results to tasks that were completed or canceled on or before a specific date and time.
-- * 'findingId' - Specify this filter to limit results to tasks that were applied to a specific audit finding.
-- * 'maxResults' - The maximum number of results to return at one time. The default is 25.
-- * 'taskStatus' - Specify this filter to limit results to tasks that are in a specific state.
mkListAuditMitigationActionsTasks ::
  -- | 'startTime'
  Lude.Timestamp ->
  -- | 'endTime'
  Lude.Timestamp ->
  ListAuditMitigationActionsTasks
mkListAuditMitigationActionsTasks pStartTime_ pEndTime_ =
  ListAuditMitigationActionsTasks'
    { startTime = pStartTime_,
      auditTaskId = Lude.Nothing,
      nextToken = Lude.Nothing,
      endTime = pEndTime_,
      findingId = Lude.Nothing,
      maxResults = Lude.Nothing,
      taskStatus = Lude.Nothing
    }

-- | Specify this filter to limit results to tasks that began on or after a specific date and time.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatStartTime :: Lens.Lens' ListAuditMitigationActionsTasks Lude.Timestamp
lamatStartTime = Lens.lens (startTime :: ListAuditMitigationActionsTasks -> Lude.Timestamp) (\s a -> s {startTime = a} :: ListAuditMitigationActionsTasks)
{-# DEPRECATED lamatStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Specify this filter to limit results to tasks that were applied to results for a specific audit.
--
-- /Note:/ Consider using 'auditTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatAuditTaskId :: Lens.Lens' ListAuditMitigationActionsTasks (Lude.Maybe Lude.Text)
lamatAuditTaskId = Lens.lens (auditTaskId :: ListAuditMitigationActionsTasks -> Lude.Maybe Lude.Text) (\s a -> s {auditTaskId = a} :: ListAuditMitigationActionsTasks)
{-# DEPRECATED lamatAuditTaskId "Use generic-lens or generic-optics with 'auditTaskId' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatNextToken :: Lens.Lens' ListAuditMitigationActionsTasks (Lude.Maybe Lude.Text)
lamatNextToken = Lens.lens (nextToken :: ListAuditMitigationActionsTasks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAuditMitigationActionsTasks)
{-# DEPRECATED lamatNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specify this filter to limit results to tasks that were completed or canceled on or before a specific date and time.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatEndTime :: Lens.Lens' ListAuditMitigationActionsTasks Lude.Timestamp
lamatEndTime = Lens.lens (endTime :: ListAuditMitigationActionsTasks -> Lude.Timestamp) (\s a -> s {endTime = a} :: ListAuditMitigationActionsTasks)
{-# DEPRECATED lamatEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Specify this filter to limit results to tasks that were applied to a specific audit finding.
--
-- /Note:/ Consider using 'findingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatFindingId :: Lens.Lens' ListAuditMitigationActionsTasks (Lude.Maybe Lude.Text)
lamatFindingId = Lens.lens (findingId :: ListAuditMitigationActionsTasks -> Lude.Maybe Lude.Text) (\s a -> s {findingId = a} :: ListAuditMitigationActionsTasks)
{-# DEPRECATED lamatFindingId "Use generic-lens or generic-optics with 'findingId' instead." #-}

-- | The maximum number of results to return at one time. The default is 25.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatMaxResults :: Lens.Lens' ListAuditMitigationActionsTasks (Lude.Maybe Lude.Natural)
lamatMaxResults = Lens.lens (maxResults :: ListAuditMitigationActionsTasks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAuditMitigationActionsTasks)
{-# DEPRECATED lamatMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Specify this filter to limit results to tasks that are in a specific state.
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatTaskStatus :: Lens.Lens' ListAuditMitigationActionsTasks (Lude.Maybe AuditMitigationActionsTaskStatus)
lamatTaskStatus = Lens.lens (taskStatus :: ListAuditMitigationActionsTasks -> Lude.Maybe AuditMitigationActionsTaskStatus) (\s a -> s {taskStatus = a} :: ListAuditMitigationActionsTasks)
{-# DEPRECATED lamatTaskStatus "Use generic-lens or generic-optics with 'taskStatus' instead." #-}

instance Page.AWSPager ListAuditMitigationActionsTasks where
  page rq rs
    | Page.stop (rs Lens.^. lamatrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lamatrsTasks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lamatNextToken Lens..~ rs Lens.^. lamatrsNextToken

instance Lude.AWSRequest ListAuditMitigationActionsTasks where
  type
    Rs ListAuditMitigationActionsTasks =
      ListAuditMitigationActionsTasksResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAuditMitigationActionsTasksResponse'
            Lude.<$> (x Lude..?> "tasks" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAuditMitigationActionsTasks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListAuditMitigationActionsTasks where
  toPath = Lude.const "/audit/mitigationactions/tasks"

instance Lude.ToQuery ListAuditMitigationActionsTasks where
  toQuery ListAuditMitigationActionsTasks' {..} =
    Lude.mconcat
      [ "startTime" Lude.=: startTime,
        "auditTaskId" Lude.=: auditTaskId,
        "nextToken" Lude.=: nextToken,
        "endTime" Lude.=: endTime,
        "findingId" Lude.=: findingId,
        "maxResults" Lude.=: maxResults,
        "taskStatus" Lude.=: taskStatus
      ]

-- | /See:/ 'mkListAuditMitigationActionsTasksResponse' smart constructor.
data ListAuditMitigationActionsTasksResponse = ListAuditMitigationActionsTasksResponse'
  { -- | The collection of audit mitigation tasks that matched the filter criteria.
    tasks :: Lude.Maybe [AuditMitigationActionsTaskMetadata],
    -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAuditMitigationActionsTasksResponse' with the minimum fields required to make a request.
--
-- * 'tasks' - The collection of audit mitigation tasks that matched the filter criteria.
-- * 'nextToken' - The token for the next set of results.
-- * 'responseStatus' - The response status code.
mkListAuditMitigationActionsTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAuditMitigationActionsTasksResponse
mkListAuditMitigationActionsTasksResponse pResponseStatus_ =
  ListAuditMitigationActionsTasksResponse'
    { tasks = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The collection of audit mitigation tasks that matched the filter criteria.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatrsTasks :: Lens.Lens' ListAuditMitigationActionsTasksResponse (Lude.Maybe [AuditMitigationActionsTaskMetadata])
lamatrsTasks = Lens.lens (tasks :: ListAuditMitigationActionsTasksResponse -> Lude.Maybe [AuditMitigationActionsTaskMetadata]) (\s a -> s {tasks = a} :: ListAuditMitigationActionsTasksResponse)
{-# DEPRECATED lamatrsTasks "Use generic-lens or generic-optics with 'tasks' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatrsNextToken :: Lens.Lens' ListAuditMitigationActionsTasksResponse (Lude.Maybe Lude.Text)
lamatrsNextToken = Lens.lens (nextToken :: ListAuditMitigationActionsTasksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAuditMitigationActionsTasksResponse)
{-# DEPRECATED lamatrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatrsResponseStatus :: Lens.Lens' ListAuditMitigationActionsTasksResponse Lude.Int
lamatrsResponseStatus = Lens.lens (responseStatus :: ListAuditMitigationActionsTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAuditMitigationActionsTasksResponse)
{-# DEPRECATED lamatrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
