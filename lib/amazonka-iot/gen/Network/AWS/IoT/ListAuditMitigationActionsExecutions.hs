{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListAuditMitigationActionsExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of audit mitigation action tasks that were executed.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuditMitigationActionsExecutions
  ( -- * Creating a request
    ListAuditMitigationActionsExecutions (..),
    mkListAuditMitigationActionsExecutions,

    -- ** Request lenses
    lamaeTaskId,
    lamaeNextToken,
    lamaeActionStatus,
    lamaeFindingId,
    lamaeMaxResults,

    -- * Destructuring the response
    ListAuditMitigationActionsExecutionsResponse (..),
    mkListAuditMitigationActionsExecutionsResponse,

    -- ** Response lenses
    lamaersActionsExecutions,
    lamaersNextToken,
    lamaersResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAuditMitigationActionsExecutions' smart constructor.
data ListAuditMitigationActionsExecutions = ListAuditMitigationActionsExecutions'
  { -- | Specify this filter to limit results to actions for a specific audit mitigation actions task.
    taskId :: Lude.Text,
    -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Specify this filter to limit results to those with a specific status.
    actionStatus :: Lude.Maybe AuditMitigationActionsExecutionStatus,
    -- | Specify this filter to limit results to those that were applied to a specific audit finding.
    findingId :: Lude.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAuditMitigationActionsExecutions' with the minimum fields required to make a request.
--
-- * 'taskId' - Specify this filter to limit results to actions for a specific audit mitigation actions task.
-- * 'nextToken' - The token for the next set of results.
-- * 'actionStatus' - Specify this filter to limit results to those with a specific status.
-- * 'findingId' - Specify this filter to limit results to those that were applied to a specific audit finding.
-- * 'maxResults' - The maximum number of results to return at one time. The default is 25.
mkListAuditMitigationActionsExecutions ::
  -- | 'taskId'
  Lude.Text ->
  -- | 'findingId'
  Lude.Text ->
  ListAuditMitigationActionsExecutions
mkListAuditMitigationActionsExecutions pTaskId_ pFindingId_ =
  ListAuditMitigationActionsExecutions'
    { taskId = pTaskId_,
      nextToken = Lude.Nothing,
      actionStatus = Lude.Nothing,
      findingId = pFindingId_,
      maxResults = Lude.Nothing
    }

-- | Specify this filter to limit results to actions for a specific audit mitigation actions task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaeTaskId :: Lens.Lens' ListAuditMitigationActionsExecutions Lude.Text
lamaeTaskId = Lens.lens (taskId :: ListAuditMitigationActionsExecutions -> Lude.Text) (\s a -> s {taskId = a} :: ListAuditMitigationActionsExecutions)
{-# DEPRECATED lamaeTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaeNextToken :: Lens.Lens' ListAuditMitigationActionsExecutions (Lude.Maybe Lude.Text)
lamaeNextToken = Lens.lens (nextToken :: ListAuditMitigationActionsExecutions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAuditMitigationActionsExecutions)
{-# DEPRECATED lamaeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specify this filter to limit results to those with a specific status.
--
-- /Note:/ Consider using 'actionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaeActionStatus :: Lens.Lens' ListAuditMitigationActionsExecutions (Lude.Maybe AuditMitigationActionsExecutionStatus)
lamaeActionStatus = Lens.lens (actionStatus :: ListAuditMitigationActionsExecutions -> Lude.Maybe AuditMitigationActionsExecutionStatus) (\s a -> s {actionStatus = a} :: ListAuditMitigationActionsExecutions)
{-# DEPRECATED lamaeActionStatus "Use generic-lens or generic-optics with 'actionStatus' instead." #-}

-- | Specify this filter to limit results to those that were applied to a specific audit finding.
--
-- /Note:/ Consider using 'findingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaeFindingId :: Lens.Lens' ListAuditMitigationActionsExecutions Lude.Text
lamaeFindingId = Lens.lens (findingId :: ListAuditMitigationActionsExecutions -> Lude.Text) (\s a -> s {findingId = a} :: ListAuditMitigationActionsExecutions)
{-# DEPRECATED lamaeFindingId "Use generic-lens or generic-optics with 'findingId' instead." #-}

-- | The maximum number of results to return at one time. The default is 25.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaeMaxResults :: Lens.Lens' ListAuditMitigationActionsExecutions (Lude.Maybe Lude.Natural)
lamaeMaxResults = Lens.lens (maxResults :: ListAuditMitigationActionsExecutions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAuditMitigationActionsExecutions)
{-# DEPRECATED lamaeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListAuditMitigationActionsExecutions where
  page rq rs
    | Page.stop (rs Lens.^. lamaersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lamaersActionsExecutions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lamaeNextToken Lens..~ rs Lens.^. lamaersNextToken

instance Lude.AWSRequest ListAuditMitigationActionsExecutions where
  type
    Rs ListAuditMitigationActionsExecutions =
      ListAuditMitigationActionsExecutionsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAuditMitigationActionsExecutionsResponse'
            Lude.<$> (x Lude..?> "actionsExecutions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAuditMitigationActionsExecutions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListAuditMitigationActionsExecutions where
  toPath = Lude.const "/audit/mitigationactions/executions"

instance Lude.ToQuery ListAuditMitigationActionsExecutions where
  toQuery ListAuditMitigationActionsExecutions' {..} =
    Lude.mconcat
      [ "taskId" Lude.=: taskId,
        "nextToken" Lude.=: nextToken,
        "actionStatus" Lude.=: actionStatus,
        "findingId" Lude.=: findingId,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListAuditMitigationActionsExecutionsResponse' smart constructor.
data ListAuditMitigationActionsExecutionsResponse = ListAuditMitigationActionsExecutionsResponse'
  { -- | A set of task execution results based on the input parameters. Details include the mitigation action applied, start time, and task status.
    actionsExecutions :: Lude.Maybe [AuditMitigationActionExecutionMetadata],
    -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAuditMitigationActionsExecutionsResponse' with the minimum fields required to make a request.
--
-- * 'actionsExecutions' - A set of task execution results based on the input parameters. Details include the mitigation action applied, start time, and task status.
-- * 'nextToken' - The token for the next set of results.
-- * 'responseStatus' - The response status code.
mkListAuditMitigationActionsExecutionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAuditMitigationActionsExecutionsResponse
mkListAuditMitigationActionsExecutionsResponse pResponseStatus_ =
  ListAuditMitigationActionsExecutionsResponse'
    { actionsExecutions =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A set of task execution results based on the input parameters. Details include the mitigation action applied, start time, and task status.
--
-- /Note:/ Consider using 'actionsExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaersActionsExecutions :: Lens.Lens' ListAuditMitigationActionsExecutionsResponse (Lude.Maybe [AuditMitigationActionExecutionMetadata])
lamaersActionsExecutions = Lens.lens (actionsExecutions :: ListAuditMitigationActionsExecutionsResponse -> Lude.Maybe [AuditMitigationActionExecutionMetadata]) (\s a -> s {actionsExecutions = a} :: ListAuditMitigationActionsExecutionsResponse)
{-# DEPRECATED lamaersActionsExecutions "Use generic-lens or generic-optics with 'actionsExecutions' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaersNextToken :: Lens.Lens' ListAuditMitigationActionsExecutionsResponse (Lude.Maybe Lude.Text)
lamaersNextToken = Lens.lens (nextToken :: ListAuditMitigationActionsExecutionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAuditMitigationActionsExecutionsResponse)
{-# DEPRECATED lamaersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaersResponseStatus :: Lens.Lens' ListAuditMitigationActionsExecutionsResponse Lude.Int
lamaersResponseStatus = Lens.lens (responseStatus :: ListAuditMitigationActionsExecutionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAuditMitigationActionsExecutionsResponse)
{-# DEPRECATED lamaersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
