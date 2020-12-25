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
    lamaeFindingId,
    lamaeActionStatus,
    lamaeMaxResults,
    lamaeNextToken,

    -- * Destructuring the response
    ListAuditMitigationActionsExecutionsResponse (..),
    mkListAuditMitigationActionsExecutionsResponse,

    -- ** Response lenses
    lamaerrsActionsExecutions,
    lamaerrsNextToken,
    lamaerrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAuditMitigationActionsExecutions' smart constructor.
data ListAuditMitigationActionsExecutions = ListAuditMitigationActionsExecutions'
  { -- | Specify this filter to limit results to actions for a specific audit mitigation actions task.
    taskId :: Types.AuditMitigationActionsTaskId,
    -- | Specify this filter to limit results to those that were applied to a specific audit finding.
    findingId :: Types.FindingId,
    -- | Specify this filter to limit results to those with a specific status.
    actionStatus :: Core.Maybe Types.AuditMitigationActionsExecutionStatus,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAuditMitigationActionsExecutions' value with any optional fields omitted.
mkListAuditMitigationActionsExecutions ::
  -- | 'taskId'
  Types.AuditMitigationActionsTaskId ->
  -- | 'findingId'
  Types.FindingId ->
  ListAuditMitigationActionsExecutions
mkListAuditMitigationActionsExecutions taskId findingId =
  ListAuditMitigationActionsExecutions'
    { taskId,
      findingId,
      actionStatus = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Specify this filter to limit results to actions for a specific audit mitigation actions task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaeTaskId :: Lens.Lens' ListAuditMitigationActionsExecutions Types.AuditMitigationActionsTaskId
lamaeTaskId = Lens.field @"taskId"
{-# DEPRECATED lamaeTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | Specify this filter to limit results to those that were applied to a specific audit finding.
--
-- /Note:/ Consider using 'findingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaeFindingId :: Lens.Lens' ListAuditMitigationActionsExecutions Types.FindingId
lamaeFindingId = Lens.field @"findingId"
{-# DEPRECATED lamaeFindingId "Use generic-lens or generic-optics with 'findingId' instead." #-}

-- | Specify this filter to limit results to those with a specific status.
--
-- /Note:/ Consider using 'actionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaeActionStatus :: Lens.Lens' ListAuditMitigationActionsExecutions (Core.Maybe Types.AuditMitigationActionsExecutionStatus)
lamaeActionStatus = Lens.field @"actionStatus"
{-# DEPRECATED lamaeActionStatus "Use generic-lens or generic-optics with 'actionStatus' instead." #-}

-- | The maximum number of results to return at one time. The default is 25.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaeMaxResults :: Lens.Lens' ListAuditMitigationActionsExecutions (Core.Maybe Core.Natural)
lamaeMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lamaeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaeNextToken :: Lens.Lens' ListAuditMitigationActionsExecutions (Core.Maybe Types.NextToken)
lamaeNextToken = Lens.field @"nextToken"
{-# DEPRECATED lamaeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListAuditMitigationActionsExecutions where
  type
    Rs ListAuditMitigationActionsExecutions =
      ListAuditMitigationActionsExecutionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/audit/mitigationactions/executions",
        Core._rqQuery =
          Core.toQueryValue "taskId" taskId
            Core.<> (Core.toQueryValue "findingId" findingId)
            Core.<> (Core.toQueryValue "actionStatus" Core.<$> actionStatus)
            Core.<> (Core.toQueryValue "maxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAuditMitigationActionsExecutionsResponse'
            Core.<$> (x Core..:? "actionsExecutions")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListAuditMitigationActionsExecutions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"actionsExecutions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListAuditMitigationActionsExecutionsResponse' smart constructor.
data ListAuditMitigationActionsExecutionsResponse = ListAuditMitigationActionsExecutionsResponse'
  { -- | A set of task execution results based on the input parameters. Details include the mitigation action applied, start time, and task status.
    actionsExecutions :: Core.Maybe [Types.AuditMitigationActionExecutionMetadata],
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAuditMitigationActionsExecutionsResponse' value with any optional fields omitted.
mkListAuditMitigationActionsExecutionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAuditMitigationActionsExecutionsResponse
mkListAuditMitigationActionsExecutionsResponse responseStatus =
  ListAuditMitigationActionsExecutionsResponse'
    { actionsExecutions =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A set of task execution results based on the input parameters. Details include the mitigation action applied, start time, and task status.
--
-- /Note:/ Consider using 'actionsExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaerrsActionsExecutions :: Lens.Lens' ListAuditMitigationActionsExecutionsResponse (Core.Maybe [Types.AuditMitigationActionExecutionMetadata])
lamaerrsActionsExecutions = Lens.field @"actionsExecutions"
{-# DEPRECATED lamaerrsActionsExecutions "Use generic-lens or generic-optics with 'actionsExecutions' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaerrsNextToken :: Lens.Lens' ListAuditMitigationActionsExecutionsResponse (Core.Maybe Types.NextToken)
lamaerrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lamaerrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaerrsResponseStatus :: Lens.Lens' ListAuditMitigationActionsExecutionsResponse Core.Int
lamaerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lamaerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
