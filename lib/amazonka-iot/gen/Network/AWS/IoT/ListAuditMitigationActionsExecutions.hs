{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListAuditMitigationActionsExecutions (..)
    , mkListAuditMitigationActionsExecutions
    -- ** Request lenses
    , lamaeTaskId
    , lamaeFindingId
    , lamaeActionStatus
    , lamaeMaxResults
    , lamaeNextToken

    -- * Destructuring the response
    , ListAuditMitigationActionsExecutionsResponse (..)
    , mkListAuditMitigationActionsExecutionsResponse
    -- ** Response lenses
    , lamaerrsActionsExecutions
    , lamaerrsNextToken
    , lamaerrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAuditMitigationActionsExecutions' smart constructor.
data ListAuditMitigationActionsExecutions = ListAuditMitigationActionsExecutions'
  { taskId :: Types.AuditMitigationActionsTaskId
    -- ^ Specify this filter to limit results to actions for a specific audit mitigation actions task.
  , findingId :: Types.FindingId
    -- ^ Specify this filter to limit results to those that were applied to a specific audit finding.
  , actionStatus :: Core.Maybe Types.AuditMitigationActionsExecutionStatus
    -- ^ Specify this filter to limit results to those with a specific status.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return at one time. The default is 25.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAuditMitigationActionsExecutions' value with any optional fields omitted.
mkListAuditMitigationActionsExecutions
    :: Types.AuditMitigationActionsTaskId -- ^ 'taskId'
    -> Types.FindingId -- ^ 'findingId'
    -> ListAuditMitigationActionsExecutions
mkListAuditMitigationActionsExecutions taskId findingId
  = ListAuditMitigationActionsExecutions'{taskId, findingId,
                                          actionStatus = Core.Nothing, maxResults = Core.Nothing,
                                          nextToken = Core.Nothing}

-- | Specify this filter to limit results to actions for a specific audit mitigation actions task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaeTaskId :: Lens.Lens' ListAuditMitigationActionsExecutions Types.AuditMitigationActionsTaskId
lamaeTaskId = Lens.field @"taskId"
{-# INLINEABLE lamaeTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

-- | Specify this filter to limit results to those that were applied to a specific audit finding.
--
-- /Note:/ Consider using 'findingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaeFindingId :: Lens.Lens' ListAuditMitigationActionsExecutions Types.FindingId
lamaeFindingId = Lens.field @"findingId"
{-# INLINEABLE lamaeFindingId #-}
{-# DEPRECATED findingId "Use generic-lens or generic-optics with 'findingId' instead"  #-}

-- | Specify this filter to limit results to those with a specific status.
--
-- /Note:/ Consider using 'actionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaeActionStatus :: Lens.Lens' ListAuditMitigationActionsExecutions (Core.Maybe Types.AuditMitigationActionsExecutionStatus)
lamaeActionStatus = Lens.field @"actionStatus"
{-# INLINEABLE lamaeActionStatus #-}
{-# DEPRECATED actionStatus "Use generic-lens or generic-optics with 'actionStatus' instead"  #-}

-- | The maximum number of results to return at one time. The default is 25.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaeMaxResults :: Lens.Lens' ListAuditMitigationActionsExecutions (Core.Maybe Core.Natural)
lamaeMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lamaeMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaeNextToken :: Lens.Lens' ListAuditMitigationActionsExecutions (Core.Maybe Types.NextToken)
lamaeNextToken = Lens.field @"nextToken"
{-# INLINEABLE lamaeNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListAuditMitigationActionsExecutions where
        toQuery ListAuditMitigationActionsExecutions{..}
          = Core.toQueryPair "taskId" taskId Core.<>
              Core.toQueryPair "findingId" findingId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "actionStatus")
                actionStatus
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListAuditMitigationActionsExecutions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListAuditMitigationActionsExecutions where
        type Rs ListAuditMitigationActionsExecutions =
             ListAuditMitigationActionsExecutionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/audit/mitigationactions/executions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAuditMitigationActionsExecutionsResponse' Core.<$>
                   (x Core..:? "actionsExecutions") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAuditMitigationActionsExecutions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"actionsExecutions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListAuditMitigationActionsExecutionsResponse' smart constructor.
data ListAuditMitigationActionsExecutionsResponse = ListAuditMitigationActionsExecutionsResponse'
  { actionsExecutions :: Core.Maybe [Types.AuditMitigationActionExecutionMetadata]
    -- ^ A set of task execution results based on the input parameters. Details include the mitigation action applied, start time, and task status.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListAuditMitigationActionsExecutionsResponse' value with any optional fields omitted.
mkListAuditMitigationActionsExecutionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAuditMitigationActionsExecutionsResponse
mkListAuditMitigationActionsExecutionsResponse responseStatus
  = ListAuditMitigationActionsExecutionsResponse'{actionsExecutions =
                                                    Core.Nothing,
                                                  nextToken = Core.Nothing, responseStatus}

-- | A set of task execution results based on the input parameters. Details include the mitigation action applied, start time, and task status.
--
-- /Note:/ Consider using 'actionsExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaerrsActionsExecutions :: Lens.Lens' ListAuditMitigationActionsExecutionsResponse (Core.Maybe [Types.AuditMitigationActionExecutionMetadata])
lamaerrsActionsExecutions = Lens.field @"actionsExecutions"
{-# INLINEABLE lamaerrsActionsExecutions #-}
{-# DEPRECATED actionsExecutions "Use generic-lens or generic-optics with 'actionsExecutions' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaerrsNextToken :: Lens.Lens' ListAuditMitigationActionsExecutionsResponse (Core.Maybe Types.NextToken)
lamaerrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lamaerrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamaerrsResponseStatus :: Lens.Lens' ListAuditMitigationActionsExecutionsResponse Core.Int
lamaerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lamaerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
