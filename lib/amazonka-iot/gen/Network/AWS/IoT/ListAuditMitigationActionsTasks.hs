{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListAuditMitigationActionsTasks (..)
    , mkListAuditMitigationActionsTasks
    -- ** Request lenses
    , lamatStartTime
    , lamatEndTime
    , lamatAuditTaskId
    , lamatFindingId
    , lamatMaxResults
    , lamatNextToken
    , lamatTaskStatus

    -- * Destructuring the response
    , ListAuditMitigationActionsTasksResponse (..)
    , mkListAuditMitigationActionsTasksResponse
    -- ** Response lenses
    , lamatrrsNextToken
    , lamatrrsTasks
    , lamatrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAuditMitigationActionsTasks' smart constructor.
data ListAuditMitigationActionsTasks = ListAuditMitigationActionsTasks'
  { startTime :: Core.NominalDiffTime
    -- ^ Specify this filter to limit results to tasks that began on or after a specific date and time.
  , endTime :: Core.NominalDiffTime
    -- ^ Specify this filter to limit results to tasks that were completed or canceled on or before a specific date and time.
  , auditTaskId :: Core.Maybe Types.AuditTaskId
    -- ^ Specify this filter to limit results to tasks that were applied to results for a specific audit.
  , findingId :: Core.Maybe Types.FindingId
    -- ^ Specify this filter to limit results to tasks that were applied to a specific audit finding.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return at one time. The default is 25.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results.
  , taskStatus :: Core.Maybe Types.AuditMitigationActionsTaskStatus
    -- ^ Specify this filter to limit results to tasks that are in a specific state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListAuditMitigationActionsTasks' value with any optional fields omitted.
mkListAuditMitigationActionsTasks
    :: Core.NominalDiffTime -- ^ 'startTime'
    -> Core.NominalDiffTime -- ^ 'endTime'
    -> ListAuditMitigationActionsTasks
mkListAuditMitigationActionsTasks startTime endTime
  = ListAuditMitigationActionsTasks'{startTime, endTime,
                                     auditTaskId = Core.Nothing, findingId = Core.Nothing,
                                     maxResults = Core.Nothing, nextToken = Core.Nothing,
                                     taskStatus = Core.Nothing}

-- | Specify this filter to limit results to tasks that began on or after a specific date and time.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatStartTime :: Lens.Lens' ListAuditMitigationActionsTasks Core.NominalDiffTime
lamatStartTime = Lens.field @"startTime"
{-# INLINEABLE lamatStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | Specify this filter to limit results to tasks that were completed or canceled on or before a specific date and time.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatEndTime :: Lens.Lens' ListAuditMitigationActionsTasks Core.NominalDiffTime
lamatEndTime = Lens.field @"endTime"
{-# INLINEABLE lamatEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | Specify this filter to limit results to tasks that were applied to results for a specific audit.
--
-- /Note:/ Consider using 'auditTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatAuditTaskId :: Lens.Lens' ListAuditMitigationActionsTasks (Core.Maybe Types.AuditTaskId)
lamatAuditTaskId = Lens.field @"auditTaskId"
{-# INLINEABLE lamatAuditTaskId #-}
{-# DEPRECATED auditTaskId "Use generic-lens or generic-optics with 'auditTaskId' instead"  #-}

-- | Specify this filter to limit results to tasks that were applied to a specific audit finding.
--
-- /Note:/ Consider using 'findingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatFindingId :: Lens.Lens' ListAuditMitigationActionsTasks (Core.Maybe Types.FindingId)
lamatFindingId = Lens.field @"findingId"
{-# INLINEABLE lamatFindingId #-}
{-# DEPRECATED findingId "Use generic-lens or generic-optics with 'findingId' instead"  #-}

-- | The maximum number of results to return at one time. The default is 25.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatMaxResults :: Lens.Lens' ListAuditMitigationActionsTasks (Core.Maybe Core.Natural)
lamatMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lamatMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatNextToken :: Lens.Lens' ListAuditMitigationActionsTasks (Core.Maybe Types.NextToken)
lamatNextToken = Lens.field @"nextToken"
{-# INLINEABLE lamatNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Specify this filter to limit results to tasks that are in a specific state.
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatTaskStatus :: Lens.Lens' ListAuditMitigationActionsTasks (Core.Maybe Types.AuditMitigationActionsTaskStatus)
lamatTaskStatus = Lens.field @"taskStatus"
{-# INLINEABLE lamatTaskStatus #-}
{-# DEPRECATED taskStatus "Use generic-lens or generic-optics with 'taskStatus' instead"  #-}

instance Core.ToQuery ListAuditMitigationActionsTasks where
        toQuery ListAuditMitigationActionsTasks{..}
          = Core.toQueryPair "startTime" startTime Core.<>
              Core.toQueryPair "endTime" endTime
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "auditTaskId") auditTaskId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "findingId") findingId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "taskStatus") taskStatus

instance Core.ToHeaders ListAuditMitigationActionsTasks where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListAuditMitigationActionsTasks where
        type Rs ListAuditMitigationActionsTasks =
             ListAuditMitigationActionsTasksResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/audit/mitigationactions/tasks",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAuditMitigationActionsTasksResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "tasks" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAuditMitigationActionsTasks where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"tasks" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListAuditMitigationActionsTasksResponse' smart constructor.
data ListAuditMitigationActionsTasksResponse = ListAuditMitigationActionsTasksResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results.
  , tasks :: Core.Maybe [Types.AuditMitigationActionsTaskMetadata]
    -- ^ The collection of audit mitigation tasks that matched the filter criteria.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListAuditMitigationActionsTasksResponse' value with any optional fields omitted.
mkListAuditMitigationActionsTasksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAuditMitigationActionsTasksResponse
mkListAuditMitigationActionsTasksResponse responseStatus
  = ListAuditMitigationActionsTasksResponse'{nextToken =
                                               Core.Nothing,
                                             tasks = Core.Nothing, responseStatus}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatrrsNextToken :: Lens.Lens' ListAuditMitigationActionsTasksResponse (Core.Maybe Types.NextToken)
lamatrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lamatrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The collection of audit mitigation tasks that matched the filter criteria.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatrrsTasks :: Lens.Lens' ListAuditMitigationActionsTasksResponse (Core.Maybe [Types.AuditMitigationActionsTaskMetadata])
lamatrrsTasks = Lens.field @"tasks"
{-# INLINEABLE lamatrrsTasks #-}
{-# DEPRECATED tasks "Use generic-lens or generic-optics with 'tasks' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamatrrsResponseStatus :: Lens.Lens' ListAuditMitigationActionsTasksResponse Core.Int
lamatrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lamatrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
