{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListAuditTasks (..)
    , mkListAuditTasks
    -- ** Request lenses
    , latStartTime
    , latEndTime
    , latMaxResults
    , latNextToken
    , latTaskStatus
    , latTaskType

    -- * Destructuring the response
    , ListAuditTasksResponse (..)
    , mkListAuditTasksResponse
    -- ** Response lenses
    , latrrsNextToken
    , latrrsTasks
    , latrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAuditTasks' smart constructor.
data ListAuditTasks = ListAuditTasks'
  { startTime :: Core.NominalDiffTime
    -- ^ The beginning of the time period. Audit information is retained for a limited time (90 days). Requesting a start time prior to what is retained results in an "InvalidRequestException".
  , endTime :: Core.NominalDiffTime
    -- ^ The end of the time period.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return at one time. The default is 25.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results.
  , taskStatus :: Core.Maybe Types.AuditTaskStatus
    -- ^ A filter to limit the output to audits with the specified completion status: can be one of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
  , taskType :: Core.Maybe Types.AuditTaskType
    -- ^ A filter to limit the output to the specified type of audit: can be one of "ON_DEMAND_AUDIT_TASK" or "SCHEDULED__AUDIT_TASK".
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListAuditTasks' value with any optional fields omitted.
mkListAuditTasks
    :: Core.NominalDiffTime -- ^ 'startTime'
    -> Core.NominalDiffTime -- ^ 'endTime'
    -> ListAuditTasks
mkListAuditTasks startTime endTime
  = ListAuditTasks'{startTime, endTime, maxResults = Core.Nothing,
                    nextToken = Core.Nothing, taskStatus = Core.Nothing,
                    taskType = Core.Nothing}

-- | The beginning of the time period. Audit information is retained for a limited time (90 days). Requesting a start time prior to what is retained results in an "InvalidRequestException".
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latStartTime :: Lens.Lens' ListAuditTasks Core.NominalDiffTime
latStartTime = Lens.field @"startTime"
{-# INLINEABLE latStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The end of the time period.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latEndTime :: Lens.Lens' ListAuditTasks Core.NominalDiffTime
latEndTime = Lens.field @"endTime"
{-# INLINEABLE latEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The maximum number of results to return at one time. The default is 25.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latMaxResults :: Lens.Lens' ListAuditTasks (Core.Maybe Core.Natural)
latMaxResults = Lens.field @"maxResults"
{-# INLINEABLE latMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latNextToken :: Lens.Lens' ListAuditTasks (Core.Maybe Types.NextToken)
latNextToken = Lens.field @"nextToken"
{-# INLINEABLE latNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A filter to limit the output to audits with the specified completion status: can be one of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latTaskStatus :: Lens.Lens' ListAuditTasks (Core.Maybe Types.AuditTaskStatus)
latTaskStatus = Lens.field @"taskStatus"
{-# INLINEABLE latTaskStatus #-}
{-# DEPRECATED taskStatus "Use generic-lens or generic-optics with 'taskStatus' instead"  #-}

-- | A filter to limit the output to the specified type of audit: can be one of "ON_DEMAND_AUDIT_TASK" or "SCHEDULED__AUDIT_TASK".
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latTaskType :: Lens.Lens' ListAuditTasks (Core.Maybe Types.AuditTaskType)
latTaskType = Lens.field @"taskType"
{-# INLINEABLE latTaskType #-}
{-# DEPRECATED taskType "Use generic-lens or generic-optics with 'taskType' instead"  #-}

instance Core.ToQuery ListAuditTasks where
        toQuery ListAuditTasks{..}
          = Core.toQueryPair "startTime" startTime Core.<>
              Core.toQueryPair "endTime" endTime
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "taskStatus") taskStatus
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "taskType") taskType

instance Core.ToHeaders ListAuditTasks where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListAuditTasks where
        type Rs ListAuditTasks = ListAuditTasksResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/audit/tasks",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAuditTasksResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "tasks" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAuditTasks where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"tasks" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListAuditTasksResponse' smart constructor.
data ListAuditTasksResponse = ListAuditTasksResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
  , tasks :: Core.Maybe [Types.AuditTaskMetadata]
    -- ^ The audits that were performed during the specified time period.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAuditTasksResponse' value with any optional fields omitted.
mkListAuditTasksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAuditTasksResponse
mkListAuditTasksResponse responseStatus
  = ListAuditTasksResponse'{nextToken = Core.Nothing,
                            tasks = Core.Nothing, responseStatus}

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrrsNextToken :: Lens.Lens' ListAuditTasksResponse (Core.Maybe Types.NextToken)
latrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE latrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The audits that were performed during the specified time period.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrrsTasks :: Lens.Lens' ListAuditTasksResponse (Core.Maybe [Types.AuditTaskMetadata])
latrrsTasks = Lens.field @"tasks"
{-# INLINEABLE latrrsTasks #-}
{-# DEPRECATED tasks "Use generic-lens or generic-optics with 'tasks' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrrsResponseStatus :: Lens.Lens' ListAuditTasksResponse Core.Int
latrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE latrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
