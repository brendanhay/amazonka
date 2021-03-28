{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeAuditTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender audit.
module Network.AWS.IoT.DescribeAuditTask
    (
    -- * Creating a request
      DescribeAuditTask (..)
    , mkDescribeAuditTask
    -- ** Request lenses
    , datTaskId

    -- * Destructuring the response
    , DescribeAuditTaskResponse (..)
    , mkDescribeAuditTaskResponse
    -- ** Response lenses
    , datrrsAuditDetails
    , datrrsScheduledAuditName
    , datrrsTaskStartTime
    , datrrsTaskStatistics
    , datrrsTaskStatus
    , datrrsTaskType
    , datrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAuditTask' smart constructor.
newtype DescribeAuditTask = DescribeAuditTask'
  { taskId :: Types.AuditTaskId
    -- ^ The ID of the audit whose information you want to get.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAuditTask' value with any optional fields omitted.
mkDescribeAuditTask
    :: Types.AuditTaskId -- ^ 'taskId'
    -> DescribeAuditTask
mkDescribeAuditTask taskId = DescribeAuditTask'{taskId}

-- | The ID of the audit whose information you want to get.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datTaskId :: Lens.Lens' DescribeAuditTask Types.AuditTaskId
datTaskId = Lens.field @"taskId"
{-# INLINEABLE datTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

instance Core.ToQuery DescribeAuditTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAuditTask where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeAuditTask where
        type Rs DescribeAuditTask = DescribeAuditTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/audit/tasks/" Core.<> Core.toText taskId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAuditTaskResponse' Core.<$>
                   (x Core..:? "auditDetails") Core.<*>
                     x Core..:? "scheduledAuditName"
                     Core.<*> x Core..:? "taskStartTime"
                     Core.<*> x Core..:? "taskStatistics"
                     Core.<*> x Core..:? "taskStatus"
                     Core.<*> x Core..:? "taskType"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAuditTaskResponse' smart constructor.
data DescribeAuditTaskResponse = DescribeAuditTaskResponse'
  { auditDetails :: Core.Maybe (Core.HashMap Types.AuditCheckName Types.AuditCheckDetails)
    -- ^ Detailed information about each check performed during this audit.
  , scheduledAuditName :: Core.Maybe Types.ScheduledAuditName
    -- ^ The name of the scheduled audit (only if the audit was a scheduled audit).
  , taskStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the audit started.
  , taskStatistics :: Core.Maybe Types.TaskStatistics
    -- ^ Statistical information about the audit.
  , taskStatus :: Core.Maybe Types.AuditTaskStatus
    -- ^ The status of the audit: one of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
  , taskType :: Core.Maybe Types.AuditTaskType
    -- ^ The type of audit: "ON_DEMAND_AUDIT_TASK" or "SCHEDULED_AUDIT_TASK".
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAuditTaskResponse' value with any optional fields omitted.
mkDescribeAuditTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAuditTaskResponse
mkDescribeAuditTaskResponse responseStatus
  = DescribeAuditTaskResponse'{auditDetails = Core.Nothing,
                               scheduledAuditName = Core.Nothing, taskStartTime = Core.Nothing,
                               taskStatistics = Core.Nothing, taskStatus = Core.Nothing,
                               taskType = Core.Nothing, responseStatus}

-- | Detailed information about each check performed during this audit.
--
-- /Note:/ Consider using 'auditDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrrsAuditDetails :: Lens.Lens' DescribeAuditTaskResponse (Core.Maybe (Core.HashMap Types.AuditCheckName Types.AuditCheckDetails))
datrrsAuditDetails = Lens.field @"auditDetails"
{-# INLINEABLE datrrsAuditDetails #-}
{-# DEPRECATED auditDetails "Use generic-lens or generic-optics with 'auditDetails' instead"  #-}

-- | The name of the scheduled audit (only if the audit was a scheduled audit).
--
-- /Note:/ Consider using 'scheduledAuditName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrrsScheduledAuditName :: Lens.Lens' DescribeAuditTaskResponse (Core.Maybe Types.ScheduledAuditName)
datrrsScheduledAuditName = Lens.field @"scheduledAuditName"
{-# INLINEABLE datrrsScheduledAuditName #-}
{-# DEPRECATED scheduledAuditName "Use generic-lens or generic-optics with 'scheduledAuditName' instead"  #-}

-- | The time the audit started.
--
-- /Note:/ Consider using 'taskStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrrsTaskStartTime :: Lens.Lens' DescribeAuditTaskResponse (Core.Maybe Core.NominalDiffTime)
datrrsTaskStartTime = Lens.field @"taskStartTime"
{-# INLINEABLE datrrsTaskStartTime #-}
{-# DEPRECATED taskStartTime "Use generic-lens or generic-optics with 'taskStartTime' instead"  #-}

-- | Statistical information about the audit.
--
-- /Note:/ Consider using 'taskStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrrsTaskStatistics :: Lens.Lens' DescribeAuditTaskResponse (Core.Maybe Types.TaskStatistics)
datrrsTaskStatistics = Lens.field @"taskStatistics"
{-# INLINEABLE datrrsTaskStatistics #-}
{-# DEPRECATED taskStatistics "Use generic-lens or generic-optics with 'taskStatistics' instead"  #-}

-- | The status of the audit: one of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrrsTaskStatus :: Lens.Lens' DescribeAuditTaskResponse (Core.Maybe Types.AuditTaskStatus)
datrrsTaskStatus = Lens.field @"taskStatus"
{-# INLINEABLE datrrsTaskStatus #-}
{-# DEPRECATED taskStatus "Use generic-lens or generic-optics with 'taskStatus' instead"  #-}

-- | The type of audit: "ON_DEMAND_AUDIT_TASK" or "SCHEDULED_AUDIT_TASK".
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrrsTaskType :: Lens.Lens' DescribeAuditTaskResponse (Core.Maybe Types.AuditTaskType)
datrrsTaskType = Lens.field @"taskType"
{-# INLINEABLE datrrsTaskType #-}
{-# DEPRECATED taskType "Use generic-lens or generic-optics with 'taskType' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrrsResponseStatus :: Lens.Lens' DescribeAuditTaskResponse Core.Int
datrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE datrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
