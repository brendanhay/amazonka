{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeAuditMitigationActionsTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an audit mitigation task that is used to apply mitigation actions to a set of audit findings. Properties include the actions being applied, the audit checks to which they're being applied, the task status, and aggregated task statistics.
module Network.AWS.IoT.DescribeAuditMitigationActionsTask
    (
    -- * Creating a request
      DescribeAuditMitigationActionsTask (..)
    , mkDescribeAuditMitigationActionsTask
    -- ** Request lenses
    , damatTaskId

    -- * Destructuring the response
    , DescribeAuditMitigationActionsTaskResponse (..)
    , mkDescribeAuditMitigationActionsTaskResponse
    -- ** Response lenses
    , damatrrsActionsDefinition
    , damatrrsAuditCheckToActionsMapping
    , damatrrsEndTime
    , damatrrsStartTime
    , damatrrsTarget
    , damatrrsTaskStatistics
    , damatrrsTaskStatus
    , damatrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAuditMitigationActionsTask' smart constructor.
newtype DescribeAuditMitigationActionsTask = DescribeAuditMitigationActionsTask'
  { taskId :: Types.AuditMitigationActionsTaskId
    -- ^ The unique identifier for the audit mitigation task.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAuditMitigationActionsTask' value with any optional fields omitted.
mkDescribeAuditMitigationActionsTask
    :: Types.AuditMitigationActionsTaskId -- ^ 'taskId'
    -> DescribeAuditMitigationActionsTask
mkDescribeAuditMitigationActionsTask taskId
  = DescribeAuditMitigationActionsTask'{taskId}

-- | The unique identifier for the audit mitigation task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatTaskId :: Lens.Lens' DescribeAuditMitigationActionsTask Types.AuditMitigationActionsTaskId
damatTaskId = Lens.field @"taskId"
{-# INLINEABLE damatTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

instance Core.ToQuery DescribeAuditMitigationActionsTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAuditMitigationActionsTask where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeAuditMitigationActionsTask where
        type Rs DescribeAuditMitigationActionsTask =
             DescribeAuditMitigationActionsTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/audit/mitigationactions/tasks/" Core.<> Core.toText taskId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAuditMitigationActionsTaskResponse' Core.<$>
                   (x Core..:? "actionsDefinition") Core.<*>
                     x Core..:? "auditCheckToActionsMapping"
                     Core.<*> x Core..:? "endTime"
                     Core.<*> x Core..:? "startTime"
                     Core.<*> x Core..:? "target"
                     Core.<*> x Core..:? "taskStatistics"
                     Core.<*> x Core..:? "taskStatus"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAuditMitigationActionsTaskResponse' smart constructor.
data DescribeAuditMitigationActionsTaskResponse = DescribeAuditMitigationActionsTaskResponse'
  { actionsDefinition :: Core.Maybe [Types.MitigationAction]
    -- ^ Specifies the mitigation actions and their parameters that are applied as part of this task.
  , auditCheckToActionsMapping :: Core.Maybe (Core.HashMap Types.AuditCheckName (Core.NonEmpty Types.MitigationActionName))
    -- ^ Specifies the mitigation actions that should be applied to specific audit checks.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the task was completed or canceled.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the task was started.
  , target :: Core.Maybe Types.AuditMitigationActionsTaskTarget
    -- ^ Identifies the findings to which the mitigation actions are applied. This can be by audit checks, by audit task, or a set of findings.
  , taskStatistics :: Core.Maybe (Core.HashMap Types.AuditCheckName Types.TaskStatisticsForAuditCheck)
    -- ^ Aggregate counts of the results when the mitigation tasks were applied to the findings for this audit mitigation actions task.
  , taskStatus :: Core.Maybe Types.AuditMitigationActionsTaskStatus
    -- ^ The current status of the task.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAuditMitigationActionsTaskResponse' value with any optional fields omitted.
mkDescribeAuditMitigationActionsTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAuditMitigationActionsTaskResponse
mkDescribeAuditMitigationActionsTaskResponse responseStatus
  = DescribeAuditMitigationActionsTaskResponse'{actionsDefinition =
                                                  Core.Nothing,
                                                auditCheckToActionsMapping = Core.Nothing,
                                                endTime = Core.Nothing, startTime = Core.Nothing,
                                                target = Core.Nothing,
                                                taskStatistics = Core.Nothing,
                                                taskStatus = Core.Nothing, responseStatus}

-- | Specifies the mitigation actions and their parameters that are applied as part of this task.
--
-- /Note:/ Consider using 'actionsDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatrrsActionsDefinition :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Core.Maybe [Types.MitigationAction])
damatrrsActionsDefinition = Lens.field @"actionsDefinition"
{-# INLINEABLE damatrrsActionsDefinition #-}
{-# DEPRECATED actionsDefinition "Use generic-lens or generic-optics with 'actionsDefinition' instead"  #-}

-- | Specifies the mitigation actions that should be applied to specific audit checks.
--
-- /Note:/ Consider using 'auditCheckToActionsMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatrrsAuditCheckToActionsMapping :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Core.Maybe (Core.HashMap Types.AuditCheckName (Core.NonEmpty Types.MitigationActionName)))
damatrrsAuditCheckToActionsMapping = Lens.field @"auditCheckToActionsMapping"
{-# INLINEABLE damatrrsAuditCheckToActionsMapping #-}
{-# DEPRECATED auditCheckToActionsMapping "Use generic-lens or generic-optics with 'auditCheckToActionsMapping' instead"  #-}

-- | The date and time when the task was completed or canceled.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatrrsEndTime :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Core.Maybe Core.NominalDiffTime)
damatrrsEndTime = Lens.field @"endTime"
{-# INLINEABLE damatrrsEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The date and time when the task was started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatrrsStartTime :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Core.Maybe Core.NominalDiffTime)
damatrrsStartTime = Lens.field @"startTime"
{-# INLINEABLE damatrrsStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | Identifies the findings to which the mitigation actions are applied. This can be by audit checks, by audit task, or a set of findings.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatrrsTarget :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Core.Maybe Types.AuditMitigationActionsTaskTarget)
damatrrsTarget = Lens.field @"target"
{-# INLINEABLE damatrrsTarget #-}
{-# DEPRECATED target "Use generic-lens or generic-optics with 'target' instead"  #-}

-- | Aggregate counts of the results when the mitigation tasks were applied to the findings for this audit mitigation actions task.
--
-- /Note:/ Consider using 'taskStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatrrsTaskStatistics :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Core.Maybe (Core.HashMap Types.AuditCheckName Types.TaskStatisticsForAuditCheck))
damatrrsTaskStatistics = Lens.field @"taskStatistics"
{-# INLINEABLE damatrrsTaskStatistics #-}
{-# DEPRECATED taskStatistics "Use generic-lens or generic-optics with 'taskStatistics' instead"  #-}

-- | The current status of the task.
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatrrsTaskStatus :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Core.Maybe Types.AuditMitigationActionsTaskStatus)
damatrrsTaskStatus = Lens.field @"taskStatus"
{-# INLINEABLE damatrrsTaskStatus #-}
{-# DEPRECATED taskStatus "Use generic-lens or generic-optics with 'taskStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatrrsResponseStatus :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse Core.Int
damatrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE damatrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
