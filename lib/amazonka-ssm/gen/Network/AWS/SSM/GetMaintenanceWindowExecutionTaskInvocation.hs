{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetMaintenanceWindowExecutionTaskInvocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specific task running on a specific target.
module Network.AWS.SSM.GetMaintenanceWindowExecutionTaskInvocation
    (
    -- * Creating a request
      GetMaintenanceWindowExecutionTaskInvocation (..)
    , mkGetMaintenanceWindowExecutionTaskInvocation
    -- ** Request lenses
    , gmwetiWindowExecutionId
    , gmwetiTaskId
    , gmwetiInvocationId

    -- * Destructuring the response
    , GetMaintenanceWindowExecutionTaskInvocationResponse (..)
    , mkGetMaintenanceWindowExecutionTaskInvocationResponse
    -- ** Response lenses
    , gmwetirrsEndTime
    , gmwetirrsExecutionId
    , gmwetirrsInvocationId
    , gmwetirrsOwnerInformation
    , gmwetirrsParameters
    , gmwetirrsStartTime
    , gmwetirrsStatus
    , gmwetirrsStatusDetails
    , gmwetirrsTaskExecutionId
    , gmwetirrsTaskType
    , gmwetirrsWindowExecutionId
    , gmwetirrsWindowTargetId
    , gmwetirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetMaintenanceWindowExecutionTaskInvocation' smart constructor.
data GetMaintenanceWindowExecutionTaskInvocation = GetMaintenanceWindowExecutionTaskInvocation'
  { windowExecutionId :: Types.MaintenanceWindowExecutionId
    -- ^ The ID of the maintenance window execution for which the task is a part.
  , taskId :: Types.MaintenanceWindowExecutionTaskId
    -- ^ The ID of the specific task in the maintenance window task that should be retrieved. 
  , invocationId :: Types.InvocationId
    -- ^ The invocation ID to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMaintenanceWindowExecutionTaskInvocation' value with any optional fields omitted.
mkGetMaintenanceWindowExecutionTaskInvocation
    :: Types.MaintenanceWindowExecutionId -- ^ 'windowExecutionId'
    -> Types.MaintenanceWindowExecutionTaskId -- ^ 'taskId'
    -> Types.InvocationId -- ^ 'invocationId'
    -> GetMaintenanceWindowExecutionTaskInvocation
mkGetMaintenanceWindowExecutionTaskInvocation windowExecutionId
  taskId invocationId
  = GetMaintenanceWindowExecutionTaskInvocation'{windowExecutionId,
                                                 taskId, invocationId}

-- | The ID of the maintenance window execution for which the task is a part.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetiWindowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocation Types.MaintenanceWindowExecutionId
gmwetiWindowExecutionId = Lens.field @"windowExecutionId"
{-# INLINEABLE gmwetiWindowExecutionId #-}
{-# DEPRECATED windowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead"  #-}

-- | The ID of the specific task in the maintenance window task that should be retrieved. 
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetiTaskId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocation Types.MaintenanceWindowExecutionTaskId
gmwetiTaskId = Lens.field @"taskId"
{-# INLINEABLE gmwetiTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

-- | The invocation ID to retrieve.
--
-- /Note:/ Consider using 'invocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetiInvocationId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocation Types.InvocationId
gmwetiInvocationId = Lens.field @"invocationId"
{-# INLINEABLE gmwetiInvocationId #-}
{-# DEPRECATED invocationId "Use generic-lens or generic-optics with 'invocationId' instead"  #-}

instance Core.ToQuery GetMaintenanceWindowExecutionTaskInvocation
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetMaintenanceWindowExecutionTaskInvocation
         where
        toHeaders GetMaintenanceWindowExecutionTaskInvocation{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonSSM.GetMaintenanceWindowExecutionTaskInvocation")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetMaintenanceWindowExecutionTaskInvocation
         where
        toJSON GetMaintenanceWindowExecutionTaskInvocation{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WindowExecutionId" Core..= windowExecutionId),
                  Core.Just ("TaskId" Core..= taskId),
                  Core.Just ("InvocationId" Core..= invocationId)])

instance Core.AWSRequest
           GetMaintenanceWindowExecutionTaskInvocation
         where
        type Rs GetMaintenanceWindowExecutionTaskInvocation =
             GetMaintenanceWindowExecutionTaskInvocationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetMaintenanceWindowExecutionTaskInvocationResponse' Core.<$>
                   (x Core..:? "EndTime") Core.<*> x Core..:? "ExecutionId" Core.<*>
                     x Core..:? "InvocationId"
                     Core.<*> x Core..:? "OwnerInformation"
                     Core.<*> x Core..:? "Parameters"
                     Core.<*> x Core..:? "StartTime"
                     Core.<*> x Core..:? "Status"
                     Core.<*> x Core..:? "StatusDetails"
                     Core.<*> x Core..:? "TaskExecutionId"
                     Core.<*> x Core..:? "TaskType"
                     Core.<*> x Core..:? "WindowExecutionId"
                     Core.<*> x Core..:? "WindowTargetId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetMaintenanceWindowExecutionTaskInvocationResponse' smart constructor.
data GetMaintenanceWindowExecutionTaskInvocationResponse = GetMaintenanceWindowExecutionTaskInvocationResponse'
  { endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the task finished running on the target.
  , executionId :: Core.Maybe Types.MaintenanceWindowExecutionTaskExecutionId
    -- ^ The execution ID.
  , invocationId :: Core.Maybe Types.MaintenanceWindowExecutionTaskInvocationId
    -- ^ The invocation ID.
  , ownerInformation :: Core.Maybe Types.OwnerInformation
    -- ^ User-provided value to be included in any CloudWatch events raised while running tasks for these targets in this maintenance window. 
  , parameters :: Core.Maybe Types.MaintenanceWindowExecutionTaskInvocationParameters
    -- ^ The parameters used at the time that the task ran.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the task started running on the target.
  , status :: Core.Maybe Types.MaintenanceWindowExecutionStatus
    -- ^ The task status for an invocation.
  , statusDetails :: Core.Maybe Types.MaintenanceWindowExecutionStatusDetails
    -- ^ The details explaining the status. Details are only available for certain status values.
  , taskExecutionId :: Core.Maybe Types.MaintenanceWindowExecutionTaskId
    -- ^ The task execution ID.
  , taskType :: Core.Maybe Types.MaintenanceWindowTaskType
    -- ^ Retrieves the task type for a maintenance window. Task types include the following: LAMBDA, STEP_FUNCTIONS, AUTOMATION, RUN_COMMAND.
  , windowExecutionId :: Core.Maybe Types.MaintenanceWindowExecutionId
    -- ^ The maintenance window execution ID.
  , windowTargetId :: Core.Maybe Types.MaintenanceWindowTaskTargetId
    -- ^ The maintenance window target ID.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetMaintenanceWindowExecutionTaskInvocationResponse' value with any optional fields omitted.
mkGetMaintenanceWindowExecutionTaskInvocationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetMaintenanceWindowExecutionTaskInvocationResponse
mkGetMaintenanceWindowExecutionTaskInvocationResponse
  responseStatus
  = GetMaintenanceWindowExecutionTaskInvocationResponse'{endTime =
                                                           Core.Nothing,
                                                         executionId = Core.Nothing,
                                                         invocationId = Core.Nothing,
                                                         ownerInformation = Core.Nothing,
                                                         parameters = Core.Nothing,
                                                         startTime = Core.Nothing,
                                                         status = Core.Nothing,
                                                         statusDetails = Core.Nothing,
                                                         taskExecutionId = Core.Nothing,
                                                         taskType = Core.Nothing,
                                                         windowExecutionId = Core.Nothing,
                                                         windowTargetId = Core.Nothing,
                                                         responseStatus}

-- | The time that the task finished running on the target.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsEndTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Core.NominalDiffTime)
gmwetirrsEndTime = Lens.field @"endTime"
{-# INLINEABLE gmwetirrsEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The execution ID.
--
-- /Note:/ Consider using 'executionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowExecutionTaskExecutionId)
gmwetirrsExecutionId = Lens.field @"executionId"
{-# INLINEABLE gmwetirrsExecutionId #-}
{-# DEPRECATED executionId "Use generic-lens or generic-optics with 'executionId' instead"  #-}

-- | The invocation ID.
--
-- /Note:/ Consider using 'invocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsInvocationId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowExecutionTaskInvocationId)
gmwetirrsInvocationId = Lens.field @"invocationId"
{-# INLINEABLE gmwetirrsInvocationId #-}
{-# DEPRECATED invocationId "Use generic-lens or generic-optics with 'invocationId' instead"  #-}

-- | User-provided value to be included in any CloudWatch events raised while running tasks for these targets in this maintenance window. 
--
-- /Note:/ Consider using 'ownerInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsOwnerInformation :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.OwnerInformation)
gmwetirrsOwnerInformation = Lens.field @"ownerInformation"
{-# INLINEABLE gmwetirrsOwnerInformation #-}
{-# DEPRECATED ownerInformation "Use generic-lens or generic-optics with 'ownerInformation' instead"  #-}

-- | The parameters used at the time that the task ran.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsParameters :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowExecutionTaskInvocationParameters)
gmwetirrsParameters = Lens.field @"parameters"
{-# INLINEABLE gmwetirrsParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The time that the task started running on the target.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsStartTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Core.NominalDiffTime)
gmwetirrsStartTime = Lens.field @"startTime"
{-# INLINEABLE gmwetirrsStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The task status for an invocation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsStatus :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowExecutionStatus)
gmwetirrsStatus = Lens.field @"status"
{-# INLINEABLE gmwetirrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The details explaining the status. Details are only available for certain status values.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsStatusDetails :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowExecutionStatusDetails)
gmwetirrsStatusDetails = Lens.field @"statusDetails"
{-# INLINEABLE gmwetirrsStatusDetails #-}
{-# DEPRECATED statusDetails "Use generic-lens or generic-optics with 'statusDetails' instead"  #-}

-- | The task execution ID.
--
-- /Note:/ Consider using 'taskExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsTaskExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowExecutionTaskId)
gmwetirrsTaskExecutionId = Lens.field @"taskExecutionId"
{-# INLINEABLE gmwetirrsTaskExecutionId #-}
{-# DEPRECATED taskExecutionId "Use generic-lens or generic-optics with 'taskExecutionId' instead"  #-}

-- | Retrieves the task type for a maintenance window. Task types include the following: LAMBDA, STEP_FUNCTIONS, AUTOMATION, RUN_COMMAND.
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsTaskType :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowTaskType)
gmwetirrsTaskType = Lens.field @"taskType"
{-# INLINEABLE gmwetirrsTaskType #-}
{-# DEPRECATED taskType "Use generic-lens or generic-optics with 'taskType' instead"  #-}

-- | The maintenance window execution ID.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsWindowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowExecutionId)
gmwetirrsWindowExecutionId = Lens.field @"windowExecutionId"
{-# INLINEABLE gmwetirrsWindowExecutionId #-}
{-# DEPRECATED windowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead"  #-}

-- | The maintenance window target ID.
--
-- /Note:/ Consider using 'windowTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsWindowTargetId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowTaskTargetId)
gmwetirrsWindowTargetId = Lens.field @"windowTargetId"
{-# INLINEABLE gmwetirrsWindowTargetId #-}
{-# DEPRECATED windowTargetId "Use generic-lens or generic-optics with 'windowTargetId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsResponseStatus :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse Core.Int
gmwetirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmwetirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
