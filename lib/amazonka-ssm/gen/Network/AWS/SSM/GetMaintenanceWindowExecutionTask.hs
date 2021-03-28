{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetMaintenanceWindowExecutionTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details about a specific task run as part of a maintenance window execution.
module Network.AWS.SSM.GetMaintenanceWindowExecutionTask
    (
    -- * Creating a request
      GetMaintenanceWindowExecutionTask (..)
    , mkGetMaintenanceWindowExecutionTask
    -- ** Request lenses
    , gmwetWindowExecutionId
    , gmwetTaskId

    -- * Destructuring the response
    , GetMaintenanceWindowExecutionTaskResponse (..)
    , mkGetMaintenanceWindowExecutionTaskResponse
    -- ** Response lenses
    , gmwetrrsEndTime
    , gmwetrrsMaxConcurrency
    , gmwetrrsMaxErrors
    , gmwetrrsPriority
    , gmwetrrsServiceRole
    , gmwetrrsStartTime
    , gmwetrrsStatus
    , gmwetrrsStatusDetails
    , gmwetrrsTaskArn
    , gmwetrrsTaskExecutionId
    , gmwetrrsTaskParameters
    , gmwetrrsType
    , gmwetrrsWindowExecutionId
    , gmwetrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetMaintenanceWindowExecutionTask' smart constructor.
data GetMaintenanceWindowExecutionTask = GetMaintenanceWindowExecutionTask'
  { windowExecutionId :: Types.MaintenanceWindowExecutionId
    -- ^ The ID of the maintenance window execution that includes the task.
  , taskId :: Types.MaintenanceWindowExecutionTaskId
    -- ^ The ID of the specific task execution in the maintenance window task that should be retrieved.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMaintenanceWindowExecutionTask' value with any optional fields omitted.
mkGetMaintenanceWindowExecutionTask
    :: Types.MaintenanceWindowExecutionId -- ^ 'windowExecutionId'
    -> Types.MaintenanceWindowExecutionTaskId -- ^ 'taskId'
    -> GetMaintenanceWindowExecutionTask
mkGetMaintenanceWindowExecutionTask windowExecutionId taskId
  = GetMaintenanceWindowExecutionTask'{windowExecutionId, taskId}

-- | The ID of the maintenance window execution that includes the task.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetWindowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTask Types.MaintenanceWindowExecutionId
gmwetWindowExecutionId = Lens.field @"windowExecutionId"
{-# INLINEABLE gmwetWindowExecutionId #-}
{-# DEPRECATED windowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead"  #-}

-- | The ID of the specific task execution in the maintenance window task that should be retrieved.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetTaskId :: Lens.Lens' GetMaintenanceWindowExecutionTask Types.MaintenanceWindowExecutionTaskId
gmwetTaskId = Lens.field @"taskId"
{-# INLINEABLE gmwetTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

instance Core.ToQuery GetMaintenanceWindowExecutionTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetMaintenanceWindowExecutionTask where
        toHeaders GetMaintenanceWindowExecutionTask{..}
          = Core.pure
              ("X-Amz-Target", "AmazonSSM.GetMaintenanceWindowExecutionTask")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetMaintenanceWindowExecutionTask where
        toJSON GetMaintenanceWindowExecutionTask{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WindowExecutionId" Core..= windowExecutionId),
                  Core.Just ("TaskId" Core..= taskId)])

instance Core.AWSRequest GetMaintenanceWindowExecutionTask where
        type Rs GetMaintenanceWindowExecutionTask =
             GetMaintenanceWindowExecutionTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetMaintenanceWindowExecutionTaskResponse' Core.<$>
                   (x Core..:? "EndTime") Core.<*> x Core..:? "MaxConcurrency"
                     Core.<*> x Core..:? "MaxErrors"
                     Core.<*> x Core..:? "Priority"
                     Core.<*> x Core..:? "ServiceRole"
                     Core.<*> x Core..:? "StartTime"
                     Core.<*> x Core..:? "Status"
                     Core.<*> x Core..:? "StatusDetails"
                     Core.<*> x Core..:? "TaskArn"
                     Core.<*> x Core..:? "TaskExecutionId"
                     Core.<*> x Core..:? "TaskParameters"
                     Core.<*> x Core..:? "Type"
                     Core.<*> x Core..:? "WindowExecutionId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetMaintenanceWindowExecutionTaskResponse' smart constructor.
data GetMaintenanceWindowExecutionTaskResponse = GetMaintenanceWindowExecutionTaskResponse'
  { endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the task execution completed.
  , maxConcurrency :: Core.Maybe Types.MaxConcurrency
    -- ^ The defined maximum number of task executions that could be run in parallel.
  , maxErrors :: Core.Maybe Types.MaxErrors
    -- ^ The defined maximum number of task execution errors allowed before scheduling of the task execution would have been stopped.
  , priority :: Core.Maybe Core.Natural
    -- ^ The priority of the task.
  , serviceRole :: Core.Maybe Types.ServiceRole
    -- ^ The role that was assumed when running the task.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the task execution started.
  , status :: Core.Maybe Types.MaintenanceWindowExecutionStatus
    -- ^ The status of the task.
  , statusDetails :: Core.Maybe Types.MaintenanceWindowExecutionStatusDetails
    -- ^ The details explaining the Status. Only available for certain status values.
  , taskArn :: Core.Maybe Types.TaskArn
    -- ^ The ARN of the task that ran.
  , taskExecutionId :: Core.Maybe Types.MaintenanceWindowExecutionTaskId
    -- ^ The ID of the specific task execution in the maintenance window task that was retrieved.
  , taskParameters :: Core.Maybe [Core.HashMap Types.MaintenanceWindowTaskParameterName Types.MaintenanceWindowTaskParameterValueExpression]
    -- ^ The parameters passed to the task when it was run.
--
-- The map has the following format:
-- Key: string, between 1 and 255 characters
-- Value: an array of strings, each string is between 1 and 255 characters
  , type' :: Core.Maybe Types.MaintenanceWindowTaskType
    -- ^ The type of task that was run.
  , windowExecutionId :: Core.Maybe Types.MaintenanceWindowExecutionId
    -- ^ The ID of the maintenance window execution that includes the task.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetMaintenanceWindowExecutionTaskResponse' value with any optional fields omitted.
mkGetMaintenanceWindowExecutionTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetMaintenanceWindowExecutionTaskResponse
mkGetMaintenanceWindowExecutionTaskResponse responseStatus
  = GetMaintenanceWindowExecutionTaskResponse'{endTime =
                                                 Core.Nothing,
                                               maxConcurrency = Core.Nothing,
                                               maxErrors = Core.Nothing, priority = Core.Nothing,
                                               serviceRole = Core.Nothing, startTime = Core.Nothing,
                                               status = Core.Nothing, statusDetails = Core.Nothing,
                                               taskArn = Core.Nothing,
                                               taskExecutionId = Core.Nothing,
                                               taskParameters = Core.Nothing, type' = Core.Nothing,
                                               windowExecutionId = Core.Nothing, responseStatus}

-- | The time the task execution completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrrsEndTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Core.Maybe Core.NominalDiffTime)
gmwetrrsEndTime = Lens.field @"endTime"
{-# INLINEABLE gmwetrrsEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The defined maximum number of task executions that could be run in parallel.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrrsMaxConcurrency :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Core.Maybe Types.MaxConcurrency)
gmwetrrsMaxConcurrency = Lens.field @"maxConcurrency"
{-# INLINEABLE gmwetrrsMaxConcurrency #-}
{-# DEPRECATED maxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead"  #-}

-- | The defined maximum number of task execution errors allowed before scheduling of the task execution would have been stopped.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrrsMaxErrors :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Core.Maybe Types.MaxErrors)
gmwetrrsMaxErrors = Lens.field @"maxErrors"
{-# INLINEABLE gmwetrrsMaxErrors #-}
{-# DEPRECATED maxErrors "Use generic-lens or generic-optics with 'maxErrors' instead"  #-}

-- | The priority of the task.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrrsPriority :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Core.Maybe Core.Natural)
gmwetrrsPriority = Lens.field @"priority"
{-# INLINEABLE gmwetrrsPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | The role that was assumed when running the task.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrrsServiceRole :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Core.Maybe Types.ServiceRole)
gmwetrrsServiceRole = Lens.field @"serviceRole"
{-# INLINEABLE gmwetrrsServiceRole #-}
{-# DEPRECATED serviceRole "Use generic-lens or generic-optics with 'serviceRole' instead"  #-}

-- | The time the task execution started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrrsStartTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Core.Maybe Core.NominalDiffTime)
gmwetrrsStartTime = Lens.field @"startTime"
{-# INLINEABLE gmwetrrsStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The status of the task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrrsStatus :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Core.Maybe Types.MaintenanceWindowExecutionStatus)
gmwetrrsStatus = Lens.field @"status"
{-# INLINEABLE gmwetrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The details explaining the Status. Only available for certain status values.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrrsStatusDetails :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Core.Maybe Types.MaintenanceWindowExecutionStatusDetails)
gmwetrrsStatusDetails = Lens.field @"statusDetails"
{-# INLINEABLE gmwetrrsStatusDetails #-}
{-# DEPRECATED statusDetails "Use generic-lens or generic-optics with 'statusDetails' instead"  #-}

-- | The ARN of the task that ran.
--
-- /Note:/ Consider using 'taskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrrsTaskArn :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Core.Maybe Types.TaskArn)
gmwetrrsTaskArn = Lens.field @"taskArn"
{-# INLINEABLE gmwetrrsTaskArn #-}
{-# DEPRECATED taskArn "Use generic-lens or generic-optics with 'taskArn' instead"  #-}

-- | The ID of the specific task execution in the maintenance window task that was retrieved.
--
-- /Note:/ Consider using 'taskExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrrsTaskExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Core.Maybe Types.MaintenanceWindowExecutionTaskId)
gmwetrrsTaskExecutionId = Lens.field @"taskExecutionId"
{-# INLINEABLE gmwetrrsTaskExecutionId #-}
{-# DEPRECATED taskExecutionId "Use generic-lens or generic-optics with 'taskExecutionId' instead"  #-}

-- | The parameters passed to the task when it was run.
--
-- The map has the following format:
-- Key: string, between 1 and 255 characters
-- Value: an array of strings, each string is between 1 and 255 characters
--
-- /Note:/ Consider using 'taskParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrrsTaskParameters :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Core.Maybe [Core.HashMap Types.MaintenanceWindowTaskParameterName Types.MaintenanceWindowTaskParameterValueExpression])
gmwetrrsTaskParameters = Lens.field @"taskParameters"
{-# INLINEABLE gmwetrrsTaskParameters #-}
{-# DEPRECATED taskParameters "Use generic-lens or generic-optics with 'taskParameters' instead"  #-}

-- | The type of task that was run.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrrsType :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Core.Maybe Types.MaintenanceWindowTaskType)
gmwetrrsType = Lens.field @"type'"
{-# INLINEABLE gmwetrrsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The ID of the maintenance window execution that includes the task.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrrsWindowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Core.Maybe Types.MaintenanceWindowExecutionId)
gmwetrrsWindowExecutionId = Lens.field @"windowExecutionId"
{-# INLINEABLE gmwetrrsWindowExecutionId #-}
{-# DEPRECATED windowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrrsResponseStatus :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse Core.Int
gmwetrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmwetrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
