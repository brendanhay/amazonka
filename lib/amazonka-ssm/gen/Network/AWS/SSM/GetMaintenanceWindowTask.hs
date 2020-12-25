{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetMaintenanceWindowTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tasks in a maintenance window.
module Network.AWS.SSM.GetMaintenanceWindowTask
  ( -- * Creating a request
    GetMaintenanceWindowTask (..),
    mkGetMaintenanceWindowTask,

    -- ** Request lenses
    gmwtWindowId,
    gmwtWindowTaskId,

    -- * Destructuring the response
    GetMaintenanceWindowTaskResponse (..),
    mkGetMaintenanceWindowTaskResponse,

    -- ** Response lenses
    gmwtrrsDescription,
    gmwtrrsLoggingInfo,
    gmwtrrsMaxConcurrency,
    gmwtrrsMaxErrors,
    gmwtrrsName,
    gmwtrrsPriority,
    gmwtrrsServiceRoleArn,
    gmwtrrsTargets,
    gmwtrrsTaskArn,
    gmwtrrsTaskInvocationParameters,
    gmwtrrsTaskParameters,
    gmwtrrsTaskType,
    gmwtrrsWindowId,
    gmwtrrsWindowTaskId,
    gmwtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetMaintenanceWindowTask' smart constructor.
data GetMaintenanceWindowTask = GetMaintenanceWindowTask'
  { -- | The maintenance window ID that includes the task to retrieve.
    windowId :: Types.MaintenanceWindowId,
    -- | The maintenance window task ID to retrieve.
    windowTaskId :: Types.MaintenanceWindowTaskId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMaintenanceWindowTask' value with any optional fields omitted.
mkGetMaintenanceWindowTask ::
  -- | 'windowId'
  Types.MaintenanceWindowId ->
  -- | 'windowTaskId'
  Types.MaintenanceWindowTaskId ->
  GetMaintenanceWindowTask
mkGetMaintenanceWindowTask windowId windowTaskId =
  GetMaintenanceWindowTask' {windowId, windowTaskId}

-- | The maintenance window ID that includes the task to retrieve.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtWindowId :: Lens.Lens' GetMaintenanceWindowTask Types.MaintenanceWindowId
gmwtWindowId = Lens.field @"windowId"
{-# DEPRECATED gmwtWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The maintenance window task ID to retrieve.
--
-- /Note:/ Consider using 'windowTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtWindowTaskId :: Lens.Lens' GetMaintenanceWindowTask Types.MaintenanceWindowTaskId
gmwtWindowTaskId = Lens.field @"windowTaskId"
{-# DEPRECATED gmwtWindowTaskId "Use generic-lens or generic-optics with 'windowTaskId' instead." #-}

instance Core.FromJSON GetMaintenanceWindowTask where
  toJSON GetMaintenanceWindowTask {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WindowId" Core..= windowId),
            Core.Just ("WindowTaskId" Core..= windowTaskId)
          ]
      )

instance Core.AWSRequest GetMaintenanceWindowTask where
  type Rs GetMaintenanceWindowTask = GetMaintenanceWindowTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.GetMaintenanceWindowTask")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMaintenanceWindowTaskResponse'
            Core.<$> (x Core..:? "Description")
            Core.<*> (x Core..:? "LoggingInfo")
            Core.<*> (x Core..:? "MaxConcurrency")
            Core.<*> (x Core..:? "MaxErrors")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Priority")
            Core.<*> (x Core..:? "ServiceRoleArn")
            Core.<*> (x Core..:? "Targets")
            Core.<*> (x Core..:? "TaskArn")
            Core.<*> (x Core..:? "TaskInvocationParameters")
            Core.<*> (x Core..:? "TaskParameters")
            Core.<*> (x Core..:? "TaskType")
            Core.<*> (x Core..:? "WindowId")
            Core.<*> (x Core..:? "WindowTaskId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetMaintenanceWindowTaskResponse' smart constructor.
data GetMaintenanceWindowTaskResponse = GetMaintenanceWindowTaskResponse'
  { -- | The retrieved task description.
    description :: Core.Maybe Types.Description,
    -- | The location in Amazon S3 where the task results are logged.
    loggingInfo :: Core.Maybe Types.LoggingInfo,
    -- | The maximum number of targets allowed to run this task in parallel.
    maxConcurrency :: Core.Maybe Types.MaxConcurrency,
    -- | The maximum number of errors allowed before the task stops being scheduled.
    maxErrors :: Core.Maybe Types.MaxErrors,
    -- | The retrieved task name.
    name :: Core.Maybe Types.Name,
    -- | The priority of the task when it runs. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
    priority :: Core.Maybe Core.Natural,
    -- | The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
    serviceRoleArn :: Core.Maybe Types.ServiceRoleArn,
    -- | The targets where the task should run.
    targets :: Core.Maybe [Types.Target],
    -- | The resource that the task used during execution. For RUN_COMMAND and AUTOMATION task types, the TaskArn is the Systems Manager Document name/ARN. For LAMBDA tasks, the value is the function name/ARN. For STEP_FUNCTIONS tasks, the value is the state machine ARN.
    taskArn :: Core.Maybe Types.TaskArn,
    -- | The parameters to pass to the task when it runs.
    taskInvocationParameters :: Core.Maybe Types.MaintenanceWindowTaskInvocationParameters,
    -- | The parameters to pass to the task when it runs.
    taskParameters :: Core.Maybe (Core.HashMap Types.MaintenanceWindowTaskParameterName Types.MaintenanceWindowTaskParameterValueExpression),
    -- | The type of task to run.
    taskType :: Core.Maybe Types.MaintenanceWindowTaskType,
    -- | The retrieved maintenance window ID.
    windowId :: Core.Maybe Types.WindowId,
    -- | The retrieved maintenance window task ID.
    windowTaskId :: Core.Maybe Types.WindowTaskId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMaintenanceWindowTaskResponse' value with any optional fields omitted.
mkGetMaintenanceWindowTaskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetMaintenanceWindowTaskResponse
mkGetMaintenanceWindowTaskResponse responseStatus =
  GetMaintenanceWindowTaskResponse'
    { description = Core.Nothing,
      loggingInfo = Core.Nothing,
      maxConcurrency = Core.Nothing,
      maxErrors = Core.Nothing,
      name = Core.Nothing,
      priority = Core.Nothing,
      serviceRoleArn = Core.Nothing,
      targets = Core.Nothing,
      taskArn = Core.Nothing,
      taskInvocationParameters = Core.Nothing,
      taskParameters = Core.Nothing,
      taskType = Core.Nothing,
      windowId = Core.Nothing,
      windowTaskId = Core.Nothing,
      responseStatus
    }

-- | The retrieved task description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrrsDescription :: Lens.Lens' GetMaintenanceWindowTaskResponse (Core.Maybe Types.Description)
gmwtrrsDescription = Lens.field @"description"
{-# DEPRECATED gmwtrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The location in Amazon S3 where the task results are logged.
--
-- /Note:/ Consider using 'loggingInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrrsLoggingInfo :: Lens.Lens' GetMaintenanceWindowTaskResponse (Core.Maybe Types.LoggingInfo)
gmwtrrsLoggingInfo = Lens.field @"loggingInfo"
{-# DEPRECATED gmwtrrsLoggingInfo "Use generic-lens or generic-optics with 'loggingInfo' instead." #-}

-- | The maximum number of targets allowed to run this task in parallel.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrrsMaxConcurrency :: Lens.Lens' GetMaintenanceWindowTaskResponse (Core.Maybe Types.MaxConcurrency)
gmwtrrsMaxConcurrency = Lens.field @"maxConcurrency"
{-# DEPRECATED gmwtrrsMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The maximum number of errors allowed before the task stops being scheduled.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrrsMaxErrors :: Lens.Lens' GetMaintenanceWindowTaskResponse (Core.Maybe Types.MaxErrors)
gmwtrrsMaxErrors = Lens.field @"maxErrors"
{-# DEPRECATED gmwtrrsMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | The retrieved task name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrrsName :: Lens.Lens' GetMaintenanceWindowTaskResponse (Core.Maybe Types.Name)
gmwtrrsName = Lens.field @"name"
{-# DEPRECATED gmwtrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The priority of the task when it runs. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrrsPriority :: Lens.Lens' GetMaintenanceWindowTaskResponse (Core.Maybe Core.Natural)
gmwtrrsPriority = Lens.field @"priority"
{-# DEPRECATED gmwtrrsPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrrsServiceRoleArn :: Lens.Lens' GetMaintenanceWindowTaskResponse (Core.Maybe Types.ServiceRoleArn)
gmwtrrsServiceRoleArn = Lens.field @"serviceRoleArn"
{-# DEPRECATED gmwtrrsServiceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead." #-}

-- | The targets where the task should run.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrrsTargets :: Lens.Lens' GetMaintenanceWindowTaskResponse (Core.Maybe [Types.Target])
gmwtrrsTargets = Lens.field @"targets"
{-# DEPRECATED gmwtrrsTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The resource that the task used during execution. For RUN_COMMAND and AUTOMATION task types, the TaskArn is the Systems Manager Document name/ARN. For LAMBDA tasks, the value is the function name/ARN. For STEP_FUNCTIONS tasks, the value is the state machine ARN.
--
-- /Note:/ Consider using 'taskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrrsTaskArn :: Lens.Lens' GetMaintenanceWindowTaskResponse (Core.Maybe Types.TaskArn)
gmwtrrsTaskArn = Lens.field @"taskArn"
{-# DEPRECATED gmwtrrsTaskArn "Use generic-lens or generic-optics with 'taskArn' instead." #-}

-- | The parameters to pass to the task when it runs.
--
-- /Note:/ Consider using 'taskInvocationParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrrsTaskInvocationParameters :: Lens.Lens' GetMaintenanceWindowTaskResponse (Core.Maybe Types.MaintenanceWindowTaskInvocationParameters)
gmwtrrsTaskInvocationParameters = Lens.field @"taskInvocationParameters"
{-# DEPRECATED gmwtrrsTaskInvocationParameters "Use generic-lens or generic-optics with 'taskInvocationParameters' instead." #-}

-- | The parameters to pass to the task when it runs.
--
-- /Note:/ Consider using 'taskParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrrsTaskParameters :: Lens.Lens' GetMaintenanceWindowTaskResponse (Core.Maybe (Core.HashMap Types.MaintenanceWindowTaskParameterName Types.MaintenanceWindowTaskParameterValueExpression))
gmwtrrsTaskParameters = Lens.field @"taskParameters"
{-# DEPRECATED gmwtrrsTaskParameters "Use generic-lens or generic-optics with 'taskParameters' instead." #-}

-- | The type of task to run.
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrrsTaskType :: Lens.Lens' GetMaintenanceWindowTaskResponse (Core.Maybe Types.MaintenanceWindowTaskType)
gmwtrrsTaskType = Lens.field @"taskType"
{-# DEPRECATED gmwtrrsTaskType "Use generic-lens or generic-optics with 'taskType' instead." #-}

-- | The retrieved maintenance window ID.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrrsWindowId :: Lens.Lens' GetMaintenanceWindowTaskResponse (Core.Maybe Types.WindowId)
gmwtrrsWindowId = Lens.field @"windowId"
{-# DEPRECATED gmwtrrsWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The retrieved maintenance window task ID.
--
-- /Note:/ Consider using 'windowTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrrsWindowTaskId :: Lens.Lens' GetMaintenanceWindowTaskResponse (Core.Maybe Types.WindowTaskId)
gmwtrrsWindowTaskId = Lens.field @"windowTaskId"
{-# DEPRECATED gmwtrrsWindowTaskId "Use generic-lens or generic-optics with 'windowTaskId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrrsResponseStatus :: Lens.Lens' GetMaintenanceWindowTaskResponse Core.Int
gmwtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmwtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
