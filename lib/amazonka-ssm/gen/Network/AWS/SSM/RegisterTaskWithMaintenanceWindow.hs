{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.RegisterTaskWithMaintenanceWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new task to a maintenance window.
module Network.AWS.SSM.RegisterTaskWithMaintenanceWindow
  ( -- * Creating a request
    RegisterTaskWithMaintenanceWindow (..),
    mkRegisterTaskWithMaintenanceWindow,

    -- ** Request lenses
    rtwmwWindowId,
    rtwmwTargets,
    rtwmwTaskArn,
    rtwmwTaskType,
    rtwmwMaxConcurrency,
    rtwmwMaxErrors,
    rtwmwClientToken,
    rtwmwDescription,
    rtwmwLoggingInfo,
    rtwmwName,
    rtwmwPriority,
    rtwmwServiceRoleArn,
    rtwmwTaskInvocationParameters,
    rtwmwTaskParameters,

    -- * Destructuring the response
    RegisterTaskWithMaintenanceWindowResponse (..),
    mkRegisterTaskWithMaintenanceWindowResponse,

    -- ** Response lenses
    rtwmwrrsWindowTaskId,
    rtwmwrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkRegisterTaskWithMaintenanceWindow' smart constructor.
data RegisterTaskWithMaintenanceWindow = RegisterTaskWithMaintenanceWindow'
  { -- | The ID of the maintenance window the task should be added to.
    windowId :: Types.WindowId,
    -- | The targets (either instances or maintenance window targets).
    --
    -- Specify instances using the following format:
    -- @Key=InstanceIds,Values=<instance-id-1>,<instance-id-2>@
    -- Specify maintenance window targets using the following format:
    -- @Key=WindowTargetIds;,Values=<window-target-id-1>,<window-target-id-2>@
    targets :: [Types.Target],
    -- | The ARN of the task to run.
    taskArn :: Types.TaskArn,
    -- | The type of task being registered.
    taskType :: Types.MaintenanceWindowTaskType,
    -- | The maximum number of targets this task can be run for in parallel.
    maxConcurrency :: Types.MaxConcurrency,
    -- | The maximum number of errors allowed before this task stops being scheduled.
    maxErrors :: Types.MaxErrors,
    -- | User-provided idempotency token.
    clientToken :: Core.Maybe Types.ClientToken,
    -- | An optional description for the task.
    description :: Core.Maybe Types.Description,
    -- | A structure containing information about an S3 bucket to write instance-level logs to.
    loggingInfo :: Core.Maybe Types.LoggingInfo,
    -- | An optional name for the task.
    name :: Core.Maybe Types.MaintenanceWindowName,
    -- | The priority of the task in the maintenance window, the lower the number the higher the priority. Tasks in a maintenance window are scheduled in priority order with tasks that have the same priority scheduled in parallel.
    priority :: Core.Maybe Core.Natural,
    -- | The ARN of the IAM service role for Systems Manager to assume when running a maintenance window task. If you do not specify a service role ARN, Systems Manager uses your account's service-linked role. If no service-linked role for Systems Manager exists in your account, it is created when you run @RegisterTaskWithMaintenanceWindow@ .
    --
    -- For more information, see the following topics in the in the /AWS Systems Manager User Guide/ :
    --
    --     * <https://docs.aws.amazon.com/systems-manager/latest/userguide/using-service-linked-roles.html#slr-permissions Using service-linked roles for Systems Manager>
    --
    --
    --     * <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-maintenance-permissions.html#maintenance-window-tasks-service-role Should I use a service-linked role or a custom service role to run maintenance window tasks? >
    serviceRoleArn :: Core.Maybe Types.ServiceRoleArn,
    -- | The parameters that the task should use during execution. Populate only the fields that match the task type. All other fields should be empty.
    taskInvocationParameters :: Core.Maybe Types.MaintenanceWindowTaskInvocationParameters,
    -- | The parameters that should be passed to the task when it is run.
    taskParameters :: Core.Maybe (Core.HashMap Types.MaintenanceWindowTaskParameterName Types.MaintenanceWindowTaskParameterValueExpression)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTaskWithMaintenanceWindow' value with any optional fields omitted.
mkRegisterTaskWithMaintenanceWindow ::
  -- | 'windowId'
  Types.WindowId ->
  -- | 'taskArn'
  Types.TaskArn ->
  -- | 'taskType'
  Types.MaintenanceWindowTaskType ->
  -- | 'maxConcurrency'
  Types.MaxConcurrency ->
  -- | 'maxErrors'
  Types.MaxErrors ->
  RegisterTaskWithMaintenanceWindow
mkRegisterTaskWithMaintenanceWindow
  windowId
  taskArn
  taskType
  maxConcurrency
  maxErrors =
    RegisterTaskWithMaintenanceWindow'
      { windowId,
        targets = Core.mempty,
        taskArn,
        taskType,
        maxConcurrency,
        maxErrors,
        clientToken = Core.Nothing,
        description = Core.Nothing,
        loggingInfo = Core.Nothing,
        name = Core.Nothing,
        priority = Core.Nothing,
        serviceRoleArn = Core.Nothing,
        taskInvocationParameters = Core.Nothing,
        taskParameters = Core.Nothing
      }

-- | The ID of the maintenance window the task should be added to.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwWindowId :: Lens.Lens' RegisterTaskWithMaintenanceWindow Types.WindowId
rtwmwWindowId = Lens.field @"windowId"
{-# DEPRECATED rtwmwWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The targets (either instances or maintenance window targets).
--
-- Specify instances using the following format:
-- @Key=InstanceIds,Values=<instance-id-1>,<instance-id-2>@
-- Specify maintenance window targets using the following format:
-- @Key=WindowTargetIds;,Values=<window-target-id-1>,<window-target-id-2>@
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwTargets :: Lens.Lens' RegisterTaskWithMaintenanceWindow [Types.Target]
rtwmwTargets = Lens.field @"targets"
{-# DEPRECATED rtwmwTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The ARN of the task to run.
--
-- /Note:/ Consider using 'taskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwTaskArn :: Lens.Lens' RegisterTaskWithMaintenanceWindow Types.TaskArn
rtwmwTaskArn = Lens.field @"taskArn"
{-# DEPRECATED rtwmwTaskArn "Use generic-lens or generic-optics with 'taskArn' instead." #-}

-- | The type of task being registered.
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwTaskType :: Lens.Lens' RegisterTaskWithMaintenanceWindow Types.MaintenanceWindowTaskType
rtwmwTaskType = Lens.field @"taskType"
{-# DEPRECATED rtwmwTaskType "Use generic-lens or generic-optics with 'taskType' instead." #-}

-- | The maximum number of targets this task can be run for in parallel.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwMaxConcurrency :: Lens.Lens' RegisterTaskWithMaintenanceWindow Types.MaxConcurrency
rtwmwMaxConcurrency = Lens.field @"maxConcurrency"
{-# DEPRECATED rtwmwMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The maximum number of errors allowed before this task stops being scheduled.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwMaxErrors :: Lens.Lens' RegisterTaskWithMaintenanceWindow Types.MaxErrors
rtwmwMaxErrors = Lens.field @"maxErrors"
{-# DEPRECATED rtwmwMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | User-provided idempotency token.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwClientToken :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Core.Maybe Types.ClientToken)
rtwmwClientToken = Lens.field @"clientToken"
{-# DEPRECATED rtwmwClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | An optional description for the task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwDescription :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Core.Maybe Types.Description)
rtwmwDescription = Lens.field @"description"
{-# DEPRECATED rtwmwDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A structure containing information about an S3 bucket to write instance-level logs to.
--
-- /Note:/ Consider using 'loggingInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwLoggingInfo :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Core.Maybe Types.LoggingInfo)
rtwmwLoggingInfo = Lens.field @"loggingInfo"
{-# DEPRECATED rtwmwLoggingInfo "Use generic-lens or generic-optics with 'loggingInfo' instead." #-}

-- | An optional name for the task.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwName :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Core.Maybe Types.MaintenanceWindowName)
rtwmwName = Lens.field @"name"
{-# DEPRECATED rtwmwName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The priority of the task in the maintenance window, the lower the number the higher the priority. Tasks in a maintenance window are scheduled in priority order with tasks that have the same priority scheduled in parallel.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwPriority :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Core.Maybe Core.Natural)
rtwmwPriority = Lens.field @"priority"
{-# DEPRECATED rtwmwPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The ARN of the IAM service role for Systems Manager to assume when running a maintenance window task. If you do not specify a service role ARN, Systems Manager uses your account's service-linked role. If no service-linked role for Systems Manager exists in your account, it is created when you run @RegisterTaskWithMaintenanceWindow@ .
--
-- For more information, see the following topics in the in the /AWS Systems Manager User Guide/ :
--
--     * <https://docs.aws.amazon.com/systems-manager/latest/userguide/using-service-linked-roles.html#slr-permissions Using service-linked roles for Systems Manager>
--
--
--     * <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-maintenance-permissions.html#maintenance-window-tasks-service-role Should I use a service-linked role or a custom service role to run maintenance window tasks? >
--
--
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwServiceRoleArn :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Core.Maybe Types.ServiceRoleArn)
rtwmwServiceRoleArn = Lens.field @"serviceRoleArn"
{-# DEPRECATED rtwmwServiceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead." #-}

-- | The parameters that the task should use during execution. Populate only the fields that match the task type. All other fields should be empty.
--
-- /Note:/ Consider using 'taskInvocationParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwTaskInvocationParameters :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Core.Maybe Types.MaintenanceWindowTaskInvocationParameters)
rtwmwTaskInvocationParameters = Lens.field @"taskInvocationParameters"
{-# DEPRECATED rtwmwTaskInvocationParameters "Use generic-lens or generic-optics with 'taskInvocationParameters' instead." #-}

-- | The parameters that should be passed to the task when it is run.
--
-- /Note:/ Consider using 'taskParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwTaskParameters :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Core.Maybe (Core.HashMap Types.MaintenanceWindowTaskParameterName Types.MaintenanceWindowTaskParameterValueExpression))
rtwmwTaskParameters = Lens.field @"taskParameters"
{-# DEPRECATED rtwmwTaskParameters "Use generic-lens or generic-optics with 'taskParameters' instead." #-}

instance Core.FromJSON RegisterTaskWithMaintenanceWindow where
  toJSON RegisterTaskWithMaintenanceWindow {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WindowId" Core..= windowId),
            Core.Just ("Targets" Core..= targets),
            Core.Just ("TaskArn" Core..= taskArn),
            Core.Just ("TaskType" Core..= taskType),
            Core.Just ("MaxConcurrency" Core..= maxConcurrency),
            Core.Just ("MaxErrors" Core..= maxErrors),
            ("ClientToken" Core..=) Core.<$> clientToken,
            ("Description" Core..=) Core.<$> description,
            ("LoggingInfo" Core..=) Core.<$> loggingInfo,
            ("Name" Core..=) Core.<$> name,
            ("Priority" Core..=) Core.<$> priority,
            ("ServiceRoleArn" Core..=) Core.<$> serviceRoleArn,
            ("TaskInvocationParameters" Core..=)
              Core.<$> taskInvocationParameters,
            ("TaskParameters" Core..=) Core.<$> taskParameters
          ]
      )

instance Core.AWSRequest RegisterTaskWithMaintenanceWindow where
  type
    Rs RegisterTaskWithMaintenanceWindow =
      RegisterTaskWithMaintenanceWindowResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonSSM.RegisterTaskWithMaintenanceWindow")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterTaskWithMaintenanceWindowResponse'
            Core.<$> (x Core..:? "WindowTaskId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterTaskWithMaintenanceWindowResponse' smart constructor.
data RegisterTaskWithMaintenanceWindowResponse = RegisterTaskWithMaintenanceWindowResponse'
  { -- | The ID of the task in the maintenance window.
    windowTaskId :: Core.Maybe Types.WindowTaskId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTaskWithMaintenanceWindowResponse' value with any optional fields omitted.
mkRegisterTaskWithMaintenanceWindowResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterTaskWithMaintenanceWindowResponse
mkRegisterTaskWithMaintenanceWindowResponse responseStatus =
  RegisterTaskWithMaintenanceWindowResponse'
    { windowTaskId =
        Core.Nothing,
      responseStatus
    }

-- | The ID of the task in the maintenance window.
--
-- /Note:/ Consider using 'windowTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwrrsWindowTaskId :: Lens.Lens' RegisterTaskWithMaintenanceWindowResponse (Core.Maybe Types.WindowTaskId)
rtwmwrrsWindowTaskId = Lens.field @"windowTaskId"
{-# DEPRECATED rtwmwrrsWindowTaskId "Use generic-lens or generic-optics with 'windowTaskId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwrrsResponseStatus :: Lens.Lens' RegisterTaskWithMaintenanceWindowResponse Core.Int
rtwmwrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtwmwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
