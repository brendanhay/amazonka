{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetMaintenanceWindowExecutionTaskInvocation (..),
    mkGetMaintenanceWindowExecutionTaskInvocation,

    -- ** Request lenses
    gmwetiWindowExecutionId,
    gmwetiTaskId,
    gmwetiInvocationId,

    -- * Destructuring the response
    GetMaintenanceWindowExecutionTaskInvocationResponse (..),
    mkGetMaintenanceWindowExecutionTaskInvocationResponse,

    -- ** Response lenses
    gmwetirrsEndTime,
    gmwetirrsExecutionId,
    gmwetirrsInvocationId,
    gmwetirrsOwnerInformation,
    gmwetirrsParameters,
    gmwetirrsStartTime,
    gmwetirrsStatus,
    gmwetirrsStatusDetails,
    gmwetirrsTaskExecutionId,
    gmwetirrsTaskType,
    gmwetirrsWindowExecutionId,
    gmwetirrsWindowTargetId,
    gmwetirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetMaintenanceWindowExecutionTaskInvocation' smart constructor.
data GetMaintenanceWindowExecutionTaskInvocation = GetMaintenanceWindowExecutionTaskInvocation'
  { -- | The ID of the maintenance window execution for which the task is a part.
    windowExecutionId :: Types.MaintenanceWindowExecutionId,
    -- | The ID of the specific task in the maintenance window task that should be retrieved.
    taskId :: Types.MaintenanceWindowExecutionTaskId,
    -- | The invocation ID to retrieve.
    invocationId :: Types.InvocationId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMaintenanceWindowExecutionTaskInvocation' value with any optional fields omitted.
mkGetMaintenanceWindowExecutionTaskInvocation ::
  -- | 'windowExecutionId'
  Types.MaintenanceWindowExecutionId ->
  -- | 'taskId'
  Types.MaintenanceWindowExecutionTaskId ->
  -- | 'invocationId'
  Types.InvocationId ->
  GetMaintenanceWindowExecutionTaskInvocation
mkGetMaintenanceWindowExecutionTaskInvocation
  windowExecutionId
  taskId
  invocationId =
    GetMaintenanceWindowExecutionTaskInvocation'
      { windowExecutionId,
        taskId,
        invocationId
      }

-- | The ID of the maintenance window execution for which the task is a part.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetiWindowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocation Types.MaintenanceWindowExecutionId
gmwetiWindowExecutionId = Lens.field @"windowExecutionId"
{-# DEPRECATED gmwetiWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

-- | The ID of the specific task in the maintenance window task that should be retrieved.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetiTaskId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocation Types.MaintenanceWindowExecutionTaskId
gmwetiTaskId = Lens.field @"taskId"
{-# DEPRECATED gmwetiTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The invocation ID to retrieve.
--
-- /Note:/ Consider using 'invocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetiInvocationId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocation Types.InvocationId
gmwetiInvocationId = Lens.field @"invocationId"
{-# DEPRECATED gmwetiInvocationId "Use generic-lens or generic-optics with 'invocationId' instead." #-}

instance Core.FromJSON GetMaintenanceWindowExecutionTaskInvocation where
  toJSON GetMaintenanceWindowExecutionTaskInvocation {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WindowExecutionId" Core..= windowExecutionId),
            Core.Just ("TaskId" Core..= taskId),
            Core.Just ("InvocationId" Core..= invocationId)
          ]
      )

instance
  Core.AWSRequest
    GetMaintenanceWindowExecutionTaskInvocation
  where
  type
    Rs GetMaintenanceWindowExecutionTaskInvocation =
      GetMaintenanceWindowExecutionTaskInvocationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonSSM.GetMaintenanceWindowExecutionTaskInvocation"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMaintenanceWindowExecutionTaskInvocationResponse'
            Core.<$> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "ExecutionId")
            Core.<*> (x Core..:? "InvocationId")
            Core.<*> (x Core..:? "OwnerInformation")
            Core.<*> (x Core..:? "Parameters")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "StatusDetails")
            Core.<*> (x Core..:? "TaskExecutionId")
            Core.<*> (x Core..:? "TaskType")
            Core.<*> (x Core..:? "WindowExecutionId")
            Core.<*> (x Core..:? "WindowTargetId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetMaintenanceWindowExecutionTaskInvocationResponse' smart constructor.
data GetMaintenanceWindowExecutionTaskInvocationResponse = GetMaintenanceWindowExecutionTaskInvocationResponse'
  { -- | The time that the task finished running on the target.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The execution ID.
    executionId :: Core.Maybe Types.MaintenanceWindowExecutionTaskExecutionId,
    -- | The invocation ID.
    invocationId :: Core.Maybe Types.MaintenanceWindowExecutionTaskInvocationId,
    -- | User-provided value to be included in any CloudWatch events raised while running tasks for these targets in this maintenance window.
    ownerInformation :: Core.Maybe Types.OwnerInformation,
    -- | The parameters used at the time that the task ran.
    parameters :: Core.Maybe Types.MaintenanceWindowExecutionTaskInvocationParameters,
    -- | The time that the task started running on the target.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The task status for an invocation.
    status :: Core.Maybe Types.MaintenanceWindowExecutionStatus,
    -- | The details explaining the status. Details are only available for certain status values.
    statusDetails :: Core.Maybe Types.MaintenanceWindowExecutionStatusDetails,
    -- | The task execution ID.
    taskExecutionId :: Core.Maybe Types.MaintenanceWindowExecutionTaskId,
    -- | Retrieves the task type for a maintenance window. Task types include the following: LAMBDA, STEP_FUNCTIONS, AUTOMATION, RUN_COMMAND.
    taskType :: Core.Maybe Types.MaintenanceWindowTaskType,
    -- | The maintenance window execution ID.
    windowExecutionId :: Core.Maybe Types.MaintenanceWindowExecutionId,
    -- | The maintenance window target ID.
    windowTargetId :: Core.Maybe Types.MaintenanceWindowTaskTargetId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetMaintenanceWindowExecutionTaskInvocationResponse' value with any optional fields omitted.
mkGetMaintenanceWindowExecutionTaskInvocationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetMaintenanceWindowExecutionTaskInvocationResponse
mkGetMaintenanceWindowExecutionTaskInvocationResponse
  responseStatus =
    GetMaintenanceWindowExecutionTaskInvocationResponse'
      { endTime =
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
        responseStatus
      }

-- | The time that the task finished running on the target.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsEndTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Core.NominalDiffTime)
gmwetirrsEndTime = Lens.field @"endTime"
{-# DEPRECATED gmwetirrsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The execution ID.
--
-- /Note:/ Consider using 'executionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowExecutionTaskExecutionId)
gmwetirrsExecutionId = Lens.field @"executionId"
{-# DEPRECATED gmwetirrsExecutionId "Use generic-lens or generic-optics with 'executionId' instead." #-}

-- | The invocation ID.
--
-- /Note:/ Consider using 'invocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsInvocationId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowExecutionTaskInvocationId)
gmwetirrsInvocationId = Lens.field @"invocationId"
{-# DEPRECATED gmwetirrsInvocationId "Use generic-lens or generic-optics with 'invocationId' instead." #-}

-- | User-provided value to be included in any CloudWatch events raised while running tasks for these targets in this maintenance window.
--
-- /Note:/ Consider using 'ownerInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsOwnerInformation :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.OwnerInformation)
gmwetirrsOwnerInformation = Lens.field @"ownerInformation"
{-# DEPRECATED gmwetirrsOwnerInformation "Use generic-lens or generic-optics with 'ownerInformation' instead." #-}

-- | The parameters used at the time that the task ran.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsParameters :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowExecutionTaskInvocationParameters)
gmwetirrsParameters = Lens.field @"parameters"
{-# DEPRECATED gmwetirrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The time that the task started running on the target.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsStartTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Core.NominalDiffTime)
gmwetirrsStartTime = Lens.field @"startTime"
{-# DEPRECATED gmwetirrsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The task status for an invocation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsStatus :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowExecutionStatus)
gmwetirrsStatus = Lens.field @"status"
{-# DEPRECATED gmwetirrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The details explaining the status. Details are only available for certain status values.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsStatusDetails :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowExecutionStatusDetails)
gmwetirrsStatusDetails = Lens.field @"statusDetails"
{-# DEPRECATED gmwetirrsStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The task execution ID.
--
-- /Note:/ Consider using 'taskExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsTaskExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowExecutionTaskId)
gmwetirrsTaskExecutionId = Lens.field @"taskExecutionId"
{-# DEPRECATED gmwetirrsTaskExecutionId "Use generic-lens or generic-optics with 'taskExecutionId' instead." #-}

-- | Retrieves the task type for a maintenance window. Task types include the following: LAMBDA, STEP_FUNCTIONS, AUTOMATION, RUN_COMMAND.
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsTaskType :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowTaskType)
gmwetirrsTaskType = Lens.field @"taskType"
{-# DEPRECATED gmwetirrsTaskType "Use generic-lens or generic-optics with 'taskType' instead." #-}

-- | The maintenance window execution ID.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsWindowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowExecutionId)
gmwetirrsWindowExecutionId = Lens.field @"windowExecutionId"
{-# DEPRECATED gmwetirrsWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

-- | The maintenance window target ID.
--
-- /Note:/ Consider using 'windowTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsWindowTargetId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Core.Maybe Types.MaintenanceWindowTaskTargetId)
gmwetirrsWindowTargetId = Lens.field @"windowTargetId"
{-# DEPRECATED gmwetirrsWindowTargetId "Use generic-lens or generic-optics with 'windowTargetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirrsResponseStatus :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse Core.Int
gmwetirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmwetirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
