{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gmwetirsStatus,
    gmwetirsExecutionId,
    gmwetirsTaskExecutionId,
    gmwetirsStartTime,
    gmwetirsInvocationId,
    gmwetirsOwnerInformation,
    gmwetirsTaskType,
    gmwetirsWindowTargetId,
    gmwetirsWindowExecutionId,
    gmwetirsStatusDetails,
    gmwetirsEndTime,
    gmwetirsParameters,
    gmwetirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetMaintenanceWindowExecutionTaskInvocation' smart constructor.
data GetMaintenanceWindowExecutionTaskInvocation = GetMaintenanceWindowExecutionTaskInvocation'
  { windowExecutionId ::
      Lude.Text,
    taskId ::
      Lude.Text,
    invocationId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMaintenanceWindowExecutionTaskInvocation' with the minimum fields required to make a request.
--
-- * 'invocationId' - The invocation ID to retrieve.
-- * 'taskId' - The ID of the specific task in the maintenance window task that should be retrieved.
-- * 'windowExecutionId' - The ID of the maintenance window execution for which the task is a part.
mkGetMaintenanceWindowExecutionTaskInvocation ::
  -- | 'windowExecutionId'
  Lude.Text ->
  -- | 'taskId'
  Lude.Text ->
  -- | 'invocationId'
  Lude.Text ->
  GetMaintenanceWindowExecutionTaskInvocation
mkGetMaintenanceWindowExecutionTaskInvocation
  pWindowExecutionId_
  pTaskId_
  pInvocationId_ =
    GetMaintenanceWindowExecutionTaskInvocation'
      { windowExecutionId =
          pWindowExecutionId_,
        taskId = pTaskId_,
        invocationId = pInvocationId_
      }

-- | The ID of the maintenance window execution for which the task is a part.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetiWindowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocation Lude.Text
gmwetiWindowExecutionId = Lens.lens (windowExecutionId :: GetMaintenanceWindowExecutionTaskInvocation -> Lude.Text) (\s a -> s {windowExecutionId = a} :: GetMaintenanceWindowExecutionTaskInvocation)
{-# DEPRECATED gmwetiWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

-- | The ID of the specific task in the maintenance window task that should be retrieved.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetiTaskId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocation Lude.Text
gmwetiTaskId = Lens.lens (taskId :: GetMaintenanceWindowExecutionTaskInvocation -> Lude.Text) (\s a -> s {taskId = a} :: GetMaintenanceWindowExecutionTaskInvocation)
{-# DEPRECATED gmwetiTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The invocation ID to retrieve.
--
-- /Note:/ Consider using 'invocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetiInvocationId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocation Lude.Text
gmwetiInvocationId = Lens.lens (invocationId :: GetMaintenanceWindowExecutionTaskInvocation -> Lude.Text) (\s a -> s {invocationId = a} :: GetMaintenanceWindowExecutionTaskInvocation)
{-# DEPRECATED gmwetiInvocationId "Use generic-lens or generic-optics with 'invocationId' instead." #-}

instance
  Lude.AWSRequest
    GetMaintenanceWindowExecutionTaskInvocation
  where
  type
    Rs GetMaintenanceWindowExecutionTaskInvocation =
      GetMaintenanceWindowExecutionTaskInvocationResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMaintenanceWindowExecutionTaskInvocationResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "ExecutionId")
            Lude.<*> (x Lude..?> "TaskExecutionId")
            Lude.<*> (x Lude..?> "StartTime")
            Lude.<*> (x Lude..?> "InvocationId")
            Lude.<*> (x Lude..?> "OwnerInformation")
            Lude.<*> (x Lude..?> "TaskType")
            Lude.<*> (x Lude..?> "WindowTargetId")
            Lude.<*> (x Lude..?> "WindowExecutionId")
            Lude.<*> (x Lude..?> "StatusDetails")
            Lude.<*> (x Lude..?> "EndTime")
            Lude.<*> (x Lude..?> "Parameters")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMaintenanceWindowExecutionTaskInvocation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonSSM.GetMaintenanceWindowExecutionTaskInvocation" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMaintenanceWindowExecutionTaskInvocation where
  toJSON GetMaintenanceWindowExecutionTaskInvocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("WindowExecutionId" Lude..= windowExecutionId),
            Lude.Just ("TaskId" Lude..= taskId),
            Lude.Just ("InvocationId" Lude..= invocationId)
          ]
      )

instance Lude.ToPath GetMaintenanceWindowExecutionTaskInvocation where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMaintenanceWindowExecutionTaskInvocation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMaintenanceWindowExecutionTaskInvocationResponse' smart constructor.
data GetMaintenanceWindowExecutionTaskInvocationResponse = GetMaintenanceWindowExecutionTaskInvocationResponse'
  { status ::
      Lude.Maybe
        MaintenanceWindowExecutionStatus,
    executionId ::
      Lude.Maybe
        Lude.Text,
    taskExecutionId ::
      Lude.Maybe
        Lude.Text,
    startTime ::
      Lude.Maybe
        Lude.Timestamp,
    invocationId ::
      Lude.Maybe
        Lude.Text,
    ownerInformation ::
      Lude.Maybe
        ( Lude.Sensitive
            Lude.Text
        ),
    taskType ::
      Lude.Maybe
        MaintenanceWindowTaskType,
    windowTargetId ::
      Lude.Maybe
        Lude.Text,
    windowExecutionId ::
      Lude.Maybe
        Lude.Text,
    statusDetails ::
      Lude.Maybe
        Lude.Text,
    endTime ::
      Lude.Maybe
        Lude.Timestamp,
    parameters ::
      Lude.Maybe
        ( Lude.Sensitive
            Lude.Text
        ),
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'GetMaintenanceWindowExecutionTaskInvocationResponse' with the minimum fields required to make a request.
--
-- * 'endTime' - The time that the task finished running on the target.
-- * 'executionId' - The execution ID.
-- * 'invocationId' - The invocation ID.
-- * 'ownerInformation' - User-provided value to be included in any CloudWatch events raised while running tasks for these targets in this maintenance window.
-- * 'parameters' - The parameters used at the time that the task ran.
-- * 'responseStatus' - The response status code.
-- * 'startTime' - The time that the task started running on the target.
-- * 'status' - The task status for an invocation.
-- * 'statusDetails' - The details explaining the status. Details are only available for certain status values.
-- * 'taskExecutionId' - The task execution ID.
-- * 'taskType' - Retrieves the task type for a maintenance window. Task types include the following: LAMBDA, STEP_FUNCTIONS, AUTOMATION, RUN_COMMAND.
-- * 'windowExecutionId' - The maintenance window execution ID.
-- * 'windowTargetId' - The maintenance window target ID.
mkGetMaintenanceWindowExecutionTaskInvocationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMaintenanceWindowExecutionTaskInvocationResponse
mkGetMaintenanceWindowExecutionTaskInvocationResponse
  pResponseStatus_ =
    GetMaintenanceWindowExecutionTaskInvocationResponse'
      { status =
          Lude.Nothing,
        executionId = Lude.Nothing,
        taskExecutionId = Lude.Nothing,
        startTime = Lude.Nothing,
        invocationId = Lude.Nothing,
        ownerInformation = Lude.Nothing,
        taskType = Lude.Nothing,
        windowTargetId = Lude.Nothing,
        windowExecutionId = Lude.Nothing,
        statusDetails = Lude.Nothing,
        endTime = Lude.Nothing,
        parameters = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The task status for an invocation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirsStatus :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Lude.Maybe MaintenanceWindowExecutionStatus)
gmwetirsStatus = Lens.lens (status :: GetMaintenanceWindowExecutionTaskInvocationResponse -> Lude.Maybe MaintenanceWindowExecutionStatus) (\s a -> s {status = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)
{-# DEPRECATED gmwetirsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The execution ID.
--
-- /Note:/ Consider using 'executionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirsExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Lude.Maybe Lude.Text)
gmwetirsExecutionId = Lens.lens (executionId :: GetMaintenanceWindowExecutionTaskInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {executionId = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)
{-# DEPRECATED gmwetirsExecutionId "Use generic-lens or generic-optics with 'executionId' instead." #-}

-- | The task execution ID.
--
-- /Note:/ Consider using 'taskExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirsTaskExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Lude.Maybe Lude.Text)
gmwetirsTaskExecutionId = Lens.lens (taskExecutionId :: GetMaintenanceWindowExecutionTaskInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskExecutionId = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)
{-# DEPRECATED gmwetirsTaskExecutionId "Use generic-lens or generic-optics with 'taskExecutionId' instead." #-}

-- | The time that the task started running on the target.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirsStartTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Lude.Maybe Lude.Timestamp)
gmwetirsStartTime = Lens.lens (startTime :: GetMaintenanceWindowExecutionTaskInvocationResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)
{-# DEPRECATED gmwetirsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The invocation ID.
--
-- /Note:/ Consider using 'invocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirsInvocationId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Lude.Maybe Lude.Text)
gmwetirsInvocationId = Lens.lens (invocationId :: GetMaintenanceWindowExecutionTaskInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {invocationId = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)
{-# DEPRECATED gmwetirsInvocationId "Use generic-lens or generic-optics with 'invocationId' instead." #-}

-- | User-provided value to be included in any CloudWatch events raised while running tasks for these targets in this maintenance window.
--
-- /Note:/ Consider using 'ownerInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirsOwnerInformation :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
gmwetirsOwnerInformation = Lens.lens (ownerInformation :: GetMaintenanceWindowExecutionTaskInvocationResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {ownerInformation = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)
{-# DEPRECATED gmwetirsOwnerInformation "Use generic-lens or generic-optics with 'ownerInformation' instead." #-}

-- | Retrieves the task type for a maintenance window. Task types include the following: LAMBDA, STEP_FUNCTIONS, AUTOMATION, RUN_COMMAND.
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirsTaskType :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Lude.Maybe MaintenanceWindowTaskType)
gmwetirsTaskType = Lens.lens (taskType :: GetMaintenanceWindowExecutionTaskInvocationResponse -> Lude.Maybe MaintenanceWindowTaskType) (\s a -> s {taskType = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)
{-# DEPRECATED gmwetirsTaskType "Use generic-lens or generic-optics with 'taskType' instead." #-}

-- | The maintenance window target ID.
--
-- /Note:/ Consider using 'windowTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirsWindowTargetId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Lude.Maybe Lude.Text)
gmwetirsWindowTargetId = Lens.lens (windowTargetId :: GetMaintenanceWindowExecutionTaskInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowTargetId = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)
{-# DEPRECATED gmwetirsWindowTargetId "Use generic-lens or generic-optics with 'windowTargetId' instead." #-}

-- | The maintenance window execution ID.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirsWindowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Lude.Maybe Lude.Text)
gmwetirsWindowExecutionId = Lens.lens (windowExecutionId :: GetMaintenanceWindowExecutionTaskInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowExecutionId = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)
{-# DEPRECATED gmwetirsWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

-- | The details explaining the status. Details are only available for certain status values.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirsStatusDetails :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Lude.Maybe Lude.Text)
gmwetirsStatusDetails = Lens.lens (statusDetails :: GetMaintenanceWindowExecutionTaskInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusDetails = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)
{-# DEPRECATED gmwetirsStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The time that the task finished running on the target.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirsEndTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Lude.Maybe Lude.Timestamp)
gmwetirsEndTime = Lens.lens (endTime :: GetMaintenanceWindowExecutionTaskInvocationResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)
{-# DEPRECATED gmwetirsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The parameters used at the time that the task ran.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirsParameters :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
gmwetirsParameters = Lens.lens (parameters :: GetMaintenanceWindowExecutionTaskInvocationResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {parameters = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)
{-# DEPRECATED gmwetirsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetirsResponseStatus :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse Lude.Int
gmwetirsResponseStatus = Lens.lens (responseStatus :: GetMaintenanceWindowExecutionTaskInvocationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)
{-# DEPRECATED gmwetirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
