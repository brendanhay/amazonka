{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetMaintenanceWindowExecutionTask (..),
    mkGetMaintenanceWindowExecutionTask,

    -- ** Request lenses
    gmwetWindowExecutionId,
    gmwetTaskId,

    -- * Destructuring the response
    GetMaintenanceWindowExecutionTaskResponse (..),
    mkGetMaintenanceWindowExecutionTaskResponse,

    -- ** Response lenses
    gmwetrsStatus,
    gmwetrsTaskParameters,
    gmwetrsTaskExecutionId,
    gmwetrsPriority,
    gmwetrsStartTime,
    gmwetrsTaskARN,
    gmwetrsWindowExecutionId,
    gmwetrsStatusDetails,
    gmwetrsMaxErrors,
    gmwetrsEndTime,
    gmwetrsType,
    gmwetrsMaxConcurrency,
    gmwetrsServiceRole,
    gmwetrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetMaintenanceWindowExecutionTask' smart constructor.
data GetMaintenanceWindowExecutionTask = GetMaintenanceWindowExecutionTask'
  { windowExecutionId ::
      Lude.Text,
    taskId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMaintenanceWindowExecutionTask' with the minimum fields required to make a request.
--
-- * 'taskId' - The ID of the specific task execution in the maintenance window task that should be retrieved.
-- * 'windowExecutionId' - The ID of the maintenance window execution that includes the task.
mkGetMaintenanceWindowExecutionTask ::
  -- | 'windowExecutionId'
  Lude.Text ->
  -- | 'taskId'
  Lude.Text ->
  GetMaintenanceWindowExecutionTask
mkGetMaintenanceWindowExecutionTask pWindowExecutionId_ pTaskId_ =
  GetMaintenanceWindowExecutionTask'
    { windowExecutionId =
        pWindowExecutionId_,
      taskId = pTaskId_
    }

-- | The ID of the maintenance window execution that includes the task.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetWindowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTask Lude.Text
gmwetWindowExecutionId = Lens.lens (windowExecutionId :: GetMaintenanceWindowExecutionTask -> Lude.Text) (\s a -> s {windowExecutionId = a} :: GetMaintenanceWindowExecutionTask)
{-# DEPRECATED gmwetWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

-- | The ID of the specific task execution in the maintenance window task that should be retrieved.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetTaskId :: Lens.Lens' GetMaintenanceWindowExecutionTask Lude.Text
gmwetTaskId = Lens.lens (taskId :: GetMaintenanceWindowExecutionTask -> Lude.Text) (\s a -> s {taskId = a} :: GetMaintenanceWindowExecutionTask)
{-# DEPRECATED gmwetTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Lude.AWSRequest GetMaintenanceWindowExecutionTask where
  type
    Rs GetMaintenanceWindowExecutionTask =
      GetMaintenanceWindowExecutionTaskResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMaintenanceWindowExecutionTaskResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "TaskParameters" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "TaskExecutionId")
            Lude.<*> (x Lude..?> "Priority")
            Lude.<*> (x Lude..?> "StartTime")
            Lude.<*> (x Lude..?> "TaskArn")
            Lude.<*> (x Lude..?> "WindowExecutionId")
            Lude.<*> (x Lude..?> "StatusDetails")
            Lude.<*> (x Lude..?> "MaxErrors")
            Lude.<*> (x Lude..?> "EndTime")
            Lude.<*> (x Lude..?> "Type")
            Lude.<*> (x Lude..?> "MaxConcurrency")
            Lude.<*> (x Lude..?> "ServiceRole")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMaintenanceWindowExecutionTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetMaintenanceWindowExecutionTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMaintenanceWindowExecutionTask where
  toJSON GetMaintenanceWindowExecutionTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("WindowExecutionId" Lude..= windowExecutionId),
            Lude.Just ("TaskId" Lude..= taskId)
          ]
      )

instance Lude.ToPath GetMaintenanceWindowExecutionTask where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMaintenanceWindowExecutionTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMaintenanceWindowExecutionTaskResponse' smart constructor.
data GetMaintenanceWindowExecutionTaskResponse = GetMaintenanceWindowExecutionTaskResponse'
  { status ::
      Lude.Maybe
        MaintenanceWindowExecutionStatus,
    taskParameters ::
      Lude.Maybe
        [ Lude.HashMap
            Lude.Text
            (MaintenanceWindowTaskParameterValueExpression)
        ],
    taskExecutionId ::
      Lude.Maybe
        Lude.Text,
    priority ::
      Lude.Maybe
        Lude.Natural,
    startTime ::
      Lude.Maybe
        Lude.Timestamp,
    taskARN ::
      Lude.Maybe
        Lude.Text,
    windowExecutionId ::
      Lude.Maybe
        Lude.Text,
    statusDetails ::
      Lude.Maybe
        Lude.Text,
    maxErrors ::
      Lude.Maybe
        Lude.Text,
    endTime ::
      Lude.Maybe
        Lude.Timestamp,
    type' ::
      Lude.Maybe
        MaintenanceWindowTaskType,
    maxConcurrency ::
      Lude.Maybe
        Lude.Text,
    serviceRole ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMaintenanceWindowExecutionTaskResponse' with the minimum fields required to make a request.
--
-- * 'endTime' - The time the task execution completed.
-- * 'maxConcurrency' - The defined maximum number of task executions that could be run in parallel.
-- * 'maxErrors' - The defined maximum number of task execution errors allowed before scheduling of the task execution would have been stopped.
-- * 'priority' - The priority of the task.
-- * 'responseStatus' - The response status code.
-- * 'serviceRole' - The role that was assumed when running the task.
-- * 'startTime' - The time the task execution started.
-- * 'status' - The status of the task.
-- * 'statusDetails' - The details explaining the Status. Only available for certain status values.
-- * 'taskARN' - The ARN of the task that ran.
-- * 'taskExecutionId' - The ID of the specific task execution in the maintenance window task that was retrieved.
-- * 'taskParameters' - The parameters passed to the task when it was run.
--
-- The map has the following format:
-- Key: string, between 1 and 255 characters
-- Value: an array of strings, each string is between 1 and 255 characters
-- * 'type'' - The type of task that was run.
-- * 'windowExecutionId' - The ID of the maintenance window execution that includes the task.
mkGetMaintenanceWindowExecutionTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMaintenanceWindowExecutionTaskResponse
mkGetMaintenanceWindowExecutionTaskResponse pResponseStatus_ =
  GetMaintenanceWindowExecutionTaskResponse'
    { status = Lude.Nothing,
      taskParameters = Lude.Nothing,
      taskExecutionId = Lude.Nothing,
      priority = Lude.Nothing,
      startTime = Lude.Nothing,
      taskARN = Lude.Nothing,
      windowExecutionId = Lude.Nothing,
      statusDetails = Lude.Nothing,
      maxErrors = Lude.Nothing,
      endTime = Lude.Nothing,
      type' = Lude.Nothing,
      maxConcurrency = Lude.Nothing,
      serviceRole = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrsStatus :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Lude.Maybe MaintenanceWindowExecutionStatus)
gmwetrsStatus = Lens.lens (status :: GetMaintenanceWindowExecutionTaskResponse -> Lude.Maybe MaintenanceWindowExecutionStatus) (\s a -> s {status = a} :: GetMaintenanceWindowExecutionTaskResponse)
{-# DEPRECATED gmwetrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The parameters passed to the task when it was run.
--
-- The map has the following format:
-- Key: string, between 1 and 255 characters
-- Value: an array of strings, each string is between 1 and 255 characters
--
-- /Note:/ Consider using 'taskParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrsTaskParameters :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Lude.Maybe [Lude.HashMap Lude.Text (MaintenanceWindowTaskParameterValueExpression)])
gmwetrsTaskParameters = Lens.lens (taskParameters :: GetMaintenanceWindowExecutionTaskResponse -> Lude.Maybe [Lude.HashMap Lude.Text (MaintenanceWindowTaskParameterValueExpression)]) (\s a -> s {taskParameters = a} :: GetMaintenanceWindowExecutionTaskResponse)
{-# DEPRECATED gmwetrsTaskParameters "Use generic-lens or generic-optics with 'taskParameters' instead." #-}

-- | The ID of the specific task execution in the maintenance window task that was retrieved.
--
-- /Note:/ Consider using 'taskExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrsTaskExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Lude.Maybe Lude.Text)
gmwetrsTaskExecutionId = Lens.lens (taskExecutionId :: GetMaintenanceWindowExecutionTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskExecutionId = a} :: GetMaintenanceWindowExecutionTaskResponse)
{-# DEPRECATED gmwetrsTaskExecutionId "Use generic-lens or generic-optics with 'taskExecutionId' instead." #-}

-- | The priority of the task.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrsPriority :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Lude.Maybe Lude.Natural)
gmwetrsPriority = Lens.lens (priority :: GetMaintenanceWindowExecutionTaskResponse -> Lude.Maybe Lude.Natural) (\s a -> s {priority = a} :: GetMaintenanceWindowExecutionTaskResponse)
{-# DEPRECATED gmwetrsPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The time the task execution started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrsStartTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Lude.Maybe Lude.Timestamp)
gmwetrsStartTime = Lens.lens (startTime :: GetMaintenanceWindowExecutionTaskResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: GetMaintenanceWindowExecutionTaskResponse)
{-# DEPRECATED gmwetrsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The ARN of the task that ran.
--
-- /Note:/ Consider using 'taskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrsTaskARN :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Lude.Maybe Lude.Text)
gmwetrsTaskARN = Lens.lens (taskARN :: GetMaintenanceWindowExecutionTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskARN = a} :: GetMaintenanceWindowExecutionTaskResponse)
{-# DEPRECATED gmwetrsTaskARN "Use generic-lens or generic-optics with 'taskARN' instead." #-}

-- | The ID of the maintenance window execution that includes the task.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrsWindowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Lude.Maybe Lude.Text)
gmwetrsWindowExecutionId = Lens.lens (windowExecutionId :: GetMaintenanceWindowExecutionTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowExecutionId = a} :: GetMaintenanceWindowExecutionTaskResponse)
{-# DEPRECATED gmwetrsWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

-- | The details explaining the Status. Only available for certain status values.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrsStatusDetails :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Lude.Maybe Lude.Text)
gmwetrsStatusDetails = Lens.lens (statusDetails :: GetMaintenanceWindowExecutionTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusDetails = a} :: GetMaintenanceWindowExecutionTaskResponse)
{-# DEPRECATED gmwetrsStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The defined maximum number of task execution errors allowed before scheduling of the task execution would have been stopped.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrsMaxErrors :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Lude.Maybe Lude.Text)
gmwetrsMaxErrors = Lens.lens (maxErrors :: GetMaintenanceWindowExecutionTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {maxErrors = a} :: GetMaintenanceWindowExecutionTaskResponse)
{-# DEPRECATED gmwetrsMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | The time the task execution completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrsEndTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Lude.Maybe Lude.Timestamp)
gmwetrsEndTime = Lens.lens (endTime :: GetMaintenanceWindowExecutionTaskResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: GetMaintenanceWindowExecutionTaskResponse)
{-# DEPRECATED gmwetrsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The type of task that was run.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrsType :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Lude.Maybe MaintenanceWindowTaskType)
gmwetrsType = Lens.lens (type' :: GetMaintenanceWindowExecutionTaskResponse -> Lude.Maybe MaintenanceWindowTaskType) (\s a -> s {type' = a} :: GetMaintenanceWindowExecutionTaskResponse)
{-# DEPRECATED gmwetrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The defined maximum number of task executions that could be run in parallel.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrsMaxConcurrency :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Lude.Maybe Lude.Text)
gmwetrsMaxConcurrency = Lens.lens (maxConcurrency :: GetMaintenanceWindowExecutionTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {maxConcurrency = a} :: GetMaintenanceWindowExecutionTaskResponse)
{-# DEPRECATED gmwetrsMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The role that was assumed when running the task.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrsServiceRole :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Lude.Maybe Lude.Text)
gmwetrsServiceRole = Lens.lens (serviceRole :: GetMaintenanceWindowExecutionTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: GetMaintenanceWindowExecutionTaskResponse)
{-# DEPRECATED gmwetrsServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwetrsResponseStatus :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse Lude.Int
gmwetrsResponseStatus = Lens.lens (responseStatus :: GetMaintenanceWindowExecutionTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMaintenanceWindowExecutionTaskResponse)
{-# DEPRECATED gmwetrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
