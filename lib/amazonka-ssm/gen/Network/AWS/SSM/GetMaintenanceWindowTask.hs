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
    gmwtWindowTaskId,
    gmwtWindowId,

    -- * Destructuring the response
    GetMaintenanceWindowTaskResponse (..),
    mkGetMaintenanceWindowTaskResponse,

    -- ** Response lenses
    gmwtrsServiceRoleARN,
    gmwtrsWindowTaskId,
    gmwtrsTaskParameters,
    gmwtrsPriority,
    gmwtrsTaskType,
    gmwtrsTaskARN,
    gmwtrsMaxErrors,
    gmwtrsTaskInvocationParameters,
    gmwtrsName,
    gmwtrsTargets,
    gmwtrsLoggingInfo,
    gmwtrsDescription,
    gmwtrsMaxConcurrency,
    gmwtrsWindowId,
    gmwtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetMaintenanceWindowTask' smart constructor.
data GetMaintenanceWindowTask = GetMaintenanceWindowTask'
  { -- | The maintenance window task ID to retrieve.
    windowTaskId :: Lude.Text,
    -- | The maintenance window ID that includes the task to retrieve.
    windowId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMaintenanceWindowTask' with the minimum fields required to make a request.
--
-- * 'windowTaskId' - The maintenance window task ID to retrieve.
-- * 'windowId' - The maintenance window ID that includes the task to retrieve.
mkGetMaintenanceWindowTask ::
  -- | 'windowTaskId'
  Lude.Text ->
  -- | 'windowId'
  Lude.Text ->
  GetMaintenanceWindowTask
mkGetMaintenanceWindowTask pWindowTaskId_ pWindowId_ =
  GetMaintenanceWindowTask'
    { windowTaskId = pWindowTaskId_,
      windowId = pWindowId_
    }

-- | The maintenance window task ID to retrieve.
--
-- /Note:/ Consider using 'windowTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtWindowTaskId :: Lens.Lens' GetMaintenanceWindowTask Lude.Text
gmwtWindowTaskId = Lens.lens (windowTaskId :: GetMaintenanceWindowTask -> Lude.Text) (\s a -> s {windowTaskId = a} :: GetMaintenanceWindowTask)
{-# DEPRECATED gmwtWindowTaskId "Use generic-lens or generic-optics with 'windowTaskId' instead." #-}

-- | The maintenance window ID that includes the task to retrieve.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtWindowId :: Lens.Lens' GetMaintenanceWindowTask Lude.Text
gmwtWindowId = Lens.lens (windowId :: GetMaintenanceWindowTask -> Lude.Text) (\s a -> s {windowId = a} :: GetMaintenanceWindowTask)
{-# DEPRECATED gmwtWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Lude.AWSRequest GetMaintenanceWindowTask where
  type Rs GetMaintenanceWindowTask = GetMaintenanceWindowTaskResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMaintenanceWindowTaskResponse'
            Lude.<$> (x Lude..?> "ServiceRoleArn")
            Lude.<*> (x Lude..?> "WindowTaskId")
            Lude.<*> (x Lude..?> "TaskParameters" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Priority")
            Lude.<*> (x Lude..?> "TaskType")
            Lude.<*> (x Lude..?> "TaskArn")
            Lude.<*> (x Lude..?> "MaxErrors")
            Lude.<*> (x Lude..?> "TaskInvocationParameters")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "Targets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "LoggingInfo")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (x Lude..?> "MaxConcurrency")
            Lude.<*> (x Lude..?> "WindowId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMaintenanceWindowTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetMaintenanceWindowTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMaintenanceWindowTask where
  toJSON GetMaintenanceWindowTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("WindowTaskId" Lude..= windowTaskId),
            Lude.Just ("WindowId" Lude..= windowId)
          ]
      )

instance Lude.ToPath GetMaintenanceWindowTask where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMaintenanceWindowTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMaintenanceWindowTaskResponse' smart constructor.
data GetMaintenanceWindowTaskResponse = GetMaintenanceWindowTaskResponse'
  { -- | The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
    serviceRoleARN :: Lude.Maybe Lude.Text,
    -- | The retrieved maintenance window task ID.
    windowTaskId :: Lude.Maybe Lude.Text,
    -- | The parameters to pass to the task when it runs.
    taskParameters :: Lude.Maybe (Lude.HashMap Lude.Text (MaintenanceWindowTaskParameterValueExpression)),
    -- | The priority of the task when it runs. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
    priority :: Lude.Maybe Lude.Natural,
    -- | The type of task to run.
    taskType :: Lude.Maybe MaintenanceWindowTaskType,
    -- | The resource that the task used during execution. For RUN_COMMAND and AUTOMATION task types, the TaskArn is the Systems Manager Document name/ARN. For LAMBDA tasks, the value is the function name/ARN. For STEP_FUNCTIONS tasks, the value is the state machine ARN.
    taskARN :: Lude.Maybe Lude.Text,
    -- | The maximum number of errors allowed before the task stops being scheduled.
    maxErrors :: Lude.Maybe Lude.Text,
    -- | The parameters to pass to the task when it runs.
    taskInvocationParameters :: Lude.Maybe MaintenanceWindowTaskInvocationParameters,
    -- | The retrieved task name.
    name :: Lude.Maybe Lude.Text,
    -- | The targets where the task should run.
    targets :: Lude.Maybe [Target],
    -- | The location in Amazon S3 where the task results are logged.
    loggingInfo :: Lude.Maybe LoggingInfo,
    -- | The retrieved task description.
    description :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The maximum number of targets allowed to run this task in parallel.
    maxConcurrency :: Lude.Maybe Lude.Text,
    -- | The retrieved maintenance window ID.
    windowId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMaintenanceWindowTaskResponse' with the minimum fields required to make a request.
--
-- * 'serviceRoleARN' - The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
-- * 'windowTaskId' - The retrieved maintenance window task ID.
-- * 'taskParameters' - The parameters to pass to the task when it runs.
-- * 'priority' - The priority of the task when it runs. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
-- * 'taskType' - The type of task to run.
-- * 'taskARN' - The resource that the task used during execution. For RUN_COMMAND and AUTOMATION task types, the TaskArn is the Systems Manager Document name/ARN. For LAMBDA tasks, the value is the function name/ARN. For STEP_FUNCTIONS tasks, the value is the state machine ARN.
-- * 'maxErrors' - The maximum number of errors allowed before the task stops being scheduled.
-- * 'taskInvocationParameters' - The parameters to pass to the task when it runs.
-- * 'name' - The retrieved task name.
-- * 'targets' - The targets where the task should run.
-- * 'loggingInfo' - The location in Amazon S3 where the task results are logged.
-- * 'description' - The retrieved task description.
-- * 'maxConcurrency' - The maximum number of targets allowed to run this task in parallel.
-- * 'windowId' - The retrieved maintenance window ID.
-- * 'responseStatus' - The response status code.
mkGetMaintenanceWindowTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMaintenanceWindowTaskResponse
mkGetMaintenanceWindowTaskResponse pResponseStatus_ =
  GetMaintenanceWindowTaskResponse'
    { serviceRoleARN = Lude.Nothing,
      windowTaskId = Lude.Nothing,
      taskParameters = Lude.Nothing,
      priority = Lude.Nothing,
      taskType = Lude.Nothing,
      taskARN = Lude.Nothing,
      maxErrors = Lude.Nothing,
      taskInvocationParameters = Lude.Nothing,
      name = Lude.Nothing,
      targets = Lude.Nothing,
      loggingInfo = Lude.Nothing,
      description = Lude.Nothing,
      maxConcurrency = Lude.Nothing,
      windowId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrsServiceRoleARN :: Lens.Lens' GetMaintenanceWindowTaskResponse (Lude.Maybe Lude.Text)
gmwtrsServiceRoleARN = Lens.lens (serviceRoleARN :: GetMaintenanceWindowTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleARN = a} :: GetMaintenanceWindowTaskResponse)
{-# DEPRECATED gmwtrsServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | The retrieved maintenance window task ID.
--
-- /Note:/ Consider using 'windowTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrsWindowTaskId :: Lens.Lens' GetMaintenanceWindowTaskResponse (Lude.Maybe Lude.Text)
gmwtrsWindowTaskId = Lens.lens (windowTaskId :: GetMaintenanceWindowTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowTaskId = a} :: GetMaintenanceWindowTaskResponse)
{-# DEPRECATED gmwtrsWindowTaskId "Use generic-lens or generic-optics with 'windowTaskId' instead." #-}

-- | The parameters to pass to the task when it runs.
--
-- /Note:/ Consider using 'taskParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrsTaskParameters :: Lens.Lens' GetMaintenanceWindowTaskResponse (Lude.Maybe (Lude.HashMap Lude.Text (MaintenanceWindowTaskParameterValueExpression)))
gmwtrsTaskParameters = Lens.lens (taskParameters :: GetMaintenanceWindowTaskResponse -> Lude.Maybe (Lude.HashMap Lude.Text (MaintenanceWindowTaskParameterValueExpression))) (\s a -> s {taskParameters = a} :: GetMaintenanceWindowTaskResponse)
{-# DEPRECATED gmwtrsTaskParameters "Use generic-lens or generic-optics with 'taskParameters' instead." #-}

-- | The priority of the task when it runs. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrsPriority :: Lens.Lens' GetMaintenanceWindowTaskResponse (Lude.Maybe Lude.Natural)
gmwtrsPriority = Lens.lens (priority :: GetMaintenanceWindowTaskResponse -> Lude.Maybe Lude.Natural) (\s a -> s {priority = a} :: GetMaintenanceWindowTaskResponse)
{-# DEPRECATED gmwtrsPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The type of task to run.
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrsTaskType :: Lens.Lens' GetMaintenanceWindowTaskResponse (Lude.Maybe MaintenanceWindowTaskType)
gmwtrsTaskType = Lens.lens (taskType :: GetMaintenanceWindowTaskResponse -> Lude.Maybe MaintenanceWindowTaskType) (\s a -> s {taskType = a} :: GetMaintenanceWindowTaskResponse)
{-# DEPRECATED gmwtrsTaskType "Use generic-lens or generic-optics with 'taskType' instead." #-}

-- | The resource that the task used during execution. For RUN_COMMAND and AUTOMATION task types, the TaskArn is the Systems Manager Document name/ARN. For LAMBDA tasks, the value is the function name/ARN. For STEP_FUNCTIONS tasks, the value is the state machine ARN.
--
-- /Note:/ Consider using 'taskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrsTaskARN :: Lens.Lens' GetMaintenanceWindowTaskResponse (Lude.Maybe Lude.Text)
gmwtrsTaskARN = Lens.lens (taskARN :: GetMaintenanceWindowTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskARN = a} :: GetMaintenanceWindowTaskResponse)
{-# DEPRECATED gmwtrsTaskARN "Use generic-lens or generic-optics with 'taskARN' instead." #-}

-- | The maximum number of errors allowed before the task stops being scheduled.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrsMaxErrors :: Lens.Lens' GetMaintenanceWindowTaskResponse (Lude.Maybe Lude.Text)
gmwtrsMaxErrors = Lens.lens (maxErrors :: GetMaintenanceWindowTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {maxErrors = a} :: GetMaintenanceWindowTaskResponse)
{-# DEPRECATED gmwtrsMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | The parameters to pass to the task when it runs.
--
-- /Note:/ Consider using 'taskInvocationParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrsTaskInvocationParameters :: Lens.Lens' GetMaintenanceWindowTaskResponse (Lude.Maybe MaintenanceWindowTaskInvocationParameters)
gmwtrsTaskInvocationParameters = Lens.lens (taskInvocationParameters :: GetMaintenanceWindowTaskResponse -> Lude.Maybe MaintenanceWindowTaskInvocationParameters) (\s a -> s {taskInvocationParameters = a} :: GetMaintenanceWindowTaskResponse)
{-# DEPRECATED gmwtrsTaskInvocationParameters "Use generic-lens or generic-optics with 'taskInvocationParameters' instead." #-}

-- | The retrieved task name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrsName :: Lens.Lens' GetMaintenanceWindowTaskResponse (Lude.Maybe Lude.Text)
gmwtrsName = Lens.lens (name :: GetMaintenanceWindowTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetMaintenanceWindowTaskResponse)
{-# DEPRECATED gmwtrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The targets where the task should run.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrsTargets :: Lens.Lens' GetMaintenanceWindowTaskResponse (Lude.Maybe [Target])
gmwtrsTargets = Lens.lens (targets :: GetMaintenanceWindowTaskResponse -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: GetMaintenanceWindowTaskResponse)
{-# DEPRECATED gmwtrsTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The location in Amazon S3 where the task results are logged.
--
-- /Note:/ Consider using 'loggingInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrsLoggingInfo :: Lens.Lens' GetMaintenanceWindowTaskResponse (Lude.Maybe LoggingInfo)
gmwtrsLoggingInfo = Lens.lens (loggingInfo :: GetMaintenanceWindowTaskResponse -> Lude.Maybe LoggingInfo) (\s a -> s {loggingInfo = a} :: GetMaintenanceWindowTaskResponse)
{-# DEPRECATED gmwtrsLoggingInfo "Use generic-lens or generic-optics with 'loggingInfo' instead." #-}

-- | The retrieved task description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrsDescription :: Lens.Lens' GetMaintenanceWindowTaskResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
gmwtrsDescription = Lens.lens (description :: GetMaintenanceWindowTaskResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: GetMaintenanceWindowTaskResponse)
{-# DEPRECATED gmwtrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The maximum number of targets allowed to run this task in parallel.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrsMaxConcurrency :: Lens.Lens' GetMaintenanceWindowTaskResponse (Lude.Maybe Lude.Text)
gmwtrsMaxConcurrency = Lens.lens (maxConcurrency :: GetMaintenanceWindowTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {maxConcurrency = a} :: GetMaintenanceWindowTaskResponse)
{-# DEPRECATED gmwtrsMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The retrieved maintenance window ID.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrsWindowId :: Lens.Lens' GetMaintenanceWindowTaskResponse (Lude.Maybe Lude.Text)
gmwtrsWindowId = Lens.lens (windowId :: GetMaintenanceWindowTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowId = a} :: GetMaintenanceWindowTaskResponse)
{-# DEPRECATED gmwtrsWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwtrsResponseStatus :: Lens.Lens' GetMaintenanceWindowTaskResponse Lude.Int
gmwtrsResponseStatus = Lens.lens (responseStatus :: GetMaintenanceWindowTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMaintenanceWindowTaskResponse)
{-# DEPRECATED gmwtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
