{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateMaintenanceWindowTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a task assigned to a maintenance window. You can't change the task type, but you can change the following values:
--
--
--     * TaskARN. For example, you can change a RUN_COMMAND task from AWS-RunPowerShellScript to AWS-RunShellScript.
--
--
--     * ServiceRoleArn
--
--
--     * TaskInvocationParameters
--
--
--     * Priority
--
--
--     * MaxConcurrency
--
--
--     * MaxErrors
--
--
-- If the value for a parameter in @UpdateMaintenanceWindowTask@ is null, then the corresponding field is not modified. If you set @Replace@ to true, then all fields required by the 'RegisterTaskWithMaintenanceWindow' action are required for this request. Optional fields that aren't specified are set to null.
-- /Important:/ When you update a maintenance window task that has options specified in @TaskInvocationParameters@ , you must provide again all the @TaskInvocationParameters@ values that you want to retain. The values you do not specify again are removed. For example, suppose that when you registered a Run Command task, you specified @TaskInvocationParameters@ values for @Comment@ , @NotificationConfig@ , and @OutputS3BucketName@ . If you update the maintenance window task and specify only a different @OutputS3BucketName@ value, the values for @Comment@ and @NotificationConfig@ are removed.
module Network.AWS.SSM.UpdateMaintenanceWindowTask
  ( -- * Creating a request
    UpdateMaintenanceWindowTask (..),
    mkUpdateMaintenanceWindowTask,

    -- ** Request lenses
    umwtServiceRoleARN,
    umwtReplace,
    umwtTaskParameters,
    umwtPriority,
    umwtTaskARN,
    umwtMaxErrors,
    umwtTaskInvocationParameters,
    umwtName,
    umwtTargets,
    umwtLoggingInfo,
    umwtDescription,
    umwtMaxConcurrency,
    umwtWindowId,
    umwtWindowTaskId,

    -- * Destructuring the response
    UpdateMaintenanceWindowTaskResponse (..),
    mkUpdateMaintenanceWindowTaskResponse,

    -- ** Response lenses
    umwtrsServiceRoleARN,
    umwtrsWindowTaskId,
    umwtrsTaskParameters,
    umwtrsPriority,
    umwtrsTaskARN,
    umwtrsMaxErrors,
    umwtrsTaskInvocationParameters,
    umwtrsName,
    umwtrsTargets,
    umwtrsLoggingInfo,
    umwtrsDescription,
    umwtrsMaxConcurrency,
    umwtrsWindowId,
    umwtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkUpdateMaintenanceWindowTask' smart constructor.
data UpdateMaintenanceWindowTask = UpdateMaintenanceWindowTask'
  { serviceRoleARN ::
      Lude.Maybe Lude.Text,
    replace :: Lude.Maybe Lude.Bool,
    taskParameters ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (MaintenanceWindowTaskParameterValueExpression)
        ),
    priority :: Lude.Maybe Lude.Natural,
    taskARN :: Lude.Maybe Lude.Text,
    maxErrors :: Lude.Maybe Lude.Text,
    taskInvocationParameters ::
      Lude.Maybe
        MaintenanceWindowTaskInvocationParameters,
    name :: Lude.Maybe Lude.Text,
    targets :: Lude.Maybe [Target],
    loggingInfo ::
      Lude.Maybe LoggingInfo,
    description ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text),
    maxConcurrency ::
      Lude.Maybe Lude.Text,
    windowId :: Lude.Text,
    windowTaskId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMaintenanceWindowTask' with the minimum fields required to make a request.
--
-- * 'description' - The new task description to specify.
-- * 'loggingInfo' - The new logging location in Amazon S3 to specify.
-- * 'maxConcurrency' - The new @MaxConcurrency@ value you want to specify. @MaxConcurrency@ is the number of targets that are allowed to run this task in parallel.
-- * 'maxErrors' - The new @MaxErrors@ value to specify. @MaxErrors@ is the maximum number of errors that are allowed before the task stops being scheduled.
-- * 'name' - The new task name to specify.
-- * 'priority' - The new task priority to specify. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
-- * 'replace' - If True, then all fields that are required by the RegisterTaskWithMaintenanceWindow action are also required for this API request. Optional fields that are not specified are set to null.
-- * 'serviceRoleARN' - The ARN of the IAM service role for Systems Manager to assume when running a maintenance window task. If you do not specify a service role ARN, Systems Manager uses your account's service-linked role. If no service-linked role for Systems Manager exists in your account, it is created when you run @RegisterTaskWithMaintenanceWindow@ .
--
-- For more information, see the following topics in the in the /AWS Systems Manager User Guide/ :
--
--     * <https://docs.aws.amazon.com/systems-manager/latest/userguide/using-service-linked-roles.html#slr-permissions Using service-linked roles for Systems Manager>
--
--
--     * <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-maintenance-permissions.html#maintenance-window-tasks-service-role Should I use a service-linked role or a custom service role to run maintenance window tasks? >
--
--
-- * 'targets' - The targets (either instances or tags) to modify. Instances are specified using Key=instanceids,Values=instanceID_1,instanceID_2. Tags are specified using Key=tag_name,Values=tag_value.
-- * 'taskARN' - The task ARN to modify.
-- * 'taskInvocationParameters' - The parameters that the task should use during execution. Populate only the fields that match the task type. All other fields should be empty.
--
-- /Important:/ When you update a maintenance window task that has options specified in @TaskInvocationParameters@ , you must provide again all the @TaskInvocationParameters@ values that you want to retain. The values you do not specify again are removed. For example, suppose that when you registered a Run Command task, you specified @TaskInvocationParameters@ values for @Comment@ , @NotificationConfig@ , and @OutputS3BucketName@ . If you update the maintenance window task and specify only a different @OutputS3BucketName@ value, the values for @Comment@ and @NotificationConfig@ are removed.
-- * 'taskParameters' - The parameters to modify.
--
-- The map has the following format:
-- Key: string, between 1 and 255 characters
-- Value: an array of strings, each string is between 1 and 255 characters
-- * 'windowId' - The maintenance window ID that contains the task to modify.
-- * 'windowTaskId' - The task ID to modify.
mkUpdateMaintenanceWindowTask ::
  -- | 'windowId'
  Lude.Text ->
  -- | 'windowTaskId'
  Lude.Text ->
  UpdateMaintenanceWindowTask
mkUpdateMaintenanceWindowTask pWindowId_ pWindowTaskId_ =
  UpdateMaintenanceWindowTask'
    { serviceRoleARN = Lude.Nothing,
      replace = Lude.Nothing,
      taskParameters = Lude.Nothing,
      priority = Lude.Nothing,
      taskARN = Lude.Nothing,
      maxErrors = Lude.Nothing,
      taskInvocationParameters = Lude.Nothing,
      name = Lude.Nothing,
      targets = Lude.Nothing,
      loggingInfo = Lude.Nothing,
      description = Lude.Nothing,
      maxConcurrency = Lude.Nothing,
      windowId = pWindowId_,
      windowTaskId = pWindowTaskId_
    }

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
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtServiceRoleARN :: Lens.Lens' UpdateMaintenanceWindowTask (Lude.Maybe Lude.Text)
umwtServiceRoleARN = Lens.lens (serviceRoleARN :: UpdateMaintenanceWindowTask -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleARN = a} :: UpdateMaintenanceWindowTask)
{-# DEPRECATED umwtServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | If True, then all fields that are required by the RegisterTaskWithMaintenanceWindow action are also required for this API request. Optional fields that are not specified are set to null.
--
-- /Note:/ Consider using 'replace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtReplace :: Lens.Lens' UpdateMaintenanceWindowTask (Lude.Maybe Lude.Bool)
umwtReplace = Lens.lens (replace :: UpdateMaintenanceWindowTask -> Lude.Maybe Lude.Bool) (\s a -> s {replace = a} :: UpdateMaintenanceWindowTask)
{-# DEPRECATED umwtReplace "Use generic-lens or generic-optics with 'replace' instead." #-}

-- | The parameters to modify.
--
-- The map has the following format:
-- Key: string, between 1 and 255 characters
-- Value: an array of strings, each string is between 1 and 255 characters
--
-- /Note:/ Consider using 'taskParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtTaskParameters :: Lens.Lens' UpdateMaintenanceWindowTask (Lude.Maybe (Lude.HashMap Lude.Text (MaintenanceWindowTaskParameterValueExpression)))
umwtTaskParameters = Lens.lens (taskParameters :: UpdateMaintenanceWindowTask -> Lude.Maybe (Lude.HashMap Lude.Text (MaintenanceWindowTaskParameterValueExpression))) (\s a -> s {taskParameters = a} :: UpdateMaintenanceWindowTask)
{-# DEPRECATED umwtTaskParameters "Use generic-lens or generic-optics with 'taskParameters' instead." #-}

-- | The new task priority to specify. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtPriority :: Lens.Lens' UpdateMaintenanceWindowTask (Lude.Maybe Lude.Natural)
umwtPriority = Lens.lens (priority :: UpdateMaintenanceWindowTask -> Lude.Maybe Lude.Natural) (\s a -> s {priority = a} :: UpdateMaintenanceWindowTask)
{-# DEPRECATED umwtPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The task ARN to modify.
--
-- /Note:/ Consider using 'taskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtTaskARN :: Lens.Lens' UpdateMaintenanceWindowTask (Lude.Maybe Lude.Text)
umwtTaskARN = Lens.lens (taskARN :: UpdateMaintenanceWindowTask -> Lude.Maybe Lude.Text) (\s a -> s {taskARN = a} :: UpdateMaintenanceWindowTask)
{-# DEPRECATED umwtTaskARN "Use generic-lens or generic-optics with 'taskARN' instead." #-}

-- | The new @MaxErrors@ value to specify. @MaxErrors@ is the maximum number of errors that are allowed before the task stops being scheduled.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtMaxErrors :: Lens.Lens' UpdateMaintenanceWindowTask (Lude.Maybe Lude.Text)
umwtMaxErrors = Lens.lens (maxErrors :: UpdateMaintenanceWindowTask -> Lude.Maybe Lude.Text) (\s a -> s {maxErrors = a} :: UpdateMaintenanceWindowTask)
{-# DEPRECATED umwtMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | The parameters that the task should use during execution. Populate only the fields that match the task type. All other fields should be empty.
--
-- /Important:/ When you update a maintenance window task that has options specified in @TaskInvocationParameters@ , you must provide again all the @TaskInvocationParameters@ values that you want to retain. The values you do not specify again are removed. For example, suppose that when you registered a Run Command task, you specified @TaskInvocationParameters@ values for @Comment@ , @NotificationConfig@ , and @OutputS3BucketName@ . If you update the maintenance window task and specify only a different @OutputS3BucketName@ value, the values for @Comment@ and @NotificationConfig@ are removed.
--
-- /Note:/ Consider using 'taskInvocationParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtTaskInvocationParameters :: Lens.Lens' UpdateMaintenanceWindowTask (Lude.Maybe MaintenanceWindowTaskInvocationParameters)
umwtTaskInvocationParameters = Lens.lens (taskInvocationParameters :: UpdateMaintenanceWindowTask -> Lude.Maybe MaintenanceWindowTaskInvocationParameters) (\s a -> s {taskInvocationParameters = a} :: UpdateMaintenanceWindowTask)
{-# DEPRECATED umwtTaskInvocationParameters "Use generic-lens or generic-optics with 'taskInvocationParameters' instead." #-}

-- | The new task name to specify.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtName :: Lens.Lens' UpdateMaintenanceWindowTask (Lude.Maybe Lude.Text)
umwtName = Lens.lens (name :: UpdateMaintenanceWindowTask -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateMaintenanceWindowTask)
{-# DEPRECATED umwtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The targets (either instances or tags) to modify. Instances are specified using Key=instanceids,Values=instanceID_1,instanceID_2. Tags are specified using Key=tag_name,Values=tag_value.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtTargets :: Lens.Lens' UpdateMaintenanceWindowTask (Lude.Maybe [Target])
umwtTargets = Lens.lens (targets :: UpdateMaintenanceWindowTask -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: UpdateMaintenanceWindowTask)
{-# DEPRECATED umwtTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The new logging location in Amazon S3 to specify.
--
-- /Note:/ Consider using 'loggingInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtLoggingInfo :: Lens.Lens' UpdateMaintenanceWindowTask (Lude.Maybe LoggingInfo)
umwtLoggingInfo = Lens.lens (loggingInfo :: UpdateMaintenanceWindowTask -> Lude.Maybe LoggingInfo) (\s a -> s {loggingInfo = a} :: UpdateMaintenanceWindowTask)
{-# DEPRECATED umwtLoggingInfo "Use generic-lens or generic-optics with 'loggingInfo' instead." #-}

-- | The new task description to specify.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtDescription :: Lens.Lens' UpdateMaintenanceWindowTask (Lude.Maybe (Lude.Sensitive Lude.Text))
umwtDescription = Lens.lens (description :: UpdateMaintenanceWindowTask -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: UpdateMaintenanceWindowTask)
{-# DEPRECATED umwtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The new @MaxConcurrency@ value you want to specify. @MaxConcurrency@ is the number of targets that are allowed to run this task in parallel.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtMaxConcurrency :: Lens.Lens' UpdateMaintenanceWindowTask (Lude.Maybe Lude.Text)
umwtMaxConcurrency = Lens.lens (maxConcurrency :: UpdateMaintenanceWindowTask -> Lude.Maybe Lude.Text) (\s a -> s {maxConcurrency = a} :: UpdateMaintenanceWindowTask)
{-# DEPRECATED umwtMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The maintenance window ID that contains the task to modify.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtWindowId :: Lens.Lens' UpdateMaintenanceWindowTask Lude.Text
umwtWindowId = Lens.lens (windowId :: UpdateMaintenanceWindowTask -> Lude.Text) (\s a -> s {windowId = a} :: UpdateMaintenanceWindowTask)
{-# DEPRECATED umwtWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The task ID to modify.
--
-- /Note:/ Consider using 'windowTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtWindowTaskId :: Lens.Lens' UpdateMaintenanceWindowTask Lude.Text
umwtWindowTaskId = Lens.lens (windowTaskId :: UpdateMaintenanceWindowTask -> Lude.Text) (\s a -> s {windowTaskId = a} :: UpdateMaintenanceWindowTask)
{-# DEPRECATED umwtWindowTaskId "Use generic-lens or generic-optics with 'windowTaskId' instead." #-}

instance Lude.AWSRequest UpdateMaintenanceWindowTask where
  type
    Rs UpdateMaintenanceWindowTask =
      UpdateMaintenanceWindowTaskResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateMaintenanceWindowTaskResponse'
            Lude.<$> (x Lude..?> "ServiceRoleArn")
            Lude.<*> (x Lude..?> "WindowTaskId")
            Lude.<*> (x Lude..?> "TaskParameters" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Priority")
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

instance Lude.ToHeaders UpdateMaintenanceWindowTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.UpdateMaintenanceWindowTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateMaintenanceWindowTask where
  toJSON UpdateMaintenanceWindowTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ServiceRoleArn" Lude..=) Lude.<$> serviceRoleARN,
            ("Replace" Lude..=) Lude.<$> replace,
            ("TaskParameters" Lude..=) Lude.<$> taskParameters,
            ("Priority" Lude..=) Lude.<$> priority,
            ("TaskArn" Lude..=) Lude.<$> taskARN,
            ("MaxErrors" Lude..=) Lude.<$> maxErrors,
            ("TaskInvocationParameters" Lude..=)
              Lude.<$> taskInvocationParameters,
            ("Name" Lude..=) Lude.<$> name,
            ("Targets" Lude..=) Lude.<$> targets,
            ("LoggingInfo" Lude..=) Lude.<$> loggingInfo,
            ("Description" Lude..=) Lude.<$> description,
            ("MaxConcurrency" Lude..=) Lude.<$> maxConcurrency,
            Lude.Just ("WindowId" Lude..= windowId),
            Lude.Just ("WindowTaskId" Lude..= windowTaskId)
          ]
      )

instance Lude.ToPath UpdateMaintenanceWindowTask where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateMaintenanceWindowTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateMaintenanceWindowTaskResponse' smart constructor.
data UpdateMaintenanceWindowTaskResponse = UpdateMaintenanceWindowTaskResponse'
  { serviceRoleARN ::
      Lude.Maybe
        Lude.Text,
    windowTaskId ::
      Lude.Maybe
        Lude.Text,
    taskParameters ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (MaintenanceWindowTaskParameterValueExpression)
        ),
    priority ::
      Lude.Maybe
        Lude.Natural,
    taskARN ::
      Lude.Maybe
        Lude.Text,
    maxErrors ::
      Lude.Maybe
        Lude.Text,
    taskInvocationParameters ::
      Lude.Maybe
        MaintenanceWindowTaskInvocationParameters,
    name ::
      Lude.Maybe
        Lude.Text,
    targets ::
      Lude.Maybe [Target],
    loggingInfo ::
      Lude.Maybe
        LoggingInfo,
    description ::
      Lude.Maybe
        ( Lude.Sensitive
            Lude.Text
        ),
    maxConcurrency ::
      Lude.Maybe
        Lude.Text,
    windowId ::
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

-- | Creates a value of 'UpdateMaintenanceWindowTaskResponse' with the minimum fields required to make a request.
--
-- * 'description' - The updated task description.
-- * 'loggingInfo' - The updated logging information in Amazon S3.
-- * 'maxConcurrency' - The updated MaxConcurrency value.
-- * 'maxErrors' - The updated MaxErrors value.
-- * 'name' - The updated task name.
-- * 'priority' - The updated priority value.
-- * 'responseStatus' - The response status code.
-- * 'serviceRoleARN' - The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
-- * 'targets' - The updated target values.
-- * 'taskARN' - The updated task ARN value.
-- * 'taskInvocationParameters' - The updated parameter values.
-- * 'taskParameters' - The updated parameter values.
-- * 'windowId' - The ID of the maintenance window that was updated.
-- * 'windowTaskId' - The task ID of the maintenance window that was updated.
mkUpdateMaintenanceWindowTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateMaintenanceWindowTaskResponse
mkUpdateMaintenanceWindowTaskResponse pResponseStatus_ =
  UpdateMaintenanceWindowTaskResponse'
    { serviceRoleARN =
        Lude.Nothing,
      windowTaskId = Lude.Nothing,
      taskParameters = Lude.Nothing,
      priority = Lude.Nothing,
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
umwtrsServiceRoleARN :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Lude.Maybe Lude.Text)
umwtrsServiceRoleARN = Lens.lens (serviceRoleARN :: UpdateMaintenanceWindowTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleARN = a} :: UpdateMaintenanceWindowTaskResponse)
{-# DEPRECATED umwtrsServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | The task ID of the maintenance window that was updated.
--
-- /Note:/ Consider using 'windowTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrsWindowTaskId :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Lude.Maybe Lude.Text)
umwtrsWindowTaskId = Lens.lens (windowTaskId :: UpdateMaintenanceWindowTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowTaskId = a} :: UpdateMaintenanceWindowTaskResponse)
{-# DEPRECATED umwtrsWindowTaskId "Use generic-lens or generic-optics with 'windowTaskId' instead." #-}

-- | The updated parameter values.
--
-- /Note:/ Consider using 'taskParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrsTaskParameters :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Lude.Maybe (Lude.HashMap Lude.Text (MaintenanceWindowTaskParameterValueExpression)))
umwtrsTaskParameters = Lens.lens (taskParameters :: UpdateMaintenanceWindowTaskResponse -> Lude.Maybe (Lude.HashMap Lude.Text (MaintenanceWindowTaskParameterValueExpression))) (\s a -> s {taskParameters = a} :: UpdateMaintenanceWindowTaskResponse)
{-# DEPRECATED umwtrsTaskParameters "Use generic-lens or generic-optics with 'taskParameters' instead." #-}

-- | The updated priority value.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrsPriority :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Lude.Maybe Lude.Natural)
umwtrsPriority = Lens.lens (priority :: UpdateMaintenanceWindowTaskResponse -> Lude.Maybe Lude.Natural) (\s a -> s {priority = a} :: UpdateMaintenanceWindowTaskResponse)
{-# DEPRECATED umwtrsPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The updated task ARN value.
--
-- /Note:/ Consider using 'taskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrsTaskARN :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Lude.Maybe Lude.Text)
umwtrsTaskARN = Lens.lens (taskARN :: UpdateMaintenanceWindowTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskARN = a} :: UpdateMaintenanceWindowTaskResponse)
{-# DEPRECATED umwtrsTaskARN "Use generic-lens or generic-optics with 'taskARN' instead." #-}

-- | The updated MaxErrors value.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrsMaxErrors :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Lude.Maybe Lude.Text)
umwtrsMaxErrors = Lens.lens (maxErrors :: UpdateMaintenanceWindowTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {maxErrors = a} :: UpdateMaintenanceWindowTaskResponse)
{-# DEPRECATED umwtrsMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | The updated parameter values.
--
-- /Note:/ Consider using 'taskInvocationParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrsTaskInvocationParameters :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Lude.Maybe MaintenanceWindowTaskInvocationParameters)
umwtrsTaskInvocationParameters = Lens.lens (taskInvocationParameters :: UpdateMaintenanceWindowTaskResponse -> Lude.Maybe MaintenanceWindowTaskInvocationParameters) (\s a -> s {taskInvocationParameters = a} :: UpdateMaintenanceWindowTaskResponse)
{-# DEPRECATED umwtrsTaskInvocationParameters "Use generic-lens or generic-optics with 'taskInvocationParameters' instead." #-}

-- | The updated task name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrsName :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Lude.Maybe Lude.Text)
umwtrsName = Lens.lens (name :: UpdateMaintenanceWindowTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateMaintenanceWindowTaskResponse)
{-# DEPRECATED umwtrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The updated target values.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrsTargets :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Lude.Maybe [Target])
umwtrsTargets = Lens.lens (targets :: UpdateMaintenanceWindowTaskResponse -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: UpdateMaintenanceWindowTaskResponse)
{-# DEPRECATED umwtrsTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The updated logging information in Amazon S3.
--
-- /Note:/ Consider using 'loggingInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrsLoggingInfo :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Lude.Maybe LoggingInfo)
umwtrsLoggingInfo = Lens.lens (loggingInfo :: UpdateMaintenanceWindowTaskResponse -> Lude.Maybe LoggingInfo) (\s a -> s {loggingInfo = a} :: UpdateMaintenanceWindowTaskResponse)
{-# DEPRECATED umwtrsLoggingInfo "Use generic-lens or generic-optics with 'loggingInfo' instead." #-}

-- | The updated task description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrsDescription :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
umwtrsDescription = Lens.lens (description :: UpdateMaintenanceWindowTaskResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: UpdateMaintenanceWindowTaskResponse)
{-# DEPRECATED umwtrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The updated MaxConcurrency value.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrsMaxConcurrency :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Lude.Maybe Lude.Text)
umwtrsMaxConcurrency = Lens.lens (maxConcurrency :: UpdateMaintenanceWindowTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {maxConcurrency = a} :: UpdateMaintenanceWindowTaskResponse)
{-# DEPRECATED umwtrsMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The ID of the maintenance window that was updated.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrsWindowId :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Lude.Maybe Lude.Text)
umwtrsWindowId = Lens.lens (windowId :: UpdateMaintenanceWindowTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowId = a} :: UpdateMaintenanceWindowTaskResponse)
{-# DEPRECATED umwtrsWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrsResponseStatus :: Lens.Lens' UpdateMaintenanceWindowTaskResponse Lude.Int
umwtrsResponseStatus = Lens.lens (responseStatus :: UpdateMaintenanceWindowTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateMaintenanceWindowTaskResponse)
{-# DEPRECATED umwtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
