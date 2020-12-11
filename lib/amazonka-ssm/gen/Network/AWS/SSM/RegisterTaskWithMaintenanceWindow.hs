{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    rtwmwServiceRoleARN,
    rtwmwTaskParameters,
    rtwmwPriority,
    rtwmwClientToken,
    rtwmwTaskInvocationParameters,
    rtwmwName,
    rtwmwLoggingInfo,
    rtwmwDescription,
    rtwmwWindowId,
    rtwmwTargets,
    rtwmwTaskARN,
    rtwmwTaskType,
    rtwmwMaxConcurrency,
    rtwmwMaxErrors,

    -- * Destructuring the response
    RegisterTaskWithMaintenanceWindowResponse (..),
    mkRegisterTaskWithMaintenanceWindowResponse,

    -- ** Response lenses
    rtwmwrsWindowTaskId,
    rtwmwrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkRegisterTaskWithMaintenanceWindow' smart constructor.
data RegisterTaskWithMaintenanceWindow = RegisterTaskWithMaintenanceWindow'
  { serviceRoleARN ::
      Lude.Maybe Lude.Text,
    taskParameters ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (MaintenanceWindowTaskParameterValueExpression)
        ),
    priority ::
      Lude.Maybe Lude.Natural,
    clientToken ::
      Lude.Maybe Lude.Text,
    taskInvocationParameters ::
      Lude.Maybe
        MaintenanceWindowTaskInvocationParameters,
    name ::
      Lude.Maybe Lude.Text,
    loggingInfo ::
      Lude.Maybe LoggingInfo,
    description ::
      Lude.Maybe
        ( Lude.Sensitive
            Lude.Text
        ),
    windowId :: Lude.Text,
    targets :: [Target],
    taskARN :: Lude.Text,
    taskType ::
      MaintenanceWindowTaskType,
    maxConcurrency ::
      Lude.Text,
    maxErrors :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterTaskWithMaintenanceWindow' with the minimum fields required to make a request.
--
-- * 'clientToken' - User-provided idempotency token.
-- * 'description' - An optional description for the task.
-- * 'loggingInfo' - A structure containing information about an S3 bucket to write instance-level logs to.
-- * 'maxConcurrency' - The maximum number of targets this task can be run for in parallel.
-- * 'maxErrors' - The maximum number of errors allowed before this task stops being scheduled.
-- * 'name' - An optional name for the task.
-- * 'priority' - The priority of the task in the maintenance window, the lower the number the higher the priority. Tasks in a maintenance window are scheduled in priority order with tasks that have the same priority scheduled in parallel.
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
-- * 'targets' - The targets (either instances or maintenance window targets).
--
-- Specify instances using the following format:
-- @Key=InstanceIds,Values=<instance-id-1>,<instance-id-2>@
-- Specify maintenance window targets using the following format:
-- @Key=WindowTargetIds;,Values=<window-target-id-1>,<window-target-id-2>@
-- * 'taskARN' - The ARN of the task to run.
-- * 'taskInvocationParameters' - The parameters that the task should use during execution. Populate only the fields that match the task type. All other fields should be empty.
-- * 'taskParameters' - The parameters that should be passed to the task when it is run.
-- * 'taskType' - The type of task being registered.
-- * 'windowId' - The ID of the maintenance window the task should be added to.
mkRegisterTaskWithMaintenanceWindow ::
  -- | 'windowId'
  Lude.Text ->
  -- | 'taskARN'
  Lude.Text ->
  -- | 'taskType'
  MaintenanceWindowTaskType ->
  -- | 'maxConcurrency'
  Lude.Text ->
  -- | 'maxErrors'
  Lude.Text ->
  RegisterTaskWithMaintenanceWindow
mkRegisterTaskWithMaintenanceWindow
  pWindowId_
  pTaskARN_
  pTaskType_
  pMaxConcurrency_
  pMaxErrors_ =
    RegisterTaskWithMaintenanceWindow'
      { serviceRoleARN = Lude.Nothing,
        taskParameters = Lude.Nothing,
        priority = Lude.Nothing,
        clientToken = Lude.Nothing,
        taskInvocationParameters = Lude.Nothing,
        name = Lude.Nothing,
        loggingInfo = Lude.Nothing,
        description = Lude.Nothing,
        windowId = pWindowId_,
        targets = Lude.mempty,
        taskARN = pTaskARN_,
        taskType = pTaskType_,
        maxConcurrency = pMaxConcurrency_,
        maxErrors = pMaxErrors_
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
rtwmwServiceRoleARN :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Lude.Maybe Lude.Text)
rtwmwServiceRoleARN = Lens.lens (serviceRoleARN :: RegisterTaskWithMaintenanceWindow -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleARN = a} :: RegisterTaskWithMaintenanceWindow)
{-# DEPRECATED rtwmwServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | The parameters that should be passed to the task when it is run.
--
-- /Note:/ Consider using 'taskParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwTaskParameters :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Lude.Maybe (Lude.HashMap Lude.Text (MaintenanceWindowTaskParameterValueExpression)))
rtwmwTaskParameters = Lens.lens (taskParameters :: RegisterTaskWithMaintenanceWindow -> Lude.Maybe (Lude.HashMap Lude.Text (MaintenanceWindowTaskParameterValueExpression))) (\s a -> s {taskParameters = a} :: RegisterTaskWithMaintenanceWindow)
{-# DEPRECATED rtwmwTaskParameters "Use generic-lens or generic-optics with 'taskParameters' instead." #-}

-- | The priority of the task in the maintenance window, the lower the number the higher the priority. Tasks in a maintenance window are scheduled in priority order with tasks that have the same priority scheduled in parallel.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwPriority :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Lude.Maybe Lude.Natural)
rtwmwPriority = Lens.lens (priority :: RegisterTaskWithMaintenanceWindow -> Lude.Maybe Lude.Natural) (\s a -> s {priority = a} :: RegisterTaskWithMaintenanceWindow)
{-# DEPRECATED rtwmwPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | User-provided idempotency token.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwClientToken :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Lude.Maybe Lude.Text)
rtwmwClientToken = Lens.lens (clientToken :: RegisterTaskWithMaintenanceWindow -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: RegisterTaskWithMaintenanceWindow)
{-# DEPRECATED rtwmwClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The parameters that the task should use during execution. Populate only the fields that match the task type. All other fields should be empty.
--
-- /Note:/ Consider using 'taskInvocationParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwTaskInvocationParameters :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Lude.Maybe MaintenanceWindowTaskInvocationParameters)
rtwmwTaskInvocationParameters = Lens.lens (taskInvocationParameters :: RegisterTaskWithMaintenanceWindow -> Lude.Maybe MaintenanceWindowTaskInvocationParameters) (\s a -> s {taskInvocationParameters = a} :: RegisterTaskWithMaintenanceWindow)
{-# DEPRECATED rtwmwTaskInvocationParameters "Use generic-lens or generic-optics with 'taskInvocationParameters' instead." #-}

-- | An optional name for the task.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwName :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Lude.Maybe Lude.Text)
rtwmwName = Lens.lens (name :: RegisterTaskWithMaintenanceWindow -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RegisterTaskWithMaintenanceWindow)
{-# DEPRECATED rtwmwName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A structure containing information about an S3 bucket to write instance-level logs to.
--
-- /Note:/ Consider using 'loggingInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwLoggingInfo :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Lude.Maybe LoggingInfo)
rtwmwLoggingInfo = Lens.lens (loggingInfo :: RegisterTaskWithMaintenanceWindow -> Lude.Maybe LoggingInfo) (\s a -> s {loggingInfo = a} :: RegisterTaskWithMaintenanceWindow)
{-# DEPRECATED rtwmwLoggingInfo "Use generic-lens or generic-optics with 'loggingInfo' instead." #-}

-- | An optional description for the task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwDescription :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Lude.Maybe (Lude.Sensitive Lude.Text))
rtwmwDescription = Lens.lens (description :: RegisterTaskWithMaintenanceWindow -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: RegisterTaskWithMaintenanceWindow)
{-# DEPRECATED rtwmwDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the maintenance window the task should be added to.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwWindowId :: Lens.Lens' RegisterTaskWithMaintenanceWindow Lude.Text
rtwmwWindowId = Lens.lens (windowId :: RegisterTaskWithMaintenanceWindow -> Lude.Text) (\s a -> s {windowId = a} :: RegisterTaskWithMaintenanceWindow)
{-# DEPRECATED rtwmwWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The targets (either instances or maintenance window targets).
--
-- Specify instances using the following format:
-- @Key=InstanceIds,Values=<instance-id-1>,<instance-id-2>@
-- Specify maintenance window targets using the following format:
-- @Key=WindowTargetIds;,Values=<window-target-id-1>,<window-target-id-2>@
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwTargets :: Lens.Lens' RegisterTaskWithMaintenanceWindow [Target]
rtwmwTargets = Lens.lens (targets :: RegisterTaskWithMaintenanceWindow -> [Target]) (\s a -> s {targets = a} :: RegisterTaskWithMaintenanceWindow)
{-# DEPRECATED rtwmwTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The ARN of the task to run.
--
-- /Note:/ Consider using 'taskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwTaskARN :: Lens.Lens' RegisterTaskWithMaintenanceWindow Lude.Text
rtwmwTaskARN = Lens.lens (taskARN :: RegisterTaskWithMaintenanceWindow -> Lude.Text) (\s a -> s {taskARN = a} :: RegisterTaskWithMaintenanceWindow)
{-# DEPRECATED rtwmwTaskARN "Use generic-lens or generic-optics with 'taskARN' instead." #-}

-- | The type of task being registered.
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwTaskType :: Lens.Lens' RegisterTaskWithMaintenanceWindow MaintenanceWindowTaskType
rtwmwTaskType = Lens.lens (taskType :: RegisterTaskWithMaintenanceWindow -> MaintenanceWindowTaskType) (\s a -> s {taskType = a} :: RegisterTaskWithMaintenanceWindow)
{-# DEPRECATED rtwmwTaskType "Use generic-lens or generic-optics with 'taskType' instead." #-}

-- | The maximum number of targets this task can be run for in parallel.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwMaxConcurrency :: Lens.Lens' RegisterTaskWithMaintenanceWindow Lude.Text
rtwmwMaxConcurrency = Lens.lens (maxConcurrency :: RegisterTaskWithMaintenanceWindow -> Lude.Text) (\s a -> s {maxConcurrency = a} :: RegisterTaskWithMaintenanceWindow)
{-# DEPRECATED rtwmwMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The maximum number of errors allowed before this task stops being scheduled.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwMaxErrors :: Lens.Lens' RegisterTaskWithMaintenanceWindow Lude.Text
rtwmwMaxErrors = Lens.lens (maxErrors :: RegisterTaskWithMaintenanceWindow -> Lude.Text) (\s a -> s {maxErrors = a} :: RegisterTaskWithMaintenanceWindow)
{-# DEPRECATED rtwmwMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

instance Lude.AWSRequest RegisterTaskWithMaintenanceWindow where
  type
    Rs RegisterTaskWithMaintenanceWindow =
      RegisterTaskWithMaintenanceWindowResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterTaskWithMaintenanceWindowResponse'
            Lude.<$> (x Lude..?> "WindowTaskId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterTaskWithMaintenanceWindow where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.RegisterTaskWithMaintenanceWindow" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterTaskWithMaintenanceWindow where
  toJSON RegisterTaskWithMaintenanceWindow' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ServiceRoleArn" Lude..=) Lude.<$> serviceRoleARN,
            ("TaskParameters" Lude..=) Lude.<$> taskParameters,
            ("Priority" Lude..=) Lude.<$> priority,
            ("ClientToken" Lude..=) Lude.<$> clientToken,
            ("TaskInvocationParameters" Lude..=)
              Lude.<$> taskInvocationParameters,
            ("Name" Lude..=) Lude.<$> name,
            ("LoggingInfo" Lude..=) Lude.<$> loggingInfo,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("WindowId" Lude..= windowId),
            Lude.Just ("Targets" Lude..= targets),
            Lude.Just ("TaskArn" Lude..= taskARN),
            Lude.Just ("TaskType" Lude..= taskType),
            Lude.Just ("MaxConcurrency" Lude..= maxConcurrency),
            Lude.Just ("MaxErrors" Lude..= maxErrors)
          ]
      )

instance Lude.ToPath RegisterTaskWithMaintenanceWindow where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterTaskWithMaintenanceWindow where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterTaskWithMaintenanceWindowResponse' smart constructor.
data RegisterTaskWithMaintenanceWindowResponse = RegisterTaskWithMaintenanceWindowResponse'
  { windowTaskId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterTaskWithMaintenanceWindowResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'windowTaskId' - The ID of the task in the maintenance window.
mkRegisterTaskWithMaintenanceWindowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterTaskWithMaintenanceWindowResponse
mkRegisterTaskWithMaintenanceWindowResponse pResponseStatus_ =
  RegisterTaskWithMaintenanceWindowResponse'
    { windowTaskId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the task in the maintenance window.
--
-- /Note:/ Consider using 'windowTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwrsWindowTaskId :: Lens.Lens' RegisterTaskWithMaintenanceWindowResponse (Lude.Maybe Lude.Text)
rtwmwrsWindowTaskId = Lens.lens (windowTaskId :: RegisterTaskWithMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowTaskId = a} :: RegisterTaskWithMaintenanceWindowResponse)
{-# DEPRECATED rtwmwrsWindowTaskId "Use generic-lens or generic-optics with 'windowTaskId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwrsResponseStatus :: Lens.Lens' RegisterTaskWithMaintenanceWindowResponse Lude.Int
rtwmwrsResponseStatus = Lens.lens (responseStatus :: RegisterTaskWithMaintenanceWindowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterTaskWithMaintenanceWindowResponse)
{-# DEPRECATED rtwmwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
