{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateMaintenanceWindowTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a task assigned to a maintenance window. You can\'t change the
-- task type, but you can change the following values:
--
-- -   TaskARN. For example, you can change a RUN_COMMAND task from
--     AWS-RunPowerShellScript to AWS-RunShellScript.
--
-- -   ServiceRoleArn
--
-- -   TaskInvocationParameters
--
-- -   Priority
--
-- -   MaxConcurrency
--
-- -   MaxErrors
--
-- One or more targets must be specified for maintenance window Run
-- Command-type tasks. Depending on the task, targets are optional for
-- other maintenance window task types (Automation, AWS Lambda, and AWS
-- Step Functions). For more information about running tasks that do not
-- specify targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html Registering maintenance window tasks without targets>
-- in the /AWS Systems Manager User Guide/.
--
-- If the value for a parameter in @UpdateMaintenanceWindowTask@ is null,
-- then the corresponding field is not modified. If you set @Replace@ to
-- true, then all fields required by the RegisterTaskWithMaintenanceWindow
-- action are required for this request. Optional fields that aren\'t
-- specified are set to null.
--
-- When you update a maintenance window task that has options specified in
-- @TaskInvocationParameters@, you must provide again all the
-- @TaskInvocationParameters@ values that you want to retain. The values
-- you do not specify again are removed. For example, suppose that when you
-- registered a Run Command task, you specified @TaskInvocationParameters@
-- values for @Comment@, @NotificationConfig@, and @OutputS3BucketName@. If
-- you update the maintenance window task and specify only a different
-- @OutputS3BucketName@ value, the values for @Comment@ and
-- @NotificationConfig@ are removed.
module Network.AWS.SSM.UpdateMaintenanceWindowTask
  ( -- * Creating a Request
    UpdateMaintenanceWindowTask (..),
    newUpdateMaintenanceWindowTask,

    -- * Request Lenses
    updateMaintenanceWindowTask_maxErrors,
    updateMaintenanceWindowTask_taskParameters,
    updateMaintenanceWindowTask_serviceRoleArn,
    updateMaintenanceWindowTask_priority,
    updateMaintenanceWindowTask_targets,
    updateMaintenanceWindowTask_taskInvocationParameters,
    updateMaintenanceWindowTask_name,
    updateMaintenanceWindowTask_replace,
    updateMaintenanceWindowTask_maxConcurrency,
    updateMaintenanceWindowTask_description,
    updateMaintenanceWindowTask_taskArn,
    updateMaintenanceWindowTask_loggingInfo,
    updateMaintenanceWindowTask_windowId,
    updateMaintenanceWindowTask_windowTaskId,

    -- * Destructuring the Response
    UpdateMaintenanceWindowTaskResponse (..),
    newUpdateMaintenanceWindowTaskResponse,

    -- * Response Lenses
    updateMaintenanceWindowTaskResponse_maxErrors,
    updateMaintenanceWindowTaskResponse_taskParameters,
    updateMaintenanceWindowTaskResponse_windowTaskId,
    updateMaintenanceWindowTaskResponse_serviceRoleArn,
    updateMaintenanceWindowTaskResponse_priority,
    updateMaintenanceWindowTaskResponse_targets,
    updateMaintenanceWindowTaskResponse_taskInvocationParameters,
    updateMaintenanceWindowTaskResponse_name,
    updateMaintenanceWindowTaskResponse_maxConcurrency,
    updateMaintenanceWindowTaskResponse_windowId,
    updateMaintenanceWindowTaskResponse_description,
    updateMaintenanceWindowTaskResponse_taskArn,
    updateMaintenanceWindowTaskResponse_loggingInfo,
    updateMaintenanceWindowTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newUpdateMaintenanceWindowTask' smart constructor.
data UpdateMaintenanceWindowTask = UpdateMaintenanceWindowTask'
  { -- | The new @MaxErrors@ value to specify. @MaxErrors@ is the maximum number
    -- of errors that are allowed before the task stops being scheduled.
    --
    -- For maintenance window tasks without a target specified, you cannot
    -- supply a value for this option. Instead, the system inserts a
    -- placeholder value of @1@, which may be reported in the response to this
    -- command. This value does not affect the running of your task and can be
    -- ignored.
    maxErrors :: Core.Maybe Core.Text,
    -- | The parameters to modify.
    --
    -- @TaskParameters@ has been deprecated. To specify parameters to pass to a
    -- task when it runs, instead use the @Parameters@ option in the
    -- @TaskInvocationParameters@ structure. For information about how Systems
    -- Manager handles these options for the supported maintenance window task
    -- types, see MaintenanceWindowTaskInvocationParameters.
    --
    -- The map has the following format:
    --
    -- Key: string, between 1 and 255 characters
    --
    -- Value: an array of strings, each string is between 1 and 255 characters
    taskParameters :: Core.Maybe (Core.Sensitive (Core.HashMap Core.Text (Core.Sensitive MaintenanceWindowTaskParameterValueExpression))),
    -- | The ARN of the IAM service role for Systems Manager to assume when
    -- running a maintenance window task. If you do not specify a service role
    -- ARN, Systems Manager uses your account\'s service-linked role. If no
    -- service-linked role for Systems Manager exists in your account, it is
    -- created when you run @RegisterTaskWithMaintenanceWindow@.
    --
    -- For more information, see the following topics in the in the /AWS
    -- Systems Manager User Guide/:
    --
    -- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/using-service-linked-roles.html#slr-permissions Using service-linked roles for Systems Manager>
    --
    -- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-maintenance-permissions.html#maintenance-window-tasks-service-role Should I use a service-linked role or a custom service role to run maintenance window tasks?>
    serviceRoleArn :: Core.Maybe Core.Text,
    -- | The new task priority to specify. The lower the number, the higher the
    -- priority. Tasks that have the same priority are scheduled in parallel.
    priority :: Core.Maybe Core.Natural,
    -- | The targets (either instances or tags) to modify. Instances are
    -- specified using Key=instanceids,Values=instanceID_1,instanceID_2. Tags
    -- are specified using Key=tag_name,Values=tag_value.
    --
    -- One or more targets must be specified for maintenance window Run
    -- Command-type tasks. Depending on the task, targets are optional for
    -- other maintenance window task types (Automation, AWS Lambda, and AWS
    -- Step Functions). For more information about running tasks that do not
    -- specify targets, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html Registering maintenance window tasks without targets>
    -- in the /AWS Systems Manager User Guide/.
    targets :: Core.Maybe [Target],
    -- | The parameters that the task should use during execution. Populate only
    -- the fields that match the task type. All other fields should be empty.
    --
    -- When you update a maintenance window task that has options specified in
    -- @TaskInvocationParameters@, you must provide again all the
    -- @TaskInvocationParameters@ values that you want to retain. The values
    -- you do not specify again are removed. For example, suppose that when you
    -- registered a Run Command task, you specified @TaskInvocationParameters@
    -- values for @Comment@, @NotificationConfig@, and @OutputS3BucketName@. If
    -- you update the maintenance window task and specify only a different
    -- @OutputS3BucketName@ value, the values for @Comment@ and
    -- @NotificationConfig@ are removed.
    taskInvocationParameters :: Core.Maybe MaintenanceWindowTaskInvocationParameters,
    -- | The new task name to specify.
    name :: Core.Maybe Core.Text,
    -- | If True, then all fields that are required by the
    -- RegisterTaskWithMaintenanceWindow action are also required for this API
    -- request. Optional fields that are not specified are set to null.
    replace :: Core.Maybe Core.Bool,
    -- | The new @MaxConcurrency@ value you want to specify. @MaxConcurrency@ is
    -- the number of targets that are allowed to run this task in parallel.
    --
    -- For maintenance window tasks without a target specified, you cannot
    -- supply a value for this option. Instead, the system inserts a
    -- placeholder value of @1@, which may be reported in the response to this
    -- command. This value does not affect the running of your task and can be
    -- ignored.
    maxConcurrency :: Core.Maybe Core.Text,
    -- | The new task description to specify.
    description :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The task ARN to modify.
    taskArn :: Core.Maybe Core.Text,
    -- | The new logging location in Amazon S3 to specify.
    --
    -- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
    -- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
    -- options in the @TaskInvocationParameters@ structure. For information
    -- about how Systems Manager handles these options for the supported
    -- maintenance window task types, see
    -- MaintenanceWindowTaskInvocationParameters.
    loggingInfo :: Core.Maybe LoggingInfo,
    -- | The maintenance window ID that contains the task to modify.
    windowId :: Core.Text,
    -- | The task ID to modify.
    windowTaskId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateMaintenanceWindowTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxErrors', 'updateMaintenanceWindowTask_maxErrors' - The new @MaxErrors@ value to specify. @MaxErrors@ is the maximum number
-- of errors that are allowed before the task stops being scheduled.
--
-- For maintenance window tasks without a target specified, you cannot
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@, which may be reported in the response to this
-- command. This value does not affect the running of your task and can be
-- ignored.
--
-- 'taskParameters', 'updateMaintenanceWindowTask_taskParameters' - The parameters to modify.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
--
-- The map has the following format:
--
-- Key: string, between 1 and 255 characters
--
-- Value: an array of strings, each string is between 1 and 255 characters
--
-- 'serviceRoleArn', 'updateMaintenanceWindowTask_serviceRoleArn' - The ARN of the IAM service role for Systems Manager to assume when
-- running a maintenance window task. If you do not specify a service role
-- ARN, Systems Manager uses your account\'s service-linked role. If no
-- service-linked role for Systems Manager exists in your account, it is
-- created when you run @RegisterTaskWithMaintenanceWindow@.
--
-- For more information, see the following topics in the in the /AWS
-- Systems Manager User Guide/:
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/using-service-linked-roles.html#slr-permissions Using service-linked roles for Systems Manager>
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-maintenance-permissions.html#maintenance-window-tasks-service-role Should I use a service-linked role or a custom service role to run maintenance window tasks?>
--
-- 'priority', 'updateMaintenanceWindowTask_priority' - The new task priority to specify. The lower the number, the higher the
-- priority. Tasks that have the same priority are scheduled in parallel.
--
-- 'targets', 'updateMaintenanceWindowTask_targets' - The targets (either instances or tags) to modify. Instances are
-- specified using Key=instanceids,Values=instanceID_1,instanceID_2. Tags
-- are specified using Key=tag_name,Values=tag_value.
--
-- One or more targets must be specified for maintenance window Run
-- Command-type tasks. Depending on the task, targets are optional for
-- other maintenance window task types (Automation, AWS Lambda, and AWS
-- Step Functions). For more information about running tasks that do not
-- specify targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html Registering maintenance window tasks without targets>
-- in the /AWS Systems Manager User Guide/.
--
-- 'taskInvocationParameters', 'updateMaintenanceWindowTask_taskInvocationParameters' - The parameters that the task should use during execution. Populate only
-- the fields that match the task type. All other fields should be empty.
--
-- When you update a maintenance window task that has options specified in
-- @TaskInvocationParameters@, you must provide again all the
-- @TaskInvocationParameters@ values that you want to retain. The values
-- you do not specify again are removed. For example, suppose that when you
-- registered a Run Command task, you specified @TaskInvocationParameters@
-- values for @Comment@, @NotificationConfig@, and @OutputS3BucketName@. If
-- you update the maintenance window task and specify only a different
-- @OutputS3BucketName@ value, the values for @Comment@ and
-- @NotificationConfig@ are removed.
--
-- 'name', 'updateMaintenanceWindowTask_name' - The new task name to specify.
--
-- 'replace', 'updateMaintenanceWindowTask_replace' - If True, then all fields that are required by the
-- RegisterTaskWithMaintenanceWindow action are also required for this API
-- request. Optional fields that are not specified are set to null.
--
-- 'maxConcurrency', 'updateMaintenanceWindowTask_maxConcurrency' - The new @MaxConcurrency@ value you want to specify. @MaxConcurrency@ is
-- the number of targets that are allowed to run this task in parallel.
--
-- For maintenance window tasks without a target specified, you cannot
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@, which may be reported in the response to this
-- command. This value does not affect the running of your task and can be
-- ignored.
--
-- 'description', 'updateMaintenanceWindowTask_description' - The new task description to specify.
--
-- 'taskArn', 'updateMaintenanceWindowTask_taskArn' - The task ARN to modify.
--
-- 'loggingInfo', 'updateMaintenanceWindowTask_loggingInfo' - The new logging location in Amazon S3 to specify.
--
-- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
-- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
-- options in the @TaskInvocationParameters@ structure. For information
-- about how Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
--
-- 'windowId', 'updateMaintenanceWindowTask_windowId' - The maintenance window ID that contains the task to modify.
--
-- 'windowTaskId', 'updateMaintenanceWindowTask_windowTaskId' - The task ID to modify.
newUpdateMaintenanceWindowTask ::
  -- | 'windowId'
  Core.Text ->
  -- | 'windowTaskId'
  Core.Text ->
  UpdateMaintenanceWindowTask
newUpdateMaintenanceWindowTask
  pWindowId_
  pWindowTaskId_ =
    UpdateMaintenanceWindowTask'
      { maxErrors =
          Core.Nothing,
        taskParameters = Core.Nothing,
        serviceRoleArn = Core.Nothing,
        priority = Core.Nothing,
        targets = Core.Nothing,
        taskInvocationParameters = Core.Nothing,
        name = Core.Nothing,
        replace = Core.Nothing,
        maxConcurrency = Core.Nothing,
        description = Core.Nothing,
        taskArn = Core.Nothing,
        loggingInfo = Core.Nothing,
        windowId = pWindowId_,
        windowTaskId = pWindowTaskId_
      }

-- | The new @MaxErrors@ value to specify. @MaxErrors@ is the maximum number
-- of errors that are allowed before the task stops being scheduled.
--
-- For maintenance window tasks without a target specified, you cannot
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@, which may be reported in the response to this
-- command. This value does not affect the running of your task and can be
-- ignored.
updateMaintenanceWindowTask_maxErrors :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Core.Text)
updateMaintenanceWindowTask_maxErrors = Lens.lens (\UpdateMaintenanceWindowTask' {maxErrors} -> maxErrors) (\s@UpdateMaintenanceWindowTask' {} a -> s {maxErrors = a} :: UpdateMaintenanceWindowTask)

-- | The parameters to modify.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
--
-- The map has the following format:
--
-- Key: string, between 1 and 255 characters
--
-- Value: an array of strings, each string is between 1 and 255 characters
updateMaintenanceWindowTask_taskParameters :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe (Core.HashMap Core.Text MaintenanceWindowTaskParameterValueExpression))
updateMaintenanceWindowTask_taskParameters = Lens.lens (\UpdateMaintenanceWindowTask' {taskParameters} -> taskParameters) (\s@UpdateMaintenanceWindowTask' {} a -> s {taskParameters = a} :: UpdateMaintenanceWindowTask) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | The ARN of the IAM service role for Systems Manager to assume when
-- running a maintenance window task. If you do not specify a service role
-- ARN, Systems Manager uses your account\'s service-linked role. If no
-- service-linked role for Systems Manager exists in your account, it is
-- created when you run @RegisterTaskWithMaintenanceWindow@.
--
-- For more information, see the following topics in the in the /AWS
-- Systems Manager User Guide/:
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/using-service-linked-roles.html#slr-permissions Using service-linked roles for Systems Manager>
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-maintenance-permissions.html#maintenance-window-tasks-service-role Should I use a service-linked role or a custom service role to run maintenance window tasks?>
updateMaintenanceWindowTask_serviceRoleArn :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Core.Text)
updateMaintenanceWindowTask_serviceRoleArn = Lens.lens (\UpdateMaintenanceWindowTask' {serviceRoleArn} -> serviceRoleArn) (\s@UpdateMaintenanceWindowTask' {} a -> s {serviceRoleArn = a} :: UpdateMaintenanceWindowTask)

-- | The new task priority to specify. The lower the number, the higher the
-- priority. Tasks that have the same priority are scheduled in parallel.
updateMaintenanceWindowTask_priority :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Core.Natural)
updateMaintenanceWindowTask_priority = Lens.lens (\UpdateMaintenanceWindowTask' {priority} -> priority) (\s@UpdateMaintenanceWindowTask' {} a -> s {priority = a} :: UpdateMaintenanceWindowTask)

-- | The targets (either instances or tags) to modify. Instances are
-- specified using Key=instanceids,Values=instanceID_1,instanceID_2. Tags
-- are specified using Key=tag_name,Values=tag_value.
--
-- One or more targets must be specified for maintenance window Run
-- Command-type tasks. Depending on the task, targets are optional for
-- other maintenance window task types (Automation, AWS Lambda, and AWS
-- Step Functions). For more information about running tasks that do not
-- specify targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html Registering maintenance window tasks without targets>
-- in the /AWS Systems Manager User Guide/.
updateMaintenanceWindowTask_targets :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe [Target])
updateMaintenanceWindowTask_targets = Lens.lens (\UpdateMaintenanceWindowTask' {targets} -> targets) (\s@UpdateMaintenanceWindowTask' {} a -> s {targets = a} :: UpdateMaintenanceWindowTask) Core.. Lens.mapping Lens._Coerce

-- | The parameters that the task should use during execution. Populate only
-- the fields that match the task type. All other fields should be empty.
--
-- When you update a maintenance window task that has options specified in
-- @TaskInvocationParameters@, you must provide again all the
-- @TaskInvocationParameters@ values that you want to retain. The values
-- you do not specify again are removed. For example, suppose that when you
-- registered a Run Command task, you specified @TaskInvocationParameters@
-- values for @Comment@, @NotificationConfig@, and @OutputS3BucketName@. If
-- you update the maintenance window task and specify only a different
-- @OutputS3BucketName@ value, the values for @Comment@ and
-- @NotificationConfig@ are removed.
updateMaintenanceWindowTask_taskInvocationParameters :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe MaintenanceWindowTaskInvocationParameters)
updateMaintenanceWindowTask_taskInvocationParameters = Lens.lens (\UpdateMaintenanceWindowTask' {taskInvocationParameters} -> taskInvocationParameters) (\s@UpdateMaintenanceWindowTask' {} a -> s {taskInvocationParameters = a} :: UpdateMaintenanceWindowTask)

-- | The new task name to specify.
updateMaintenanceWindowTask_name :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Core.Text)
updateMaintenanceWindowTask_name = Lens.lens (\UpdateMaintenanceWindowTask' {name} -> name) (\s@UpdateMaintenanceWindowTask' {} a -> s {name = a} :: UpdateMaintenanceWindowTask)

-- | If True, then all fields that are required by the
-- RegisterTaskWithMaintenanceWindow action are also required for this API
-- request. Optional fields that are not specified are set to null.
updateMaintenanceWindowTask_replace :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Core.Bool)
updateMaintenanceWindowTask_replace = Lens.lens (\UpdateMaintenanceWindowTask' {replace} -> replace) (\s@UpdateMaintenanceWindowTask' {} a -> s {replace = a} :: UpdateMaintenanceWindowTask)

-- | The new @MaxConcurrency@ value you want to specify. @MaxConcurrency@ is
-- the number of targets that are allowed to run this task in parallel.
--
-- For maintenance window tasks without a target specified, you cannot
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@, which may be reported in the response to this
-- command. This value does not affect the running of your task and can be
-- ignored.
updateMaintenanceWindowTask_maxConcurrency :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Core.Text)
updateMaintenanceWindowTask_maxConcurrency = Lens.lens (\UpdateMaintenanceWindowTask' {maxConcurrency} -> maxConcurrency) (\s@UpdateMaintenanceWindowTask' {} a -> s {maxConcurrency = a} :: UpdateMaintenanceWindowTask)

-- | The new task description to specify.
updateMaintenanceWindowTask_description :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Core.Text)
updateMaintenanceWindowTask_description = Lens.lens (\UpdateMaintenanceWindowTask' {description} -> description) (\s@UpdateMaintenanceWindowTask' {} a -> s {description = a} :: UpdateMaintenanceWindowTask) Core.. Lens.mapping Core._Sensitive

-- | The task ARN to modify.
updateMaintenanceWindowTask_taskArn :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Core.Text)
updateMaintenanceWindowTask_taskArn = Lens.lens (\UpdateMaintenanceWindowTask' {taskArn} -> taskArn) (\s@UpdateMaintenanceWindowTask' {} a -> s {taskArn = a} :: UpdateMaintenanceWindowTask)

-- | The new logging location in Amazon S3 to specify.
--
-- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
-- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
-- options in the @TaskInvocationParameters@ structure. For information
-- about how Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
updateMaintenanceWindowTask_loggingInfo :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe LoggingInfo)
updateMaintenanceWindowTask_loggingInfo = Lens.lens (\UpdateMaintenanceWindowTask' {loggingInfo} -> loggingInfo) (\s@UpdateMaintenanceWindowTask' {} a -> s {loggingInfo = a} :: UpdateMaintenanceWindowTask)

-- | The maintenance window ID that contains the task to modify.
updateMaintenanceWindowTask_windowId :: Lens.Lens' UpdateMaintenanceWindowTask Core.Text
updateMaintenanceWindowTask_windowId = Lens.lens (\UpdateMaintenanceWindowTask' {windowId} -> windowId) (\s@UpdateMaintenanceWindowTask' {} a -> s {windowId = a} :: UpdateMaintenanceWindowTask)

-- | The task ID to modify.
updateMaintenanceWindowTask_windowTaskId :: Lens.Lens' UpdateMaintenanceWindowTask Core.Text
updateMaintenanceWindowTask_windowTaskId = Lens.lens (\UpdateMaintenanceWindowTask' {windowTaskId} -> windowTaskId) (\s@UpdateMaintenanceWindowTask' {} a -> s {windowTaskId = a} :: UpdateMaintenanceWindowTask)

instance Core.AWSRequest UpdateMaintenanceWindowTask where
  type
    AWSResponse UpdateMaintenanceWindowTask =
      UpdateMaintenanceWindowTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMaintenanceWindowTaskResponse'
            Core.<$> (x Core..?> "MaxErrors")
            Core.<*> (x Core..?> "TaskParameters" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "WindowTaskId")
            Core.<*> (x Core..?> "ServiceRoleArn")
            Core.<*> (x Core..?> "Priority")
            Core.<*> (x Core..?> "Targets" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "TaskInvocationParameters")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "MaxConcurrency")
            Core.<*> (x Core..?> "WindowId")
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "TaskArn")
            Core.<*> (x Core..?> "LoggingInfo")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateMaintenanceWindowTask

instance Core.NFData UpdateMaintenanceWindowTask

instance Core.ToHeaders UpdateMaintenanceWindowTask where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.UpdateMaintenanceWindowTask" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateMaintenanceWindowTask where
  toJSON UpdateMaintenanceWindowTask' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxErrors" Core..=) Core.<$> maxErrors,
            ("TaskParameters" Core..=) Core.<$> taskParameters,
            ("ServiceRoleArn" Core..=) Core.<$> serviceRoleArn,
            ("Priority" Core..=) Core.<$> priority,
            ("Targets" Core..=) Core.<$> targets,
            ("TaskInvocationParameters" Core..=)
              Core.<$> taskInvocationParameters,
            ("Name" Core..=) Core.<$> name,
            ("Replace" Core..=) Core.<$> replace,
            ("MaxConcurrency" Core..=) Core.<$> maxConcurrency,
            ("Description" Core..=) Core.<$> description,
            ("TaskArn" Core..=) Core.<$> taskArn,
            ("LoggingInfo" Core..=) Core.<$> loggingInfo,
            Core.Just ("WindowId" Core..= windowId),
            Core.Just ("WindowTaskId" Core..= windowTaskId)
          ]
      )

instance Core.ToPath UpdateMaintenanceWindowTask where
  toPath = Core.const "/"

instance Core.ToQuery UpdateMaintenanceWindowTask where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateMaintenanceWindowTaskResponse' smart constructor.
data UpdateMaintenanceWindowTaskResponse = UpdateMaintenanceWindowTaskResponse'
  { -- | The updated MaxErrors value.
    maxErrors :: Core.Maybe Core.Text,
    -- | The updated parameter values.
    --
    -- @TaskParameters@ has been deprecated. To specify parameters to pass to a
    -- task when it runs, instead use the @Parameters@ option in the
    -- @TaskInvocationParameters@ structure. For information about how Systems
    -- Manager handles these options for the supported maintenance window task
    -- types, see MaintenanceWindowTaskInvocationParameters.
    taskParameters :: Core.Maybe (Core.Sensitive (Core.HashMap Core.Text (Core.Sensitive MaintenanceWindowTaskParameterValueExpression))),
    -- | The task ID of the maintenance window that was updated.
    windowTaskId :: Core.Maybe Core.Text,
    -- | The ARN of the IAM service role to use to publish Amazon Simple
    -- Notification Service (Amazon SNS) notifications for maintenance window
    -- Run Command tasks.
    serviceRoleArn :: Core.Maybe Core.Text,
    -- | The updated priority value.
    priority :: Core.Maybe Core.Natural,
    -- | The updated target values.
    targets :: Core.Maybe [Target],
    -- | The updated parameter values.
    taskInvocationParameters :: Core.Maybe MaintenanceWindowTaskInvocationParameters,
    -- | The updated task name.
    name :: Core.Maybe Core.Text,
    -- | The updated MaxConcurrency value.
    maxConcurrency :: Core.Maybe Core.Text,
    -- | The ID of the maintenance window that was updated.
    windowId :: Core.Maybe Core.Text,
    -- | The updated task description.
    description :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The updated task ARN value.
    taskArn :: Core.Maybe Core.Text,
    -- | The updated logging information in Amazon S3.
    --
    -- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
    -- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
    -- options in the @TaskInvocationParameters@ structure. For information
    -- about how Systems Manager handles these options for the supported
    -- maintenance window task types, see
    -- MaintenanceWindowTaskInvocationParameters.
    loggingInfo :: Core.Maybe LoggingInfo,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateMaintenanceWindowTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxErrors', 'updateMaintenanceWindowTaskResponse_maxErrors' - The updated MaxErrors value.
--
-- 'taskParameters', 'updateMaintenanceWindowTaskResponse_taskParameters' - The updated parameter values.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
--
-- 'windowTaskId', 'updateMaintenanceWindowTaskResponse_windowTaskId' - The task ID of the maintenance window that was updated.
--
-- 'serviceRoleArn', 'updateMaintenanceWindowTaskResponse_serviceRoleArn' - The ARN of the IAM service role to use to publish Amazon Simple
-- Notification Service (Amazon SNS) notifications for maintenance window
-- Run Command tasks.
--
-- 'priority', 'updateMaintenanceWindowTaskResponse_priority' - The updated priority value.
--
-- 'targets', 'updateMaintenanceWindowTaskResponse_targets' - The updated target values.
--
-- 'taskInvocationParameters', 'updateMaintenanceWindowTaskResponse_taskInvocationParameters' - The updated parameter values.
--
-- 'name', 'updateMaintenanceWindowTaskResponse_name' - The updated task name.
--
-- 'maxConcurrency', 'updateMaintenanceWindowTaskResponse_maxConcurrency' - The updated MaxConcurrency value.
--
-- 'windowId', 'updateMaintenanceWindowTaskResponse_windowId' - The ID of the maintenance window that was updated.
--
-- 'description', 'updateMaintenanceWindowTaskResponse_description' - The updated task description.
--
-- 'taskArn', 'updateMaintenanceWindowTaskResponse_taskArn' - The updated task ARN value.
--
-- 'loggingInfo', 'updateMaintenanceWindowTaskResponse_loggingInfo' - The updated logging information in Amazon S3.
--
-- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
-- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
-- options in the @TaskInvocationParameters@ structure. For information
-- about how Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
--
-- 'httpStatus', 'updateMaintenanceWindowTaskResponse_httpStatus' - The response's http status code.
newUpdateMaintenanceWindowTaskResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateMaintenanceWindowTaskResponse
newUpdateMaintenanceWindowTaskResponse pHttpStatus_ =
  UpdateMaintenanceWindowTaskResponse'
    { maxErrors =
        Core.Nothing,
      taskParameters = Core.Nothing,
      windowTaskId = Core.Nothing,
      serviceRoleArn = Core.Nothing,
      priority = Core.Nothing,
      targets = Core.Nothing,
      taskInvocationParameters =
        Core.Nothing,
      name = Core.Nothing,
      maxConcurrency = Core.Nothing,
      windowId = Core.Nothing,
      description = Core.Nothing,
      taskArn = Core.Nothing,
      loggingInfo = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated MaxErrors value.
updateMaintenanceWindowTaskResponse_maxErrors :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Core.Text)
updateMaintenanceWindowTaskResponse_maxErrors = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {maxErrors} -> maxErrors) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {maxErrors = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated parameter values.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
updateMaintenanceWindowTaskResponse_taskParameters :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe (Core.HashMap Core.Text MaintenanceWindowTaskParameterValueExpression))
updateMaintenanceWindowTaskResponse_taskParameters = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {taskParameters} -> taskParameters) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {taskParameters = a} :: UpdateMaintenanceWindowTaskResponse) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | The task ID of the maintenance window that was updated.
updateMaintenanceWindowTaskResponse_windowTaskId :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Core.Text)
updateMaintenanceWindowTaskResponse_windowTaskId = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {windowTaskId} -> windowTaskId) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {windowTaskId = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The ARN of the IAM service role to use to publish Amazon Simple
-- Notification Service (Amazon SNS) notifications for maintenance window
-- Run Command tasks.
updateMaintenanceWindowTaskResponse_serviceRoleArn :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Core.Text)
updateMaintenanceWindowTaskResponse_serviceRoleArn = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {serviceRoleArn} -> serviceRoleArn) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {serviceRoleArn = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated priority value.
updateMaintenanceWindowTaskResponse_priority :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Core.Natural)
updateMaintenanceWindowTaskResponse_priority = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {priority} -> priority) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {priority = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated target values.
updateMaintenanceWindowTaskResponse_targets :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe [Target])
updateMaintenanceWindowTaskResponse_targets = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {targets} -> targets) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {targets = a} :: UpdateMaintenanceWindowTaskResponse) Core.. Lens.mapping Lens._Coerce

-- | The updated parameter values.
updateMaintenanceWindowTaskResponse_taskInvocationParameters :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe MaintenanceWindowTaskInvocationParameters)
updateMaintenanceWindowTaskResponse_taskInvocationParameters = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {taskInvocationParameters} -> taskInvocationParameters) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {taskInvocationParameters = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated task name.
updateMaintenanceWindowTaskResponse_name :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Core.Text)
updateMaintenanceWindowTaskResponse_name = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {name} -> name) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {name = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated MaxConcurrency value.
updateMaintenanceWindowTaskResponse_maxConcurrency :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Core.Text)
updateMaintenanceWindowTaskResponse_maxConcurrency = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {maxConcurrency} -> maxConcurrency) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {maxConcurrency = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The ID of the maintenance window that was updated.
updateMaintenanceWindowTaskResponse_windowId :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Core.Text)
updateMaintenanceWindowTaskResponse_windowId = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {windowId} -> windowId) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {windowId = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated task description.
updateMaintenanceWindowTaskResponse_description :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Core.Text)
updateMaintenanceWindowTaskResponse_description = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {description} -> description) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {description = a} :: UpdateMaintenanceWindowTaskResponse) Core.. Lens.mapping Core._Sensitive

-- | The updated task ARN value.
updateMaintenanceWindowTaskResponse_taskArn :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Core.Text)
updateMaintenanceWindowTaskResponse_taskArn = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {taskArn} -> taskArn) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {taskArn = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated logging information in Amazon S3.
--
-- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
-- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
-- options in the @TaskInvocationParameters@ structure. For information
-- about how Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
updateMaintenanceWindowTaskResponse_loggingInfo :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe LoggingInfo)
updateMaintenanceWindowTaskResponse_loggingInfo = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {loggingInfo} -> loggingInfo) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {loggingInfo = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The response's http status code.
updateMaintenanceWindowTaskResponse_httpStatus :: Lens.Lens' UpdateMaintenanceWindowTaskResponse Core.Int
updateMaintenanceWindowTaskResponse_httpStatus = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {httpStatus} -> httpStatus) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {httpStatus = a} :: UpdateMaintenanceWindowTaskResponse)

instance
  Core.NFData
    UpdateMaintenanceWindowTaskResponse
