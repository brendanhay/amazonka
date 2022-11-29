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
-- Module      : Amazonka.SSM.UpdateMaintenanceWindowTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a task assigned to a maintenance window. You can\'t change the
-- task type, but you can change the following values:
--
-- -   @TaskARN@. For example, you can change a @RUN_COMMAND@ task from
--     @AWS-RunPowerShellScript@ to @AWS-RunShellScript@.
--
-- -   @ServiceRoleArn@
--
-- -   @TaskInvocationParameters@
--
-- -   @Priority@
--
-- -   @MaxConcurrency@
--
-- -   @MaxErrors@
--
-- One or more targets must be specified for maintenance window Run
-- Command-type tasks. Depending on the task, targets are optional for
-- other maintenance window task types (Automation, Lambda, and Step
-- Functions). For more information about running tasks that don\'t specify
-- targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html Registering maintenance window tasks without targets>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- If the value for a parameter in @UpdateMaintenanceWindowTask@ is null,
-- then the corresponding field isn\'t modified. If you set @Replace@ to
-- true, then all fields required by the RegisterTaskWithMaintenanceWindow
-- operation are required for this request. Optional fields that aren\'t
-- specified are set to null.
--
-- When you update a maintenance window task that has options specified in
-- @TaskInvocationParameters@, you must provide again all the
-- @TaskInvocationParameters@ values that you want to retain. The values
-- you don\'t specify again are removed. For example, suppose that when you
-- registered a Run Command task, you specified @TaskInvocationParameters@
-- values for @Comment@, @NotificationConfig@, and @OutputS3BucketName@. If
-- you update the maintenance window task and specify only a different
-- @OutputS3BucketName@ value, the values for @Comment@ and
-- @NotificationConfig@ are removed.
module Amazonka.SSM.UpdateMaintenanceWindowTask
  ( -- * Creating a Request
    UpdateMaintenanceWindowTask (..),
    newUpdateMaintenanceWindowTask,

    -- * Request Lenses
    updateMaintenanceWindowTask_name,
    updateMaintenanceWindowTask_taskParameters,
    updateMaintenanceWindowTask_serviceRoleArn,
    updateMaintenanceWindowTask_taskInvocationParameters,
    updateMaintenanceWindowTask_taskArn,
    updateMaintenanceWindowTask_targets,
    updateMaintenanceWindowTask_description,
    updateMaintenanceWindowTask_alarmConfiguration,
    updateMaintenanceWindowTask_priority,
    updateMaintenanceWindowTask_maxConcurrency,
    updateMaintenanceWindowTask_maxErrors,
    updateMaintenanceWindowTask_loggingInfo,
    updateMaintenanceWindowTask_replace,
    updateMaintenanceWindowTask_cutoffBehavior,
    updateMaintenanceWindowTask_windowId,
    updateMaintenanceWindowTask_windowTaskId,

    -- * Destructuring the Response
    UpdateMaintenanceWindowTaskResponse (..),
    newUpdateMaintenanceWindowTaskResponse,

    -- * Response Lenses
    updateMaintenanceWindowTaskResponse_name,
    updateMaintenanceWindowTaskResponse_taskParameters,
    updateMaintenanceWindowTaskResponse_serviceRoleArn,
    updateMaintenanceWindowTaskResponse_taskInvocationParameters,
    updateMaintenanceWindowTaskResponse_windowTaskId,
    updateMaintenanceWindowTaskResponse_taskArn,
    updateMaintenanceWindowTaskResponse_windowId,
    updateMaintenanceWindowTaskResponse_targets,
    updateMaintenanceWindowTaskResponse_description,
    updateMaintenanceWindowTaskResponse_alarmConfiguration,
    updateMaintenanceWindowTaskResponse_priority,
    updateMaintenanceWindowTaskResponse_maxConcurrency,
    updateMaintenanceWindowTaskResponse_maxErrors,
    updateMaintenanceWindowTaskResponse_loggingInfo,
    updateMaintenanceWindowTaskResponse_cutoffBehavior,
    updateMaintenanceWindowTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newUpdateMaintenanceWindowTask' smart constructor.
data UpdateMaintenanceWindowTask = UpdateMaintenanceWindowTask'
  { -- | The new task name to specify.
    name :: Prelude.Maybe Prelude.Text,
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
    taskParameters :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text (Core.Sensitive MaintenanceWindowTaskParameterValueExpression))),
    -- | The Amazon Resource Name (ARN) of the IAM service role for Amazon Web
    -- Services Systems Manager to assume when running a maintenance window
    -- task. If you do not specify a service role ARN, Systems Manager uses
    -- your account\'s service-linked role. If no service-linked role for
    -- Systems Manager exists in your account, it is created when you run
    -- @RegisterTaskWithMaintenanceWindow@.
    --
    -- For more information, see the following topics in the in the /Amazon Web
    -- Services Systems Manager User Guide/:
    --
    -- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/using-service-linked-roles.html#slr-permissions Using service-linked roles for Systems Manager>
    --
    -- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-maintenance-permissions.html#maintenance-window-tasks-service-role Should I use a service-linked role or a custom service role to run maintenance window tasks?>
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The parameters that the task should use during execution. Populate only
    -- the fields that match the task type. All other fields should be empty.
    --
    -- When you update a maintenance window task that has options specified in
    -- @TaskInvocationParameters@, you must provide again all the
    -- @TaskInvocationParameters@ values that you want to retain. The values
    -- you don\'t specify again are removed. For example, suppose that when you
    -- registered a Run Command task, you specified @TaskInvocationParameters@
    -- values for @Comment@, @NotificationConfig@, and @OutputS3BucketName@. If
    -- you update the maintenance window task and specify only a different
    -- @OutputS3BucketName@ value, the values for @Comment@ and
    -- @NotificationConfig@ are removed.
    taskInvocationParameters :: Prelude.Maybe MaintenanceWindowTaskInvocationParameters,
    -- | The task ARN to modify.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The targets (either managed nodes or tags) to modify. Managed nodes are
    -- specified using the format
    -- @Key=instanceids,Values=instanceID_1,instanceID_2@. Tags are specified
    -- using the format @ Key=tag_name,Values=tag_value@.
    --
    -- One or more targets must be specified for maintenance window Run
    -- Command-type tasks. Depending on the task, targets are optional for
    -- other maintenance window task types (Automation, Lambda, and Step
    -- Functions). For more information about running tasks that don\'t specify
    -- targets, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html Registering maintenance window tasks without targets>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    targets :: Prelude.Maybe [Target],
    -- | The new task description to specify.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The CloudWatch alarm you want to apply to your maintenance window task.
    alarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | The new task priority to specify. The lower the number, the higher the
    -- priority. Tasks that have the same priority are scheduled in parallel.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The new @MaxConcurrency@ value you want to specify. @MaxConcurrency@ is
    -- the number of targets that are allowed to run this task, in parallel.
    --
    -- Although this element is listed as \"Required: No\", a value can be
    -- omitted only when you are registering or updating a
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html targetless task>
    -- You must provide a value in all other cases.
    --
    -- For maintenance window tasks without a target specified, you can\'t
    -- supply a value for this option. Instead, the system inserts a
    -- placeholder value of @1@. This value doesn\'t affect the running of your
    -- task.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The new @MaxErrors@ value to specify. @MaxErrors@ is the maximum number
    -- of errors that are allowed before the task stops being scheduled.
    --
    -- Although this element is listed as \"Required: No\", a value can be
    -- omitted only when you are registering or updating a
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html targetless task>
    -- You must provide a value in all other cases.
    --
    -- For maintenance window tasks without a target specified, you can\'t
    -- supply a value for this option. Instead, the system inserts a
    -- placeholder value of @1@. This value doesn\'t affect the running of your
    -- task.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | The new logging location in Amazon S3 to specify.
    --
    -- @LoggingInfo@ has been deprecated. To specify an Amazon Simple Storage
    -- Service (Amazon S3) bucket to contain logs, instead use the
    -- @OutputS3BucketName@ and @OutputS3KeyPrefix@ options in the
    -- @TaskInvocationParameters@ structure. For information about how Amazon
    -- Web Services Systems Manager handles these options for the supported
    -- maintenance window task types, see
    -- MaintenanceWindowTaskInvocationParameters.
    loggingInfo :: Prelude.Maybe LoggingInfo,
    -- | If True, then all fields that are required by the
    -- RegisterTaskWithMaintenanceWindow operation are also required for this
    -- API request. Optional fields that aren\'t specified are set to null.
    replace :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether tasks should continue to run after the cutoff time
    -- specified in the maintenance windows is reached.
    --
    -- -   @CONTINUE_TASK@: When the cutoff time is reached, any tasks that are
    --     running continue. The default value.
    --
    -- -   @CANCEL_TASK@:
    --
    --     -   For Automation, Lambda, Step Functions tasks: When the cutoff
    --         time is reached, any task invocations that are already running
    --         continue, but no new task invocations are started.
    --
    --     -   For Run Command tasks: When the cutoff time is reached, the
    --         system sends a CancelCommand operation that attempts to cancel
    --         the command associated with the task. However, there is no
    --         guarantee that the command will be terminated and the underlying
    --         process stopped.
    --
    --     The status for tasks that are not completed is @TIMED_OUT@.
    cutoffBehavior :: Prelude.Maybe MaintenanceWindowTaskCutoffBehavior,
    -- | The maintenance window ID that contains the task to modify.
    windowId :: Prelude.Text,
    -- | The task ID to modify.
    windowTaskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMaintenanceWindowTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateMaintenanceWindowTask_name' - The new task name to specify.
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
-- 'serviceRoleArn', 'updateMaintenanceWindowTask_serviceRoleArn' - The Amazon Resource Name (ARN) of the IAM service role for Amazon Web
-- Services Systems Manager to assume when running a maintenance window
-- task. If you do not specify a service role ARN, Systems Manager uses
-- your account\'s service-linked role. If no service-linked role for
-- Systems Manager exists in your account, it is created when you run
-- @RegisterTaskWithMaintenanceWindow@.
--
-- For more information, see the following topics in the in the /Amazon Web
-- Services Systems Manager User Guide/:
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/using-service-linked-roles.html#slr-permissions Using service-linked roles for Systems Manager>
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-maintenance-permissions.html#maintenance-window-tasks-service-role Should I use a service-linked role or a custom service role to run maintenance window tasks?>
--
-- 'taskInvocationParameters', 'updateMaintenanceWindowTask_taskInvocationParameters' - The parameters that the task should use during execution. Populate only
-- the fields that match the task type. All other fields should be empty.
--
-- When you update a maintenance window task that has options specified in
-- @TaskInvocationParameters@, you must provide again all the
-- @TaskInvocationParameters@ values that you want to retain. The values
-- you don\'t specify again are removed. For example, suppose that when you
-- registered a Run Command task, you specified @TaskInvocationParameters@
-- values for @Comment@, @NotificationConfig@, and @OutputS3BucketName@. If
-- you update the maintenance window task and specify only a different
-- @OutputS3BucketName@ value, the values for @Comment@ and
-- @NotificationConfig@ are removed.
--
-- 'taskArn', 'updateMaintenanceWindowTask_taskArn' - The task ARN to modify.
--
-- 'targets', 'updateMaintenanceWindowTask_targets' - The targets (either managed nodes or tags) to modify. Managed nodes are
-- specified using the format
-- @Key=instanceids,Values=instanceID_1,instanceID_2@. Tags are specified
-- using the format @ Key=tag_name,Values=tag_value@.
--
-- One or more targets must be specified for maintenance window Run
-- Command-type tasks. Depending on the task, targets are optional for
-- other maintenance window task types (Automation, Lambda, and Step
-- Functions). For more information about running tasks that don\'t specify
-- targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html Registering maintenance window tasks without targets>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'description', 'updateMaintenanceWindowTask_description' - The new task description to specify.
--
-- 'alarmConfiguration', 'updateMaintenanceWindowTask_alarmConfiguration' - The CloudWatch alarm you want to apply to your maintenance window task.
--
-- 'priority', 'updateMaintenanceWindowTask_priority' - The new task priority to specify. The lower the number, the higher the
-- priority. Tasks that have the same priority are scheduled in parallel.
--
-- 'maxConcurrency', 'updateMaintenanceWindowTask_maxConcurrency' - The new @MaxConcurrency@ value you want to specify. @MaxConcurrency@ is
-- the number of targets that are allowed to run this task, in parallel.
--
-- Although this element is listed as \"Required: No\", a value can be
-- omitted only when you are registering or updating a
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html targetless task>
-- You must provide a value in all other cases.
--
-- For maintenance window tasks without a target specified, you can\'t
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@. This value doesn\'t affect the running of your
-- task.
--
-- 'maxErrors', 'updateMaintenanceWindowTask_maxErrors' - The new @MaxErrors@ value to specify. @MaxErrors@ is the maximum number
-- of errors that are allowed before the task stops being scheduled.
--
-- Although this element is listed as \"Required: No\", a value can be
-- omitted only when you are registering or updating a
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html targetless task>
-- You must provide a value in all other cases.
--
-- For maintenance window tasks without a target specified, you can\'t
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@. This value doesn\'t affect the running of your
-- task.
--
-- 'loggingInfo', 'updateMaintenanceWindowTask_loggingInfo' - The new logging location in Amazon S3 to specify.
--
-- @LoggingInfo@ has been deprecated. To specify an Amazon Simple Storage
-- Service (Amazon S3) bucket to contain logs, instead use the
-- @OutputS3BucketName@ and @OutputS3KeyPrefix@ options in the
-- @TaskInvocationParameters@ structure. For information about how Amazon
-- Web Services Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
--
-- 'replace', 'updateMaintenanceWindowTask_replace' - If True, then all fields that are required by the
-- RegisterTaskWithMaintenanceWindow operation are also required for this
-- API request. Optional fields that aren\'t specified are set to null.
--
-- 'cutoffBehavior', 'updateMaintenanceWindowTask_cutoffBehavior' - Indicates whether tasks should continue to run after the cutoff time
-- specified in the maintenance windows is reached.
--
-- -   @CONTINUE_TASK@: When the cutoff time is reached, any tasks that are
--     running continue. The default value.
--
-- -   @CANCEL_TASK@:
--
--     -   For Automation, Lambda, Step Functions tasks: When the cutoff
--         time is reached, any task invocations that are already running
--         continue, but no new task invocations are started.
--
--     -   For Run Command tasks: When the cutoff time is reached, the
--         system sends a CancelCommand operation that attempts to cancel
--         the command associated with the task. However, there is no
--         guarantee that the command will be terminated and the underlying
--         process stopped.
--
--     The status for tasks that are not completed is @TIMED_OUT@.
--
-- 'windowId', 'updateMaintenanceWindowTask_windowId' - The maintenance window ID that contains the task to modify.
--
-- 'windowTaskId', 'updateMaintenanceWindowTask_windowTaskId' - The task ID to modify.
newUpdateMaintenanceWindowTask ::
  -- | 'windowId'
  Prelude.Text ->
  -- | 'windowTaskId'
  Prelude.Text ->
  UpdateMaintenanceWindowTask
newUpdateMaintenanceWindowTask
  pWindowId_
  pWindowTaskId_ =
    UpdateMaintenanceWindowTask'
      { name =
          Prelude.Nothing,
        taskParameters = Prelude.Nothing,
        serviceRoleArn = Prelude.Nothing,
        taskInvocationParameters = Prelude.Nothing,
        taskArn = Prelude.Nothing,
        targets = Prelude.Nothing,
        description = Prelude.Nothing,
        alarmConfiguration = Prelude.Nothing,
        priority = Prelude.Nothing,
        maxConcurrency = Prelude.Nothing,
        maxErrors = Prelude.Nothing,
        loggingInfo = Prelude.Nothing,
        replace = Prelude.Nothing,
        cutoffBehavior = Prelude.Nothing,
        windowId = pWindowId_,
        windowTaskId = pWindowTaskId_
      }

-- | The new task name to specify.
updateMaintenanceWindowTask_name :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTask_name = Lens.lens (\UpdateMaintenanceWindowTask' {name} -> name) (\s@UpdateMaintenanceWindowTask' {} a -> s {name = a} :: UpdateMaintenanceWindowTask)

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
updateMaintenanceWindowTask_taskParameters :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe (Prelude.HashMap Prelude.Text MaintenanceWindowTaskParameterValueExpression))
updateMaintenanceWindowTask_taskParameters = Lens.lens (\UpdateMaintenanceWindowTask' {taskParameters} -> taskParameters) (\s@UpdateMaintenanceWindowTask' {} a -> s {taskParameters = a} :: UpdateMaintenanceWindowTask) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The Amazon Resource Name (ARN) of the IAM service role for Amazon Web
-- Services Systems Manager to assume when running a maintenance window
-- task. If you do not specify a service role ARN, Systems Manager uses
-- your account\'s service-linked role. If no service-linked role for
-- Systems Manager exists in your account, it is created when you run
-- @RegisterTaskWithMaintenanceWindow@.
--
-- For more information, see the following topics in the in the /Amazon Web
-- Services Systems Manager User Guide/:
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/using-service-linked-roles.html#slr-permissions Using service-linked roles for Systems Manager>
--
-- -   <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-maintenance-permissions.html#maintenance-window-tasks-service-role Should I use a service-linked role or a custom service role to run maintenance window tasks?>
updateMaintenanceWindowTask_serviceRoleArn :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTask_serviceRoleArn = Lens.lens (\UpdateMaintenanceWindowTask' {serviceRoleArn} -> serviceRoleArn) (\s@UpdateMaintenanceWindowTask' {} a -> s {serviceRoleArn = a} :: UpdateMaintenanceWindowTask)

-- | The parameters that the task should use during execution. Populate only
-- the fields that match the task type. All other fields should be empty.
--
-- When you update a maintenance window task that has options specified in
-- @TaskInvocationParameters@, you must provide again all the
-- @TaskInvocationParameters@ values that you want to retain. The values
-- you don\'t specify again are removed. For example, suppose that when you
-- registered a Run Command task, you specified @TaskInvocationParameters@
-- values for @Comment@, @NotificationConfig@, and @OutputS3BucketName@. If
-- you update the maintenance window task and specify only a different
-- @OutputS3BucketName@ value, the values for @Comment@ and
-- @NotificationConfig@ are removed.
updateMaintenanceWindowTask_taskInvocationParameters :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe MaintenanceWindowTaskInvocationParameters)
updateMaintenanceWindowTask_taskInvocationParameters = Lens.lens (\UpdateMaintenanceWindowTask' {taskInvocationParameters} -> taskInvocationParameters) (\s@UpdateMaintenanceWindowTask' {} a -> s {taskInvocationParameters = a} :: UpdateMaintenanceWindowTask)

-- | The task ARN to modify.
updateMaintenanceWindowTask_taskArn :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTask_taskArn = Lens.lens (\UpdateMaintenanceWindowTask' {taskArn} -> taskArn) (\s@UpdateMaintenanceWindowTask' {} a -> s {taskArn = a} :: UpdateMaintenanceWindowTask)

-- | The targets (either managed nodes or tags) to modify. Managed nodes are
-- specified using the format
-- @Key=instanceids,Values=instanceID_1,instanceID_2@. Tags are specified
-- using the format @ Key=tag_name,Values=tag_value@.
--
-- One or more targets must be specified for maintenance window Run
-- Command-type tasks. Depending on the task, targets are optional for
-- other maintenance window task types (Automation, Lambda, and Step
-- Functions). For more information about running tasks that don\'t specify
-- targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html Registering maintenance window tasks without targets>
-- in the /Amazon Web Services Systems Manager User Guide/.
updateMaintenanceWindowTask_targets :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe [Target])
updateMaintenanceWindowTask_targets = Lens.lens (\UpdateMaintenanceWindowTask' {targets} -> targets) (\s@UpdateMaintenanceWindowTask' {} a -> s {targets = a} :: UpdateMaintenanceWindowTask) Prelude.. Lens.mapping Lens.coerced

-- | The new task description to specify.
updateMaintenanceWindowTask_description :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTask_description = Lens.lens (\UpdateMaintenanceWindowTask' {description} -> description) (\s@UpdateMaintenanceWindowTask' {} a -> s {description = a} :: UpdateMaintenanceWindowTask) Prelude.. Lens.mapping Core._Sensitive

-- | The CloudWatch alarm you want to apply to your maintenance window task.
updateMaintenanceWindowTask_alarmConfiguration :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe AlarmConfiguration)
updateMaintenanceWindowTask_alarmConfiguration = Lens.lens (\UpdateMaintenanceWindowTask' {alarmConfiguration} -> alarmConfiguration) (\s@UpdateMaintenanceWindowTask' {} a -> s {alarmConfiguration = a} :: UpdateMaintenanceWindowTask)

-- | The new task priority to specify. The lower the number, the higher the
-- priority. Tasks that have the same priority are scheduled in parallel.
updateMaintenanceWindowTask_priority :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe Prelude.Natural)
updateMaintenanceWindowTask_priority = Lens.lens (\UpdateMaintenanceWindowTask' {priority} -> priority) (\s@UpdateMaintenanceWindowTask' {} a -> s {priority = a} :: UpdateMaintenanceWindowTask)

-- | The new @MaxConcurrency@ value you want to specify. @MaxConcurrency@ is
-- the number of targets that are allowed to run this task, in parallel.
--
-- Although this element is listed as \"Required: No\", a value can be
-- omitted only when you are registering or updating a
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html targetless task>
-- You must provide a value in all other cases.
--
-- For maintenance window tasks without a target specified, you can\'t
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@. This value doesn\'t affect the running of your
-- task.
updateMaintenanceWindowTask_maxConcurrency :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTask_maxConcurrency = Lens.lens (\UpdateMaintenanceWindowTask' {maxConcurrency} -> maxConcurrency) (\s@UpdateMaintenanceWindowTask' {} a -> s {maxConcurrency = a} :: UpdateMaintenanceWindowTask)

-- | The new @MaxErrors@ value to specify. @MaxErrors@ is the maximum number
-- of errors that are allowed before the task stops being scheduled.
--
-- Although this element is listed as \"Required: No\", a value can be
-- omitted only when you are registering or updating a
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html targetless task>
-- You must provide a value in all other cases.
--
-- For maintenance window tasks without a target specified, you can\'t
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@. This value doesn\'t affect the running of your
-- task.
updateMaintenanceWindowTask_maxErrors :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTask_maxErrors = Lens.lens (\UpdateMaintenanceWindowTask' {maxErrors} -> maxErrors) (\s@UpdateMaintenanceWindowTask' {} a -> s {maxErrors = a} :: UpdateMaintenanceWindowTask)

-- | The new logging location in Amazon S3 to specify.
--
-- @LoggingInfo@ has been deprecated. To specify an Amazon Simple Storage
-- Service (Amazon S3) bucket to contain logs, instead use the
-- @OutputS3BucketName@ and @OutputS3KeyPrefix@ options in the
-- @TaskInvocationParameters@ structure. For information about how Amazon
-- Web Services Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
updateMaintenanceWindowTask_loggingInfo :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe LoggingInfo)
updateMaintenanceWindowTask_loggingInfo = Lens.lens (\UpdateMaintenanceWindowTask' {loggingInfo} -> loggingInfo) (\s@UpdateMaintenanceWindowTask' {} a -> s {loggingInfo = a} :: UpdateMaintenanceWindowTask)

-- | If True, then all fields that are required by the
-- RegisterTaskWithMaintenanceWindow operation are also required for this
-- API request. Optional fields that aren\'t specified are set to null.
updateMaintenanceWindowTask_replace :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe Prelude.Bool)
updateMaintenanceWindowTask_replace = Lens.lens (\UpdateMaintenanceWindowTask' {replace} -> replace) (\s@UpdateMaintenanceWindowTask' {} a -> s {replace = a} :: UpdateMaintenanceWindowTask)

-- | Indicates whether tasks should continue to run after the cutoff time
-- specified in the maintenance windows is reached.
--
-- -   @CONTINUE_TASK@: When the cutoff time is reached, any tasks that are
--     running continue. The default value.
--
-- -   @CANCEL_TASK@:
--
--     -   For Automation, Lambda, Step Functions tasks: When the cutoff
--         time is reached, any task invocations that are already running
--         continue, but no new task invocations are started.
--
--     -   For Run Command tasks: When the cutoff time is reached, the
--         system sends a CancelCommand operation that attempts to cancel
--         the command associated with the task. However, there is no
--         guarantee that the command will be terminated and the underlying
--         process stopped.
--
--     The status for tasks that are not completed is @TIMED_OUT@.
updateMaintenanceWindowTask_cutoffBehavior :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe MaintenanceWindowTaskCutoffBehavior)
updateMaintenanceWindowTask_cutoffBehavior = Lens.lens (\UpdateMaintenanceWindowTask' {cutoffBehavior} -> cutoffBehavior) (\s@UpdateMaintenanceWindowTask' {} a -> s {cutoffBehavior = a} :: UpdateMaintenanceWindowTask)

-- | The maintenance window ID that contains the task to modify.
updateMaintenanceWindowTask_windowId :: Lens.Lens' UpdateMaintenanceWindowTask Prelude.Text
updateMaintenanceWindowTask_windowId = Lens.lens (\UpdateMaintenanceWindowTask' {windowId} -> windowId) (\s@UpdateMaintenanceWindowTask' {} a -> s {windowId = a} :: UpdateMaintenanceWindowTask)

-- | The task ID to modify.
updateMaintenanceWindowTask_windowTaskId :: Lens.Lens' UpdateMaintenanceWindowTask Prelude.Text
updateMaintenanceWindowTask_windowTaskId = Lens.lens (\UpdateMaintenanceWindowTask' {windowTaskId} -> windowTaskId) (\s@UpdateMaintenanceWindowTask' {} a -> s {windowTaskId = a} :: UpdateMaintenanceWindowTask)

instance Core.AWSRequest UpdateMaintenanceWindowTask where
  type
    AWSResponse UpdateMaintenanceWindowTask =
      UpdateMaintenanceWindowTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMaintenanceWindowTaskResponse'
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "TaskParameters" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "ServiceRoleArn")
            Prelude.<*> (x Core..?> "TaskInvocationParameters")
            Prelude.<*> (x Core..?> "WindowTaskId")
            Prelude.<*> (x Core..?> "TaskArn")
            Prelude.<*> (x Core..?> "WindowId")
            Prelude.<*> (x Core..?> "Targets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "AlarmConfiguration")
            Prelude.<*> (x Core..?> "Priority")
            Prelude.<*> (x Core..?> "MaxConcurrency")
            Prelude.<*> (x Core..?> "MaxErrors")
            Prelude.<*> (x Core..?> "LoggingInfo")
            Prelude.<*> (x Core..?> "CutoffBehavior")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMaintenanceWindowTask where
  hashWithSalt _salt UpdateMaintenanceWindowTask' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` taskParameters
      `Prelude.hashWithSalt` serviceRoleArn
      `Prelude.hashWithSalt` taskInvocationParameters
      `Prelude.hashWithSalt` taskArn
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` alarmConfiguration
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` maxConcurrency
      `Prelude.hashWithSalt` maxErrors
      `Prelude.hashWithSalt` loggingInfo
      `Prelude.hashWithSalt` replace
      `Prelude.hashWithSalt` cutoffBehavior
      `Prelude.hashWithSalt` windowId
      `Prelude.hashWithSalt` windowTaskId

instance Prelude.NFData UpdateMaintenanceWindowTask where
  rnf UpdateMaintenanceWindowTask' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf taskParameters
      `Prelude.seq` Prelude.rnf serviceRoleArn
      `Prelude.seq` Prelude.rnf taskInvocationParameters
      `Prelude.seq` Prelude.rnf taskArn
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf alarmConfiguration
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf maxConcurrency
      `Prelude.seq` Prelude.rnf maxErrors
      `Prelude.seq` Prelude.rnf loggingInfo
      `Prelude.seq` Prelude.rnf replace
      `Prelude.seq` Prelude.rnf cutoffBehavior
      `Prelude.seq` Prelude.rnf windowId
      `Prelude.seq` Prelude.rnf windowTaskId

instance Core.ToHeaders UpdateMaintenanceWindowTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.UpdateMaintenanceWindowTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateMaintenanceWindowTask where
  toJSON UpdateMaintenanceWindowTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("TaskParameters" Core..=)
              Prelude.<$> taskParameters,
            ("ServiceRoleArn" Core..=)
              Prelude.<$> serviceRoleArn,
            ("TaskInvocationParameters" Core..=)
              Prelude.<$> taskInvocationParameters,
            ("TaskArn" Core..=) Prelude.<$> taskArn,
            ("Targets" Core..=) Prelude.<$> targets,
            ("Description" Core..=) Prelude.<$> description,
            ("AlarmConfiguration" Core..=)
              Prelude.<$> alarmConfiguration,
            ("Priority" Core..=) Prelude.<$> priority,
            ("MaxConcurrency" Core..=)
              Prelude.<$> maxConcurrency,
            ("MaxErrors" Core..=) Prelude.<$> maxErrors,
            ("LoggingInfo" Core..=) Prelude.<$> loggingInfo,
            ("Replace" Core..=) Prelude.<$> replace,
            ("CutoffBehavior" Core..=)
              Prelude.<$> cutoffBehavior,
            Prelude.Just ("WindowId" Core..= windowId),
            Prelude.Just ("WindowTaskId" Core..= windowTaskId)
          ]
      )

instance Core.ToPath UpdateMaintenanceWindowTask where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateMaintenanceWindowTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMaintenanceWindowTaskResponse' smart constructor.
data UpdateMaintenanceWindowTaskResponse = UpdateMaintenanceWindowTaskResponse'
  { -- | The updated task name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The updated parameter values.
    --
    -- @TaskParameters@ has been deprecated. To specify parameters to pass to a
    -- task when it runs, instead use the @Parameters@ option in the
    -- @TaskInvocationParameters@ structure. For information about how Systems
    -- Manager handles these options for the supported maintenance window task
    -- types, see MaintenanceWindowTaskInvocationParameters.
    taskParameters :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text (Core.Sensitive MaintenanceWindowTaskParameterValueExpression))),
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) service role to use to publish Amazon Simple Notification Service
    -- (Amazon SNS) notifications for maintenance window Run Command tasks.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The updated parameter values.
    taskInvocationParameters :: Prelude.Maybe MaintenanceWindowTaskInvocationParameters,
    -- | The task ID of the maintenance window that was updated.
    windowTaskId :: Prelude.Maybe Prelude.Text,
    -- | The updated task ARN value.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window that was updated.
    windowId :: Prelude.Maybe Prelude.Text,
    -- | The updated target values.
    targets :: Prelude.Maybe [Target],
    -- | The updated task description.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The details for the CloudWatch alarm you applied to your maintenance
    -- window task.
    alarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | The updated priority value.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The updated @MaxConcurrency@ value.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The updated @MaxErrors@ value.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | The updated logging information in Amazon S3.
    --
    -- @LoggingInfo@ has been deprecated. To specify an Amazon Simple Storage
    -- Service (Amazon S3) bucket to contain logs, instead use the
    -- @OutputS3BucketName@ and @OutputS3KeyPrefix@ options in the
    -- @TaskInvocationParameters@ structure. For information about how Amazon
    -- Web Services Systems Manager handles these options for the supported
    -- maintenance window task types, see
    -- MaintenanceWindowTaskInvocationParameters.
    loggingInfo :: Prelude.Maybe LoggingInfo,
    -- | The specification for whether tasks should continue to run after the
    -- cutoff time specified in the maintenance windows is reached.
    cutoffBehavior :: Prelude.Maybe MaintenanceWindowTaskCutoffBehavior,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMaintenanceWindowTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateMaintenanceWindowTaskResponse_name' - The updated task name.
--
-- 'taskParameters', 'updateMaintenanceWindowTaskResponse_taskParameters' - The updated parameter values.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
--
-- 'serviceRoleArn', 'updateMaintenanceWindowTaskResponse_serviceRoleArn' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) service role to use to publish Amazon Simple Notification Service
-- (Amazon SNS) notifications for maintenance window Run Command tasks.
--
-- 'taskInvocationParameters', 'updateMaintenanceWindowTaskResponse_taskInvocationParameters' - The updated parameter values.
--
-- 'windowTaskId', 'updateMaintenanceWindowTaskResponse_windowTaskId' - The task ID of the maintenance window that was updated.
--
-- 'taskArn', 'updateMaintenanceWindowTaskResponse_taskArn' - The updated task ARN value.
--
-- 'windowId', 'updateMaintenanceWindowTaskResponse_windowId' - The ID of the maintenance window that was updated.
--
-- 'targets', 'updateMaintenanceWindowTaskResponse_targets' - The updated target values.
--
-- 'description', 'updateMaintenanceWindowTaskResponse_description' - The updated task description.
--
-- 'alarmConfiguration', 'updateMaintenanceWindowTaskResponse_alarmConfiguration' - The details for the CloudWatch alarm you applied to your maintenance
-- window task.
--
-- 'priority', 'updateMaintenanceWindowTaskResponse_priority' - The updated priority value.
--
-- 'maxConcurrency', 'updateMaintenanceWindowTaskResponse_maxConcurrency' - The updated @MaxConcurrency@ value.
--
-- 'maxErrors', 'updateMaintenanceWindowTaskResponse_maxErrors' - The updated @MaxErrors@ value.
--
-- 'loggingInfo', 'updateMaintenanceWindowTaskResponse_loggingInfo' - The updated logging information in Amazon S3.
--
-- @LoggingInfo@ has been deprecated. To specify an Amazon Simple Storage
-- Service (Amazon S3) bucket to contain logs, instead use the
-- @OutputS3BucketName@ and @OutputS3KeyPrefix@ options in the
-- @TaskInvocationParameters@ structure. For information about how Amazon
-- Web Services Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
--
-- 'cutoffBehavior', 'updateMaintenanceWindowTaskResponse_cutoffBehavior' - The specification for whether tasks should continue to run after the
-- cutoff time specified in the maintenance windows is reached.
--
-- 'httpStatus', 'updateMaintenanceWindowTaskResponse_httpStatus' - The response's http status code.
newUpdateMaintenanceWindowTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMaintenanceWindowTaskResponse
newUpdateMaintenanceWindowTaskResponse pHttpStatus_ =
  UpdateMaintenanceWindowTaskResponse'
    { name =
        Prelude.Nothing,
      taskParameters = Prelude.Nothing,
      serviceRoleArn = Prelude.Nothing,
      taskInvocationParameters =
        Prelude.Nothing,
      windowTaskId = Prelude.Nothing,
      taskArn = Prelude.Nothing,
      windowId = Prelude.Nothing,
      targets = Prelude.Nothing,
      description = Prelude.Nothing,
      alarmConfiguration = Prelude.Nothing,
      priority = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      maxErrors = Prelude.Nothing,
      loggingInfo = Prelude.Nothing,
      cutoffBehavior = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated task name.
updateMaintenanceWindowTaskResponse_name :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTaskResponse_name = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {name} -> name) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {name = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated parameter values.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
updateMaintenanceWindowTaskResponse_taskParameters :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text MaintenanceWindowTaskParameterValueExpression))
updateMaintenanceWindowTaskResponse_taskParameters = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {taskParameters} -> taskParameters) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {taskParameters = a} :: UpdateMaintenanceWindowTaskResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) service role to use to publish Amazon Simple Notification Service
-- (Amazon SNS) notifications for maintenance window Run Command tasks.
updateMaintenanceWindowTaskResponse_serviceRoleArn :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTaskResponse_serviceRoleArn = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {serviceRoleArn} -> serviceRoleArn) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {serviceRoleArn = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated parameter values.
updateMaintenanceWindowTaskResponse_taskInvocationParameters :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe MaintenanceWindowTaskInvocationParameters)
updateMaintenanceWindowTaskResponse_taskInvocationParameters = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {taskInvocationParameters} -> taskInvocationParameters) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {taskInvocationParameters = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The task ID of the maintenance window that was updated.
updateMaintenanceWindowTaskResponse_windowTaskId :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTaskResponse_windowTaskId = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {windowTaskId} -> windowTaskId) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {windowTaskId = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated task ARN value.
updateMaintenanceWindowTaskResponse_taskArn :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTaskResponse_taskArn = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {taskArn} -> taskArn) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {taskArn = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The ID of the maintenance window that was updated.
updateMaintenanceWindowTaskResponse_windowId :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTaskResponse_windowId = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {windowId} -> windowId) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {windowId = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated target values.
updateMaintenanceWindowTaskResponse_targets :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe [Target])
updateMaintenanceWindowTaskResponse_targets = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {targets} -> targets) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {targets = a} :: UpdateMaintenanceWindowTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | The updated task description.
updateMaintenanceWindowTaskResponse_description :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTaskResponse_description = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {description} -> description) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {description = a} :: UpdateMaintenanceWindowTaskResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The details for the CloudWatch alarm you applied to your maintenance
-- window task.
updateMaintenanceWindowTaskResponse_alarmConfiguration :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe AlarmConfiguration)
updateMaintenanceWindowTaskResponse_alarmConfiguration = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {alarmConfiguration} -> alarmConfiguration) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {alarmConfiguration = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated priority value.
updateMaintenanceWindowTaskResponse_priority :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Natural)
updateMaintenanceWindowTaskResponse_priority = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {priority} -> priority) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {priority = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated @MaxConcurrency@ value.
updateMaintenanceWindowTaskResponse_maxConcurrency :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTaskResponse_maxConcurrency = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {maxConcurrency} -> maxConcurrency) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {maxConcurrency = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated @MaxErrors@ value.
updateMaintenanceWindowTaskResponse_maxErrors :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTaskResponse_maxErrors = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {maxErrors} -> maxErrors) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {maxErrors = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated logging information in Amazon S3.
--
-- @LoggingInfo@ has been deprecated. To specify an Amazon Simple Storage
-- Service (Amazon S3) bucket to contain logs, instead use the
-- @OutputS3BucketName@ and @OutputS3KeyPrefix@ options in the
-- @TaskInvocationParameters@ structure. For information about how Amazon
-- Web Services Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
updateMaintenanceWindowTaskResponse_loggingInfo :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe LoggingInfo)
updateMaintenanceWindowTaskResponse_loggingInfo = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {loggingInfo} -> loggingInfo) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {loggingInfo = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The specification for whether tasks should continue to run after the
-- cutoff time specified in the maintenance windows is reached.
updateMaintenanceWindowTaskResponse_cutoffBehavior :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe MaintenanceWindowTaskCutoffBehavior)
updateMaintenanceWindowTaskResponse_cutoffBehavior = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {cutoffBehavior} -> cutoffBehavior) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {cutoffBehavior = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The response's http status code.
updateMaintenanceWindowTaskResponse_httpStatus :: Lens.Lens' UpdateMaintenanceWindowTaskResponse Prelude.Int
updateMaintenanceWindowTaskResponse_httpStatus = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {httpStatus} -> httpStatus) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {httpStatus = a} :: UpdateMaintenanceWindowTaskResponse)

instance
  Prelude.NFData
    UpdateMaintenanceWindowTaskResponse
  where
  rnf UpdateMaintenanceWindowTaskResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf taskParameters
      `Prelude.seq` Prelude.rnf serviceRoleArn
      `Prelude.seq` Prelude.rnf taskInvocationParameters
      `Prelude.seq` Prelude.rnf windowTaskId
      `Prelude.seq` Prelude.rnf taskArn
      `Prelude.seq` Prelude.rnf windowId
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf alarmConfiguration
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf maxConcurrency
      `Prelude.seq` Prelude.rnf maxErrors
      `Prelude.seq` Prelude.rnf loggingInfo
      `Prelude.seq` Prelude.rnf cutoffBehavior
      `Prelude.seq` Prelude.rnf httpStatus
