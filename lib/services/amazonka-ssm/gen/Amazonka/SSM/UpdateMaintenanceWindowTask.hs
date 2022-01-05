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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    updateMaintenanceWindowTask_serviceRoleArn,
    updateMaintenanceWindowTask_replace,
    updateMaintenanceWindowTask_taskParameters,
    updateMaintenanceWindowTask_priority,
    updateMaintenanceWindowTask_taskArn,
    updateMaintenanceWindowTask_cutoffBehavior,
    updateMaintenanceWindowTask_maxErrors,
    updateMaintenanceWindowTask_taskInvocationParameters,
    updateMaintenanceWindowTask_name,
    updateMaintenanceWindowTask_targets,
    updateMaintenanceWindowTask_loggingInfo,
    updateMaintenanceWindowTask_description,
    updateMaintenanceWindowTask_maxConcurrency,
    updateMaintenanceWindowTask_windowId,
    updateMaintenanceWindowTask_windowTaskId,

    -- * Destructuring the Response
    UpdateMaintenanceWindowTaskResponse (..),
    newUpdateMaintenanceWindowTaskResponse,

    -- * Response Lenses
    updateMaintenanceWindowTaskResponse_serviceRoleArn,
    updateMaintenanceWindowTaskResponse_windowTaskId,
    updateMaintenanceWindowTaskResponse_taskParameters,
    updateMaintenanceWindowTaskResponse_priority,
    updateMaintenanceWindowTaskResponse_taskArn,
    updateMaintenanceWindowTaskResponse_cutoffBehavior,
    updateMaintenanceWindowTaskResponse_maxErrors,
    updateMaintenanceWindowTaskResponse_taskInvocationParameters,
    updateMaintenanceWindowTaskResponse_name,
    updateMaintenanceWindowTaskResponse_targets,
    updateMaintenanceWindowTaskResponse_loggingInfo,
    updateMaintenanceWindowTaskResponse_description,
    updateMaintenanceWindowTaskResponse_maxConcurrency,
    updateMaintenanceWindowTaskResponse_windowId,
    updateMaintenanceWindowTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newUpdateMaintenanceWindowTask' smart constructor.
data UpdateMaintenanceWindowTask = UpdateMaintenanceWindowTask'
  { -- | The Amazon Resource Name (ARN) of the IAM service role for Amazon Web
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
    -- | If True, then all fields that are required by the
    -- RegisterTaskWithMaintenanceWindow operation are also required for this
    -- API request. Optional fields that aren\'t specified are set to null.
    replace :: Prelude.Maybe Prelude.Bool,
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
    -- | The new task priority to specify. The lower the number, the higher the
    -- priority. Tasks that have the same priority are scheduled in parallel.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The task ARN to modify.
    taskArn :: Prelude.Maybe Prelude.Text,
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
    -- | The new @MaxErrors@ value to specify. @MaxErrors@ is the maximum number
    -- of errors that are allowed before the task stops being scheduled.
    --
    -- For maintenance window tasks without a target specified, you can\'t
    -- supply a value for this option. Instead, the system inserts a
    -- placeholder value of @1@, which may be reported in the response to this
    -- command. This value doesn\'t affect the running of your task and can be
    -- ignored.
    maxErrors :: Prelude.Maybe Prelude.Text,
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
    -- | The new task name to specify.
    name :: Prelude.Maybe Prelude.Text,
    -- | The targets (either instances or tags) to modify. Instances are
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
    -- | The new task description to specify.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The new @MaxConcurrency@ value you want to specify. @MaxConcurrency@ is
    -- the number of targets that are allowed to run this task in parallel.
    --
    -- For maintenance window tasks without a target specified, you can\'t
    -- supply a value for this option. Instead, the system inserts a
    -- placeholder value of @1@, which may be reported in the response to this
    -- command. This value doesn\'t affect the running of your task and can be
    -- ignored.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
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
-- 'replace', 'updateMaintenanceWindowTask_replace' - If True, then all fields that are required by the
-- RegisterTaskWithMaintenanceWindow operation are also required for this
-- API request. Optional fields that aren\'t specified are set to null.
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
-- 'priority', 'updateMaintenanceWindowTask_priority' - The new task priority to specify. The lower the number, the higher the
-- priority. Tasks that have the same priority are scheduled in parallel.
--
-- 'taskArn', 'updateMaintenanceWindowTask_taskArn' - The task ARN to modify.
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
-- 'maxErrors', 'updateMaintenanceWindowTask_maxErrors' - The new @MaxErrors@ value to specify. @MaxErrors@ is the maximum number
-- of errors that are allowed before the task stops being scheduled.
--
-- For maintenance window tasks without a target specified, you can\'t
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@, which may be reported in the response to this
-- command. This value doesn\'t affect the running of your task and can be
-- ignored.
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
-- 'name', 'updateMaintenanceWindowTask_name' - The new task name to specify.
--
-- 'targets', 'updateMaintenanceWindowTask_targets' - The targets (either instances or tags) to modify. Instances are
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
-- 'description', 'updateMaintenanceWindowTask_description' - The new task description to specify.
--
-- 'maxConcurrency', 'updateMaintenanceWindowTask_maxConcurrency' - The new @MaxConcurrency@ value you want to specify. @MaxConcurrency@ is
-- the number of targets that are allowed to run this task in parallel.
--
-- For maintenance window tasks without a target specified, you can\'t
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@, which may be reported in the response to this
-- command. This value doesn\'t affect the running of your task and can be
-- ignored.
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
      { serviceRoleArn =
          Prelude.Nothing,
        replace = Prelude.Nothing,
        taskParameters = Prelude.Nothing,
        priority = Prelude.Nothing,
        taskArn = Prelude.Nothing,
        cutoffBehavior = Prelude.Nothing,
        maxErrors = Prelude.Nothing,
        taskInvocationParameters = Prelude.Nothing,
        name = Prelude.Nothing,
        targets = Prelude.Nothing,
        loggingInfo = Prelude.Nothing,
        description = Prelude.Nothing,
        maxConcurrency = Prelude.Nothing,
        windowId = pWindowId_,
        windowTaskId = pWindowTaskId_
      }

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

-- | If True, then all fields that are required by the
-- RegisterTaskWithMaintenanceWindow operation are also required for this
-- API request. Optional fields that aren\'t specified are set to null.
updateMaintenanceWindowTask_replace :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe Prelude.Bool)
updateMaintenanceWindowTask_replace = Lens.lens (\UpdateMaintenanceWindowTask' {replace} -> replace) (\s@UpdateMaintenanceWindowTask' {} a -> s {replace = a} :: UpdateMaintenanceWindowTask)

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

-- | The new task priority to specify. The lower the number, the higher the
-- priority. Tasks that have the same priority are scheduled in parallel.
updateMaintenanceWindowTask_priority :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe Prelude.Natural)
updateMaintenanceWindowTask_priority = Lens.lens (\UpdateMaintenanceWindowTask' {priority} -> priority) (\s@UpdateMaintenanceWindowTask' {} a -> s {priority = a} :: UpdateMaintenanceWindowTask)

-- | The task ARN to modify.
updateMaintenanceWindowTask_taskArn :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTask_taskArn = Lens.lens (\UpdateMaintenanceWindowTask' {taskArn} -> taskArn) (\s@UpdateMaintenanceWindowTask' {} a -> s {taskArn = a} :: UpdateMaintenanceWindowTask)

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

-- | The new @MaxErrors@ value to specify. @MaxErrors@ is the maximum number
-- of errors that are allowed before the task stops being scheduled.
--
-- For maintenance window tasks without a target specified, you can\'t
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@, which may be reported in the response to this
-- command. This value doesn\'t affect the running of your task and can be
-- ignored.
updateMaintenanceWindowTask_maxErrors :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTask_maxErrors = Lens.lens (\UpdateMaintenanceWindowTask' {maxErrors} -> maxErrors) (\s@UpdateMaintenanceWindowTask' {} a -> s {maxErrors = a} :: UpdateMaintenanceWindowTask)

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

-- | The new task name to specify.
updateMaintenanceWindowTask_name :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTask_name = Lens.lens (\UpdateMaintenanceWindowTask' {name} -> name) (\s@UpdateMaintenanceWindowTask' {} a -> s {name = a} :: UpdateMaintenanceWindowTask)

-- | The targets (either instances or tags) to modify. Instances are
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

-- | The new task description to specify.
updateMaintenanceWindowTask_description :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTask_description = Lens.lens (\UpdateMaintenanceWindowTask' {description} -> description) (\s@UpdateMaintenanceWindowTask' {} a -> s {description = a} :: UpdateMaintenanceWindowTask) Prelude.. Lens.mapping Core._Sensitive

-- | The new @MaxConcurrency@ value you want to specify. @MaxConcurrency@ is
-- the number of targets that are allowed to run this task in parallel.
--
-- For maintenance window tasks without a target specified, you can\'t
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@, which may be reported in the response to this
-- command. This value doesn\'t affect the running of your task and can be
-- ignored.
updateMaintenanceWindowTask_maxConcurrency :: Lens.Lens' UpdateMaintenanceWindowTask (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTask_maxConcurrency = Lens.lens (\UpdateMaintenanceWindowTask' {maxConcurrency} -> maxConcurrency) (\s@UpdateMaintenanceWindowTask' {} a -> s {maxConcurrency = a} :: UpdateMaintenanceWindowTask)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMaintenanceWindowTaskResponse'
            Prelude.<$> (x Core..?> "ServiceRoleArn")
            Prelude.<*> (x Core..?> "WindowTaskId")
            Prelude.<*> (x Core..?> "TaskParameters" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Priority")
            Prelude.<*> (x Core..?> "TaskArn")
            Prelude.<*> (x Core..?> "CutoffBehavior")
            Prelude.<*> (x Core..?> "MaxErrors")
            Prelude.<*> (x Core..?> "TaskInvocationParameters")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Targets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "LoggingInfo")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "MaxConcurrency")
            Prelude.<*> (x Core..?> "WindowId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMaintenanceWindowTask where
  hashWithSalt _salt UpdateMaintenanceWindowTask' {..} =
    _salt `Prelude.hashWithSalt` serviceRoleArn
      `Prelude.hashWithSalt` replace
      `Prelude.hashWithSalt` taskParameters
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` taskArn
      `Prelude.hashWithSalt` cutoffBehavior
      `Prelude.hashWithSalt` maxErrors
      `Prelude.hashWithSalt` taskInvocationParameters
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` loggingInfo
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` maxConcurrency
      `Prelude.hashWithSalt` windowId
      `Prelude.hashWithSalt` windowTaskId

instance Prelude.NFData UpdateMaintenanceWindowTask where
  rnf UpdateMaintenanceWindowTask' {..} =
    Prelude.rnf serviceRoleArn
      `Prelude.seq` Prelude.rnf replace
      `Prelude.seq` Prelude.rnf taskParameters
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf taskArn
      `Prelude.seq` Prelude.rnf cutoffBehavior
      `Prelude.seq` Prelude.rnf maxErrors
      `Prelude.seq` Prelude.rnf taskInvocationParameters
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf loggingInfo
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf maxConcurrency
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
          [ ("ServiceRoleArn" Core..=)
              Prelude.<$> serviceRoleArn,
            ("Replace" Core..=) Prelude.<$> replace,
            ("TaskParameters" Core..=)
              Prelude.<$> taskParameters,
            ("Priority" Core..=) Prelude.<$> priority,
            ("TaskArn" Core..=) Prelude.<$> taskArn,
            ("CutoffBehavior" Core..=)
              Prelude.<$> cutoffBehavior,
            ("MaxErrors" Core..=) Prelude.<$> maxErrors,
            ("TaskInvocationParameters" Core..=)
              Prelude.<$> taskInvocationParameters,
            ("Name" Core..=) Prelude.<$> name,
            ("Targets" Core..=) Prelude.<$> targets,
            ("LoggingInfo" Core..=) Prelude.<$> loggingInfo,
            ("Description" Core..=) Prelude.<$> description,
            ("MaxConcurrency" Core..=)
              Prelude.<$> maxConcurrency,
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
  { -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) service role to use to publish Amazon Simple Notification Service
    -- (Amazon SNS) notifications for maintenance window Run Command tasks.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The task ID of the maintenance window that was updated.
    windowTaskId :: Prelude.Maybe Prelude.Text,
    -- | The updated parameter values.
    --
    -- @TaskParameters@ has been deprecated. To specify parameters to pass to a
    -- task when it runs, instead use the @Parameters@ option in the
    -- @TaskInvocationParameters@ structure. For information about how Systems
    -- Manager handles these options for the supported maintenance window task
    -- types, see MaintenanceWindowTaskInvocationParameters.
    taskParameters :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text (Core.Sensitive MaintenanceWindowTaskParameterValueExpression))),
    -- | The updated priority value.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The updated task ARN value.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The specification for whether tasks should continue to run after the
    -- cutoff time specified in the maintenance windows is reached.
    cutoffBehavior :: Prelude.Maybe MaintenanceWindowTaskCutoffBehavior,
    -- | The updated @MaxErrors@ value.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | The updated parameter values.
    taskInvocationParameters :: Prelude.Maybe MaintenanceWindowTaskInvocationParameters,
    -- | The updated task name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The updated target values.
    targets :: Prelude.Maybe [Target],
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
    -- | The updated task description.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The updated @MaxConcurrency@ value.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window that was updated.
    windowId :: Prelude.Maybe Prelude.Text,
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
-- 'serviceRoleArn', 'updateMaintenanceWindowTaskResponse_serviceRoleArn' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) service role to use to publish Amazon Simple Notification Service
-- (Amazon SNS) notifications for maintenance window Run Command tasks.
--
-- 'windowTaskId', 'updateMaintenanceWindowTaskResponse_windowTaskId' - The task ID of the maintenance window that was updated.
--
-- 'taskParameters', 'updateMaintenanceWindowTaskResponse_taskParameters' - The updated parameter values.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
--
-- 'priority', 'updateMaintenanceWindowTaskResponse_priority' - The updated priority value.
--
-- 'taskArn', 'updateMaintenanceWindowTaskResponse_taskArn' - The updated task ARN value.
--
-- 'cutoffBehavior', 'updateMaintenanceWindowTaskResponse_cutoffBehavior' - The specification for whether tasks should continue to run after the
-- cutoff time specified in the maintenance windows is reached.
--
-- 'maxErrors', 'updateMaintenanceWindowTaskResponse_maxErrors' - The updated @MaxErrors@ value.
--
-- 'taskInvocationParameters', 'updateMaintenanceWindowTaskResponse_taskInvocationParameters' - The updated parameter values.
--
-- 'name', 'updateMaintenanceWindowTaskResponse_name' - The updated task name.
--
-- 'targets', 'updateMaintenanceWindowTaskResponse_targets' - The updated target values.
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
-- 'description', 'updateMaintenanceWindowTaskResponse_description' - The updated task description.
--
-- 'maxConcurrency', 'updateMaintenanceWindowTaskResponse_maxConcurrency' - The updated @MaxConcurrency@ value.
--
-- 'windowId', 'updateMaintenanceWindowTaskResponse_windowId' - The ID of the maintenance window that was updated.
--
-- 'httpStatus', 'updateMaintenanceWindowTaskResponse_httpStatus' - The response's http status code.
newUpdateMaintenanceWindowTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMaintenanceWindowTaskResponse
newUpdateMaintenanceWindowTaskResponse pHttpStatus_ =
  UpdateMaintenanceWindowTaskResponse'
    { serviceRoleArn =
        Prelude.Nothing,
      windowTaskId = Prelude.Nothing,
      taskParameters = Prelude.Nothing,
      priority = Prelude.Nothing,
      taskArn = Prelude.Nothing,
      cutoffBehavior = Prelude.Nothing,
      maxErrors = Prelude.Nothing,
      taskInvocationParameters =
        Prelude.Nothing,
      name = Prelude.Nothing,
      targets = Prelude.Nothing,
      loggingInfo = Prelude.Nothing,
      description = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      windowId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) service role to use to publish Amazon Simple Notification Service
-- (Amazon SNS) notifications for maintenance window Run Command tasks.
updateMaintenanceWindowTaskResponse_serviceRoleArn :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTaskResponse_serviceRoleArn = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {serviceRoleArn} -> serviceRoleArn) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {serviceRoleArn = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The task ID of the maintenance window that was updated.
updateMaintenanceWindowTaskResponse_windowTaskId :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTaskResponse_windowTaskId = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {windowTaskId} -> windowTaskId) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {windowTaskId = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated parameter values.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
updateMaintenanceWindowTaskResponse_taskParameters :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text MaintenanceWindowTaskParameterValueExpression))
updateMaintenanceWindowTaskResponse_taskParameters = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {taskParameters} -> taskParameters) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {taskParameters = a} :: UpdateMaintenanceWindowTaskResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The updated priority value.
updateMaintenanceWindowTaskResponse_priority :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Natural)
updateMaintenanceWindowTaskResponse_priority = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {priority} -> priority) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {priority = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated task ARN value.
updateMaintenanceWindowTaskResponse_taskArn :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTaskResponse_taskArn = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {taskArn} -> taskArn) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {taskArn = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The specification for whether tasks should continue to run after the
-- cutoff time specified in the maintenance windows is reached.
updateMaintenanceWindowTaskResponse_cutoffBehavior :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe MaintenanceWindowTaskCutoffBehavior)
updateMaintenanceWindowTaskResponse_cutoffBehavior = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {cutoffBehavior} -> cutoffBehavior) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {cutoffBehavior = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated @MaxErrors@ value.
updateMaintenanceWindowTaskResponse_maxErrors :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTaskResponse_maxErrors = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {maxErrors} -> maxErrors) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {maxErrors = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated parameter values.
updateMaintenanceWindowTaskResponse_taskInvocationParameters :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe MaintenanceWindowTaskInvocationParameters)
updateMaintenanceWindowTaskResponse_taskInvocationParameters = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {taskInvocationParameters} -> taskInvocationParameters) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {taskInvocationParameters = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated task name.
updateMaintenanceWindowTaskResponse_name :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTaskResponse_name = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {name} -> name) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {name = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The updated target values.
updateMaintenanceWindowTaskResponse_targets :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe [Target])
updateMaintenanceWindowTaskResponse_targets = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {targets} -> targets) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {targets = a} :: UpdateMaintenanceWindowTaskResponse) Prelude.. Lens.mapping Lens.coerced

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

-- | The updated task description.
updateMaintenanceWindowTaskResponse_description :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTaskResponse_description = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {description} -> description) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {description = a} :: UpdateMaintenanceWindowTaskResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The updated @MaxConcurrency@ value.
updateMaintenanceWindowTaskResponse_maxConcurrency :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTaskResponse_maxConcurrency = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {maxConcurrency} -> maxConcurrency) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {maxConcurrency = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The ID of the maintenance window that was updated.
updateMaintenanceWindowTaskResponse_windowId :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTaskResponse_windowId = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {windowId} -> windowId) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {windowId = a} :: UpdateMaintenanceWindowTaskResponse)

-- | The response's http status code.
updateMaintenanceWindowTaskResponse_httpStatus :: Lens.Lens' UpdateMaintenanceWindowTaskResponse Prelude.Int
updateMaintenanceWindowTaskResponse_httpStatus = Lens.lens (\UpdateMaintenanceWindowTaskResponse' {httpStatus} -> httpStatus) (\s@UpdateMaintenanceWindowTaskResponse' {} a -> s {httpStatus = a} :: UpdateMaintenanceWindowTaskResponse)

instance
  Prelude.NFData
    UpdateMaintenanceWindowTaskResponse
  where
  rnf UpdateMaintenanceWindowTaskResponse' {..} =
    Prelude.rnf serviceRoleArn
      `Prelude.seq` Prelude.rnf windowTaskId
      `Prelude.seq` Prelude.rnf taskParameters
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf taskArn
      `Prelude.seq` Prelude.rnf cutoffBehavior
      `Prelude.seq` Prelude.rnf maxErrors
      `Prelude.seq` Prelude.rnf taskInvocationParameters
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf loggingInfo
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf maxConcurrency
      `Prelude.seq` Prelude.rnf windowId
      `Prelude.seq` Prelude.rnf httpStatus
