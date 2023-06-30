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
-- Module      : Amazonka.SSM.RegisterTaskWithMaintenanceWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new task to a maintenance window.
module Amazonka.SSM.RegisterTaskWithMaintenanceWindow
  ( -- * Creating a Request
    RegisterTaskWithMaintenanceWindow (..),
    newRegisterTaskWithMaintenanceWindow,

    -- * Request Lenses
    registerTaskWithMaintenanceWindow_alarmConfiguration,
    registerTaskWithMaintenanceWindow_clientToken,
    registerTaskWithMaintenanceWindow_cutoffBehavior,
    registerTaskWithMaintenanceWindow_description,
    registerTaskWithMaintenanceWindow_loggingInfo,
    registerTaskWithMaintenanceWindow_maxConcurrency,
    registerTaskWithMaintenanceWindow_maxErrors,
    registerTaskWithMaintenanceWindow_name,
    registerTaskWithMaintenanceWindow_priority,
    registerTaskWithMaintenanceWindow_serviceRoleArn,
    registerTaskWithMaintenanceWindow_targets,
    registerTaskWithMaintenanceWindow_taskInvocationParameters,
    registerTaskWithMaintenanceWindow_taskParameters,
    registerTaskWithMaintenanceWindow_windowId,
    registerTaskWithMaintenanceWindow_taskArn,
    registerTaskWithMaintenanceWindow_taskType,

    -- * Destructuring the Response
    RegisterTaskWithMaintenanceWindowResponse (..),
    newRegisterTaskWithMaintenanceWindowResponse,

    -- * Response Lenses
    registerTaskWithMaintenanceWindowResponse_windowTaskId,
    registerTaskWithMaintenanceWindowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newRegisterTaskWithMaintenanceWindow' smart constructor.
data RegisterTaskWithMaintenanceWindow = RegisterTaskWithMaintenanceWindow'
  { -- | The CloudWatch alarm you want to apply to your maintenance window task.
    alarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | User-provided idempotency token.
    clientToken :: Prelude.Maybe Prelude.Text,
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
    -- | An optional description for the task.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A structure containing information about an Amazon Simple Storage
    -- Service (Amazon S3) bucket to write managed node-level logs to.
    --
    -- @LoggingInfo@ has been deprecated. To specify an Amazon Simple Storage
    -- Service (Amazon S3) bucket to contain logs, instead use the
    -- @OutputS3BucketName@ and @OutputS3KeyPrefix@ options in the
    -- @TaskInvocationParameters@ structure. For information about how Amazon
    -- Web Services Systems Manager handles these options for the supported
    -- maintenance window task types, see
    -- MaintenanceWindowTaskInvocationParameters.
    loggingInfo :: Prelude.Maybe LoggingInfo,
    -- | The maximum number of targets this task can be run for, in parallel.
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
    -- | The maximum number of errors allowed before this task stops being
    -- scheduled.
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
    -- | An optional name for the task.
    name :: Prelude.Maybe Prelude.Text,
    -- | The priority of the task in the maintenance window, the lower the number
    -- the higher the priority. Tasks in a maintenance window are scheduled in
    -- priority order with tasks that have the same priority scheduled in
    -- parallel.
    priority :: Prelude.Maybe Prelude.Natural,
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
    -- | The targets (either managed nodes or maintenance window targets).
    --
    -- One or more targets must be specified for maintenance window Run
    -- Command-type tasks. Depending on the task, targets are optional for
    -- other maintenance window task types (Automation, Lambda, and Step
    -- Functions). For more information about running tasks that don\'t specify
    -- targets, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html Registering maintenance window tasks without targets>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    --
    -- Specify managed nodes using the following format:
    --
    -- @Key=InstanceIds,Values=\<instance-id-1>,\<instance-id-2>@
    --
    -- Specify maintenance window targets using the following format:
    --
    -- @Key=WindowTargetIds,Values=\<window-target-id-1>,\<window-target-id-2>@
    targets :: Prelude.Maybe [Target],
    -- | The parameters that the task should use during execution. Populate only
    -- the fields that match the task type. All other fields should be empty.
    taskInvocationParameters :: Prelude.Maybe MaintenanceWindowTaskInvocationParameters,
    -- | The parameters that should be passed to the task when it is run.
    --
    -- @TaskParameters@ has been deprecated. To specify parameters to pass to a
    -- task when it runs, instead use the @Parameters@ option in the
    -- @TaskInvocationParameters@ structure. For information about how Systems
    -- Manager handles these options for the supported maintenance window task
    -- types, see MaintenanceWindowTaskInvocationParameters.
    taskParameters :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text (Data.Sensitive MaintenanceWindowTaskParameterValueExpression))),
    -- | The ID of the maintenance window the task should be added to.
    windowId :: Prelude.Text,
    -- | The ARN of the task to run.
    taskArn :: Prelude.Text,
    -- | The type of task being registered.
    taskType :: MaintenanceWindowTaskType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterTaskWithMaintenanceWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmConfiguration', 'registerTaskWithMaintenanceWindow_alarmConfiguration' - The CloudWatch alarm you want to apply to your maintenance window task.
--
-- 'clientToken', 'registerTaskWithMaintenanceWindow_clientToken' - User-provided idempotency token.
--
-- 'cutoffBehavior', 'registerTaskWithMaintenanceWindow_cutoffBehavior' - Indicates whether tasks should continue to run after the cutoff time
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
-- 'description', 'registerTaskWithMaintenanceWindow_description' - An optional description for the task.
--
-- 'loggingInfo', 'registerTaskWithMaintenanceWindow_loggingInfo' - A structure containing information about an Amazon Simple Storage
-- Service (Amazon S3) bucket to write managed node-level logs to.
--
-- @LoggingInfo@ has been deprecated. To specify an Amazon Simple Storage
-- Service (Amazon S3) bucket to contain logs, instead use the
-- @OutputS3BucketName@ and @OutputS3KeyPrefix@ options in the
-- @TaskInvocationParameters@ structure. For information about how Amazon
-- Web Services Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
--
-- 'maxConcurrency', 'registerTaskWithMaintenanceWindow_maxConcurrency' - The maximum number of targets this task can be run for, in parallel.
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
-- 'maxErrors', 'registerTaskWithMaintenanceWindow_maxErrors' - The maximum number of errors allowed before this task stops being
-- scheduled.
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
-- 'name', 'registerTaskWithMaintenanceWindow_name' - An optional name for the task.
--
-- 'priority', 'registerTaskWithMaintenanceWindow_priority' - The priority of the task in the maintenance window, the lower the number
-- the higher the priority. Tasks in a maintenance window are scheduled in
-- priority order with tasks that have the same priority scheduled in
-- parallel.
--
-- 'serviceRoleArn', 'registerTaskWithMaintenanceWindow_serviceRoleArn' - The Amazon Resource Name (ARN) of the IAM service role for Amazon Web
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
-- 'targets', 'registerTaskWithMaintenanceWindow_targets' - The targets (either managed nodes or maintenance window targets).
--
-- One or more targets must be specified for maintenance window Run
-- Command-type tasks. Depending on the task, targets are optional for
-- other maintenance window task types (Automation, Lambda, and Step
-- Functions). For more information about running tasks that don\'t specify
-- targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html Registering maintenance window tasks without targets>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- Specify managed nodes using the following format:
--
-- @Key=InstanceIds,Values=\<instance-id-1>,\<instance-id-2>@
--
-- Specify maintenance window targets using the following format:
--
-- @Key=WindowTargetIds,Values=\<window-target-id-1>,\<window-target-id-2>@
--
-- 'taskInvocationParameters', 'registerTaskWithMaintenanceWindow_taskInvocationParameters' - The parameters that the task should use during execution. Populate only
-- the fields that match the task type. All other fields should be empty.
--
-- 'taskParameters', 'registerTaskWithMaintenanceWindow_taskParameters' - The parameters that should be passed to the task when it is run.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
--
-- 'windowId', 'registerTaskWithMaintenanceWindow_windowId' - The ID of the maintenance window the task should be added to.
--
-- 'taskArn', 'registerTaskWithMaintenanceWindow_taskArn' - The ARN of the task to run.
--
-- 'taskType', 'registerTaskWithMaintenanceWindow_taskType' - The type of task being registered.
newRegisterTaskWithMaintenanceWindow ::
  -- | 'windowId'
  Prelude.Text ->
  -- | 'taskArn'
  Prelude.Text ->
  -- | 'taskType'
  MaintenanceWindowTaskType ->
  RegisterTaskWithMaintenanceWindow
newRegisterTaskWithMaintenanceWindow
  pWindowId_
  pTaskArn_
  pTaskType_ =
    RegisterTaskWithMaintenanceWindow'
      { alarmConfiguration =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        cutoffBehavior = Prelude.Nothing,
        description = Prelude.Nothing,
        loggingInfo = Prelude.Nothing,
        maxConcurrency = Prelude.Nothing,
        maxErrors = Prelude.Nothing,
        name = Prelude.Nothing,
        priority = Prelude.Nothing,
        serviceRoleArn = Prelude.Nothing,
        targets = Prelude.Nothing,
        taskInvocationParameters =
          Prelude.Nothing,
        taskParameters = Prelude.Nothing,
        windowId = pWindowId_,
        taskArn = pTaskArn_,
        taskType = pTaskType_
      }

-- | The CloudWatch alarm you want to apply to your maintenance window task.
registerTaskWithMaintenanceWindow_alarmConfiguration :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe AlarmConfiguration)
registerTaskWithMaintenanceWindow_alarmConfiguration = Lens.lens (\RegisterTaskWithMaintenanceWindow' {alarmConfiguration} -> alarmConfiguration) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {alarmConfiguration = a} :: RegisterTaskWithMaintenanceWindow)

-- | User-provided idempotency token.
registerTaskWithMaintenanceWindow_clientToken :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe Prelude.Text)
registerTaskWithMaintenanceWindow_clientToken = Lens.lens (\RegisterTaskWithMaintenanceWindow' {clientToken} -> clientToken) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {clientToken = a} :: RegisterTaskWithMaintenanceWindow)

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
registerTaskWithMaintenanceWindow_cutoffBehavior :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe MaintenanceWindowTaskCutoffBehavior)
registerTaskWithMaintenanceWindow_cutoffBehavior = Lens.lens (\RegisterTaskWithMaintenanceWindow' {cutoffBehavior} -> cutoffBehavior) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {cutoffBehavior = a} :: RegisterTaskWithMaintenanceWindow)

-- | An optional description for the task.
registerTaskWithMaintenanceWindow_description :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe Prelude.Text)
registerTaskWithMaintenanceWindow_description = Lens.lens (\RegisterTaskWithMaintenanceWindow' {description} -> description) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {description = a} :: RegisterTaskWithMaintenanceWindow) Prelude.. Lens.mapping Data._Sensitive

-- | A structure containing information about an Amazon Simple Storage
-- Service (Amazon S3) bucket to write managed node-level logs to.
--
-- @LoggingInfo@ has been deprecated. To specify an Amazon Simple Storage
-- Service (Amazon S3) bucket to contain logs, instead use the
-- @OutputS3BucketName@ and @OutputS3KeyPrefix@ options in the
-- @TaskInvocationParameters@ structure. For information about how Amazon
-- Web Services Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
registerTaskWithMaintenanceWindow_loggingInfo :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe LoggingInfo)
registerTaskWithMaintenanceWindow_loggingInfo = Lens.lens (\RegisterTaskWithMaintenanceWindow' {loggingInfo} -> loggingInfo) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {loggingInfo = a} :: RegisterTaskWithMaintenanceWindow)

-- | The maximum number of targets this task can be run for, in parallel.
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
registerTaskWithMaintenanceWindow_maxConcurrency :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe Prelude.Text)
registerTaskWithMaintenanceWindow_maxConcurrency = Lens.lens (\RegisterTaskWithMaintenanceWindow' {maxConcurrency} -> maxConcurrency) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {maxConcurrency = a} :: RegisterTaskWithMaintenanceWindow)

-- | The maximum number of errors allowed before this task stops being
-- scheduled.
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
registerTaskWithMaintenanceWindow_maxErrors :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe Prelude.Text)
registerTaskWithMaintenanceWindow_maxErrors = Lens.lens (\RegisterTaskWithMaintenanceWindow' {maxErrors} -> maxErrors) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {maxErrors = a} :: RegisterTaskWithMaintenanceWindow)

-- | An optional name for the task.
registerTaskWithMaintenanceWindow_name :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe Prelude.Text)
registerTaskWithMaintenanceWindow_name = Lens.lens (\RegisterTaskWithMaintenanceWindow' {name} -> name) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {name = a} :: RegisterTaskWithMaintenanceWindow)

-- | The priority of the task in the maintenance window, the lower the number
-- the higher the priority. Tasks in a maintenance window are scheduled in
-- priority order with tasks that have the same priority scheduled in
-- parallel.
registerTaskWithMaintenanceWindow_priority :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe Prelude.Natural)
registerTaskWithMaintenanceWindow_priority = Lens.lens (\RegisterTaskWithMaintenanceWindow' {priority} -> priority) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {priority = a} :: RegisterTaskWithMaintenanceWindow)

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
registerTaskWithMaintenanceWindow_serviceRoleArn :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe Prelude.Text)
registerTaskWithMaintenanceWindow_serviceRoleArn = Lens.lens (\RegisterTaskWithMaintenanceWindow' {serviceRoleArn} -> serviceRoleArn) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {serviceRoleArn = a} :: RegisterTaskWithMaintenanceWindow)

-- | The targets (either managed nodes or maintenance window targets).
--
-- One or more targets must be specified for maintenance window Run
-- Command-type tasks. Depending on the task, targets are optional for
-- other maintenance window task types (Automation, Lambda, and Step
-- Functions). For more information about running tasks that don\'t specify
-- targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html Registering maintenance window tasks without targets>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- Specify managed nodes using the following format:
--
-- @Key=InstanceIds,Values=\<instance-id-1>,\<instance-id-2>@
--
-- Specify maintenance window targets using the following format:
--
-- @Key=WindowTargetIds,Values=\<window-target-id-1>,\<window-target-id-2>@
registerTaskWithMaintenanceWindow_targets :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe [Target])
registerTaskWithMaintenanceWindow_targets = Lens.lens (\RegisterTaskWithMaintenanceWindow' {targets} -> targets) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {targets = a} :: RegisterTaskWithMaintenanceWindow) Prelude.. Lens.mapping Lens.coerced

-- | The parameters that the task should use during execution. Populate only
-- the fields that match the task type. All other fields should be empty.
registerTaskWithMaintenanceWindow_taskInvocationParameters :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe MaintenanceWindowTaskInvocationParameters)
registerTaskWithMaintenanceWindow_taskInvocationParameters = Lens.lens (\RegisterTaskWithMaintenanceWindow' {taskInvocationParameters} -> taskInvocationParameters) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {taskInvocationParameters = a} :: RegisterTaskWithMaintenanceWindow)

-- | The parameters that should be passed to the task when it is run.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
registerTaskWithMaintenanceWindow_taskParameters :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe (Prelude.HashMap Prelude.Text MaintenanceWindowTaskParameterValueExpression))
registerTaskWithMaintenanceWindow_taskParameters = Lens.lens (\RegisterTaskWithMaintenanceWindow' {taskParameters} -> taskParameters) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {taskParameters = a} :: RegisterTaskWithMaintenanceWindow) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The ID of the maintenance window the task should be added to.
registerTaskWithMaintenanceWindow_windowId :: Lens.Lens' RegisterTaskWithMaintenanceWindow Prelude.Text
registerTaskWithMaintenanceWindow_windowId = Lens.lens (\RegisterTaskWithMaintenanceWindow' {windowId} -> windowId) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {windowId = a} :: RegisterTaskWithMaintenanceWindow)

-- | The ARN of the task to run.
registerTaskWithMaintenanceWindow_taskArn :: Lens.Lens' RegisterTaskWithMaintenanceWindow Prelude.Text
registerTaskWithMaintenanceWindow_taskArn = Lens.lens (\RegisterTaskWithMaintenanceWindow' {taskArn} -> taskArn) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {taskArn = a} :: RegisterTaskWithMaintenanceWindow)

-- | The type of task being registered.
registerTaskWithMaintenanceWindow_taskType :: Lens.Lens' RegisterTaskWithMaintenanceWindow MaintenanceWindowTaskType
registerTaskWithMaintenanceWindow_taskType = Lens.lens (\RegisterTaskWithMaintenanceWindow' {taskType} -> taskType) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {taskType = a} :: RegisterTaskWithMaintenanceWindow)

instance
  Core.AWSRequest
    RegisterTaskWithMaintenanceWindow
  where
  type
    AWSResponse RegisterTaskWithMaintenanceWindow =
      RegisterTaskWithMaintenanceWindowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterTaskWithMaintenanceWindowResponse'
            Prelude.<$> (x Data..?> "WindowTaskId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RegisterTaskWithMaintenanceWindow
  where
  hashWithSalt
    _salt
    RegisterTaskWithMaintenanceWindow' {..} =
      _salt
        `Prelude.hashWithSalt` alarmConfiguration
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` cutoffBehavior
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` loggingInfo
        `Prelude.hashWithSalt` maxConcurrency
        `Prelude.hashWithSalt` maxErrors
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` priority
        `Prelude.hashWithSalt` serviceRoleArn
        `Prelude.hashWithSalt` targets
        `Prelude.hashWithSalt` taskInvocationParameters
        `Prelude.hashWithSalt` taskParameters
        `Prelude.hashWithSalt` windowId
        `Prelude.hashWithSalt` taskArn
        `Prelude.hashWithSalt` taskType

instance
  Prelude.NFData
    RegisterTaskWithMaintenanceWindow
  where
  rnf RegisterTaskWithMaintenanceWindow' {..} =
    Prelude.rnf alarmConfiguration
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf cutoffBehavior
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf loggingInfo
      `Prelude.seq` Prelude.rnf maxConcurrency
      `Prelude.seq` Prelude.rnf maxErrors
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf serviceRoleArn
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf taskInvocationParameters
      `Prelude.seq` Prelude.rnf taskParameters
      `Prelude.seq` Prelude.rnf windowId
      `Prelude.seq` Prelude.rnf taskArn
      `Prelude.seq` Prelude.rnf taskType

instance
  Data.ToHeaders
    RegisterTaskWithMaintenanceWindow
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.RegisterTaskWithMaintenanceWindow" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    RegisterTaskWithMaintenanceWindow
  where
  toJSON RegisterTaskWithMaintenanceWindow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AlarmConfiguration" Data..=)
              Prelude.<$> alarmConfiguration,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("CutoffBehavior" Data..=)
              Prelude.<$> cutoffBehavior,
            ("Description" Data..=) Prelude.<$> description,
            ("LoggingInfo" Data..=) Prelude.<$> loggingInfo,
            ("MaxConcurrency" Data..=)
              Prelude.<$> maxConcurrency,
            ("MaxErrors" Data..=) Prelude.<$> maxErrors,
            ("Name" Data..=) Prelude.<$> name,
            ("Priority" Data..=) Prelude.<$> priority,
            ("ServiceRoleArn" Data..=)
              Prelude.<$> serviceRoleArn,
            ("Targets" Data..=) Prelude.<$> targets,
            ("TaskInvocationParameters" Data..=)
              Prelude.<$> taskInvocationParameters,
            ("TaskParameters" Data..=)
              Prelude.<$> taskParameters,
            Prelude.Just ("WindowId" Data..= windowId),
            Prelude.Just ("TaskArn" Data..= taskArn),
            Prelude.Just ("TaskType" Data..= taskType)
          ]
      )

instance
  Data.ToPath
    RegisterTaskWithMaintenanceWindow
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    RegisterTaskWithMaintenanceWindow
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterTaskWithMaintenanceWindowResponse' smart constructor.
data RegisterTaskWithMaintenanceWindowResponse = RegisterTaskWithMaintenanceWindowResponse'
  { -- | The ID of the task in the maintenance window.
    windowTaskId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterTaskWithMaintenanceWindowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowTaskId', 'registerTaskWithMaintenanceWindowResponse_windowTaskId' - The ID of the task in the maintenance window.
--
-- 'httpStatus', 'registerTaskWithMaintenanceWindowResponse_httpStatus' - The response's http status code.
newRegisterTaskWithMaintenanceWindowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterTaskWithMaintenanceWindowResponse
newRegisterTaskWithMaintenanceWindowResponse
  pHttpStatus_ =
    RegisterTaskWithMaintenanceWindowResponse'
      { windowTaskId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the task in the maintenance window.
registerTaskWithMaintenanceWindowResponse_windowTaskId :: Lens.Lens' RegisterTaskWithMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
registerTaskWithMaintenanceWindowResponse_windowTaskId = Lens.lens (\RegisterTaskWithMaintenanceWindowResponse' {windowTaskId} -> windowTaskId) (\s@RegisterTaskWithMaintenanceWindowResponse' {} a -> s {windowTaskId = a} :: RegisterTaskWithMaintenanceWindowResponse)

-- | The response's http status code.
registerTaskWithMaintenanceWindowResponse_httpStatus :: Lens.Lens' RegisterTaskWithMaintenanceWindowResponse Prelude.Int
registerTaskWithMaintenanceWindowResponse_httpStatus = Lens.lens (\RegisterTaskWithMaintenanceWindowResponse' {httpStatus} -> httpStatus) (\s@RegisterTaskWithMaintenanceWindowResponse' {} a -> s {httpStatus = a} :: RegisterTaskWithMaintenanceWindowResponse)

instance
  Prelude.NFData
    RegisterTaskWithMaintenanceWindowResponse
  where
  rnf RegisterTaskWithMaintenanceWindowResponse' {..} =
    Prelude.rnf windowTaskId
      `Prelude.seq` Prelude.rnf httpStatus
