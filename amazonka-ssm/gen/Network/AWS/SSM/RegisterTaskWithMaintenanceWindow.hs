{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SSM.RegisterTaskWithMaintenanceWindow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new task to a maintenance window.
module Network.AWS.SSM.RegisterTaskWithMaintenanceWindow
  ( -- * Creating a Request
    RegisterTaskWithMaintenanceWindow (..),
    newRegisterTaskWithMaintenanceWindow,

    -- * Request Lenses
    registerTaskWithMaintenanceWindow_maxErrors,
    registerTaskWithMaintenanceWindow_taskParameters,
    registerTaskWithMaintenanceWindow_serviceRoleArn,
    registerTaskWithMaintenanceWindow_priority,
    registerTaskWithMaintenanceWindow_targets,
    registerTaskWithMaintenanceWindow_taskInvocationParameters,
    registerTaskWithMaintenanceWindow_name,
    registerTaskWithMaintenanceWindow_maxConcurrency,
    registerTaskWithMaintenanceWindow_description,
    registerTaskWithMaintenanceWindow_loggingInfo,
    registerTaskWithMaintenanceWindow_clientToken,
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newRegisterTaskWithMaintenanceWindow' smart constructor.
data RegisterTaskWithMaintenanceWindow = RegisterTaskWithMaintenanceWindow'
  { -- | The maximum number of errors allowed before this task stops being
    -- scheduled.
    --
    -- For maintenance window tasks without a target specified, you cannot
    -- supply a value for this option. Instead, the system inserts a
    -- placeholder value of @1@. This value does not affect the running of your
    -- task.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | The parameters that should be passed to the task when it is run.
    --
    -- @TaskParameters@ has been deprecated. To specify parameters to pass to a
    -- task when it runs, instead use the @Parameters@ option in the
    -- @TaskInvocationParameters@ structure. For information about how Systems
    -- Manager handles these options for the supported maintenance window task
    -- types, see MaintenanceWindowTaskInvocationParameters.
    taskParameters :: Prelude.Maybe (Prelude.Sensitive (Prelude.HashMap Prelude.Text (Prelude.Sensitive MaintenanceWindowTaskParameterValueExpression))),
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
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The priority of the task in the maintenance window, the lower the number
    -- the higher the priority. Tasks in a maintenance window are scheduled in
    -- priority order with tasks that have the same priority scheduled in
    -- parallel.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The targets (either instances or maintenance window targets).
    --
    -- One or more targets must be specified for maintenance window Run
    -- Command-type tasks. Depending on the task, targets are optional for
    -- other maintenance window task types (Automation, AWS Lambda, and AWS
    -- Step Functions). For more information about running tasks that do not
    -- specify targets, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html Registering maintenance window tasks without targets>
    -- in the /AWS Systems Manager User Guide/.
    --
    -- Specify instances using the following format:
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
    -- | An optional name for the task.
    name :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of targets this task can be run for in parallel.
    --
    -- For maintenance window tasks without a target specified, you cannot
    -- supply a value for this option. Instead, the system inserts a
    -- placeholder value of @1@. This value does not affect the running of your
    -- task.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | An optional description for the task.
    description :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | A structure containing information about an S3 bucket to write
    -- instance-level logs to.
    --
    -- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
    -- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
    -- options in the @TaskInvocationParameters@ structure. For information
    -- about how Systems Manager handles these options for the supported
    -- maintenance window task types, see
    -- MaintenanceWindowTaskInvocationParameters.
    loggingInfo :: Prelude.Maybe LoggingInfo,
    -- | User-provided idempotency token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window the task should be added to.
    windowId :: Prelude.Text,
    -- | The ARN of the task to run.
    taskArn :: Prelude.Text,
    -- | The type of task being registered.
    taskType :: MaintenanceWindowTaskType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterTaskWithMaintenanceWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxErrors', 'registerTaskWithMaintenanceWindow_maxErrors' - The maximum number of errors allowed before this task stops being
-- scheduled.
--
-- For maintenance window tasks without a target specified, you cannot
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@. This value does not affect the running of your
-- task.
--
-- 'taskParameters', 'registerTaskWithMaintenanceWindow_taskParameters' - The parameters that should be passed to the task when it is run.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
--
-- 'serviceRoleArn', 'registerTaskWithMaintenanceWindow_serviceRoleArn' - The ARN of the IAM service role for Systems Manager to assume when
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
-- 'priority', 'registerTaskWithMaintenanceWindow_priority' - The priority of the task in the maintenance window, the lower the number
-- the higher the priority. Tasks in a maintenance window are scheduled in
-- priority order with tasks that have the same priority scheduled in
-- parallel.
--
-- 'targets', 'registerTaskWithMaintenanceWindow_targets' - The targets (either instances or maintenance window targets).
--
-- One or more targets must be specified for maintenance window Run
-- Command-type tasks. Depending on the task, targets are optional for
-- other maintenance window task types (Automation, AWS Lambda, and AWS
-- Step Functions). For more information about running tasks that do not
-- specify targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html Registering maintenance window tasks without targets>
-- in the /AWS Systems Manager User Guide/.
--
-- Specify instances using the following format:
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
-- 'name', 'registerTaskWithMaintenanceWindow_name' - An optional name for the task.
--
-- 'maxConcurrency', 'registerTaskWithMaintenanceWindow_maxConcurrency' - The maximum number of targets this task can be run for in parallel.
--
-- For maintenance window tasks without a target specified, you cannot
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@. This value does not affect the running of your
-- task.
--
-- 'description', 'registerTaskWithMaintenanceWindow_description' - An optional description for the task.
--
-- 'loggingInfo', 'registerTaskWithMaintenanceWindow_loggingInfo' - A structure containing information about an S3 bucket to write
-- instance-level logs to.
--
-- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
-- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
-- options in the @TaskInvocationParameters@ structure. For information
-- about how Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
--
-- 'clientToken', 'registerTaskWithMaintenanceWindow_clientToken' - User-provided idempotency token.
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
      { maxErrors =
          Prelude.Nothing,
        taskParameters = Prelude.Nothing,
        serviceRoleArn = Prelude.Nothing,
        priority = Prelude.Nothing,
        targets = Prelude.Nothing,
        taskInvocationParameters =
          Prelude.Nothing,
        name = Prelude.Nothing,
        maxConcurrency = Prelude.Nothing,
        description = Prelude.Nothing,
        loggingInfo = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        windowId = pWindowId_,
        taskArn = pTaskArn_,
        taskType = pTaskType_
      }

-- | The maximum number of errors allowed before this task stops being
-- scheduled.
--
-- For maintenance window tasks without a target specified, you cannot
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@. This value does not affect the running of your
-- task.
registerTaskWithMaintenanceWindow_maxErrors :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe Prelude.Text)
registerTaskWithMaintenanceWindow_maxErrors = Lens.lens (\RegisterTaskWithMaintenanceWindow' {maxErrors} -> maxErrors) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {maxErrors = a} :: RegisterTaskWithMaintenanceWindow)

-- | The parameters that should be passed to the task when it is run.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
registerTaskWithMaintenanceWindow_taskParameters :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe (Prelude.HashMap Prelude.Text MaintenanceWindowTaskParameterValueExpression))
registerTaskWithMaintenanceWindow_taskParameters = Lens.lens (\RegisterTaskWithMaintenanceWindow' {taskParameters} -> taskParameters) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {taskParameters = a} :: RegisterTaskWithMaintenanceWindow) Prelude.. Lens.mapping (Prelude._Sensitive Prelude.. Prelude._Coerce)

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
registerTaskWithMaintenanceWindow_serviceRoleArn :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe Prelude.Text)
registerTaskWithMaintenanceWindow_serviceRoleArn = Lens.lens (\RegisterTaskWithMaintenanceWindow' {serviceRoleArn} -> serviceRoleArn) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {serviceRoleArn = a} :: RegisterTaskWithMaintenanceWindow)

-- | The priority of the task in the maintenance window, the lower the number
-- the higher the priority. Tasks in a maintenance window are scheduled in
-- priority order with tasks that have the same priority scheduled in
-- parallel.
registerTaskWithMaintenanceWindow_priority :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe Prelude.Natural)
registerTaskWithMaintenanceWindow_priority = Lens.lens (\RegisterTaskWithMaintenanceWindow' {priority} -> priority) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {priority = a} :: RegisterTaskWithMaintenanceWindow)

-- | The targets (either instances or maintenance window targets).
--
-- One or more targets must be specified for maintenance window Run
-- Command-type tasks. Depending on the task, targets are optional for
-- other maintenance window task types (Automation, AWS Lambda, and AWS
-- Step Functions). For more information about running tasks that do not
-- specify targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/maintenance-windows-targetless-tasks.html Registering maintenance window tasks without targets>
-- in the /AWS Systems Manager User Guide/.
--
-- Specify instances using the following format:
--
-- @Key=InstanceIds,Values=\<instance-id-1>,\<instance-id-2>@
--
-- Specify maintenance window targets using the following format:
--
-- @Key=WindowTargetIds,Values=\<window-target-id-1>,\<window-target-id-2>@
registerTaskWithMaintenanceWindow_targets :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe [Target])
registerTaskWithMaintenanceWindow_targets = Lens.lens (\RegisterTaskWithMaintenanceWindow' {targets} -> targets) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {targets = a} :: RegisterTaskWithMaintenanceWindow) Prelude.. Lens.mapping Prelude._Coerce

-- | The parameters that the task should use during execution. Populate only
-- the fields that match the task type. All other fields should be empty.
registerTaskWithMaintenanceWindow_taskInvocationParameters :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe MaintenanceWindowTaskInvocationParameters)
registerTaskWithMaintenanceWindow_taskInvocationParameters = Lens.lens (\RegisterTaskWithMaintenanceWindow' {taskInvocationParameters} -> taskInvocationParameters) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {taskInvocationParameters = a} :: RegisterTaskWithMaintenanceWindow)

-- | An optional name for the task.
registerTaskWithMaintenanceWindow_name :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe Prelude.Text)
registerTaskWithMaintenanceWindow_name = Lens.lens (\RegisterTaskWithMaintenanceWindow' {name} -> name) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {name = a} :: RegisterTaskWithMaintenanceWindow)

-- | The maximum number of targets this task can be run for in parallel.
--
-- For maintenance window tasks without a target specified, you cannot
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@. This value does not affect the running of your
-- task.
registerTaskWithMaintenanceWindow_maxConcurrency :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe Prelude.Text)
registerTaskWithMaintenanceWindow_maxConcurrency = Lens.lens (\RegisterTaskWithMaintenanceWindow' {maxConcurrency} -> maxConcurrency) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {maxConcurrency = a} :: RegisterTaskWithMaintenanceWindow)

-- | An optional description for the task.
registerTaskWithMaintenanceWindow_description :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe Prelude.Text)
registerTaskWithMaintenanceWindow_description = Lens.lens (\RegisterTaskWithMaintenanceWindow' {description} -> description) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {description = a} :: RegisterTaskWithMaintenanceWindow) Prelude.. Lens.mapping Prelude._Sensitive

-- | A structure containing information about an S3 bucket to write
-- instance-level logs to.
--
-- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
-- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
-- options in the @TaskInvocationParameters@ structure. For information
-- about how Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
registerTaskWithMaintenanceWindow_loggingInfo :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe LoggingInfo)
registerTaskWithMaintenanceWindow_loggingInfo = Lens.lens (\RegisterTaskWithMaintenanceWindow' {loggingInfo} -> loggingInfo) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {loggingInfo = a} :: RegisterTaskWithMaintenanceWindow)

-- | User-provided idempotency token.
registerTaskWithMaintenanceWindow_clientToken :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Prelude.Maybe Prelude.Text)
registerTaskWithMaintenanceWindow_clientToken = Lens.lens (\RegisterTaskWithMaintenanceWindow' {clientToken} -> clientToken) (\s@RegisterTaskWithMaintenanceWindow' {} a -> s {clientToken = a} :: RegisterTaskWithMaintenanceWindow)

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
  Prelude.AWSRequest
    RegisterTaskWithMaintenanceWindow
  where
  type
    Rs RegisterTaskWithMaintenanceWindow =
      RegisterTaskWithMaintenanceWindowResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterTaskWithMaintenanceWindowResponse'
            Prelude.<$> (x Prelude..?> "WindowTaskId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RegisterTaskWithMaintenanceWindow

instance
  Prelude.NFData
    RegisterTaskWithMaintenanceWindow

instance
  Prelude.ToHeaders
    RegisterTaskWithMaintenanceWindow
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.RegisterTaskWithMaintenanceWindow" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    RegisterTaskWithMaintenanceWindow
  where
  toJSON RegisterTaskWithMaintenanceWindow' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MaxErrors" Prelude..=) Prelude.<$> maxErrors,
            ("TaskParameters" Prelude..=)
              Prelude.<$> taskParameters,
            ("ServiceRoleArn" Prelude..=)
              Prelude.<$> serviceRoleArn,
            ("Priority" Prelude..=) Prelude.<$> priority,
            ("Targets" Prelude..=) Prelude.<$> targets,
            ("TaskInvocationParameters" Prelude..=)
              Prelude.<$> taskInvocationParameters,
            ("Name" Prelude..=) Prelude.<$> name,
            ("MaxConcurrency" Prelude..=)
              Prelude.<$> maxConcurrency,
            ("Description" Prelude..=) Prelude.<$> description,
            ("LoggingInfo" Prelude..=) Prelude.<$> loggingInfo,
            ("ClientToken" Prelude..=) Prelude.<$> clientToken,
            Prelude.Just ("WindowId" Prelude..= windowId),
            Prelude.Just ("TaskArn" Prelude..= taskArn),
            Prelude.Just ("TaskType" Prelude..= taskType)
          ]
      )

instance
  Prelude.ToPath
    RegisterTaskWithMaintenanceWindow
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
