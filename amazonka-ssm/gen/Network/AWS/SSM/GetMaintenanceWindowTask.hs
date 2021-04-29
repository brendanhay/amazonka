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
-- Module      : Network.AWS.SSM.GetMaintenanceWindowTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tasks in a maintenance window.
--
-- For maintenance window tasks without a specified target, you cannot
-- supply values for @--max-errors@ and @--max-concurrency@. Instead, the
-- system inserts a placeholder value of @1@, which may be reported in the
-- response to this command. These values do not affect the running of your
-- task and can be ignored.
module Network.AWS.SSM.GetMaintenanceWindowTask
  ( -- * Creating a Request
    GetMaintenanceWindowTask (..),
    newGetMaintenanceWindowTask,

    -- * Request Lenses
    getMaintenanceWindowTask_windowId,
    getMaintenanceWindowTask_windowTaskId,

    -- * Destructuring the Response
    GetMaintenanceWindowTaskResponse (..),
    newGetMaintenanceWindowTaskResponse,

    -- * Response Lenses
    getMaintenanceWindowTaskResponse_maxErrors,
    getMaintenanceWindowTaskResponse_taskParameters,
    getMaintenanceWindowTaskResponse_windowTaskId,
    getMaintenanceWindowTaskResponse_serviceRoleArn,
    getMaintenanceWindowTaskResponse_priority,
    getMaintenanceWindowTaskResponse_targets,
    getMaintenanceWindowTaskResponse_taskInvocationParameters,
    getMaintenanceWindowTaskResponse_name,
    getMaintenanceWindowTaskResponse_maxConcurrency,
    getMaintenanceWindowTaskResponse_windowId,
    getMaintenanceWindowTaskResponse_description,
    getMaintenanceWindowTaskResponse_taskArn,
    getMaintenanceWindowTaskResponse_taskType,
    getMaintenanceWindowTaskResponse_loggingInfo,
    getMaintenanceWindowTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetMaintenanceWindowTask' smart constructor.
data GetMaintenanceWindowTask = GetMaintenanceWindowTask'
  { -- | The maintenance window ID that includes the task to retrieve.
    windowId :: Prelude.Text,
    -- | The maintenance window task ID to retrieve.
    windowTaskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetMaintenanceWindowTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowId', 'getMaintenanceWindowTask_windowId' - The maintenance window ID that includes the task to retrieve.
--
-- 'windowTaskId', 'getMaintenanceWindowTask_windowTaskId' - The maintenance window task ID to retrieve.
newGetMaintenanceWindowTask ::
  -- | 'windowId'
  Prelude.Text ->
  -- | 'windowTaskId'
  Prelude.Text ->
  GetMaintenanceWindowTask
newGetMaintenanceWindowTask pWindowId_ pWindowTaskId_ =
  GetMaintenanceWindowTask'
    { windowId = pWindowId_,
      windowTaskId = pWindowTaskId_
    }

-- | The maintenance window ID that includes the task to retrieve.
getMaintenanceWindowTask_windowId :: Lens.Lens' GetMaintenanceWindowTask Prelude.Text
getMaintenanceWindowTask_windowId = Lens.lens (\GetMaintenanceWindowTask' {windowId} -> windowId) (\s@GetMaintenanceWindowTask' {} a -> s {windowId = a} :: GetMaintenanceWindowTask)

-- | The maintenance window task ID to retrieve.
getMaintenanceWindowTask_windowTaskId :: Lens.Lens' GetMaintenanceWindowTask Prelude.Text
getMaintenanceWindowTask_windowTaskId = Lens.lens (\GetMaintenanceWindowTask' {windowTaskId} -> windowTaskId) (\s@GetMaintenanceWindowTask' {} a -> s {windowTaskId = a} :: GetMaintenanceWindowTask)

instance Prelude.AWSRequest GetMaintenanceWindowTask where
  type
    Rs GetMaintenanceWindowTask =
      GetMaintenanceWindowTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMaintenanceWindowTaskResponse'
            Prelude.<$> (x Prelude..?> "MaxErrors")
            Prelude.<*> ( x Prelude..?> "TaskParameters"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "WindowTaskId")
            Prelude.<*> (x Prelude..?> "ServiceRoleArn")
            Prelude.<*> (x Prelude..?> "Priority")
            Prelude.<*> (x Prelude..?> "Targets" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "TaskInvocationParameters")
            Prelude.<*> (x Prelude..?> "Name")
            Prelude.<*> (x Prelude..?> "MaxConcurrency")
            Prelude.<*> (x Prelude..?> "WindowId")
            Prelude.<*> (x Prelude..?> "Description")
            Prelude.<*> (x Prelude..?> "TaskArn")
            Prelude.<*> (x Prelude..?> "TaskType")
            Prelude.<*> (x Prelude..?> "LoggingInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMaintenanceWindowTask

instance Prelude.NFData GetMaintenanceWindowTask

instance Prelude.ToHeaders GetMaintenanceWindowTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.GetMaintenanceWindowTask" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetMaintenanceWindowTask where
  toJSON GetMaintenanceWindowTask' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("WindowId" Prelude..= windowId),
            Prelude.Just
              ("WindowTaskId" Prelude..= windowTaskId)
          ]
      )

instance Prelude.ToPath GetMaintenanceWindowTask where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetMaintenanceWindowTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMaintenanceWindowTaskResponse' smart constructor.
data GetMaintenanceWindowTaskResponse = GetMaintenanceWindowTaskResponse'
  { -- | The maximum number of errors allowed before the task stops being
    -- scheduled.
    --
    -- For maintenance window tasks without a target specified, you cannot
    -- supply a value for this option. Instead, the system inserts a
    -- placeholder value of @1@, which may be reported in the response to this
    -- command. This value does not affect the running of your task and can be
    -- ignored.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | The parameters to pass to the task when it runs.
    --
    -- @TaskParameters@ has been deprecated. To specify parameters to pass to a
    -- task when it runs, instead use the @Parameters@ option in the
    -- @TaskInvocationParameters@ structure. For information about how Systems
    -- Manager handles these options for the supported maintenance window task
    -- types, see MaintenanceWindowTaskInvocationParameters.
    taskParameters :: Prelude.Maybe (Prelude.Sensitive (Prelude.HashMap Prelude.Text (Prelude.Sensitive MaintenanceWindowTaskParameterValueExpression))),
    -- | The retrieved maintenance window task ID.
    windowTaskId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM service role to use to publish Amazon Simple
    -- Notification Service (Amazon SNS) notifications for maintenance window
    -- Run Command tasks.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The priority of the task when it runs. The lower the number, the higher
    -- the priority. Tasks that have the same priority are scheduled in
    -- parallel.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The targets where the task should run.
    targets :: Prelude.Maybe [Target],
    -- | The parameters to pass to the task when it runs.
    taskInvocationParameters :: Prelude.Maybe MaintenanceWindowTaskInvocationParameters,
    -- | The retrieved task name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of targets allowed to run this task in parallel.
    --
    -- For maintenance window tasks without a target specified, you cannot
    -- supply a value for this option. Instead, the system inserts a
    -- placeholder value of @1@, which may be reported in the response to this
    -- command. This value does not affect the running of your task and can be
    -- ignored.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The retrieved maintenance window ID.
    windowId :: Prelude.Maybe Prelude.Text,
    -- | The retrieved task description.
    description :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The resource that the task used during execution. For RUN_COMMAND and
    -- AUTOMATION task types, the TaskArn is the Systems Manager Document
    -- name\/ARN. For LAMBDA tasks, the value is the function name\/ARN. For
    -- STEP_FUNCTIONS tasks, the value is the state machine ARN.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The type of task to run.
    taskType :: Prelude.Maybe MaintenanceWindowTaskType,
    -- | The location in Amazon S3 where the task results are logged.
    --
    -- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
    -- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
    -- options in the @TaskInvocationParameters@ structure. For information
    -- about how Systems Manager handles these options for the supported
    -- maintenance window task types, see
    -- MaintenanceWindowTaskInvocationParameters.
    loggingInfo :: Prelude.Maybe LoggingInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetMaintenanceWindowTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxErrors', 'getMaintenanceWindowTaskResponse_maxErrors' - The maximum number of errors allowed before the task stops being
-- scheduled.
--
-- For maintenance window tasks without a target specified, you cannot
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@, which may be reported in the response to this
-- command. This value does not affect the running of your task and can be
-- ignored.
--
-- 'taskParameters', 'getMaintenanceWindowTaskResponse_taskParameters' - The parameters to pass to the task when it runs.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
--
-- 'windowTaskId', 'getMaintenanceWindowTaskResponse_windowTaskId' - The retrieved maintenance window task ID.
--
-- 'serviceRoleArn', 'getMaintenanceWindowTaskResponse_serviceRoleArn' - The ARN of the IAM service role to use to publish Amazon Simple
-- Notification Service (Amazon SNS) notifications for maintenance window
-- Run Command tasks.
--
-- 'priority', 'getMaintenanceWindowTaskResponse_priority' - The priority of the task when it runs. The lower the number, the higher
-- the priority. Tasks that have the same priority are scheduled in
-- parallel.
--
-- 'targets', 'getMaintenanceWindowTaskResponse_targets' - The targets where the task should run.
--
-- 'taskInvocationParameters', 'getMaintenanceWindowTaskResponse_taskInvocationParameters' - The parameters to pass to the task when it runs.
--
-- 'name', 'getMaintenanceWindowTaskResponse_name' - The retrieved task name.
--
-- 'maxConcurrency', 'getMaintenanceWindowTaskResponse_maxConcurrency' - The maximum number of targets allowed to run this task in parallel.
--
-- For maintenance window tasks without a target specified, you cannot
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@, which may be reported in the response to this
-- command. This value does not affect the running of your task and can be
-- ignored.
--
-- 'windowId', 'getMaintenanceWindowTaskResponse_windowId' - The retrieved maintenance window ID.
--
-- 'description', 'getMaintenanceWindowTaskResponse_description' - The retrieved task description.
--
-- 'taskArn', 'getMaintenanceWindowTaskResponse_taskArn' - The resource that the task used during execution. For RUN_COMMAND and
-- AUTOMATION task types, the TaskArn is the Systems Manager Document
-- name\/ARN. For LAMBDA tasks, the value is the function name\/ARN. For
-- STEP_FUNCTIONS tasks, the value is the state machine ARN.
--
-- 'taskType', 'getMaintenanceWindowTaskResponse_taskType' - The type of task to run.
--
-- 'loggingInfo', 'getMaintenanceWindowTaskResponse_loggingInfo' - The location in Amazon S3 where the task results are logged.
--
-- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
-- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
-- options in the @TaskInvocationParameters@ structure. For information
-- about how Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
--
-- 'httpStatus', 'getMaintenanceWindowTaskResponse_httpStatus' - The response's http status code.
newGetMaintenanceWindowTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMaintenanceWindowTaskResponse
newGetMaintenanceWindowTaskResponse pHttpStatus_ =
  GetMaintenanceWindowTaskResponse'
    { maxErrors =
        Prelude.Nothing,
      taskParameters = Prelude.Nothing,
      windowTaskId = Prelude.Nothing,
      serviceRoleArn = Prelude.Nothing,
      priority = Prelude.Nothing,
      targets = Prelude.Nothing,
      taskInvocationParameters =
        Prelude.Nothing,
      name = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      windowId = Prelude.Nothing,
      description = Prelude.Nothing,
      taskArn = Prelude.Nothing,
      taskType = Prelude.Nothing,
      loggingInfo = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The maximum number of errors allowed before the task stops being
-- scheduled.
--
-- For maintenance window tasks without a target specified, you cannot
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@, which may be reported in the response to this
-- command. This value does not affect the running of your task and can be
-- ignored.
getMaintenanceWindowTaskResponse_maxErrors :: Lens.Lens' GetMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowTaskResponse_maxErrors = Lens.lens (\GetMaintenanceWindowTaskResponse' {maxErrors} -> maxErrors) (\s@GetMaintenanceWindowTaskResponse' {} a -> s {maxErrors = a} :: GetMaintenanceWindowTaskResponse)

-- | The parameters to pass to the task when it runs.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
getMaintenanceWindowTaskResponse_taskParameters :: Lens.Lens' GetMaintenanceWindowTaskResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text MaintenanceWindowTaskParameterValueExpression))
getMaintenanceWindowTaskResponse_taskParameters = Lens.lens (\GetMaintenanceWindowTaskResponse' {taskParameters} -> taskParameters) (\s@GetMaintenanceWindowTaskResponse' {} a -> s {taskParameters = a} :: GetMaintenanceWindowTaskResponse) Prelude.. Lens.mapping (Prelude._Sensitive Prelude.. Prelude._Coerce)

-- | The retrieved maintenance window task ID.
getMaintenanceWindowTaskResponse_windowTaskId :: Lens.Lens' GetMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowTaskResponse_windowTaskId = Lens.lens (\GetMaintenanceWindowTaskResponse' {windowTaskId} -> windowTaskId) (\s@GetMaintenanceWindowTaskResponse' {} a -> s {windowTaskId = a} :: GetMaintenanceWindowTaskResponse)

-- | The ARN of the IAM service role to use to publish Amazon Simple
-- Notification Service (Amazon SNS) notifications for maintenance window
-- Run Command tasks.
getMaintenanceWindowTaskResponse_serviceRoleArn :: Lens.Lens' GetMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowTaskResponse_serviceRoleArn = Lens.lens (\GetMaintenanceWindowTaskResponse' {serviceRoleArn} -> serviceRoleArn) (\s@GetMaintenanceWindowTaskResponse' {} a -> s {serviceRoleArn = a} :: GetMaintenanceWindowTaskResponse)

-- | The priority of the task when it runs. The lower the number, the higher
-- the priority. Tasks that have the same priority are scheduled in
-- parallel.
getMaintenanceWindowTaskResponse_priority :: Lens.Lens' GetMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Natural)
getMaintenanceWindowTaskResponse_priority = Lens.lens (\GetMaintenanceWindowTaskResponse' {priority} -> priority) (\s@GetMaintenanceWindowTaskResponse' {} a -> s {priority = a} :: GetMaintenanceWindowTaskResponse)

-- | The targets where the task should run.
getMaintenanceWindowTaskResponse_targets :: Lens.Lens' GetMaintenanceWindowTaskResponse (Prelude.Maybe [Target])
getMaintenanceWindowTaskResponse_targets = Lens.lens (\GetMaintenanceWindowTaskResponse' {targets} -> targets) (\s@GetMaintenanceWindowTaskResponse' {} a -> s {targets = a} :: GetMaintenanceWindowTaskResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The parameters to pass to the task when it runs.
getMaintenanceWindowTaskResponse_taskInvocationParameters :: Lens.Lens' GetMaintenanceWindowTaskResponse (Prelude.Maybe MaintenanceWindowTaskInvocationParameters)
getMaintenanceWindowTaskResponse_taskInvocationParameters = Lens.lens (\GetMaintenanceWindowTaskResponse' {taskInvocationParameters} -> taskInvocationParameters) (\s@GetMaintenanceWindowTaskResponse' {} a -> s {taskInvocationParameters = a} :: GetMaintenanceWindowTaskResponse)

-- | The retrieved task name.
getMaintenanceWindowTaskResponse_name :: Lens.Lens' GetMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowTaskResponse_name = Lens.lens (\GetMaintenanceWindowTaskResponse' {name} -> name) (\s@GetMaintenanceWindowTaskResponse' {} a -> s {name = a} :: GetMaintenanceWindowTaskResponse)

-- | The maximum number of targets allowed to run this task in parallel.
--
-- For maintenance window tasks without a target specified, you cannot
-- supply a value for this option. Instead, the system inserts a
-- placeholder value of @1@, which may be reported in the response to this
-- command. This value does not affect the running of your task and can be
-- ignored.
getMaintenanceWindowTaskResponse_maxConcurrency :: Lens.Lens' GetMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowTaskResponse_maxConcurrency = Lens.lens (\GetMaintenanceWindowTaskResponse' {maxConcurrency} -> maxConcurrency) (\s@GetMaintenanceWindowTaskResponse' {} a -> s {maxConcurrency = a} :: GetMaintenanceWindowTaskResponse)

-- | The retrieved maintenance window ID.
getMaintenanceWindowTaskResponse_windowId :: Lens.Lens' GetMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowTaskResponse_windowId = Lens.lens (\GetMaintenanceWindowTaskResponse' {windowId} -> windowId) (\s@GetMaintenanceWindowTaskResponse' {} a -> s {windowId = a} :: GetMaintenanceWindowTaskResponse)

-- | The retrieved task description.
getMaintenanceWindowTaskResponse_description :: Lens.Lens' GetMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowTaskResponse_description = Lens.lens (\GetMaintenanceWindowTaskResponse' {description} -> description) (\s@GetMaintenanceWindowTaskResponse' {} a -> s {description = a} :: GetMaintenanceWindowTaskResponse) Prelude.. Lens.mapping Prelude._Sensitive

-- | The resource that the task used during execution. For RUN_COMMAND and
-- AUTOMATION task types, the TaskArn is the Systems Manager Document
-- name\/ARN. For LAMBDA tasks, the value is the function name\/ARN. For
-- STEP_FUNCTIONS tasks, the value is the state machine ARN.
getMaintenanceWindowTaskResponse_taskArn :: Lens.Lens' GetMaintenanceWindowTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowTaskResponse_taskArn = Lens.lens (\GetMaintenanceWindowTaskResponse' {taskArn} -> taskArn) (\s@GetMaintenanceWindowTaskResponse' {} a -> s {taskArn = a} :: GetMaintenanceWindowTaskResponse)

-- | The type of task to run.
getMaintenanceWindowTaskResponse_taskType :: Lens.Lens' GetMaintenanceWindowTaskResponse (Prelude.Maybe MaintenanceWindowTaskType)
getMaintenanceWindowTaskResponse_taskType = Lens.lens (\GetMaintenanceWindowTaskResponse' {taskType} -> taskType) (\s@GetMaintenanceWindowTaskResponse' {} a -> s {taskType = a} :: GetMaintenanceWindowTaskResponse)

-- | The location in Amazon S3 where the task results are logged.
--
-- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
-- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
-- options in the @TaskInvocationParameters@ structure. For information
-- about how Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
getMaintenanceWindowTaskResponse_loggingInfo :: Lens.Lens' GetMaintenanceWindowTaskResponse (Prelude.Maybe LoggingInfo)
getMaintenanceWindowTaskResponse_loggingInfo = Lens.lens (\GetMaintenanceWindowTaskResponse' {loggingInfo} -> loggingInfo) (\s@GetMaintenanceWindowTaskResponse' {} a -> s {loggingInfo = a} :: GetMaintenanceWindowTaskResponse)

-- | The response's http status code.
getMaintenanceWindowTaskResponse_httpStatus :: Lens.Lens' GetMaintenanceWindowTaskResponse Prelude.Int
getMaintenanceWindowTaskResponse_httpStatus = Lens.lens (\GetMaintenanceWindowTaskResponse' {httpStatus} -> httpStatus) (\s@GetMaintenanceWindowTaskResponse' {} a -> s {httpStatus = a} :: GetMaintenanceWindowTaskResponse)

instance
  Prelude.NFData
    GetMaintenanceWindowTaskResponse
