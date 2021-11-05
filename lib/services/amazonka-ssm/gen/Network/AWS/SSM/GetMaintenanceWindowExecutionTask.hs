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
-- Module      : Amazonka.SSM.GetMaintenanceWindowExecutionTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details about a specific task run as part of a maintenance
-- window execution.
module Amazonka.SSM.GetMaintenanceWindowExecutionTask
  ( -- * Creating a Request
    GetMaintenanceWindowExecutionTask (..),
    newGetMaintenanceWindowExecutionTask,

    -- * Request Lenses
    getMaintenanceWindowExecutionTask_windowExecutionId,
    getMaintenanceWindowExecutionTask_taskId,

    -- * Destructuring the Response
    GetMaintenanceWindowExecutionTaskResponse (..),
    newGetMaintenanceWindowExecutionTaskResponse,

    -- * Response Lenses
    getMaintenanceWindowExecutionTaskResponse_status,
    getMaintenanceWindowExecutionTaskResponse_taskParameters,
    getMaintenanceWindowExecutionTaskResponse_taskExecutionId,
    getMaintenanceWindowExecutionTaskResponse_priority,
    getMaintenanceWindowExecutionTaskResponse_startTime,
    getMaintenanceWindowExecutionTaskResponse_taskArn,
    getMaintenanceWindowExecutionTaskResponse_windowExecutionId,
    getMaintenanceWindowExecutionTaskResponse_statusDetails,
    getMaintenanceWindowExecutionTaskResponse_maxErrors,
    getMaintenanceWindowExecutionTaskResponse_endTime,
    getMaintenanceWindowExecutionTaskResponse_type,
    getMaintenanceWindowExecutionTaskResponse_maxConcurrency,
    getMaintenanceWindowExecutionTaskResponse_serviceRole,
    getMaintenanceWindowExecutionTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetMaintenanceWindowExecutionTask' smart constructor.
data GetMaintenanceWindowExecutionTask = GetMaintenanceWindowExecutionTask'
  { -- | The ID of the maintenance window execution that includes the task.
    windowExecutionId :: Prelude.Text,
    -- | The ID of the specific task execution in the maintenance window task
    -- that should be retrieved.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMaintenanceWindowExecutionTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowExecutionId', 'getMaintenanceWindowExecutionTask_windowExecutionId' - The ID of the maintenance window execution that includes the task.
--
-- 'taskId', 'getMaintenanceWindowExecutionTask_taskId' - The ID of the specific task execution in the maintenance window task
-- that should be retrieved.
newGetMaintenanceWindowExecutionTask ::
  -- | 'windowExecutionId'
  Prelude.Text ->
  -- | 'taskId'
  Prelude.Text ->
  GetMaintenanceWindowExecutionTask
newGetMaintenanceWindowExecutionTask
  pWindowExecutionId_
  pTaskId_ =
    GetMaintenanceWindowExecutionTask'
      { windowExecutionId =
          pWindowExecutionId_,
        taskId = pTaskId_
      }

-- | The ID of the maintenance window execution that includes the task.
getMaintenanceWindowExecutionTask_windowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTask Prelude.Text
getMaintenanceWindowExecutionTask_windowExecutionId = Lens.lens (\GetMaintenanceWindowExecutionTask' {windowExecutionId} -> windowExecutionId) (\s@GetMaintenanceWindowExecutionTask' {} a -> s {windowExecutionId = a} :: GetMaintenanceWindowExecutionTask)

-- | The ID of the specific task execution in the maintenance window task
-- that should be retrieved.
getMaintenanceWindowExecutionTask_taskId :: Lens.Lens' GetMaintenanceWindowExecutionTask Prelude.Text
getMaintenanceWindowExecutionTask_taskId = Lens.lens (\GetMaintenanceWindowExecutionTask' {taskId} -> taskId) (\s@GetMaintenanceWindowExecutionTask' {} a -> s {taskId = a} :: GetMaintenanceWindowExecutionTask)

instance
  Core.AWSRequest
    GetMaintenanceWindowExecutionTask
  where
  type
    AWSResponse GetMaintenanceWindowExecutionTask =
      GetMaintenanceWindowExecutionTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMaintenanceWindowExecutionTaskResponse'
            Prelude.<$> (x Core..?> "Status")
              Prelude.<*> (x Core..?> "TaskParameters" Core..!@ Prelude.mempty)
              Prelude.<*> (x Core..?> "TaskExecutionId")
              Prelude.<*> (x Core..?> "Priority")
              Prelude.<*> (x Core..?> "StartTime")
              Prelude.<*> (x Core..?> "TaskArn")
              Prelude.<*> (x Core..?> "WindowExecutionId")
              Prelude.<*> (x Core..?> "StatusDetails")
              Prelude.<*> (x Core..?> "MaxErrors")
              Prelude.<*> (x Core..?> "EndTime")
              Prelude.<*> (x Core..?> "Type")
              Prelude.<*> (x Core..?> "MaxConcurrency")
              Prelude.<*> (x Core..?> "ServiceRole")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetMaintenanceWindowExecutionTask

instance
  Prelude.NFData
    GetMaintenanceWindowExecutionTask

instance
  Core.ToHeaders
    GetMaintenanceWindowExecutionTask
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.GetMaintenanceWindowExecutionTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    GetMaintenanceWindowExecutionTask
  where
  toJSON GetMaintenanceWindowExecutionTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WindowExecutionId" Core..= windowExecutionId),
            Prelude.Just ("TaskId" Core..= taskId)
          ]
      )

instance
  Core.ToPath
    GetMaintenanceWindowExecutionTask
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetMaintenanceWindowExecutionTask
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMaintenanceWindowExecutionTaskResponse' smart constructor.
data GetMaintenanceWindowExecutionTaskResponse = GetMaintenanceWindowExecutionTaskResponse'
  { -- | The status of the task.
    status :: Prelude.Maybe MaintenanceWindowExecutionStatus,
    -- | The parameters passed to the task when it was run.
    --
    -- @TaskParameters@ has been deprecated. To specify parameters to pass to a
    -- task when it runs, instead use the @Parameters@ option in the
    -- @TaskInvocationParameters@ structure. For information about how Systems
    -- Manager handles these options for the supported maintenance window task
    -- types, see MaintenanceWindowTaskInvocationParameters.
    --
    -- The map has the following format:
    --
    -- -   @Key@: string, between 1 and 255 characters
    --
    -- -   @Value@: an array of strings, each between 1 and 255 characters
    taskParameters :: Prelude.Maybe (Core.Sensitive [Core.Sensitive (Prelude.HashMap Prelude.Text (Core.Sensitive MaintenanceWindowTaskParameterValueExpression))]),
    -- | The ID of the specific task execution in the maintenance window task
    -- that was retrieved.
    taskExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The priority of the task.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The time the task execution started.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the task that ran.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window execution that includes the task.
    windowExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The details explaining the status. Not available for all status values.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The defined maximum number of task execution errors allowed before
    -- scheduling of the task execution would have been stopped.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | The time the task execution completed.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The type of task that was run.
    type' :: Prelude.Maybe MaintenanceWindowTaskType,
    -- | The defined maximum number of task executions that could be run in
    -- parallel.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The role that was assumed when running the task.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMaintenanceWindowExecutionTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getMaintenanceWindowExecutionTaskResponse_status' - The status of the task.
--
-- 'taskParameters', 'getMaintenanceWindowExecutionTaskResponse_taskParameters' - The parameters passed to the task when it was run.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
--
-- The map has the following format:
--
-- -   @Key@: string, between 1 and 255 characters
--
-- -   @Value@: an array of strings, each between 1 and 255 characters
--
-- 'taskExecutionId', 'getMaintenanceWindowExecutionTaskResponse_taskExecutionId' - The ID of the specific task execution in the maintenance window task
-- that was retrieved.
--
-- 'priority', 'getMaintenanceWindowExecutionTaskResponse_priority' - The priority of the task.
--
-- 'startTime', 'getMaintenanceWindowExecutionTaskResponse_startTime' - The time the task execution started.
--
-- 'taskArn', 'getMaintenanceWindowExecutionTaskResponse_taskArn' - The Amazon Resource Name (ARN) of the task that ran.
--
-- 'windowExecutionId', 'getMaintenanceWindowExecutionTaskResponse_windowExecutionId' - The ID of the maintenance window execution that includes the task.
--
-- 'statusDetails', 'getMaintenanceWindowExecutionTaskResponse_statusDetails' - The details explaining the status. Not available for all status values.
--
-- 'maxErrors', 'getMaintenanceWindowExecutionTaskResponse_maxErrors' - The defined maximum number of task execution errors allowed before
-- scheduling of the task execution would have been stopped.
--
-- 'endTime', 'getMaintenanceWindowExecutionTaskResponse_endTime' - The time the task execution completed.
--
-- 'type'', 'getMaintenanceWindowExecutionTaskResponse_type' - The type of task that was run.
--
-- 'maxConcurrency', 'getMaintenanceWindowExecutionTaskResponse_maxConcurrency' - The defined maximum number of task executions that could be run in
-- parallel.
--
-- 'serviceRole', 'getMaintenanceWindowExecutionTaskResponse_serviceRole' - The role that was assumed when running the task.
--
-- 'httpStatus', 'getMaintenanceWindowExecutionTaskResponse_httpStatus' - The response's http status code.
newGetMaintenanceWindowExecutionTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMaintenanceWindowExecutionTaskResponse
newGetMaintenanceWindowExecutionTaskResponse
  pHttpStatus_ =
    GetMaintenanceWindowExecutionTaskResponse'
      { status =
          Prelude.Nothing,
        taskParameters = Prelude.Nothing,
        taskExecutionId =
          Prelude.Nothing,
        priority = Prelude.Nothing,
        startTime = Prelude.Nothing,
        taskArn = Prelude.Nothing,
        windowExecutionId =
          Prelude.Nothing,
        statusDetails = Prelude.Nothing,
        maxErrors = Prelude.Nothing,
        endTime = Prelude.Nothing,
        type' = Prelude.Nothing,
        maxConcurrency = Prelude.Nothing,
        serviceRole = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The status of the task.
getMaintenanceWindowExecutionTaskResponse_status :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe MaintenanceWindowExecutionStatus)
getMaintenanceWindowExecutionTaskResponse_status = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {status} -> status) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {status = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The parameters passed to the task when it was run.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
--
-- The map has the following format:
--
-- -   @Key@: string, between 1 and 255 characters
--
-- -   @Value@: an array of strings, each between 1 and 255 characters
getMaintenanceWindowExecutionTaskResponse_taskParameters :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe [Prelude.HashMap Prelude.Text MaintenanceWindowTaskParameterValueExpression])
getMaintenanceWindowExecutionTaskResponse_taskParameters = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {taskParameters} -> taskParameters) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {taskParameters = a} :: GetMaintenanceWindowExecutionTaskResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The ID of the specific task execution in the maintenance window task
-- that was retrieved.
getMaintenanceWindowExecutionTaskResponse_taskExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_taskExecutionId = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {taskExecutionId} -> taskExecutionId) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {taskExecutionId = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The priority of the task.
getMaintenanceWindowExecutionTaskResponse_priority :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Natural)
getMaintenanceWindowExecutionTaskResponse_priority = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {priority} -> priority) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {priority = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The time the task execution started.
getMaintenanceWindowExecutionTaskResponse_startTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.UTCTime)
getMaintenanceWindowExecutionTaskResponse_startTime = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {startTime} -> startTime) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {startTime = a} :: GetMaintenanceWindowExecutionTaskResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the task that ran.
getMaintenanceWindowExecutionTaskResponse_taskArn :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_taskArn = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {taskArn} -> taskArn) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {taskArn = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The ID of the maintenance window execution that includes the task.
getMaintenanceWindowExecutionTaskResponse_windowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_windowExecutionId = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {windowExecutionId} -> windowExecutionId) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {windowExecutionId = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The details explaining the status. Not available for all status values.
getMaintenanceWindowExecutionTaskResponse_statusDetails :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_statusDetails = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {statusDetails} -> statusDetails) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {statusDetails = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The defined maximum number of task execution errors allowed before
-- scheduling of the task execution would have been stopped.
getMaintenanceWindowExecutionTaskResponse_maxErrors :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_maxErrors = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {maxErrors} -> maxErrors) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {maxErrors = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The time the task execution completed.
getMaintenanceWindowExecutionTaskResponse_endTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.UTCTime)
getMaintenanceWindowExecutionTaskResponse_endTime = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {endTime} -> endTime) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {endTime = a} :: GetMaintenanceWindowExecutionTaskResponse) Prelude.. Lens.mapping Core._Time

-- | The type of task that was run.
getMaintenanceWindowExecutionTaskResponse_type :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe MaintenanceWindowTaskType)
getMaintenanceWindowExecutionTaskResponse_type = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {type'} -> type') (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {type' = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The defined maximum number of task executions that could be run in
-- parallel.
getMaintenanceWindowExecutionTaskResponse_maxConcurrency :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_maxConcurrency = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {maxConcurrency} -> maxConcurrency) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {maxConcurrency = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The role that was assumed when running the task.
getMaintenanceWindowExecutionTaskResponse_serviceRole :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_serviceRole = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {serviceRole} -> serviceRole) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {serviceRole = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The response's http status code.
getMaintenanceWindowExecutionTaskResponse_httpStatus :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse Prelude.Int
getMaintenanceWindowExecutionTaskResponse_httpStatus = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {httpStatus} -> httpStatus) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {httpStatus = a} :: GetMaintenanceWindowExecutionTaskResponse)

instance
  Prelude.NFData
    GetMaintenanceWindowExecutionTaskResponse
