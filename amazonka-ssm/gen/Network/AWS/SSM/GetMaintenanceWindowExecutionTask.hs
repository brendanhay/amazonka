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
-- Module      : Network.AWS.SSM.GetMaintenanceWindowExecutionTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details about a specific task run as part of a maintenance
-- window execution.
module Network.AWS.SSM.GetMaintenanceWindowExecutionTask
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
    getMaintenanceWindowExecutionTaskResponse_maxErrors,
    getMaintenanceWindowExecutionTaskResponse_taskParameters,
    getMaintenanceWindowExecutionTaskResponse_status,
    getMaintenanceWindowExecutionTaskResponse_serviceRole,
    getMaintenanceWindowExecutionTaskResponse_statusDetails,
    getMaintenanceWindowExecutionTaskResponse_startTime,
    getMaintenanceWindowExecutionTaskResponse_priority,
    getMaintenanceWindowExecutionTaskResponse_endTime,
    getMaintenanceWindowExecutionTaskResponse_maxConcurrency,
    getMaintenanceWindowExecutionTaskResponse_windowExecutionId,
    getMaintenanceWindowExecutionTaskResponse_type,
    getMaintenanceWindowExecutionTaskResponse_taskArn,
    getMaintenanceWindowExecutionTaskResponse_taskExecutionId,
    getMaintenanceWindowExecutionTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

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
            Prelude.<$> (x Core..?> "MaxErrors")
              Prelude.<*> (x Core..?> "TaskParameters" Core..!@ Prelude.mempty)
              Prelude.<*> (x Core..?> "Status")
              Prelude.<*> (x Core..?> "ServiceRole")
              Prelude.<*> (x Core..?> "StatusDetails")
              Prelude.<*> (x Core..?> "StartTime")
              Prelude.<*> (x Core..?> "Priority")
              Prelude.<*> (x Core..?> "EndTime")
              Prelude.<*> (x Core..?> "MaxConcurrency")
              Prelude.<*> (x Core..?> "WindowExecutionId")
              Prelude.<*> (x Core..?> "Type")
              Prelude.<*> (x Core..?> "TaskArn")
              Prelude.<*> (x Core..?> "TaskExecutionId")
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
  { -- | The defined maximum number of task execution errors allowed before
    -- scheduling of the task execution would have been stopped.
    maxErrors :: Prelude.Maybe Prelude.Text,
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
    -- Key: string, between 1 and 255 characters
    --
    -- Value: an array of strings, each string is between 1 and 255 characters
    taskParameters :: Prelude.Maybe (Core.Sensitive [Core.Sensitive (Prelude.HashMap Prelude.Text (Core.Sensitive MaintenanceWindowTaskParameterValueExpression))]),
    -- | The status of the task.
    status :: Prelude.Maybe MaintenanceWindowExecutionStatus,
    -- | The role that was assumed when running the task.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The details explaining the Status. Only available for certain status
    -- values.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The time the task execution started.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The priority of the task.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The time the task execution completed.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The defined maximum number of task executions that could be run in
    -- parallel.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window execution that includes the task.
    windowExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The type of task that was run.
    type' :: Prelude.Maybe MaintenanceWindowTaskType,
    -- | The ARN of the task that ran.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the specific task execution in the maintenance window task
    -- that was retrieved.
    taskExecutionId :: Prelude.Maybe Prelude.Text,
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
-- 'maxErrors', 'getMaintenanceWindowExecutionTaskResponse_maxErrors' - The defined maximum number of task execution errors allowed before
-- scheduling of the task execution would have been stopped.
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
-- Key: string, between 1 and 255 characters
--
-- Value: an array of strings, each string is between 1 and 255 characters
--
-- 'status', 'getMaintenanceWindowExecutionTaskResponse_status' - The status of the task.
--
-- 'serviceRole', 'getMaintenanceWindowExecutionTaskResponse_serviceRole' - The role that was assumed when running the task.
--
-- 'statusDetails', 'getMaintenanceWindowExecutionTaskResponse_statusDetails' - The details explaining the Status. Only available for certain status
-- values.
--
-- 'startTime', 'getMaintenanceWindowExecutionTaskResponse_startTime' - The time the task execution started.
--
-- 'priority', 'getMaintenanceWindowExecutionTaskResponse_priority' - The priority of the task.
--
-- 'endTime', 'getMaintenanceWindowExecutionTaskResponse_endTime' - The time the task execution completed.
--
-- 'maxConcurrency', 'getMaintenanceWindowExecutionTaskResponse_maxConcurrency' - The defined maximum number of task executions that could be run in
-- parallel.
--
-- 'windowExecutionId', 'getMaintenanceWindowExecutionTaskResponse_windowExecutionId' - The ID of the maintenance window execution that includes the task.
--
-- 'type'', 'getMaintenanceWindowExecutionTaskResponse_type' - The type of task that was run.
--
-- 'taskArn', 'getMaintenanceWindowExecutionTaskResponse_taskArn' - The ARN of the task that ran.
--
-- 'taskExecutionId', 'getMaintenanceWindowExecutionTaskResponse_taskExecutionId' - The ID of the specific task execution in the maintenance window task
-- that was retrieved.
--
-- 'httpStatus', 'getMaintenanceWindowExecutionTaskResponse_httpStatus' - The response's http status code.
newGetMaintenanceWindowExecutionTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMaintenanceWindowExecutionTaskResponse
newGetMaintenanceWindowExecutionTaskResponse
  pHttpStatus_ =
    GetMaintenanceWindowExecutionTaskResponse'
      { maxErrors =
          Prelude.Nothing,
        taskParameters = Prelude.Nothing,
        status = Prelude.Nothing,
        serviceRole = Prelude.Nothing,
        statusDetails = Prelude.Nothing,
        startTime = Prelude.Nothing,
        priority = Prelude.Nothing,
        endTime = Prelude.Nothing,
        maxConcurrency = Prelude.Nothing,
        windowExecutionId =
          Prelude.Nothing,
        type' = Prelude.Nothing,
        taskArn = Prelude.Nothing,
        taskExecutionId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The defined maximum number of task execution errors allowed before
-- scheduling of the task execution would have been stopped.
getMaintenanceWindowExecutionTaskResponse_maxErrors :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_maxErrors = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {maxErrors} -> maxErrors) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {maxErrors = a} :: GetMaintenanceWindowExecutionTaskResponse)

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
-- Key: string, between 1 and 255 characters
--
-- Value: an array of strings, each string is between 1 and 255 characters
getMaintenanceWindowExecutionTaskResponse_taskParameters :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe [Prelude.HashMap Prelude.Text MaintenanceWindowTaskParameterValueExpression])
getMaintenanceWindowExecutionTaskResponse_taskParameters = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {taskParameters} -> taskParameters) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {taskParameters = a} :: GetMaintenanceWindowExecutionTaskResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens._Coerce)

-- | The status of the task.
getMaintenanceWindowExecutionTaskResponse_status :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe MaintenanceWindowExecutionStatus)
getMaintenanceWindowExecutionTaskResponse_status = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {status} -> status) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {status = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The role that was assumed when running the task.
getMaintenanceWindowExecutionTaskResponse_serviceRole :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_serviceRole = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {serviceRole} -> serviceRole) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {serviceRole = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The details explaining the Status. Only available for certain status
-- values.
getMaintenanceWindowExecutionTaskResponse_statusDetails :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_statusDetails = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {statusDetails} -> statusDetails) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {statusDetails = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The time the task execution started.
getMaintenanceWindowExecutionTaskResponse_startTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.UTCTime)
getMaintenanceWindowExecutionTaskResponse_startTime = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {startTime} -> startTime) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {startTime = a} :: GetMaintenanceWindowExecutionTaskResponse) Prelude.. Lens.mapping Core._Time

-- | The priority of the task.
getMaintenanceWindowExecutionTaskResponse_priority :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Natural)
getMaintenanceWindowExecutionTaskResponse_priority = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {priority} -> priority) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {priority = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The time the task execution completed.
getMaintenanceWindowExecutionTaskResponse_endTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.UTCTime)
getMaintenanceWindowExecutionTaskResponse_endTime = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {endTime} -> endTime) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {endTime = a} :: GetMaintenanceWindowExecutionTaskResponse) Prelude.. Lens.mapping Core._Time

-- | The defined maximum number of task executions that could be run in
-- parallel.
getMaintenanceWindowExecutionTaskResponse_maxConcurrency :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_maxConcurrency = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {maxConcurrency} -> maxConcurrency) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {maxConcurrency = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The ID of the maintenance window execution that includes the task.
getMaintenanceWindowExecutionTaskResponse_windowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_windowExecutionId = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {windowExecutionId} -> windowExecutionId) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {windowExecutionId = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The type of task that was run.
getMaintenanceWindowExecutionTaskResponse_type :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe MaintenanceWindowTaskType)
getMaintenanceWindowExecutionTaskResponse_type = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {type'} -> type') (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {type' = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The ARN of the task that ran.
getMaintenanceWindowExecutionTaskResponse_taskArn :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_taskArn = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {taskArn} -> taskArn) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {taskArn = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The ID of the specific task execution in the maintenance window task
-- that was retrieved.
getMaintenanceWindowExecutionTaskResponse_taskExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_taskExecutionId = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {taskExecutionId} -> taskExecutionId) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {taskExecutionId = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The response's http status code.
getMaintenanceWindowExecutionTaskResponse_httpStatus :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse Prelude.Int
getMaintenanceWindowExecutionTaskResponse_httpStatus = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {httpStatus} -> httpStatus) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {httpStatus = a} :: GetMaintenanceWindowExecutionTaskResponse)

instance
  Prelude.NFData
    GetMaintenanceWindowExecutionTaskResponse
