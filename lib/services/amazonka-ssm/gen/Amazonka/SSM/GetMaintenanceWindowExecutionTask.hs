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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    getMaintenanceWindowExecutionTaskResponse_type,
    getMaintenanceWindowExecutionTaskResponse_windowExecutionId,
    getMaintenanceWindowExecutionTaskResponse_taskParameters,
    getMaintenanceWindowExecutionTaskResponse_taskArn,
    getMaintenanceWindowExecutionTaskResponse_statusDetails,
    getMaintenanceWindowExecutionTaskResponse_status,
    getMaintenanceWindowExecutionTaskResponse_endTime,
    getMaintenanceWindowExecutionTaskResponse_serviceRole,
    getMaintenanceWindowExecutionTaskResponse_alarmConfiguration,
    getMaintenanceWindowExecutionTaskResponse_priority,
    getMaintenanceWindowExecutionTaskResponse_maxConcurrency,
    getMaintenanceWindowExecutionTaskResponse_maxErrors,
    getMaintenanceWindowExecutionTaskResponse_triggeredAlarms,
    getMaintenanceWindowExecutionTaskResponse_startTime,
    getMaintenanceWindowExecutionTaskResponse_taskExecutionId,
    getMaintenanceWindowExecutionTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMaintenanceWindowExecutionTaskResponse'
            Prelude.<$> (x Data..?> "Type")
              Prelude.<*> (x Data..?> "WindowExecutionId")
              Prelude.<*> (x Data..?> "TaskParameters" Core..!@ Prelude.mempty)
              Prelude.<*> (x Data..?> "TaskArn")
              Prelude.<*> (x Data..?> "StatusDetails")
              Prelude.<*> (x Data..?> "Status")
              Prelude.<*> (x Data..?> "EndTime")
              Prelude.<*> (x Data..?> "ServiceRole")
              Prelude.<*> (x Data..?> "AlarmConfiguration")
              Prelude.<*> (x Data..?> "Priority")
              Prelude.<*> (x Data..?> "MaxConcurrency")
              Prelude.<*> (x Data..?> "MaxErrors")
              Prelude.<*> (x Data..?> "TriggeredAlarms")
              Prelude.<*> (x Data..?> "StartTime")
              Prelude.<*> (x Data..?> "TaskExecutionId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetMaintenanceWindowExecutionTask
  where
  hashWithSalt
    _salt
    GetMaintenanceWindowExecutionTask' {..} =
      _salt `Prelude.hashWithSalt` windowExecutionId
        `Prelude.hashWithSalt` taskId

instance
  Prelude.NFData
    GetMaintenanceWindowExecutionTask
  where
  rnf GetMaintenanceWindowExecutionTask' {..} =
    Prelude.rnf windowExecutionId
      `Prelude.seq` Prelude.rnf taskId

instance
  Data.ToHeaders
    GetMaintenanceWindowExecutionTask
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.GetMaintenanceWindowExecutionTask" ::
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
    GetMaintenanceWindowExecutionTask
  where
  toJSON GetMaintenanceWindowExecutionTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WindowExecutionId" Data..= windowExecutionId),
            Prelude.Just ("TaskId" Data..= taskId)
          ]
      )

instance
  Data.ToPath
    GetMaintenanceWindowExecutionTask
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetMaintenanceWindowExecutionTask
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMaintenanceWindowExecutionTaskResponse' smart constructor.
data GetMaintenanceWindowExecutionTaskResponse = GetMaintenanceWindowExecutionTaskResponse'
  { -- | The type of task that was run.
    type' :: Prelude.Maybe MaintenanceWindowTaskType,
    -- | The ID of the maintenance window execution that includes the task.
    windowExecutionId :: Prelude.Maybe Prelude.Text,
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
    taskParameters :: Prelude.Maybe (Data.Sensitive [Data.Sensitive (Prelude.HashMap Prelude.Text (Data.Sensitive MaintenanceWindowTaskParameterValueExpression))]),
    -- | The Amazon Resource Name (ARN) of the task that ran.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The details explaining the status. Not available for all status values.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The status of the task.
    status :: Prelude.Maybe MaintenanceWindowExecutionStatus,
    -- | The time the task execution completed.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The role that was assumed when running the task.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The details for the CloudWatch alarm you applied to your maintenance
    -- window task.
    alarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | The priority of the task.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The defined maximum number of task executions that could be run in
    -- parallel.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The defined maximum number of task execution errors allowed before
    -- scheduling of the task execution would have been stopped.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | The CloudWatch alarms that were invoked by the maintenance window task.
    triggeredAlarms :: Prelude.Maybe (Prelude.NonEmpty AlarmStateInformation),
    -- | The time the task execution started.
    startTime :: Prelude.Maybe Data.POSIX,
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
-- 'type'', 'getMaintenanceWindowExecutionTaskResponse_type' - The type of task that was run.
--
-- 'windowExecutionId', 'getMaintenanceWindowExecutionTaskResponse_windowExecutionId' - The ID of the maintenance window execution that includes the task.
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
-- 'taskArn', 'getMaintenanceWindowExecutionTaskResponse_taskArn' - The Amazon Resource Name (ARN) of the task that ran.
--
-- 'statusDetails', 'getMaintenanceWindowExecutionTaskResponse_statusDetails' - The details explaining the status. Not available for all status values.
--
-- 'status', 'getMaintenanceWindowExecutionTaskResponse_status' - The status of the task.
--
-- 'endTime', 'getMaintenanceWindowExecutionTaskResponse_endTime' - The time the task execution completed.
--
-- 'serviceRole', 'getMaintenanceWindowExecutionTaskResponse_serviceRole' - The role that was assumed when running the task.
--
-- 'alarmConfiguration', 'getMaintenanceWindowExecutionTaskResponse_alarmConfiguration' - The details for the CloudWatch alarm you applied to your maintenance
-- window task.
--
-- 'priority', 'getMaintenanceWindowExecutionTaskResponse_priority' - The priority of the task.
--
-- 'maxConcurrency', 'getMaintenanceWindowExecutionTaskResponse_maxConcurrency' - The defined maximum number of task executions that could be run in
-- parallel.
--
-- 'maxErrors', 'getMaintenanceWindowExecutionTaskResponse_maxErrors' - The defined maximum number of task execution errors allowed before
-- scheduling of the task execution would have been stopped.
--
-- 'triggeredAlarms', 'getMaintenanceWindowExecutionTaskResponse_triggeredAlarms' - The CloudWatch alarms that were invoked by the maintenance window task.
--
-- 'startTime', 'getMaintenanceWindowExecutionTaskResponse_startTime' - The time the task execution started.
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
      { type' =
          Prelude.Nothing,
        windowExecutionId =
          Prelude.Nothing,
        taskParameters = Prelude.Nothing,
        taskArn = Prelude.Nothing,
        statusDetails = Prelude.Nothing,
        status = Prelude.Nothing,
        endTime = Prelude.Nothing,
        serviceRole = Prelude.Nothing,
        alarmConfiguration =
          Prelude.Nothing,
        priority = Prelude.Nothing,
        maxConcurrency = Prelude.Nothing,
        maxErrors = Prelude.Nothing,
        triggeredAlarms =
          Prelude.Nothing,
        startTime = Prelude.Nothing,
        taskExecutionId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The type of task that was run.
getMaintenanceWindowExecutionTaskResponse_type :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe MaintenanceWindowTaskType)
getMaintenanceWindowExecutionTaskResponse_type = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {type'} -> type') (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {type' = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The ID of the maintenance window execution that includes the task.
getMaintenanceWindowExecutionTaskResponse_windowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_windowExecutionId = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {windowExecutionId} -> windowExecutionId) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {windowExecutionId = a} :: GetMaintenanceWindowExecutionTaskResponse)

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
getMaintenanceWindowExecutionTaskResponse_taskParameters = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {taskParameters} -> taskParameters) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {taskParameters = a} :: GetMaintenanceWindowExecutionTaskResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The Amazon Resource Name (ARN) of the task that ran.
getMaintenanceWindowExecutionTaskResponse_taskArn :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_taskArn = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {taskArn} -> taskArn) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {taskArn = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The details explaining the status. Not available for all status values.
getMaintenanceWindowExecutionTaskResponse_statusDetails :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_statusDetails = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {statusDetails} -> statusDetails) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {statusDetails = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The status of the task.
getMaintenanceWindowExecutionTaskResponse_status :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe MaintenanceWindowExecutionStatus)
getMaintenanceWindowExecutionTaskResponse_status = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {status} -> status) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {status = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The time the task execution completed.
getMaintenanceWindowExecutionTaskResponse_endTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.UTCTime)
getMaintenanceWindowExecutionTaskResponse_endTime = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {endTime} -> endTime) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {endTime = a} :: GetMaintenanceWindowExecutionTaskResponse) Prelude.. Lens.mapping Data._Time

-- | The role that was assumed when running the task.
getMaintenanceWindowExecutionTaskResponse_serviceRole :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_serviceRole = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {serviceRole} -> serviceRole) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {serviceRole = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The details for the CloudWatch alarm you applied to your maintenance
-- window task.
getMaintenanceWindowExecutionTaskResponse_alarmConfiguration :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe AlarmConfiguration)
getMaintenanceWindowExecutionTaskResponse_alarmConfiguration = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {alarmConfiguration} -> alarmConfiguration) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {alarmConfiguration = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The priority of the task.
getMaintenanceWindowExecutionTaskResponse_priority :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Natural)
getMaintenanceWindowExecutionTaskResponse_priority = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {priority} -> priority) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {priority = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The defined maximum number of task executions that could be run in
-- parallel.
getMaintenanceWindowExecutionTaskResponse_maxConcurrency :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_maxConcurrency = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {maxConcurrency} -> maxConcurrency) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {maxConcurrency = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The defined maximum number of task execution errors allowed before
-- scheduling of the task execution would have been stopped.
getMaintenanceWindowExecutionTaskResponse_maxErrors :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskResponse_maxErrors = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {maxErrors} -> maxErrors) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {maxErrors = a} :: GetMaintenanceWindowExecutionTaskResponse)

-- | The CloudWatch alarms that were invoked by the maintenance window task.
getMaintenanceWindowExecutionTaskResponse_triggeredAlarms :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe (Prelude.NonEmpty AlarmStateInformation))
getMaintenanceWindowExecutionTaskResponse_triggeredAlarms = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {triggeredAlarms} -> triggeredAlarms) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {triggeredAlarms = a} :: GetMaintenanceWindowExecutionTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time the task execution started.
getMaintenanceWindowExecutionTaskResponse_startTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskResponse (Prelude.Maybe Prelude.UTCTime)
getMaintenanceWindowExecutionTaskResponse_startTime = Lens.lens (\GetMaintenanceWindowExecutionTaskResponse' {startTime} -> startTime) (\s@GetMaintenanceWindowExecutionTaskResponse' {} a -> s {startTime = a} :: GetMaintenanceWindowExecutionTaskResponse) Prelude.. Lens.mapping Data._Time

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
  where
  rnf GetMaintenanceWindowExecutionTaskResponse' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf windowExecutionId
      `Prelude.seq` Prelude.rnf taskParameters
      `Prelude.seq` Prelude.rnf taskArn
      `Prelude.seq` Prelude.rnf statusDetails
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf serviceRole
      `Prelude.seq` Prelude.rnf alarmConfiguration
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf maxConcurrency
      `Prelude.seq` Prelude.rnf maxErrors
      `Prelude.seq` Prelude.rnf triggeredAlarms
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf taskExecutionId
      `Prelude.seq` Prelude.rnf httpStatus
