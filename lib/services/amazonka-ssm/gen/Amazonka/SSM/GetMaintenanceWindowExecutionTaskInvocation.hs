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
-- Module      : Amazonka.SSM.GetMaintenanceWindowExecutionTaskInvocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specific task running on a specific
-- target.
module Amazonka.SSM.GetMaintenanceWindowExecutionTaskInvocation
  ( -- * Creating a Request
    GetMaintenanceWindowExecutionTaskInvocation (..),
    newGetMaintenanceWindowExecutionTaskInvocation,

    -- * Request Lenses
    getMaintenanceWindowExecutionTaskInvocation_windowExecutionId,
    getMaintenanceWindowExecutionTaskInvocation_taskId,
    getMaintenanceWindowExecutionTaskInvocation_invocationId,

    -- * Destructuring the Response
    GetMaintenanceWindowExecutionTaskInvocationResponse (..),
    newGetMaintenanceWindowExecutionTaskInvocationResponse,

    -- * Response Lenses
    getMaintenanceWindowExecutionTaskInvocationResponse_windowTargetId,
    getMaintenanceWindowExecutionTaskInvocationResponse_windowExecutionId,
    getMaintenanceWindowExecutionTaskInvocationResponse_invocationId,
    getMaintenanceWindowExecutionTaskInvocationResponse_statusDetails,
    getMaintenanceWindowExecutionTaskInvocationResponse_status,
    getMaintenanceWindowExecutionTaskInvocationResponse_endTime,
    getMaintenanceWindowExecutionTaskInvocationResponse_taskType,
    getMaintenanceWindowExecutionTaskInvocationResponse_executionId,
    getMaintenanceWindowExecutionTaskInvocationResponse_ownerInformation,
    getMaintenanceWindowExecutionTaskInvocationResponse_startTime,
    getMaintenanceWindowExecutionTaskInvocationResponse_taskExecutionId,
    getMaintenanceWindowExecutionTaskInvocationResponse_parameters,
    getMaintenanceWindowExecutionTaskInvocationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetMaintenanceWindowExecutionTaskInvocation' smart constructor.
data GetMaintenanceWindowExecutionTaskInvocation = GetMaintenanceWindowExecutionTaskInvocation'
  { -- | The ID of the maintenance window execution for which the task is a part.
    windowExecutionId :: Prelude.Text,
    -- | The ID of the specific task in the maintenance window task that should
    -- be retrieved.
    taskId :: Prelude.Text,
    -- | The invocation ID to retrieve.
    invocationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMaintenanceWindowExecutionTaskInvocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowExecutionId', 'getMaintenanceWindowExecutionTaskInvocation_windowExecutionId' - The ID of the maintenance window execution for which the task is a part.
--
-- 'taskId', 'getMaintenanceWindowExecutionTaskInvocation_taskId' - The ID of the specific task in the maintenance window task that should
-- be retrieved.
--
-- 'invocationId', 'getMaintenanceWindowExecutionTaskInvocation_invocationId' - The invocation ID to retrieve.
newGetMaintenanceWindowExecutionTaskInvocation ::
  -- | 'windowExecutionId'
  Prelude.Text ->
  -- | 'taskId'
  Prelude.Text ->
  -- | 'invocationId'
  Prelude.Text ->
  GetMaintenanceWindowExecutionTaskInvocation
newGetMaintenanceWindowExecutionTaskInvocation
  pWindowExecutionId_
  pTaskId_
  pInvocationId_ =
    GetMaintenanceWindowExecutionTaskInvocation'
      { windowExecutionId =
          pWindowExecutionId_,
        taskId = pTaskId_,
        invocationId = pInvocationId_
      }

-- | The ID of the maintenance window execution for which the task is a part.
getMaintenanceWindowExecutionTaskInvocation_windowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocation Prelude.Text
getMaintenanceWindowExecutionTaskInvocation_windowExecutionId = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocation' {windowExecutionId} -> windowExecutionId) (\s@GetMaintenanceWindowExecutionTaskInvocation' {} a -> s {windowExecutionId = a} :: GetMaintenanceWindowExecutionTaskInvocation)

-- | The ID of the specific task in the maintenance window task that should
-- be retrieved.
getMaintenanceWindowExecutionTaskInvocation_taskId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocation Prelude.Text
getMaintenanceWindowExecutionTaskInvocation_taskId = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocation' {taskId} -> taskId) (\s@GetMaintenanceWindowExecutionTaskInvocation' {} a -> s {taskId = a} :: GetMaintenanceWindowExecutionTaskInvocation)

-- | The invocation ID to retrieve.
getMaintenanceWindowExecutionTaskInvocation_invocationId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocation Prelude.Text
getMaintenanceWindowExecutionTaskInvocation_invocationId = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocation' {invocationId} -> invocationId) (\s@GetMaintenanceWindowExecutionTaskInvocation' {} a -> s {invocationId = a} :: GetMaintenanceWindowExecutionTaskInvocation)

instance
  Core.AWSRequest
    GetMaintenanceWindowExecutionTaskInvocation
  where
  type
    AWSResponse
      GetMaintenanceWindowExecutionTaskInvocation =
      GetMaintenanceWindowExecutionTaskInvocationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMaintenanceWindowExecutionTaskInvocationResponse'
            Prelude.<$> (x Data..?> "WindowTargetId")
              Prelude.<*> (x Data..?> "WindowExecutionId")
              Prelude.<*> (x Data..?> "InvocationId")
              Prelude.<*> (x Data..?> "StatusDetails")
              Prelude.<*> (x Data..?> "Status")
              Prelude.<*> (x Data..?> "EndTime")
              Prelude.<*> (x Data..?> "TaskType")
              Prelude.<*> (x Data..?> "ExecutionId")
              Prelude.<*> (x Data..?> "OwnerInformation")
              Prelude.<*> (x Data..?> "StartTime")
              Prelude.<*> (x Data..?> "TaskExecutionId")
              Prelude.<*> (x Data..?> "Parameters")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetMaintenanceWindowExecutionTaskInvocation
  where
  hashWithSalt
    _salt
    GetMaintenanceWindowExecutionTaskInvocation' {..} =
      _salt `Prelude.hashWithSalt` windowExecutionId
        `Prelude.hashWithSalt` taskId
        `Prelude.hashWithSalt` invocationId

instance
  Prelude.NFData
    GetMaintenanceWindowExecutionTaskInvocation
  where
  rnf GetMaintenanceWindowExecutionTaskInvocation' {..} =
    Prelude.rnf windowExecutionId
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf invocationId

instance
  Data.ToHeaders
    GetMaintenanceWindowExecutionTaskInvocation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.GetMaintenanceWindowExecutionTaskInvocation" ::
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
    GetMaintenanceWindowExecutionTaskInvocation
  where
  toJSON
    GetMaintenanceWindowExecutionTaskInvocation' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("WindowExecutionId" Data..= windowExecutionId),
              Prelude.Just ("TaskId" Data..= taskId),
              Prelude.Just ("InvocationId" Data..= invocationId)
            ]
        )

instance
  Data.ToPath
    GetMaintenanceWindowExecutionTaskInvocation
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetMaintenanceWindowExecutionTaskInvocation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMaintenanceWindowExecutionTaskInvocationResponse' smart constructor.
data GetMaintenanceWindowExecutionTaskInvocationResponse = GetMaintenanceWindowExecutionTaskInvocationResponse'
  { -- | The maintenance window target ID.
    windowTargetId :: Prelude.Maybe Prelude.Text,
    -- | The maintenance window execution ID.
    windowExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The invocation ID.
    invocationId :: Prelude.Maybe Prelude.Text,
    -- | The details explaining the status. Details are only available for
    -- certain status values.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The task status for an invocation.
    status :: Prelude.Maybe MaintenanceWindowExecutionStatus,
    -- | The time that the task finished running on the target.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Retrieves the task type for a maintenance window.
    taskType :: Prelude.Maybe MaintenanceWindowTaskType,
    -- | The execution ID.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | User-provided value to be included in any Amazon CloudWatch Events or
    -- Amazon EventBridge events raised while running tasks for these targets
    -- in this maintenance window.
    ownerInformation :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The time that the task started running on the target.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The task execution ID.
    taskExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The parameters used at the time that the task ran.
    parameters :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMaintenanceWindowExecutionTaskInvocationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowTargetId', 'getMaintenanceWindowExecutionTaskInvocationResponse_windowTargetId' - The maintenance window target ID.
--
-- 'windowExecutionId', 'getMaintenanceWindowExecutionTaskInvocationResponse_windowExecutionId' - The maintenance window execution ID.
--
-- 'invocationId', 'getMaintenanceWindowExecutionTaskInvocationResponse_invocationId' - The invocation ID.
--
-- 'statusDetails', 'getMaintenanceWindowExecutionTaskInvocationResponse_statusDetails' - The details explaining the status. Details are only available for
-- certain status values.
--
-- 'status', 'getMaintenanceWindowExecutionTaskInvocationResponse_status' - The task status for an invocation.
--
-- 'endTime', 'getMaintenanceWindowExecutionTaskInvocationResponse_endTime' - The time that the task finished running on the target.
--
-- 'taskType', 'getMaintenanceWindowExecutionTaskInvocationResponse_taskType' - Retrieves the task type for a maintenance window.
--
-- 'executionId', 'getMaintenanceWindowExecutionTaskInvocationResponse_executionId' - The execution ID.
--
-- 'ownerInformation', 'getMaintenanceWindowExecutionTaskInvocationResponse_ownerInformation' - User-provided value to be included in any Amazon CloudWatch Events or
-- Amazon EventBridge events raised while running tasks for these targets
-- in this maintenance window.
--
-- 'startTime', 'getMaintenanceWindowExecutionTaskInvocationResponse_startTime' - The time that the task started running on the target.
--
-- 'taskExecutionId', 'getMaintenanceWindowExecutionTaskInvocationResponse_taskExecutionId' - The task execution ID.
--
-- 'parameters', 'getMaintenanceWindowExecutionTaskInvocationResponse_parameters' - The parameters used at the time that the task ran.
--
-- 'httpStatus', 'getMaintenanceWindowExecutionTaskInvocationResponse_httpStatus' - The response's http status code.
newGetMaintenanceWindowExecutionTaskInvocationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMaintenanceWindowExecutionTaskInvocationResponse
newGetMaintenanceWindowExecutionTaskInvocationResponse
  pHttpStatus_ =
    GetMaintenanceWindowExecutionTaskInvocationResponse'
      { windowTargetId =
          Prelude.Nothing,
        windowExecutionId =
          Prelude.Nothing,
        invocationId =
          Prelude.Nothing,
        statusDetails =
          Prelude.Nothing,
        status =
          Prelude.Nothing,
        endTime =
          Prelude.Nothing,
        taskType =
          Prelude.Nothing,
        executionId =
          Prelude.Nothing,
        ownerInformation =
          Prelude.Nothing,
        startTime =
          Prelude.Nothing,
        taskExecutionId =
          Prelude.Nothing,
        parameters =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The maintenance window target ID.
getMaintenanceWindowExecutionTaskInvocationResponse_windowTargetId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskInvocationResponse_windowTargetId = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {windowTargetId} -> windowTargetId) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {windowTargetId = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

-- | The maintenance window execution ID.
getMaintenanceWindowExecutionTaskInvocationResponse_windowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskInvocationResponse_windowExecutionId = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {windowExecutionId} -> windowExecutionId) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {windowExecutionId = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

-- | The invocation ID.
getMaintenanceWindowExecutionTaskInvocationResponse_invocationId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskInvocationResponse_invocationId = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {invocationId} -> invocationId) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {invocationId = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

-- | The details explaining the status. Details are only available for
-- certain status values.
getMaintenanceWindowExecutionTaskInvocationResponse_statusDetails :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskInvocationResponse_statusDetails = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {statusDetails} -> statusDetails) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {statusDetails = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

-- | The task status for an invocation.
getMaintenanceWindowExecutionTaskInvocationResponse_status :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe MaintenanceWindowExecutionStatus)
getMaintenanceWindowExecutionTaskInvocationResponse_status = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {status} -> status) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {status = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

-- | The time that the task finished running on the target.
getMaintenanceWindowExecutionTaskInvocationResponse_endTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.UTCTime)
getMaintenanceWindowExecutionTaskInvocationResponse_endTime = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {endTime} -> endTime) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {endTime = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse) Prelude.. Lens.mapping Data._Time

-- | Retrieves the task type for a maintenance window.
getMaintenanceWindowExecutionTaskInvocationResponse_taskType :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe MaintenanceWindowTaskType)
getMaintenanceWindowExecutionTaskInvocationResponse_taskType = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {taskType} -> taskType) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {taskType = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

-- | The execution ID.
getMaintenanceWindowExecutionTaskInvocationResponse_executionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskInvocationResponse_executionId = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {executionId} -> executionId) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {executionId = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

-- | User-provided value to be included in any Amazon CloudWatch Events or
-- Amazon EventBridge events raised while running tasks for these targets
-- in this maintenance window.
getMaintenanceWindowExecutionTaskInvocationResponse_ownerInformation :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskInvocationResponse_ownerInformation = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {ownerInformation} -> ownerInformation) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {ownerInformation = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The time that the task started running on the target.
getMaintenanceWindowExecutionTaskInvocationResponse_startTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.UTCTime)
getMaintenanceWindowExecutionTaskInvocationResponse_startTime = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {startTime} -> startTime) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {startTime = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse) Prelude.. Lens.mapping Data._Time

-- | The task execution ID.
getMaintenanceWindowExecutionTaskInvocationResponse_taskExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskInvocationResponse_taskExecutionId = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {taskExecutionId} -> taskExecutionId) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {taskExecutionId = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

-- | The parameters used at the time that the task ran.
getMaintenanceWindowExecutionTaskInvocationResponse_parameters :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskInvocationResponse_parameters = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {parameters} -> parameters) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {parameters = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
getMaintenanceWindowExecutionTaskInvocationResponse_httpStatus :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse Prelude.Int
getMaintenanceWindowExecutionTaskInvocationResponse_httpStatus = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {httpStatus} -> httpStatus) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {httpStatus = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

instance
  Prelude.NFData
    GetMaintenanceWindowExecutionTaskInvocationResponse
  where
  rnf
    GetMaintenanceWindowExecutionTaskInvocationResponse' {..} =
      Prelude.rnf windowTargetId
        `Prelude.seq` Prelude.rnf windowExecutionId
        `Prelude.seq` Prelude.rnf invocationId
        `Prelude.seq` Prelude.rnf statusDetails
        `Prelude.seq` Prelude.rnf status
        `Prelude.seq` Prelude.rnf endTime
        `Prelude.seq` Prelude.rnf taskType
        `Prelude.seq` Prelude.rnf executionId
        `Prelude.seq` Prelude.rnf ownerInformation
        `Prelude.seq` Prelude.rnf startTime
        `Prelude.seq` Prelude.rnf taskExecutionId
        `Prelude.seq` Prelude.rnf parameters
        `Prelude.seq` Prelude.rnf httpStatus
