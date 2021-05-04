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
-- Module      : Network.AWS.SSM.GetMaintenanceWindowExecutionTaskInvocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specific task running on a specific
-- target.
module Network.AWS.SSM.GetMaintenanceWindowExecutionTaskInvocation
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
    getMaintenanceWindowExecutionTaskInvocationResponse_status,
    getMaintenanceWindowExecutionTaskInvocationResponse_statusDetails,
    getMaintenanceWindowExecutionTaskInvocationResponse_windowTargetId,
    getMaintenanceWindowExecutionTaskInvocationResponse_startTime,
    getMaintenanceWindowExecutionTaskInvocationResponse_endTime,
    getMaintenanceWindowExecutionTaskInvocationResponse_executionId,
    getMaintenanceWindowExecutionTaskInvocationResponse_windowExecutionId,
    getMaintenanceWindowExecutionTaskInvocationResponse_ownerInformation,
    getMaintenanceWindowExecutionTaskInvocationResponse_taskType,
    getMaintenanceWindowExecutionTaskInvocationResponse_invocationId,
    getMaintenanceWindowExecutionTaskInvocationResponse_parameters,
    getMaintenanceWindowExecutionTaskInvocationResponse_taskExecutionId,
    getMaintenanceWindowExecutionTaskInvocationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    GetMaintenanceWindowExecutionTaskInvocation
  where
  type
    Rs GetMaintenanceWindowExecutionTaskInvocation =
      GetMaintenanceWindowExecutionTaskInvocationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMaintenanceWindowExecutionTaskInvocationResponse'
            Prelude.<$> (x Prelude..?> "Status")
              Prelude.<*> (x Prelude..?> "StatusDetails")
              Prelude.<*> (x Prelude..?> "WindowTargetId")
              Prelude.<*> (x Prelude..?> "StartTime")
              Prelude.<*> (x Prelude..?> "EndTime")
              Prelude.<*> (x Prelude..?> "ExecutionId")
              Prelude.<*> (x Prelude..?> "WindowExecutionId")
              Prelude.<*> (x Prelude..?> "OwnerInformation")
              Prelude.<*> (x Prelude..?> "TaskType")
              Prelude.<*> (x Prelude..?> "InvocationId")
              Prelude.<*> (x Prelude..?> "Parameters")
              Prelude.<*> (x Prelude..?> "TaskExecutionId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetMaintenanceWindowExecutionTaskInvocation

instance
  Prelude.NFData
    GetMaintenanceWindowExecutionTaskInvocation

instance
  Prelude.ToHeaders
    GetMaintenanceWindowExecutionTaskInvocation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.GetMaintenanceWindowExecutionTaskInvocation" ::
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
    GetMaintenanceWindowExecutionTaskInvocation
  where
  toJSON
    GetMaintenanceWindowExecutionTaskInvocation' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("WindowExecutionId" Prelude..= windowExecutionId),
              Prelude.Just ("TaskId" Prelude..= taskId),
              Prelude.Just
                ("InvocationId" Prelude..= invocationId)
            ]
        )

instance
  Prelude.ToPath
    GetMaintenanceWindowExecutionTaskInvocation
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    GetMaintenanceWindowExecutionTaskInvocation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMaintenanceWindowExecutionTaskInvocationResponse' smart constructor.
data GetMaintenanceWindowExecutionTaskInvocationResponse = GetMaintenanceWindowExecutionTaskInvocationResponse'
  { -- | The task status for an invocation.
    status :: Prelude.Maybe MaintenanceWindowExecutionStatus,
    -- | The details explaining the status. Details are only available for
    -- certain status values.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The maintenance window target ID.
    windowTargetId :: Prelude.Maybe Prelude.Text,
    -- | The time that the task started running on the target.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time that the task finished running on the target.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | The execution ID.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | The maintenance window execution ID.
    windowExecutionId :: Prelude.Maybe Prelude.Text,
    -- | User-provided value to be included in any CloudWatch events raised while
    -- running tasks for these targets in this maintenance window.
    ownerInformation :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | Retrieves the task type for a maintenance window. Task types include the
    -- following: LAMBDA, STEP_FUNCTIONS, AUTOMATION, RUN_COMMAND.
    taskType :: Prelude.Maybe MaintenanceWindowTaskType,
    -- | The invocation ID.
    invocationId :: Prelude.Maybe Prelude.Text,
    -- | The parameters used at the time that the task ran.
    parameters :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The task execution ID.
    taskExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetMaintenanceWindowExecutionTaskInvocationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getMaintenanceWindowExecutionTaskInvocationResponse_status' - The task status for an invocation.
--
-- 'statusDetails', 'getMaintenanceWindowExecutionTaskInvocationResponse_statusDetails' - The details explaining the status. Details are only available for
-- certain status values.
--
-- 'windowTargetId', 'getMaintenanceWindowExecutionTaskInvocationResponse_windowTargetId' - The maintenance window target ID.
--
-- 'startTime', 'getMaintenanceWindowExecutionTaskInvocationResponse_startTime' - The time that the task started running on the target.
--
-- 'endTime', 'getMaintenanceWindowExecutionTaskInvocationResponse_endTime' - The time that the task finished running on the target.
--
-- 'executionId', 'getMaintenanceWindowExecutionTaskInvocationResponse_executionId' - The execution ID.
--
-- 'windowExecutionId', 'getMaintenanceWindowExecutionTaskInvocationResponse_windowExecutionId' - The maintenance window execution ID.
--
-- 'ownerInformation', 'getMaintenanceWindowExecutionTaskInvocationResponse_ownerInformation' - User-provided value to be included in any CloudWatch events raised while
-- running tasks for these targets in this maintenance window.
--
-- 'taskType', 'getMaintenanceWindowExecutionTaskInvocationResponse_taskType' - Retrieves the task type for a maintenance window. Task types include the
-- following: LAMBDA, STEP_FUNCTIONS, AUTOMATION, RUN_COMMAND.
--
-- 'invocationId', 'getMaintenanceWindowExecutionTaskInvocationResponse_invocationId' - The invocation ID.
--
-- 'parameters', 'getMaintenanceWindowExecutionTaskInvocationResponse_parameters' - The parameters used at the time that the task ran.
--
-- 'taskExecutionId', 'getMaintenanceWindowExecutionTaskInvocationResponse_taskExecutionId' - The task execution ID.
--
-- 'httpStatus', 'getMaintenanceWindowExecutionTaskInvocationResponse_httpStatus' - The response's http status code.
newGetMaintenanceWindowExecutionTaskInvocationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMaintenanceWindowExecutionTaskInvocationResponse
newGetMaintenanceWindowExecutionTaskInvocationResponse
  pHttpStatus_ =
    GetMaintenanceWindowExecutionTaskInvocationResponse'
      { status =
          Prelude.Nothing,
        statusDetails =
          Prelude.Nothing,
        windowTargetId =
          Prelude.Nothing,
        startTime =
          Prelude.Nothing,
        endTime =
          Prelude.Nothing,
        executionId =
          Prelude.Nothing,
        windowExecutionId =
          Prelude.Nothing,
        ownerInformation =
          Prelude.Nothing,
        taskType =
          Prelude.Nothing,
        invocationId =
          Prelude.Nothing,
        parameters =
          Prelude.Nothing,
        taskExecutionId =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The task status for an invocation.
getMaintenanceWindowExecutionTaskInvocationResponse_status :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe MaintenanceWindowExecutionStatus)
getMaintenanceWindowExecutionTaskInvocationResponse_status = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {status} -> status) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {status = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

-- | The details explaining the status. Details are only available for
-- certain status values.
getMaintenanceWindowExecutionTaskInvocationResponse_statusDetails :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskInvocationResponse_statusDetails = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {statusDetails} -> statusDetails) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {statusDetails = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

-- | The maintenance window target ID.
getMaintenanceWindowExecutionTaskInvocationResponse_windowTargetId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskInvocationResponse_windowTargetId = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {windowTargetId} -> windowTargetId) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {windowTargetId = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

-- | The time that the task started running on the target.
getMaintenanceWindowExecutionTaskInvocationResponse_startTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.UTCTime)
getMaintenanceWindowExecutionTaskInvocationResponse_startTime = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {startTime} -> startTime) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {startTime = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse) Prelude.. Lens.mapping Prelude._Time

-- | The time that the task finished running on the target.
getMaintenanceWindowExecutionTaskInvocationResponse_endTime :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.UTCTime)
getMaintenanceWindowExecutionTaskInvocationResponse_endTime = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {endTime} -> endTime) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {endTime = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse) Prelude.. Lens.mapping Prelude._Time

-- | The execution ID.
getMaintenanceWindowExecutionTaskInvocationResponse_executionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskInvocationResponse_executionId = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {executionId} -> executionId) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {executionId = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

-- | The maintenance window execution ID.
getMaintenanceWindowExecutionTaskInvocationResponse_windowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskInvocationResponse_windowExecutionId = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {windowExecutionId} -> windowExecutionId) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {windowExecutionId = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

-- | User-provided value to be included in any CloudWatch events raised while
-- running tasks for these targets in this maintenance window.
getMaintenanceWindowExecutionTaskInvocationResponse_ownerInformation :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskInvocationResponse_ownerInformation = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {ownerInformation} -> ownerInformation) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {ownerInformation = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse) Prelude.. Lens.mapping Prelude._Sensitive

-- | Retrieves the task type for a maintenance window. Task types include the
-- following: LAMBDA, STEP_FUNCTIONS, AUTOMATION, RUN_COMMAND.
getMaintenanceWindowExecutionTaskInvocationResponse_taskType :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe MaintenanceWindowTaskType)
getMaintenanceWindowExecutionTaskInvocationResponse_taskType = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {taskType} -> taskType) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {taskType = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

-- | The invocation ID.
getMaintenanceWindowExecutionTaskInvocationResponse_invocationId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskInvocationResponse_invocationId = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {invocationId} -> invocationId) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {invocationId = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

-- | The parameters used at the time that the task ran.
getMaintenanceWindowExecutionTaskInvocationResponse_parameters :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskInvocationResponse_parameters = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {parameters} -> parameters) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {parameters = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse) Prelude.. Lens.mapping Prelude._Sensitive

-- | The task execution ID.
getMaintenanceWindowExecutionTaskInvocationResponse_taskExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowExecutionTaskInvocationResponse_taskExecutionId = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {taskExecutionId} -> taskExecutionId) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {taskExecutionId = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

-- | The response's http status code.
getMaintenanceWindowExecutionTaskInvocationResponse_httpStatus :: Lens.Lens' GetMaintenanceWindowExecutionTaskInvocationResponse Prelude.Int
getMaintenanceWindowExecutionTaskInvocationResponse_httpStatus = Lens.lens (\GetMaintenanceWindowExecutionTaskInvocationResponse' {httpStatus} -> httpStatus) (\s@GetMaintenanceWindowExecutionTaskInvocationResponse' {} a -> s {httpStatus = a} :: GetMaintenanceWindowExecutionTaskInvocationResponse)

instance
  Prelude.NFData
    GetMaintenanceWindowExecutionTaskInvocationResponse
