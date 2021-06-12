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
-- Module      : Network.AWS.SSM.GetMaintenanceWindowExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about a specific a maintenance window execution.
module Network.AWS.SSM.GetMaintenanceWindowExecution
  ( -- * Creating a Request
    GetMaintenanceWindowExecution (..),
    newGetMaintenanceWindowExecution,

    -- * Request Lenses
    getMaintenanceWindowExecution_windowExecutionId,

    -- * Destructuring the Response
    GetMaintenanceWindowExecutionResponse (..),
    newGetMaintenanceWindowExecutionResponse,

    -- * Response Lenses
    getMaintenanceWindowExecutionResponse_status,
    getMaintenanceWindowExecutionResponse_statusDetails,
    getMaintenanceWindowExecutionResponse_taskIds,
    getMaintenanceWindowExecutionResponse_startTime,
    getMaintenanceWindowExecutionResponse_endTime,
    getMaintenanceWindowExecutionResponse_windowExecutionId,
    getMaintenanceWindowExecutionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetMaintenanceWindowExecution' smart constructor.
data GetMaintenanceWindowExecution = GetMaintenanceWindowExecution'
  { -- | The ID of the maintenance window execution that includes the task.
    windowExecutionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMaintenanceWindowExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowExecutionId', 'getMaintenanceWindowExecution_windowExecutionId' - The ID of the maintenance window execution that includes the task.
newGetMaintenanceWindowExecution ::
  -- | 'windowExecutionId'
  Core.Text ->
  GetMaintenanceWindowExecution
newGetMaintenanceWindowExecution pWindowExecutionId_ =
  GetMaintenanceWindowExecution'
    { windowExecutionId =
        pWindowExecutionId_
    }

-- | The ID of the maintenance window execution that includes the task.
getMaintenanceWindowExecution_windowExecutionId :: Lens.Lens' GetMaintenanceWindowExecution Core.Text
getMaintenanceWindowExecution_windowExecutionId = Lens.lens (\GetMaintenanceWindowExecution' {windowExecutionId} -> windowExecutionId) (\s@GetMaintenanceWindowExecution' {} a -> s {windowExecutionId = a} :: GetMaintenanceWindowExecution)

instance
  Core.AWSRequest
    GetMaintenanceWindowExecution
  where
  type
    AWSResponse GetMaintenanceWindowExecution =
      GetMaintenanceWindowExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMaintenanceWindowExecutionResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "StatusDetails")
            Core.<*> (x Core..?> "TaskIds" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "StartTime")
            Core.<*> (x Core..?> "EndTime")
            Core.<*> (x Core..?> "WindowExecutionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetMaintenanceWindowExecution

instance Core.NFData GetMaintenanceWindowExecution

instance Core.ToHeaders GetMaintenanceWindowExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.GetMaintenanceWindowExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetMaintenanceWindowExecution where
  toJSON GetMaintenanceWindowExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("WindowExecutionId" Core..= windowExecutionId)
          ]
      )

instance Core.ToPath GetMaintenanceWindowExecution where
  toPath = Core.const "/"

instance Core.ToQuery GetMaintenanceWindowExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetMaintenanceWindowExecutionResponse' smart constructor.
data GetMaintenanceWindowExecutionResponse = GetMaintenanceWindowExecutionResponse'
  { -- | The status of the maintenance window execution.
    status :: Core.Maybe MaintenanceWindowExecutionStatus,
    -- | The details explaining the Status. Only available for certain status
    -- values.
    statusDetails :: Core.Maybe Core.Text,
    -- | The ID of the task executions from the maintenance window execution.
    taskIds :: Core.Maybe [Core.Text],
    -- | The time the maintenance window started running.
    startTime :: Core.Maybe Core.POSIX,
    -- | The time the maintenance window finished running.
    endTime :: Core.Maybe Core.POSIX,
    -- | The ID of the maintenance window execution.
    windowExecutionId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMaintenanceWindowExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getMaintenanceWindowExecutionResponse_status' - The status of the maintenance window execution.
--
-- 'statusDetails', 'getMaintenanceWindowExecutionResponse_statusDetails' - The details explaining the Status. Only available for certain status
-- values.
--
-- 'taskIds', 'getMaintenanceWindowExecutionResponse_taskIds' - The ID of the task executions from the maintenance window execution.
--
-- 'startTime', 'getMaintenanceWindowExecutionResponse_startTime' - The time the maintenance window started running.
--
-- 'endTime', 'getMaintenanceWindowExecutionResponse_endTime' - The time the maintenance window finished running.
--
-- 'windowExecutionId', 'getMaintenanceWindowExecutionResponse_windowExecutionId' - The ID of the maintenance window execution.
--
-- 'httpStatus', 'getMaintenanceWindowExecutionResponse_httpStatus' - The response's http status code.
newGetMaintenanceWindowExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetMaintenanceWindowExecutionResponse
newGetMaintenanceWindowExecutionResponse pHttpStatus_ =
  GetMaintenanceWindowExecutionResponse'
    { status =
        Core.Nothing,
      statusDetails = Core.Nothing,
      taskIds = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      windowExecutionId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the maintenance window execution.
getMaintenanceWindowExecutionResponse_status :: Lens.Lens' GetMaintenanceWindowExecutionResponse (Core.Maybe MaintenanceWindowExecutionStatus)
getMaintenanceWindowExecutionResponse_status = Lens.lens (\GetMaintenanceWindowExecutionResponse' {status} -> status) (\s@GetMaintenanceWindowExecutionResponse' {} a -> s {status = a} :: GetMaintenanceWindowExecutionResponse)

-- | The details explaining the Status. Only available for certain status
-- values.
getMaintenanceWindowExecutionResponse_statusDetails :: Lens.Lens' GetMaintenanceWindowExecutionResponse (Core.Maybe Core.Text)
getMaintenanceWindowExecutionResponse_statusDetails = Lens.lens (\GetMaintenanceWindowExecutionResponse' {statusDetails} -> statusDetails) (\s@GetMaintenanceWindowExecutionResponse' {} a -> s {statusDetails = a} :: GetMaintenanceWindowExecutionResponse)

-- | The ID of the task executions from the maintenance window execution.
getMaintenanceWindowExecutionResponse_taskIds :: Lens.Lens' GetMaintenanceWindowExecutionResponse (Core.Maybe [Core.Text])
getMaintenanceWindowExecutionResponse_taskIds = Lens.lens (\GetMaintenanceWindowExecutionResponse' {taskIds} -> taskIds) (\s@GetMaintenanceWindowExecutionResponse' {} a -> s {taskIds = a} :: GetMaintenanceWindowExecutionResponse) Core.. Lens.mapping Lens._Coerce

-- | The time the maintenance window started running.
getMaintenanceWindowExecutionResponse_startTime :: Lens.Lens' GetMaintenanceWindowExecutionResponse (Core.Maybe Core.UTCTime)
getMaintenanceWindowExecutionResponse_startTime = Lens.lens (\GetMaintenanceWindowExecutionResponse' {startTime} -> startTime) (\s@GetMaintenanceWindowExecutionResponse' {} a -> s {startTime = a} :: GetMaintenanceWindowExecutionResponse) Core.. Lens.mapping Core._Time

-- | The time the maintenance window finished running.
getMaintenanceWindowExecutionResponse_endTime :: Lens.Lens' GetMaintenanceWindowExecutionResponse (Core.Maybe Core.UTCTime)
getMaintenanceWindowExecutionResponse_endTime = Lens.lens (\GetMaintenanceWindowExecutionResponse' {endTime} -> endTime) (\s@GetMaintenanceWindowExecutionResponse' {} a -> s {endTime = a} :: GetMaintenanceWindowExecutionResponse) Core.. Lens.mapping Core._Time

-- | The ID of the maintenance window execution.
getMaintenanceWindowExecutionResponse_windowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionResponse (Core.Maybe Core.Text)
getMaintenanceWindowExecutionResponse_windowExecutionId = Lens.lens (\GetMaintenanceWindowExecutionResponse' {windowExecutionId} -> windowExecutionId) (\s@GetMaintenanceWindowExecutionResponse' {} a -> s {windowExecutionId = a} :: GetMaintenanceWindowExecutionResponse)

-- | The response's http status code.
getMaintenanceWindowExecutionResponse_httpStatus :: Lens.Lens' GetMaintenanceWindowExecutionResponse Core.Int
getMaintenanceWindowExecutionResponse_httpStatus = Lens.lens (\GetMaintenanceWindowExecutionResponse' {httpStatus} -> httpStatus) (\s@GetMaintenanceWindowExecutionResponse' {} a -> s {httpStatus = a} :: GetMaintenanceWindowExecutionResponse)

instance
  Core.NFData
    GetMaintenanceWindowExecutionResponse
