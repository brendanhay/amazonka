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
-- Module      : Amazonka.IoT.DescribeAuditTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender audit.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeAuditTask>
-- action.
module Amazonka.IoT.DescribeAuditTask
  ( -- * Creating a Request
    DescribeAuditTask (..),
    newDescribeAuditTask,

    -- * Request Lenses
    describeAuditTask_taskId,

    -- * Destructuring the Response
    DescribeAuditTaskResponse (..),
    newDescribeAuditTaskResponse,

    -- * Response Lenses
    describeAuditTaskResponse_auditDetails,
    describeAuditTaskResponse_taskType,
    describeAuditTaskResponse_taskStartTime,
    describeAuditTaskResponse_taskStatistics,
    describeAuditTaskResponse_scheduledAuditName,
    describeAuditTaskResponse_taskStatus,
    describeAuditTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAuditTask' smart constructor.
data DescribeAuditTask = DescribeAuditTask'
  { -- | The ID of the audit whose information you want to get.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAuditTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'describeAuditTask_taskId' - The ID of the audit whose information you want to get.
newDescribeAuditTask ::
  -- | 'taskId'
  Prelude.Text ->
  DescribeAuditTask
newDescribeAuditTask pTaskId_ =
  DescribeAuditTask' {taskId = pTaskId_}

-- | The ID of the audit whose information you want to get.
describeAuditTask_taskId :: Lens.Lens' DescribeAuditTask Prelude.Text
describeAuditTask_taskId = Lens.lens (\DescribeAuditTask' {taskId} -> taskId) (\s@DescribeAuditTask' {} a -> s {taskId = a} :: DescribeAuditTask)

instance Core.AWSRequest DescribeAuditTask where
  type
    AWSResponse DescribeAuditTask =
      DescribeAuditTaskResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAuditTaskResponse'
            Prelude.<$> (x Core..?> "auditDetails" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "taskType")
            Prelude.<*> (x Core..?> "taskStartTime")
            Prelude.<*> (x Core..?> "taskStatistics")
            Prelude.<*> (x Core..?> "scheduledAuditName")
            Prelude.<*> (x Core..?> "taskStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAuditTask

instance Prelude.NFData DescribeAuditTask

instance Core.ToHeaders DescribeAuditTask where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeAuditTask where
  toPath DescribeAuditTask' {..} =
    Prelude.mconcat ["/audit/tasks/", Core.toBS taskId]

instance Core.ToQuery DescribeAuditTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAuditTaskResponse' smart constructor.
data DescribeAuditTaskResponse = DescribeAuditTaskResponse'
  { -- | Detailed information about each check performed during this audit.
    auditDetails :: Prelude.Maybe (Prelude.HashMap Prelude.Text AuditCheckDetails),
    -- | The type of audit: \"ON_DEMAND_AUDIT_TASK\" or \"SCHEDULED_AUDIT_TASK\".
    taskType :: Prelude.Maybe AuditTaskType,
    -- | The time the audit started.
    taskStartTime :: Prelude.Maybe Core.POSIX,
    -- | Statistical information about the audit.
    taskStatistics :: Prelude.Maybe TaskStatistics,
    -- | The name of the scheduled audit (only if the audit was a scheduled
    -- audit).
    scheduledAuditName :: Prelude.Maybe Prelude.Text,
    -- | The status of the audit: one of \"IN_PROGRESS\", \"COMPLETED\",
    -- \"FAILED\", or \"CANCELED\".
    taskStatus :: Prelude.Maybe AuditTaskStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAuditTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditDetails', 'describeAuditTaskResponse_auditDetails' - Detailed information about each check performed during this audit.
--
-- 'taskType', 'describeAuditTaskResponse_taskType' - The type of audit: \"ON_DEMAND_AUDIT_TASK\" or \"SCHEDULED_AUDIT_TASK\".
--
-- 'taskStartTime', 'describeAuditTaskResponse_taskStartTime' - The time the audit started.
--
-- 'taskStatistics', 'describeAuditTaskResponse_taskStatistics' - Statistical information about the audit.
--
-- 'scheduledAuditName', 'describeAuditTaskResponse_scheduledAuditName' - The name of the scheduled audit (only if the audit was a scheduled
-- audit).
--
-- 'taskStatus', 'describeAuditTaskResponse_taskStatus' - The status of the audit: one of \"IN_PROGRESS\", \"COMPLETED\",
-- \"FAILED\", or \"CANCELED\".
--
-- 'httpStatus', 'describeAuditTaskResponse_httpStatus' - The response's http status code.
newDescribeAuditTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAuditTaskResponse
newDescribeAuditTaskResponse pHttpStatus_ =
  DescribeAuditTaskResponse'
    { auditDetails =
        Prelude.Nothing,
      taskType = Prelude.Nothing,
      taskStartTime = Prelude.Nothing,
      taskStatistics = Prelude.Nothing,
      scheduledAuditName = Prelude.Nothing,
      taskStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about each check performed during this audit.
describeAuditTaskResponse_auditDetails :: Lens.Lens' DescribeAuditTaskResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text AuditCheckDetails))
describeAuditTaskResponse_auditDetails = Lens.lens (\DescribeAuditTaskResponse' {auditDetails} -> auditDetails) (\s@DescribeAuditTaskResponse' {} a -> s {auditDetails = a} :: DescribeAuditTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | The type of audit: \"ON_DEMAND_AUDIT_TASK\" or \"SCHEDULED_AUDIT_TASK\".
describeAuditTaskResponse_taskType :: Lens.Lens' DescribeAuditTaskResponse (Prelude.Maybe AuditTaskType)
describeAuditTaskResponse_taskType = Lens.lens (\DescribeAuditTaskResponse' {taskType} -> taskType) (\s@DescribeAuditTaskResponse' {} a -> s {taskType = a} :: DescribeAuditTaskResponse)

-- | The time the audit started.
describeAuditTaskResponse_taskStartTime :: Lens.Lens' DescribeAuditTaskResponse (Prelude.Maybe Prelude.UTCTime)
describeAuditTaskResponse_taskStartTime = Lens.lens (\DescribeAuditTaskResponse' {taskStartTime} -> taskStartTime) (\s@DescribeAuditTaskResponse' {} a -> s {taskStartTime = a} :: DescribeAuditTaskResponse) Prelude.. Lens.mapping Core._Time

-- | Statistical information about the audit.
describeAuditTaskResponse_taskStatistics :: Lens.Lens' DescribeAuditTaskResponse (Prelude.Maybe TaskStatistics)
describeAuditTaskResponse_taskStatistics = Lens.lens (\DescribeAuditTaskResponse' {taskStatistics} -> taskStatistics) (\s@DescribeAuditTaskResponse' {} a -> s {taskStatistics = a} :: DescribeAuditTaskResponse)

-- | The name of the scheduled audit (only if the audit was a scheduled
-- audit).
describeAuditTaskResponse_scheduledAuditName :: Lens.Lens' DescribeAuditTaskResponse (Prelude.Maybe Prelude.Text)
describeAuditTaskResponse_scheduledAuditName = Lens.lens (\DescribeAuditTaskResponse' {scheduledAuditName} -> scheduledAuditName) (\s@DescribeAuditTaskResponse' {} a -> s {scheduledAuditName = a} :: DescribeAuditTaskResponse)

-- | The status of the audit: one of \"IN_PROGRESS\", \"COMPLETED\",
-- \"FAILED\", or \"CANCELED\".
describeAuditTaskResponse_taskStatus :: Lens.Lens' DescribeAuditTaskResponse (Prelude.Maybe AuditTaskStatus)
describeAuditTaskResponse_taskStatus = Lens.lens (\DescribeAuditTaskResponse' {taskStatus} -> taskStatus) (\s@DescribeAuditTaskResponse' {} a -> s {taskStatus = a} :: DescribeAuditTaskResponse)

-- | The response's http status code.
describeAuditTaskResponse_httpStatus :: Lens.Lens' DescribeAuditTaskResponse Prelude.Int
describeAuditTaskResponse_httpStatus = Lens.lens (\DescribeAuditTaskResponse' {httpStatus} -> httpStatus) (\s@DescribeAuditTaskResponse' {} a -> s {httpStatus = a} :: DescribeAuditTaskResponse)

instance Prelude.NFData DescribeAuditTaskResponse
