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
-- Module      : Amazonka.IoT.DescribeAuditMitigationActionsTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an audit mitigation task that is used to apply
-- mitigation actions to a set of audit findings. Properties include the
-- actions being applied, the audit checks to which they\'re being applied,
-- the task status, and aggregated task statistics.
module Amazonka.IoT.DescribeAuditMitigationActionsTask
  ( -- * Creating a Request
    DescribeAuditMitigationActionsTask (..),
    newDescribeAuditMitigationActionsTask,

    -- * Request Lenses
    describeAuditMitigationActionsTask_taskId,

    -- * Destructuring the Response
    DescribeAuditMitigationActionsTaskResponse (..),
    newDescribeAuditMitigationActionsTaskResponse,

    -- * Response Lenses
    describeAuditMitigationActionsTaskResponse_auditCheckToActionsMapping,
    describeAuditMitigationActionsTaskResponse_taskStatus,
    describeAuditMitigationActionsTaskResponse_target,
    describeAuditMitigationActionsTaskResponse_endTime,
    describeAuditMitigationActionsTaskResponse_actionsDefinition,
    describeAuditMitigationActionsTaskResponse_taskStatistics,
    describeAuditMitigationActionsTaskResponse_startTime,
    describeAuditMitigationActionsTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAuditMitigationActionsTask' smart constructor.
data DescribeAuditMitigationActionsTask = DescribeAuditMitigationActionsTask'
  { -- | The unique identifier for the audit mitigation task.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAuditMitigationActionsTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'describeAuditMitigationActionsTask_taskId' - The unique identifier for the audit mitigation task.
newDescribeAuditMitigationActionsTask ::
  -- | 'taskId'
  Prelude.Text ->
  DescribeAuditMitigationActionsTask
newDescribeAuditMitigationActionsTask pTaskId_ =
  DescribeAuditMitigationActionsTask'
    { taskId =
        pTaskId_
    }

-- | The unique identifier for the audit mitigation task.
describeAuditMitigationActionsTask_taskId :: Lens.Lens' DescribeAuditMitigationActionsTask Prelude.Text
describeAuditMitigationActionsTask_taskId = Lens.lens (\DescribeAuditMitigationActionsTask' {taskId} -> taskId) (\s@DescribeAuditMitigationActionsTask' {} a -> s {taskId = a} :: DescribeAuditMitigationActionsTask)

instance
  Core.AWSRequest
    DescribeAuditMitigationActionsTask
  where
  type
    AWSResponse DescribeAuditMitigationActionsTask =
      DescribeAuditMitigationActionsTaskResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAuditMitigationActionsTaskResponse'
            Prelude.<$> ( x Data..?> "auditCheckToActionsMapping"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Data..?> "taskStatus")
              Prelude.<*> (x Data..?> "target")
              Prelude.<*> (x Data..?> "endTime")
              Prelude.<*> ( x Data..?> "actionsDefinition"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (x Data..?> "taskStatistics" Core..!@ Prelude.mempty)
              Prelude.<*> (x Data..?> "startTime")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAuditMitigationActionsTask
  where
  hashWithSalt
    _salt
    DescribeAuditMitigationActionsTask' {..} =
      _salt `Prelude.hashWithSalt` taskId

instance
  Prelude.NFData
    DescribeAuditMitigationActionsTask
  where
  rnf DescribeAuditMitigationActionsTask' {..} =
    Prelude.rnf taskId

instance
  Data.ToHeaders
    DescribeAuditMitigationActionsTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeAuditMitigationActionsTask
  where
  toPath DescribeAuditMitigationActionsTask' {..} =
    Prelude.mconcat
      ["/audit/mitigationactions/tasks/", Data.toBS taskId]

instance
  Data.ToQuery
    DescribeAuditMitigationActionsTask
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAuditMitigationActionsTaskResponse' smart constructor.
data DescribeAuditMitigationActionsTaskResponse = DescribeAuditMitigationActionsTaskResponse'
  { -- | Specifies the mitigation actions that should be applied to specific
    -- audit checks.
    auditCheckToActionsMapping :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)),
    -- | The current status of the task.
    taskStatus :: Prelude.Maybe AuditMitigationActionsTaskStatus,
    -- | Identifies the findings to which the mitigation actions are applied.
    -- This can be by audit checks, by audit task, or a set of findings.
    target :: Prelude.Maybe AuditMitigationActionsTaskTarget,
    -- | The date and time when the task was completed or canceled.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies the mitigation actions and their parameters that are applied
    -- as part of this task.
    actionsDefinition :: Prelude.Maybe [MitigationAction],
    -- | Aggregate counts of the results when the mitigation tasks were applied
    -- to the findings for this audit mitigation actions task.
    taskStatistics :: Prelude.Maybe (Prelude.HashMap Prelude.Text TaskStatisticsForAuditCheck),
    -- | The date and time when the task was started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAuditMitigationActionsTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditCheckToActionsMapping', 'describeAuditMitigationActionsTaskResponse_auditCheckToActionsMapping' - Specifies the mitigation actions that should be applied to specific
-- audit checks.
--
-- 'taskStatus', 'describeAuditMitigationActionsTaskResponse_taskStatus' - The current status of the task.
--
-- 'target', 'describeAuditMitigationActionsTaskResponse_target' - Identifies the findings to which the mitigation actions are applied.
-- This can be by audit checks, by audit task, or a set of findings.
--
-- 'endTime', 'describeAuditMitigationActionsTaskResponse_endTime' - The date and time when the task was completed or canceled.
--
-- 'actionsDefinition', 'describeAuditMitigationActionsTaskResponse_actionsDefinition' - Specifies the mitigation actions and their parameters that are applied
-- as part of this task.
--
-- 'taskStatistics', 'describeAuditMitigationActionsTaskResponse_taskStatistics' - Aggregate counts of the results when the mitigation tasks were applied
-- to the findings for this audit mitigation actions task.
--
-- 'startTime', 'describeAuditMitigationActionsTaskResponse_startTime' - The date and time when the task was started.
--
-- 'httpStatus', 'describeAuditMitigationActionsTaskResponse_httpStatus' - The response's http status code.
newDescribeAuditMitigationActionsTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAuditMitigationActionsTaskResponse
newDescribeAuditMitigationActionsTaskResponse
  pHttpStatus_ =
    DescribeAuditMitigationActionsTaskResponse'
      { auditCheckToActionsMapping =
          Prelude.Nothing,
        taskStatus = Prelude.Nothing,
        target = Prelude.Nothing,
        endTime = Prelude.Nothing,
        actionsDefinition =
          Prelude.Nothing,
        taskStatistics =
          Prelude.Nothing,
        startTime = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Specifies the mitigation actions that should be applied to specific
-- audit checks.
describeAuditMitigationActionsTaskResponse_auditCheckToActionsMapping :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
describeAuditMitigationActionsTaskResponse_auditCheckToActionsMapping = Lens.lens (\DescribeAuditMitigationActionsTaskResponse' {auditCheckToActionsMapping} -> auditCheckToActionsMapping) (\s@DescribeAuditMitigationActionsTaskResponse' {} a -> s {auditCheckToActionsMapping = a} :: DescribeAuditMitigationActionsTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the task.
describeAuditMitigationActionsTaskResponse_taskStatus :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Prelude.Maybe AuditMitigationActionsTaskStatus)
describeAuditMitigationActionsTaskResponse_taskStatus = Lens.lens (\DescribeAuditMitigationActionsTaskResponse' {taskStatus} -> taskStatus) (\s@DescribeAuditMitigationActionsTaskResponse' {} a -> s {taskStatus = a} :: DescribeAuditMitigationActionsTaskResponse)

-- | Identifies the findings to which the mitigation actions are applied.
-- This can be by audit checks, by audit task, or a set of findings.
describeAuditMitigationActionsTaskResponse_target :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Prelude.Maybe AuditMitigationActionsTaskTarget)
describeAuditMitigationActionsTaskResponse_target = Lens.lens (\DescribeAuditMitigationActionsTaskResponse' {target} -> target) (\s@DescribeAuditMitigationActionsTaskResponse' {} a -> s {target = a} :: DescribeAuditMitigationActionsTaskResponse)

-- | The date and time when the task was completed or canceled.
describeAuditMitigationActionsTaskResponse_endTime :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Prelude.Maybe Prelude.UTCTime)
describeAuditMitigationActionsTaskResponse_endTime = Lens.lens (\DescribeAuditMitigationActionsTaskResponse' {endTime} -> endTime) (\s@DescribeAuditMitigationActionsTaskResponse' {} a -> s {endTime = a} :: DescribeAuditMitigationActionsTaskResponse) Prelude.. Lens.mapping Data._Time

-- | Specifies the mitigation actions and their parameters that are applied
-- as part of this task.
describeAuditMitigationActionsTaskResponse_actionsDefinition :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Prelude.Maybe [MitigationAction])
describeAuditMitigationActionsTaskResponse_actionsDefinition = Lens.lens (\DescribeAuditMitigationActionsTaskResponse' {actionsDefinition} -> actionsDefinition) (\s@DescribeAuditMitigationActionsTaskResponse' {} a -> s {actionsDefinition = a} :: DescribeAuditMitigationActionsTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | Aggregate counts of the results when the mitigation tasks were applied
-- to the findings for this audit mitigation actions task.
describeAuditMitigationActionsTaskResponse_taskStatistics :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text TaskStatisticsForAuditCheck))
describeAuditMitigationActionsTaskResponse_taskStatistics = Lens.lens (\DescribeAuditMitigationActionsTaskResponse' {taskStatistics} -> taskStatistics) (\s@DescribeAuditMitigationActionsTaskResponse' {} a -> s {taskStatistics = a} :: DescribeAuditMitigationActionsTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time when the task was started.
describeAuditMitigationActionsTaskResponse_startTime :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Prelude.Maybe Prelude.UTCTime)
describeAuditMitigationActionsTaskResponse_startTime = Lens.lens (\DescribeAuditMitigationActionsTaskResponse' {startTime} -> startTime) (\s@DescribeAuditMitigationActionsTaskResponse' {} a -> s {startTime = a} :: DescribeAuditMitigationActionsTaskResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeAuditMitigationActionsTaskResponse_httpStatus :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse Prelude.Int
describeAuditMitigationActionsTaskResponse_httpStatus = Lens.lens (\DescribeAuditMitigationActionsTaskResponse' {httpStatus} -> httpStatus) (\s@DescribeAuditMitigationActionsTaskResponse' {} a -> s {httpStatus = a} :: DescribeAuditMitigationActionsTaskResponse)

instance
  Prelude.NFData
    DescribeAuditMitigationActionsTaskResponse
  where
  rnf DescribeAuditMitigationActionsTaskResponse' {..} =
    Prelude.rnf auditCheckToActionsMapping
      `Prelude.seq` Prelude.rnf taskStatus
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf actionsDefinition
      `Prelude.seq` Prelude.rnf taskStatistics
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf httpStatus
