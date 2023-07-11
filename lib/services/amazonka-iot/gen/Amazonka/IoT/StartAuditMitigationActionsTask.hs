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
-- Module      : Amazonka.IoT.StartAuditMitigationActionsTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a task that applies a set of mitigation actions to the specified
-- target.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions StartAuditMitigationActionsTask>
-- action.
module Amazonka.IoT.StartAuditMitigationActionsTask
  ( -- * Creating a Request
    StartAuditMitigationActionsTask (..),
    newStartAuditMitigationActionsTask,

    -- * Request Lenses
    startAuditMitigationActionsTask_taskId,
    startAuditMitigationActionsTask_target,
    startAuditMitigationActionsTask_auditCheckToActionsMapping,
    startAuditMitigationActionsTask_clientRequestToken,

    -- * Destructuring the Response
    StartAuditMitigationActionsTaskResponse (..),
    newStartAuditMitigationActionsTaskResponse,

    -- * Response Lenses
    startAuditMitigationActionsTaskResponse_taskId,
    startAuditMitigationActionsTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartAuditMitigationActionsTask' smart constructor.
data StartAuditMitigationActionsTask = StartAuditMitigationActionsTask'
  { -- | A unique identifier for the task. You can use this identifier to check
    -- the status of the task or to cancel it.
    taskId :: Prelude.Text,
    -- | Specifies the audit findings to which the mitigation actions are
    -- applied. You can apply them to a type of audit check, to all findings
    -- from an audit, or to a specific set of findings.
    target :: AuditMitigationActionsTaskTarget,
    -- | For an audit check, specifies which mitigation actions to apply. Those
    -- actions must be defined in your Amazon Web Services accounts.
    auditCheckToActionsMapping :: Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text),
    -- | Each audit mitigation task must have a unique client request token. If
    -- you try to start a new task with the same token as a task that already
    -- exists, an exception occurs. If you omit this value, a unique client
    -- request token is generated automatically.
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAuditMitigationActionsTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'startAuditMitigationActionsTask_taskId' - A unique identifier for the task. You can use this identifier to check
-- the status of the task or to cancel it.
--
-- 'target', 'startAuditMitigationActionsTask_target' - Specifies the audit findings to which the mitigation actions are
-- applied. You can apply them to a type of audit check, to all findings
-- from an audit, or to a specific set of findings.
--
-- 'auditCheckToActionsMapping', 'startAuditMitigationActionsTask_auditCheckToActionsMapping' - For an audit check, specifies which mitigation actions to apply. Those
-- actions must be defined in your Amazon Web Services accounts.
--
-- 'clientRequestToken', 'startAuditMitigationActionsTask_clientRequestToken' - Each audit mitigation task must have a unique client request token. If
-- you try to start a new task with the same token as a task that already
-- exists, an exception occurs. If you omit this value, a unique client
-- request token is generated automatically.
newStartAuditMitigationActionsTask ::
  -- | 'taskId'
  Prelude.Text ->
  -- | 'target'
  AuditMitigationActionsTaskTarget ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  StartAuditMitigationActionsTask
newStartAuditMitigationActionsTask
  pTaskId_
  pTarget_
  pClientRequestToken_ =
    StartAuditMitigationActionsTask'
      { taskId = pTaskId_,
        target = pTarget_,
        auditCheckToActionsMapping =
          Prelude.mempty,
        clientRequestToken = pClientRequestToken_
      }

-- | A unique identifier for the task. You can use this identifier to check
-- the status of the task or to cancel it.
startAuditMitigationActionsTask_taskId :: Lens.Lens' StartAuditMitigationActionsTask Prelude.Text
startAuditMitigationActionsTask_taskId = Lens.lens (\StartAuditMitigationActionsTask' {taskId} -> taskId) (\s@StartAuditMitigationActionsTask' {} a -> s {taskId = a} :: StartAuditMitigationActionsTask)

-- | Specifies the audit findings to which the mitigation actions are
-- applied. You can apply them to a type of audit check, to all findings
-- from an audit, or to a specific set of findings.
startAuditMitigationActionsTask_target :: Lens.Lens' StartAuditMitigationActionsTask AuditMitigationActionsTaskTarget
startAuditMitigationActionsTask_target = Lens.lens (\StartAuditMitigationActionsTask' {target} -> target) (\s@StartAuditMitigationActionsTask' {} a -> s {target = a} :: StartAuditMitigationActionsTask)

-- | For an audit check, specifies which mitigation actions to apply. Those
-- actions must be defined in your Amazon Web Services accounts.
startAuditMitigationActionsTask_auditCheckToActionsMapping :: Lens.Lens' StartAuditMitigationActionsTask (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text))
startAuditMitigationActionsTask_auditCheckToActionsMapping = Lens.lens (\StartAuditMitigationActionsTask' {auditCheckToActionsMapping} -> auditCheckToActionsMapping) (\s@StartAuditMitigationActionsTask' {} a -> s {auditCheckToActionsMapping = a} :: StartAuditMitigationActionsTask) Prelude.. Lens.coerced

-- | Each audit mitigation task must have a unique client request token. If
-- you try to start a new task with the same token as a task that already
-- exists, an exception occurs. If you omit this value, a unique client
-- request token is generated automatically.
startAuditMitigationActionsTask_clientRequestToken :: Lens.Lens' StartAuditMitigationActionsTask Prelude.Text
startAuditMitigationActionsTask_clientRequestToken = Lens.lens (\StartAuditMitigationActionsTask' {clientRequestToken} -> clientRequestToken) (\s@StartAuditMitigationActionsTask' {} a -> s {clientRequestToken = a} :: StartAuditMitigationActionsTask)

instance
  Core.AWSRequest
    StartAuditMitigationActionsTask
  where
  type
    AWSResponse StartAuditMitigationActionsTask =
      StartAuditMitigationActionsTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartAuditMitigationActionsTaskResponse'
            Prelude.<$> (x Data..?> "taskId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartAuditMitigationActionsTask
  where
  hashWithSalt
    _salt
    StartAuditMitigationActionsTask' {..} =
      _salt
        `Prelude.hashWithSalt` taskId
        `Prelude.hashWithSalt` target
        `Prelude.hashWithSalt` auditCheckToActionsMapping
        `Prelude.hashWithSalt` clientRequestToken

instance
  Prelude.NFData
    StartAuditMitigationActionsTask
  where
  rnf StartAuditMitigationActionsTask' {..} =
    Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf auditCheckToActionsMapping
      `Prelude.seq` Prelude.rnf clientRequestToken

instance
  Data.ToHeaders
    StartAuditMitigationActionsTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StartAuditMitigationActionsTask where
  toJSON StartAuditMitigationActionsTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("target" Data..= target),
            Prelude.Just
              ( "auditCheckToActionsMapping"
                  Data..= auditCheckToActionsMapping
              ),
            Prelude.Just
              ("clientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath StartAuditMitigationActionsTask where
  toPath StartAuditMitigationActionsTask' {..} =
    Prelude.mconcat
      ["/audit/mitigationactions/tasks/", Data.toBS taskId]

instance Data.ToQuery StartAuditMitigationActionsTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartAuditMitigationActionsTaskResponse' smart constructor.
data StartAuditMitigationActionsTaskResponse = StartAuditMitigationActionsTaskResponse'
  { -- | The unique identifier for the audit mitigation task. This matches the
    -- @taskId@ that you specified in the request.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAuditMitigationActionsTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'startAuditMitigationActionsTaskResponse_taskId' - The unique identifier for the audit mitigation task. This matches the
-- @taskId@ that you specified in the request.
--
-- 'httpStatus', 'startAuditMitigationActionsTaskResponse_httpStatus' - The response's http status code.
newStartAuditMitigationActionsTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartAuditMitigationActionsTaskResponse
newStartAuditMitigationActionsTaskResponse
  pHttpStatus_ =
    StartAuditMitigationActionsTaskResponse'
      { taskId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The unique identifier for the audit mitigation task. This matches the
-- @taskId@ that you specified in the request.
startAuditMitigationActionsTaskResponse_taskId :: Lens.Lens' StartAuditMitigationActionsTaskResponse (Prelude.Maybe Prelude.Text)
startAuditMitigationActionsTaskResponse_taskId = Lens.lens (\StartAuditMitigationActionsTaskResponse' {taskId} -> taskId) (\s@StartAuditMitigationActionsTaskResponse' {} a -> s {taskId = a} :: StartAuditMitigationActionsTaskResponse)

-- | The response's http status code.
startAuditMitigationActionsTaskResponse_httpStatus :: Lens.Lens' StartAuditMitigationActionsTaskResponse Prelude.Int
startAuditMitigationActionsTaskResponse_httpStatus = Lens.lens (\StartAuditMitigationActionsTaskResponse' {httpStatus} -> httpStatus) (\s@StartAuditMitigationActionsTaskResponse' {} a -> s {httpStatus = a} :: StartAuditMitigationActionsTaskResponse)

instance
  Prelude.NFData
    StartAuditMitigationActionsTaskResponse
  where
  rnf StartAuditMitigationActionsTaskResponse' {..} =
    Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf httpStatus
