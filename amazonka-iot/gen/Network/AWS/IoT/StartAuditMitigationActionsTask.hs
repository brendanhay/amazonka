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
-- Module      : Network.AWS.IoT.StartAuditMitigationActionsTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a task that applies a set of mitigation actions to the specified
-- target.
module Network.AWS.IoT.StartAuditMitigationActionsTask
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    -- actions must be defined in your AWS account.
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
-- actions must be defined in your AWS account.
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
-- actions must be defined in your AWS account.
startAuditMitigationActionsTask_auditCheckToActionsMapping :: Lens.Lens' StartAuditMitigationActionsTask (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text))
startAuditMitigationActionsTask_auditCheckToActionsMapping = Lens.lens (\StartAuditMitigationActionsTask' {auditCheckToActionsMapping} -> auditCheckToActionsMapping) (\s@StartAuditMitigationActionsTask' {} a -> s {auditCheckToActionsMapping = a} :: StartAuditMitigationActionsTask) Prelude.. Lens._Coerce

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartAuditMitigationActionsTaskResponse'
            Prelude.<$> (x Core..?> "taskId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartAuditMitigationActionsTask

instance
  Prelude.NFData
    StartAuditMitigationActionsTask

instance
  Core.ToHeaders
    StartAuditMitigationActionsTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON StartAuditMitigationActionsTask where
  toJSON StartAuditMitigationActionsTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("target" Core..= target),
            Prelude.Just
              ( "auditCheckToActionsMapping"
                  Core..= auditCheckToActionsMapping
              ),
            Prelude.Just
              ("clientRequestToken" Core..= clientRequestToken)
          ]
      )

instance Core.ToPath StartAuditMitigationActionsTask where
  toPath StartAuditMitigationActionsTask' {..} =
    Prelude.mconcat
      ["/audit/mitigationactions/tasks/", Core.toBS taskId]

instance Core.ToQuery StartAuditMitigationActionsTask where
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
