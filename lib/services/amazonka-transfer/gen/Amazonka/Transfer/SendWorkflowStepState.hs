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
-- Module      : Amazonka.Transfer.SendWorkflowStepState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a callback for asynchronous custom steps.
--
-- The @ExecutionId@, @WorkflowId@, and @Token@ are passed to the target
-- resource during execution of a custom step of a workflow. You must
-- include those with their callback as well as providing a status.
module Amazonka.Transfer.SendWorkflowStepState
  ( -- * Creating a Request
    SendWorkflowStepState (..),
    newSendWorkflowStepState,

    -- * Request Lenses
    sendWorkflowStepState_workflowId,
    sendWorkflowStepState_executionId,
    sendWorkflowStepState_token,
    sendWorkflowStepState_status,

    -- * Destructuring the Response
    SendWorkflowStepStateResponse (..),
    newSendWorkflowStepStateResponse,

    -- * Response Lenses
    sendWorkflowStepStateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newSendWorkflowStepState' smart constructor.
data SendWorkflowStepState = SendWorkflowStepState'
  { -- | A unique identifier for the workflow.
    workflowId :: Prelude.Text,
    -- | A unique identifier for the execution of a workflow.
    executionId :: Prelude.Text,
    -- | Used to distinguish between multiple callbacks for multiple Lambda steps
    -- within the same execution.
    token :: Prelude.Text,
    -- | Indicates whether the specified step succeeded or failed.
    status :: CustomStepStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendWorkflowStepState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowId', 'sendWorkflowStepState_workflowId' - A unique identifier for the workflow.
--
-- 'executionId', 'sendWorkflowStepState_executionId' - A unique identifier for the execution of a workflow.
--
-- 'token', 'sendWorkflowStepState_token' - Used to distinguish between multiple callbacks for multiple Lambda steps
-- within the same execution.
--
-- 'status', 'sendWorkflowStepState_status' - Indicates whether the specified step succeeded or failed.
newSendWorkflowStepState ::
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'executionId'
  Prelude.Text ->
  -- | 'token'
  Prelude.Text ->
  -- | 'status'
  CustomStepStatus ->
  SendWorkflowStepState
newSendWorkflowStepState
  pWorkflowId_
  pExecutionId_
  pToken_
  pStatus_ =
    SendWorkflowStepState'
      { workflowId = pWorkflowId_,
        executionId = pExecutionId_,
        token = pToken_,
        status = pStatus_
      }

-- | A unique identifier for the workflow.
sendWorkflowStepState_workflowId :: Lens.Lens' SendWorkflowStepState Prelude.Text
sendWorkflowStepState_workflowId = Lens.lens (\SendWorkflowStepState' {workflowId} -> workflowId) (\s@SendWorkflowStepState' {} a -> s {workflowId = a} :: SendWorkflowStepState)

-- | A unique identifier for the execution of a workflow.
sendWorkflowStepState_executionId :: Lens.Lens' SendWorkflowStepState Prelude.Text
sendWorkflowStepState_executionId = Lens.lens (\SendWorkflowStepState' {executionId} -> executionId) (\s@SendWorkflowStepState' {} a -> s {executionId = a} :: SendWorkflowStepState)

-- | Used to distinguish between multiple callbacks for multiple Lambda steps
-- within the same execution.
sendWorkflowStepState_token :: Lens.Lens' SendWorkflowStepState Prelude.Text
sendWorkflowStepState_token = Lens.lens (\SendWorkflowStepState' {token} -> token) (\s@SendWorkflowStepState' {} a -> s {token = a} :: SendWorkflowStepState)

-- | Indicates whether the specified step succeeded or failed.
sendWorkflowStepState_status :: Lens.Lens' SendWorkflowStepState CustomStepStatus
sendWorkflowStepState_status = Lens.lens (\SendWorkflowStepState' {status} -> status) (\s@SendWorkflowStepState' {} a -> s {status = a} :: SendWorkflowStepState)

instance Core.AWSRequest SendWorkflowStepState where
  type
    AWSResponse SendWorkflowStepState =
      SendWorkflowStepStateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          SendWorkflowStepStateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendWorkflowStepState where
  hashWithSalt _salt SendWorkflowStepState' {..} =
    _salt `Prelude.hashWithSalt` workflowId
      `Prelude.hashWithSalt` executionId
      `Prelude.hashWithSalt` token
      `Prelude.hashWithSalt` status

instance Prelude.NFData SendWorkflowStepState where
  rnf SendWorkflowStepState' {..} =
    Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf executionId
      `Prelude.seq` Prelude.rnf token
      `Prelude.seq` Prelude.rnf status

instance Core.ToHeaders SendWorkflowStepState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TransferService.SendWorkflowStepState" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SendWorkflowStepState where
  toJSON SendWorkflowStepState' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("WorkflowId" Core..= workflowId),
            Prelude.Just ("ExecutionId" Core..= executionId),
            Prelude.Just ("Token" Core..= token),
            Prelude.Just ("Status" Core..= status)
          ]
      )

instance Core.ToPath SendWorkflowStepState where
  toPath = Prelude.const "/"

instance Core.ToQuery SendWorkflowStepState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendWorkflowStepStateResponse' smart constructor.
data SendWorkflowStepStateResponse = SendWorkflowStepStateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendWorkflowStepStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'sendWorkflowStepStateResponse_httpStatus' - The response's http status code.
newSendWorkflowStepStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendWorkflowStepStateResponse
newSendWorkflowStepStateResponse pHttpStatus_ =
  SendWorkflowStepStateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
sendWorkflowStepStateResponse_httpStatus :: Lens.Lens' SendWorkflowStepStateResponse Prelude.Int
sendWorkflowStepStateResponse_httpStatus = Lens.lens (\SendWorkflowStepStateResponse' {httpStatus} -> httpStatus) (\s@SendWorkflowStepStateResponse' {} a -> s {httpStatus = a} :: SendWorkflowStepStateResponse)

instance Prelude.NFData SendWorkflowStepStateResponse where
  rnf SendWorkflowStepStateResponse' {..} =
    Prelude.rnf httpStatus
