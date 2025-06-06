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
-- Module      : Amazonka.StepFunctions.SendTaskSuccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by activity workers and task states using the
-- <https://docs.aws.amazon.com/step-functions/latest/dg/connect-to-resource.html#connect-wait-token callback>
-- pattern to report that the task identified by the @taskToken@ completed
-- successfully.
module Amazonka.StepFunctions.SendTaskSuccess
  ( -- * Creating a Request
    SendTaskSuccess (..),
    newSendTaskSuccess,

    -- * Request Lenses
    sendTaskSuccess_taskToken,
    sendTaskSuccess_output,

    -- * Destructuring the Response
    SendTaskSuccessResponse (..),
    newSendTaskSuccessResponse,

    -- * Response Lenses
    sendTaskSuccessResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newSendTaskSuccess' smart constructor.
data SendTaskSuccess = SendTaskSuccess'
  { -- | The token that represents this task. Task tokens are generated by Step
    -- Functions when tasks are assigned to a worker, or in the
    -- <https://docs.aws.amazon.com/step-functions/latest/dg/input-output-contextobject.html context object>
    -- when a workflow enters a task state. See
    -- GetActivityTaskOutput$taskToken.
    taskToken :: Prelude.Text,
    -- | The JSON output of the task. Length constraints apply to the payload
    -- size, and are expressed as bytes in UTF-8 encoding.
    output :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendTaskSuccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskToken', 'sendTaskSuccess_taskToken' - The token that represents this task. Task tokens are generated by Step
-- Functions when tasks are assigned to a worker, or in the
-- <https://docs.aws.amazon.com/step-functions/latest/dg/input-output-contextobject.html context object>
-- when a workflow enters a task state. See
-- GetActivityTaskOutput$taskToken.
--
-- 'output', 'sendTaskSuccess_output' - The JSON output of the task. Length constraints apply to the payload
-- size, and are expressed as bytes in UTF-8 encoding.
newSendTaskSuccess ::
  -- | 'taskToken'
  Prelude.Text ->
  -- | 'output'
  Prelude.Text ->
  SendTaskSuccess
newSendTaskSuccess pTaskToken_ pOutput_ =
  SendTaskSuccess'
    { taskToken = pTaskToken_,
      output = Data._Sensitive Lens.# pOutput_
    }

-- | The token that represents this task. Task tokens are generated by Step
-- Functions when tasks are assigned to a worker, or in the
-- <https://docs.aws.amazon.com/step-functions/latest/dg/input-output-contextobject.html context object>
-- when a workflow enters a task state. See
-- GetActivityTaskOutput$taskToken.
sendTaskSuccess_taskToken :: Lens.Lens' SendTaskSuccess Prelude.Text
sendTaskSuccess_taskToken = Lens.lens (\SendTaskSuccess' {taskToken} -> taskToken) (\s@SendTaskSuccess' {} a -> s {taskToken = a} :: SendTaskSuccess)

-- | The JSON output of the task. Length constraints apply to the payload
-- size, and are expressed as bytes in UTF-8 encoding.
sendTaskSuccess_output :: Lens.Lens' SendTaskSuccess Prelude.Text
sendTaskSuccess_output = Lens.lens (\SendTaskSuccess' {output} -> output) (\s@SendTaskSuccess' {} a -> s {output = a} :: SendTaskSuccess) Prelude.. Data._Sensitive

instance Core.AWSRequest SendTaskSuccess where
  type
    AWSResponse SendTaskSuccess =
      SendTaskSuccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          SendTaskSuccessResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendTaskSuccess where
  hashWithSalt _salt SendTaskSuccess' {..} =
    _salt
      `Prelude.hashWithSalt` taskToken
      `Prelude.hashWithSalt` output

instance Prelude.NFData SendTaskSuccess where
  rnf SendTaskSuccess' {..} =
    Prelude.rnf taskToken `Prelude.seq`
      Prelude.rnf output

instance Data.ToHeaders SendTaskSuccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.SendTaskSuccess" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendTaskSuccess where
  toJSON SendTaskSuccess' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("taskToken" Data..= taskToken),
            Prelude.Just ("output" Data..= output)
          ]
      )

instance Data.ToPath SendTaskSuccess where
  toPath = Prelude.const "/"

instance Data.ToQuery SendTaskSuccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendTaskSuccessResponse' smart constructor.
data SendTaskSuccessResponse = SendTaskSuccessResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendTaskSuccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'sendTaskSuccessResponse_httpStatus' - The response's http status code.
newSendTaskSuccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendTaskSuccessResponse
newSendTaskSuccessResponse pHttpStatus_ =
  SendTaskSuccessResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
sendTaskSuccessResponse_httpStatus :: Lens.Lens' SendTaskSuccessResponse Prelude.Int
sendTaskSuccessResponse_httpStatus = Lens.lens (\SendTaskSuccessResponse' {httpStatus} -> httpStatus) (\s@SendTaskSuccessResponse' {} a -> s {httpStatus = a} :: SendTaskSuccessResponse)

instance Prelude.NFData SendTaskSuccessResponse where
  rnf SendTaskSuccessResponse' {..} =
    Prelude.rnf httpStatus
