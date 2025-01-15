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
-- Module      : Amazonka.SageMaker.SendPipelineExecutionStepFailure
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Notifies the pipeline that the execution of a callback step failed,
-- along with a message describing why. When a callback step is run, the
-- pipeline generates a callback token and includes the token in a message
-- sent to Amazon Simple Queue Service (Amazon SQS).
module Amazonka.SageMaker.SendPipelineExecutionStepFailure
  ( -- * Creating a Request
    SendPipelineExecutionStepFailure (..),
    newSendPipelineExecutionStepFailure,

    -- * Request Lenses
    sendPipelineExecutionStepFailure_clientRequestToken,
    sendPipelineExecutionStepFailure_failureReason,
    sendPipelineExecutionStepFailure_callbackToken,

    -- * Destructuring the Response
    SendPipelineExecutionStepFailureResponse (..),
    newSendPipelineExecutionStepFailureResponse,

    -- * Response Lenses
    sendPipelineExecutionStepFailureResponse_pipelineExecutionArn,
    sendPipelineExecutionStepFailureResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newSendPipelineExecutionStepFailure' smart constructor.
data SendPipelineExecutionStepFailure = SendPipelineExecutionStepFailure'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the operation. An idempotent operation completes no more
    -- than one time.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | A message describing why the step failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The pipeline generated token from the Amazon SQS queue.
    callbackToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendPipelineExecutionStepFailure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'sendPipelineExecutionStepFailure_clientRequestToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time.
--
-- 'failureReason', 'sendPipelineExecutionStepFailure_failureReason' - A message describing why the step failed.
--
-- 'callbackToken', 'sendPipelineExecutionStepFailure_callbackToken' - The pipeline generated token from the Amazon SQS queue.
newSendPipelineExecutionStepFailure ::
  -- | 'callbackToken'
  Prelude.Text ->
  SendPipelineExecutionStepFailure
newSendPipelineExecutionStepFailure pCallbackToken_ =
  SendPipelineExecutionStepFailure'
    { clientRequestToken =
        Prelude.Nothing,
      failureReason = Prelude.Nothing,
      callbackToken = pCallbackToken_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time.
sendPipelineExecutionStepFailure_clientRequestToken :: Lens.Lens' SendPipelineExecutionStepFailure (Prelude.Maybe Prelude.Text)
sendPipelineExecutionStepFailure_clientRequestToken = Lens.lens (\SendPipelineExecutionStepFailure' {clientRequestToken} -> clientRequestToken) (\s@SendPipelineExecutionStepFailure' {} a -> s {clientRequestToken = a} :: SendPipelineExecutionStepFailure)

-- | A message describing why the step failed.
sendPipelineExecutionStepFailure_failureReason :: Lens.Lens' SendPipelineExecutionStepFailure (Prelude.Maybe Prelude.Text)
sendPipelineExecutionStepFailure_failureReason = Lens.lens (\SendPipelineExecutionStepFailure' {failureReason} -> failureReason) (\s@SendPipelineExecutionStepFailure' {} a -> s {failureReason = a} :: SendPipelineExecutionStepFailure)

-- | The pipeline generated token from the Amazon SQS queue.
sendPipelineExecutionStepFailure_callbackToken :: Lens.Lens' SendPipelineExecutionStepFailure Prelude.Text
sendPipelineExecutionStepFailure_callbackToken = Lens.lens (\SendPipelineExecutionStepFailure' {callbackToken} -> callbackToken) (\s@SendPipelineExecutionStepFailure' {} a -> s {callbackToken = a} :: SendPipelineExecutionStepFailure)

instance
  Core.AWSRequest
    SendPipelineExecutionStepFailure
  where
  type
    AWSResponse SendPipelineExecutionStepFailure =
      SendPipelineExecutionStepFailureResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendPipelineExecutionStepFailureResponse'
            Prelude.<$> (x Data..?> "PipelineExecutionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SendPipelineExecutionStepFailure
  where
  hashWithSalt
    _salt
    SendPipelineExecutionStepFailure' {..} =
      _salt
        `Prelude.hashWithSalt` clientRequestToken
        `Prelude.hashWithSalt` failureReason
        `Prelude.hashWithSalt` callbackToken

instance
  Prelude.NFData
    SendPipelineExecutionStepFailure
  where
  rnf SendPipelineExecutionStepFailure' {..} =
    Prelude.rnf clientRequestToken `Prelude.seq`
      Prelude.rnf failureReason `Prelude.seq`
        Prelude.rnf callbackToken

instance
  Data.ToHeaders
    SendPipelineExecutionStepFailure
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.SendPipelineExecutionStepFailure" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendPipelineExecutionStepFailure where
  toJSON SendPipelineExecutionStepFailure' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("FailureReason" Data..=) Prelude.<$> failureReason,
            Prelude.Just
              ("CallbackToken" Data..= callbackToken)
          ]
      )

instance Data.ToPath SendPipelineExecutionStepFailure where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    SendPipelineExecutionStepFailure
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendPipelineExecutionStepFailureResponse' smart constructor.
data SendPipelineExecutionStepFailureResponse = SendPipelineExecutionStepFailureResponse'
  { -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendPipelineExecutionStepFailureResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionArn', 'sendPipelineExecutionStepFailureResponse_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
--
-- 'httpStatus', 'sendPipelineExecutionStepFailureResponse_httpStatus' - The response's http status code.
newSendPipelineExecutionStepFailureResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendPipelineExecutionStepFailureResponse
newSendPipelineExecutionStepFailureResponse
  pHttpStatus_ =
    SendPipelineExecutionStepFailureResponse'
      { pipelineExecutionArn =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the pipeline execution.
sendPipelineExecutionStepFailureResponse_pipelineExecutionArn :: Lens.Lens' SendPipelineExecutionStepFailureResponse (Prelude.Maybe Prelude.Text)
sendPipelineExecutionStepFailureResponse_pipelineExecutionArn = Lens.lens (\SendPipelineExecutionStepFailureResponse' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@SendPipelineExecutionStepFailureResponse' {} a -> s {pipelineExecutionArn = a} :: SendPipelineExecutionStepFailureResponse)

-- | The response's http status code.
sendPipelineExecutionStepFailureResponse_httpStatus :: Lens.Lens' SendPipelineExecutionStepFailureResponse Prelude.Int
sendPipelineExecutionStepFailureResponse_httpStatus = Lens.lens (\SendPipelineExecutionStepFailureResponse' {httpStatus} -> httpStatus) (\s@SendPipelineExecutionStepFailureResponse' {} a -> s {httpStatus = a} :: SendPipelineExecutionStepFailureResponse)

instance
  Prelude.NFData
    SendPipelineExecutionStepFailureResponse
  where
  rnf SendPipelineExecutionStepFailureResponse' {..} =
    Prelude.rnf pipelineExecutionArn `Prelude.seq`
      Prelude.rnf httpStatus
