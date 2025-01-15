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
-- Module      : Amazonka.SageMaker.SendPipelineExecutionStepSuccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Notifies the pipeline that the execution of a callback step succeeded
-- and provides a list of the step\'s output parameters. When a callback
-- step is run, the pipeline generates a callback token and includes the
-- token in a message sent to Amazon Simple Queue Service (Amazon SQS).
module Amazonka.SageMaker.SendPipelineExecutionStepSuccess
  ( -- * Creating a Request
    SendPipelineExecutionStepSuccess (..),
    newSendPipelineExecutionStepSuccess,

    -- * Request Lenses
    sendPipelineExecutionStepSuccess_clientRequestToken,
    sendPipelineExecutionStepSuccess_outputParameters,
    sendPipelineExecutionStepSuccess_callbackToken,

    -- * Destructuring the Response
    SendPipelineExecutionStepSuccessResponse (..),
    newSendPipelineExecutionStepSuccessResponse,

    -- * Response Lenses
    sendPipelineExecutionStepSuccessResponse_pipelineExecutionArn,
    sendPipelineExecutionStepSuccessResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newSendPipelineExecutionStepSuccess' smart constructor.
data SendPipelineExecutionStepSuccess = SendPipelineExecutionStepSuccess'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the operation. An idempotent operation completes no more
    -- than one time.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the output parameters of the callback step.
    outputParameters :: Prelude.Maybe [OutputParameter],
    -- | The pipeline generated token from the Amazon SQS queue.
    callbackToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendPipelineExecutionStepSuccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'sendPipelineExecutionStepSuccess_clientRequestToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time.
--
-- 'outputParameters', 'sendPipelineExecutionStepSuccess_outputParameters' - A list of the output parameters of the callback step.
--
-- 'callbackToken', 'sendPipelineExecutionStepSuccess_callbackToken' - The pipeline generated token from the Amazon SQS queue.
newSendPipelineExecutionStepSuccess ::
  -- | 'callbackToken'
  Prelude.Text ->
  SendPipelineExecutionStepSuccess
newSendPipelineExecutionStepSuccess pCallbackToken_ =
  SendPipelineExecutionStepSuccess'
    { clientRequestToken =
        Prelude.Nothing,
      outputParameters = Prelude.Nothing,
      callbackToken = pCallbackToken_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time.
sendPipelineExecutionStepSuccess_clientRequestToken :: Lens.Lens' SendPipelineExecutionStepSuccess (Prelude.Maybe Prelude.Text)
sendPipelineExecutionStepSuccess_clientRequestToken = Lens.lens (\SendPipelineExecutionStepSuccess' {clientRequestToken} -> clientRequestToken) (\s@SendPipelineExecutionStepSuccess' {} a -> s {clientRequestToken = a} :: SendPipelineExecutionStepSuccess)

-- | A list of the output parameters of the callback step.
sendPipelineExecutionStepSuccess_outputParameters :: Lens.Lens' SendPipelineExecutionStepSuccess (Prelude.Maybe [OutputParameter])
sendPipelineExecutionStepSuccess_outputParameters = Lens.lens (\SendPipelineExecutionStepSuccess' {outputParameters} -> outputParameters) (\s@SendPipelineExecutionStepSuccess' {} a -> s {outputParameters = a} :: SendPipelineExecutionStepSuccess) Prelude.. Lens.mapping Lens.coerced

-- | The pipeline generated token from the Amazon SQS queue.
sendPipelineExecutionStepSuccess_callbackToken :: Lens.Lens' SendPipelineExecutionStepSuccess Prelude.Text
sendPipelineExecutionStepSuccess_callbackToken = Lens.lens (\SendPipelineExecutionStepSuccess' {callbackToken} -> callbackToken) (\s@SendPipelineExecutionStepSuccess' {} a -> s {callbackToken = a} :: SendPipelineExecutionStepSuccess)

instance
  Core.AWSRequest
    SendPipelineExecutionStepSuccess
  where
  type
    AWSResponse SendPipelineExecutionStepSuccess =
      SendPipelineExecutionStepSuccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendPipelineExecutionStepSuccessResponse'
            Prelude.<$> (x Data..?> "PipelineExecutionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SendPipelineExecutionStepSuccess
  where
  hashWithSalt
    _salt
    SendPipelineExecutionStepSuccess' {..} =
      _salt
        `Prelude.hashWithSalt` clientRequestToken
        `Prelude.hashWithSalt` outputParameters
        `Prelude.hashWithSalt` callbackToken

instance
  Prelude.NFData
    SendPipelineExecutionStepSuccess
  where
  rnf SendPipelineExecutionStepSuccess' {..} =
    Prelude.rnf clientRequestToken `Prelude.seq`
      Prelude.rnf outputParameters `Prelude.seq`
        Prelude.rnf callbackToken

instance
  Data.ToHeaders
    SendPipelineExecutionStepSuccess
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.SendPipelineExecutionStepSuccess" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendPipelineExecutionStepSuccess where
  toJSON SendPipelineExecutionStepSuccess' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("OutputParameters" Data..=)
              Prelude.<$> outputParameters,
            Prelude.Just
              ("CallbackToken" Data..= callbackToken)
          ]
      )

instance Data.ToPath SendPipelineExecutionStepSuccess where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    SendPipelineExecutionStepSuccess
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendPipelineExecutionStepSuccessResponse' smart constructor.
data SendPipelineExecutionStepSuccessResponse = SendPipelineExecutionStepSuccessResponse'
  { -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendPipelineExecutionStepSuccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionArn', 'sendPipelineExecutionStepSuccessResponse_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
--
-- 'httpStatus', 'sendPipelineExecutionStepSuccessResponse_httpStatus' - The response's http status code.
newSendPipelineExecutionStepSuccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendPipelineExecutionStepSuccessResponse
newSendPipelineExecutionStepSuccessResponse
  pHttpStatus_ =
    SendPipelineExecutionStepSuccessResponse'
      { pipelineExecutionArn =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the pipeline execution.
sendPipelineExecutionStepSuccessResponse_pipelineExecutionArn :: Lens.Lens' SendPipelineExecutionStepSuccessResponse (Prelude.Maybe Prelude.Text)
sendPipelineExecutionStepSuccessResponse_pipelineExecutionArn = Lens.lens (\SendPipelineExecutionStepSuccessResponse' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@SendPipelineExecutionStepSuccessResponse' {} a -> s {pipelineExecutionArn = a} :: SendPipelineExecutionStepSuccessResponse)

-- | The response's http status code.
sendPipelineExecutionStepSuccessResponse_httpStatus :: Lens.Lens' SendPipelineExecutionStepSuccessResponse Prelude.Int
sendPipelineExecutionStepSuccessResponse_httpStatus = Lens.lens (\SendPipelineExecutionStepSuccessResponse' {httpStatus} -> httpStatus) (\s@SendPipelineExecutionStepSuccessResponse' {} a -> s {httpStatus = a} :: SendPipelineExecutionStepSuccessResponse)

instance
  Prelude.NFData
    SendPipelineExecutionStepSuccessResponse
  where
  rnf SendPipelineExecutionStepSuccessResponse' {..} =
    Prelude.rnf pipelineExecutionArn `Prelude.seq`
      Prelude.rnf httpStatus
