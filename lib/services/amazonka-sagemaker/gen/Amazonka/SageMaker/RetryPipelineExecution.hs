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
-- Module      : Amazonka.SageMaker.RetryPipelineExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retry the execution of the pipeline.
module Amazonka.SageMaker.RetryPipelineExecution
  ( -- * Creating a Request
    RetryPipelineExecution (..),
    newRetryPipelineExecution,

    -- * Request Lenses
    retryPipelineExecution_parallelismConfiguration,
    retryPipelineExecution_pipelineExecutionArn,
    retryPipelineExecution_clientRequestToken,

    -- * Destructuring the Response
    RetryPipelineExecutionResponse (..),
    newRetryPipelineExecutionResponse,

    -- * Response Lenses
    retryPipelineExecutionResponse_pipelineExecutionArn,
    retryPipelineExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newRetryPipelineExecution' smart constructor.
data RetryPipelineExecution = RetryPipelineExecution'
  { -- | This configuration, if specified, overrides the parallelism
    -- configuration of the parent pipeline.
    parallelismConfiguration :: Prelude.Maybe ParallelismConfiguration,
    -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Prelude.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the operation. An idempotent operation completes no more
    -- than once.
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetryPipelineExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parallelismConfiguration', 'retryPipelineExecution_parallelismConfiguration' - This configuration, if specified, overrides the parallelism
-- configuration of the parent pipeline.
--
-- 'pipelineExecutionArn', 'retryPipelineExecution_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
--
-- 'clientRequestToken', 'retryPipelineExecution_clientRequestToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than once.
newRetryPipelineExecution ::
  -- | 'pipelineExecutionArn'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  RetryPipelineExecution
newRetryPipelineExecution
  pPipelineExecutionArn_
  pClientRequestToken_ =
    RetryPipelineExecution'
      { parallelismConfiguration =
          Prelude.Nothing,
        pipelineExecutionArn = pPipelineExecutionArn_,
        clientRequestToken = pClientRequestToken_
      }

-- | This configuration, if specified, overrides the parallelism
-- configuration of the parent pipeline.
retryPipelineExecution_parallelismConfiguration :: Lens.Lens' RetryPipelineExecution (Prelude.Maybe ParallelismConfiguration)
retryPipelineExecution_parallelismConfiguration = Lens.lens (\RetryPipelineExecution' {parallelismConfiguration} -> parallelismConfiguration) (\s@RetryPipelineExecution' {} a -> s {parallelismConfiguration = a} :: RetryPipelineExecution)

-- | The Amazon Resource Name (ARN) of the pipeline execution.
retryPipelineExecution_pipelineExecutionArn :: Lens.Lens' RetryPipelineExecution Prelude.Text
retryPipelineExecution_pipelineExecutionArn = Lens.lens (\RetryPipelineExecution' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@RetryPipelineExecution' {} a -> s {pipelineExecutionArn = a} :: RetryPipelineExecution)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than once.
retryPipelineExecution_clientRequestToken :: Lens.Lens' RetryPipelineExecution Prelude.Text
retryPipelineExecution_clientRequestToken = Lens.lens (\RetryPipelineExecution' {clientRequestToken} -> clientRequestToken) (\s@RetryPipelineExecution' {} a -> s {clientRequestToken = a} :: RetryPipelineExecution)

instance Core.AWSRequest RetryPipelineExecution where
  type
    AWSResponse RetryPipelineExecution =
      RetryPipelineExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RetryPipelineExecutionResponse'
            Prelude.<$> (x Data..?> "PipelineExecutionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RetryPipelineExecution where
  hashWithSalt _salt RetryPipelineExecution' {..} =
    _salt
      `Prelude.hashWithSalt` parallelismConfiguration
      `Prelude.hashWithSalt` pipelineExecutionArn
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData RetryPipelineExecution where
  rnf RetryPipelineExecution' {..} =
    Prelude.rnf parallelismConfiguration `Prelude.seq`
      Prelude.rnf pipelineExecutionArn `Prelude.seq`
        Prelude.rnf clientRequestToken

instance Data.ToHeaders RetryPipelineExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.RetryPipelineExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RetryPipelineExecution where
  toJSON RetryPipelineExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ParallelismConfiguration" Data..=)
              Prelude.<$> parallelismConfiguration,
            Prelude.Just
              ( "PipelineExecutionArn"
                  Data..= pipelineExecutionArn
              ),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath RetryPipelineExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery RetryPipelineExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRetryPipelineExecutionResponse' smart constructor.
data RetryPipelineExecutionResponse = RetryPipelineExecutionResponse'
  { -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetryPipelineExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionArn', 'retryPipelineExecutionResponse_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
--
-- 'httpStatus', 'retryPipelineExecutionResponse_httpStatus' - The response's http status code.
newRetryPipelineExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RetryPipelineExecutionResponse
newRetryPipelineExecutionResponse pHttpStatus_ =
  RetryPipelineExecutionResponse'
    { pipelineExecutionArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the pipeline execution.
retryPipelineExecutionResponse_pipelineExecutionArn :: Lens.Lens' RetryPipelineExecutionResponse (Prelude.Maybe Prelude.Text)
retryPipelineExecutionResponse_pipelineExecutionArn = Lens.lens (\RetryPipelineExecutionResponse' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@RetryPipelineExecutionResponse' {} a -> s {pipelineExecutionArn = a} :: RetryPipelineExecutionResponse)

-- | The response's http status code.
retryPipelineExecutionResponse_httpStatus :: Lens.Lens' RetryPipelineExecutionResponse Prelude.Int
retryPipelineExecutionResponse_httpStatus = Lens.lens (\RetryPipelineExecutionResponse' {httpStatus} -> httpStatus) (\s@RetryPipelineExecutionResponse' {} a -> s {httpStatus = a} :: RetryPipelineExecutionResponse)

instance
  Prelude.NFData
    RetryPipelineExecutionResponse
  where
  rnf RetryPipelineExecutionResponse' {..} =
    Prelude.rnf pipelineExecutionArn `Prelude.seq`
      Prelude.rnf httpStatus
