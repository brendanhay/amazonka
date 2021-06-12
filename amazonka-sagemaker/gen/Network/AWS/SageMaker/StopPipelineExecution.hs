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
-- Module      : Network.AWS.SageMaker.StopPipelineExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a pipeline execution.
module Network.AWS.SageMaker.StopPipelineExecution
  ( -- * Creating a Request
    StopPipelineExecution (..),
    newStopPipelineExecution,

    -- * Request Lenses
    stopPipelineExecution_pipelineExecutionArn,
    stopPipelineExecution_clientRequestToken,

    -- * Destructuring the Response
    StopPipelineExecutionResponse (..),
    newStopPipelineExecutionResponse,

    -- * Response Lenses
    stopPipelineExecutionResponse_pipelineExecutionArn,
    stopPipelineExecutionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newStopPipelineExecution' smart constructor.
data StopPipelineExecution = StopPipelineExecution'
  { -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Core.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the operation. An idempotent operation completes no more
    -- than one time.
    clientRequestToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopPipelineExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionArn', 'stopPipelineExecution_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
--
-- 'clientRequestToken', 'stopPipelineExecution_clientRequestToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time.
newStopPipelineExecution ::
  -- | 'pipelineExecutionArn'
  Core.Text ->
  -- | 'clientRequestToken'
  Core.Text ->
  StopPipelineExecution
newStopPipelineExecution
  pPipelineExecutionArn_
  pClientRequestToken_ =
    StopPipelineExecution'
      { pipelineExecutionArn =
          pPipelineExecutionArn_,
        clientRequestToken = pClientRequestToken_
      }

-- | The Amazon Resource Name (ARN) of the pipeline execution.
stopPipelineExecution_pipelineExecutionArn :: Lens.Lens' StopPipelineExecution Core.Text
stopPipelineExecution_pipelineExecutionArn = Lens.lens (\StopPipelineExecution' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@StopPipelineExecution' {} a -> s {pipelineExecutionArn = a} :: StopPipelineExecution)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time.
stopPipelineExecution_clientRequestToken :: Lens.Lens' StopPipelineExecution Core.Text
stopPipelineExecution_clientRequestToken = Lens.lens (\StopPipelineExecution' {clientRequestToken} -> clientRequestToken) (\s@StopPipelineExecution' {} a -> s {clientRequestToken = a} :: StopPipelineExecution)

instance Core.AWSRequest StopPipelineExecution where
  type
    AWSResponse StopPipelineExecution =
      StopPipelineExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopPipelineExecutionResponse'
            Core.<$> (x Core..?> "PipelineExecutionArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopPipelineExecution

instance Core.NFData StopPipelineExecution

instance Core.ToHeaders StopPipelineExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.StopPipelineExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopPipelineExecution where
  toJSON StopPipelineExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "PipelineExecutionArn"
                  Core..= pipelineExecutionArn
              ),
            Core.Just
              ("ClientRequestToken" Core..= clientRequestToken)
          ]
      )

instance Core.ToPath StopPipelineExecution where
  toPath = Core.const "/"

instance Core.ToQuery StopPipelineExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopPipelineExecutionResponse' smart constructor.
data StopPipelineExecutionResponse = StopPipelineExecutionResponse'
  { -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopPipelineExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionArn', 'stopPipelineExecutionResponse_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
--
-- 'httpStatus', 'stopPipelineExecutionResponse_httpStatus' - The response's http status code.
newStopPipelineExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopPipelineExecutionResponse
newStopPipelineExecutionResponse pHttpStatus_ =
  StopPipelineExecutionResponse'
    { pipelineExecutionArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the pipeline execution.
stopPipelineExecutionResponse_pipelineExecutionArn :: Lens.Lens' StopPipelineExecutionResponse (Core.Maybe Core.Text)
stopPipelineExecutionResponse_pipelineExecutionArn = Lens.lens (\StopPipelineExecutionResponse' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@StopPipelineExecutionResponse' {} a -> s {pipelineExecutionArn = a} :: StopPipelineExecutionResponse)

-- | The response's http status code.
stopPipelineExecutionResponse_httpStatus :: Lens.Lens' StopPipelineExecutionResponse Core.Int
stopPipelineExecutionResponse_httpStatus = Lens.lens (\StopPipelineExecutionResponse' {httpStatus} -> httpStatus) (\s@StopPipelineExecutionResponse' {} a -> s {httpStatus = a} :: StopPipelineExecutionResponse)

instance Core.NFData StopPipelineExecutionResponse
