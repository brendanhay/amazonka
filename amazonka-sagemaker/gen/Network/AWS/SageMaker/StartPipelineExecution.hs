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
-- Module      : Network.AWS.SageMaker.StartPipelineExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a pipeline execution.
module Network.AWS.SageMaker.StartPipelineExecution
  ( -- * Creating a Request
    StartPipelineExecution (..),
    newStartPipelineExecution,

    -- * Request Lenses
    startPipelineExecution_pipelineExecutionDescription,
    startPipelineExecution_pipelineParameters,
    startPipelineExecution_pipelineExecutionDisplayName,
    startPipelineExecution_pipelineName,
    startPipelineExecution_clientRequestToken,

    -- * Destructuring the Response
    StartPipelineExecutionResponse (..),
    newStartPipelineExecutionResponse,

    -- * Response Lenses
    startPipelineExecutionResponse_pipelineExecutionArn,
    startPipelineExecutionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newStartPipelineExecution' smart constructor.
data StartPipelineExecution = StartPipelineExecution'
  { -- | The description of the pipeline execution.
    pipelineExecutionDescription :: Core.Maybe Core.Text,
    -- | Contains a list of pipeline parameters. This list can be empty.
    pipelineParameters :: Core.Maybe [Parameter],
    -- | The display name of the pipeline execution.
    pipelineExecutionDisplayName :: Core.Maybe Core.Text,
    -- | The name of the pipeline.
    pipelineName :: Core.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the operation. An idempotent operation completes no more
    -- than one time.
    clientRequestToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartPipelineExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionDescription', 'startPipelineExecution_pipelineExecutionDescription' - The description of the pipeline execution.
--
-- 'pipelineParameters', 'startPipelineExecution_pipelineParameters' - Contains a list of pipeline parameters. This list can be empty.
--
-- 'pipelineExecutionDisplayName', 'startPipelineExecution_pipelineExecutionDisplayName' - The display name of the pipeline execution.
--
-- 'pipelineName', 'startPipelineExecution_pipelineName' - The name of the pipeline.
--
-- 'clientRequestToken', 'startPipelineExecution_clientRequestToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time.
newStartPipelineExecution ::
  -- | 'pipelineName'
  Core.Text ->
  -- | 'clientRequestToken'
  Core.Text ->
  StartPipelineExecution
newStartPipelineExecution
  pPipelineName_
  pClientRequestToken_ =
    StartPipelineExecution'
      { pipelineExecutionDescription =
          Core.Nothing,
        pipelineParameters = Core.Nothing,
        pipelineExecutionDisplayName = Core.Nothing,
        pipelineName = pPipelineName_,
        clientRequestToken = pClientRequestToken_
      }

-- | The description of the pipeline execution.
startPipelineExecution_pipelineExecutionDescription :: Lens.Lens' StartPipelineExecution (Core.Maybe Core.Text)
startPipelineExecution_pipelineExecutionDescription = Lens.lens (\StartPipelineExecution' {pipelineExecutionDescription} -> pipelineExecutionDescription) (\s@StartPipelineExecution' {} a -> s {pipelineExecutionDescription = a} :: StartPipelineExecution)

-- | Contains a list of pipeline parameters. This list can be empty.
startPipelineExecution_pipelineParameters :: Lens.Lens' StartPipelineExecution (Core.Maybe [Parameter])
startPipelineExecution_pipelineParameters = Lens.lens (\StartPipelineExecution' {pipelineParameters} -> pipelineParameters) (\s@StartPipelineExecution' {} a -> s {pipelineParameters = a} :: StartPipelineExecution) Core.. Lens.mapping Lens._Coerce

-- | The display name of the pipeline execution.
startPipelineExecution_pipelineExecutionDisplayName :: Lens.Lens' StartPipelineExecution (Core.Maybe Core.Text)
startPipelineExecution_pipelineExecutionDisplayName = Lens.lens (\StartPipelineExecution' {pipelineExecutionDisplayName} -> pipelineExecutionDisplayName) (\s@StartPipelineExecution' {} a -> s {pipelineExecutionDisplayName = a} :: StartPipelineExecution)

-- | The name of the pipeline.
startPipelineExecution_pipelineName :: Lens.Lens' StartPipelineExecution Core.Text
startPipelineExecution_pipelineName = Lens.lens (\StartPipelineExecution' {pipelineName} -> pipelineName) (\s@StartPipelineExecution' {} a -> s {pipelineName = a} :: StartPipelineExecution)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time.
startPipelineExecution_clientRequestToken :: Lens.Lens' StartPipelineExecution Core.Text
startPipelineExecution_clientRequestToken = Lens.lens (\StartPipelineExecution' {clientRequestToken} -> clientRequestToken) (\s@StartPipelineExecution' {} a -> s {clientRequestToken = a} :: StartPipelineExecution)

instance Core.AWSRequest StartPipelineExecution where
  type
    AWSResponse StartPipelineExecution =
      StartPipelineExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartPipelineExecutionResponse'
            Core.<$> (x Core..?> "PipelineExecutionArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartPipelineExecution

instance Core.NFData StartPipelineExecution

instance Core.ToHeaders StartPipelineExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.StartPipelineExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartPipelineExecution where
  toJSON StartPipelineExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PipelineExecutionDescription" Core..=)
              Core.<$> pipelineExecutionDescription,
            ("PipelineParameters" Core..=)
              Core.<$> pipelineParameters,
            ("PipelineExecutionDisplayName" Core..=)
              Core.<$> pipelineExecutionDisplayName,
            Core.Just ("PipelineName" Core..= pipelineName),
            Core.Just
              ("ClientRequestToken" Core..= clientRequestToken)
          ]
      )

instance Core.ToPath StartPipelineExecution where
  toPath = Core.const "/"

instance Core.ToQuery StartPipelineExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartPipelineExecutionResponse' smart constructor.
data StartPipelineExecutionResponse = StartPipelineExecutionResponse'
  { -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartPipelineExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionArn', 'startPipelineExecutionResponse_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
--
-- 'httpStatus', 'startPipelineExecutionResponse_httpStatus' - The response's http status code.
newStartPipelineExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartPipelineExecutionResponse
newStartPipelineExecutionResponse pHttpStatus_ =
  StartPipelineExecutionResponse'
    { pipelineExecutionArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the pipeline execution.
startPipelineExecutionResponse_pipelineExecutionArn :: Lens.Lens' StartPipelineExecutionResponse (Core.Maybe Core.Text)
startPipelineExecutionResponse_pipelineExecutionArn = Lens.lens (\StartPipelineExecutionResponse' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@StartPipelineExecutionResponse' {} a -> s {pipelineExecutionArn = a} :: StartPipelineExecutionResponse)

-- | The response's http status code.
startPipelineExecutionResponse_httpStatus :: Lens.Lens' StartPipelineExecutionResponse Core.Int
startPipelineExecutionResponse_httpStatus = Lens.lens (\StartPipelineExecutionResponse' {httpStatus} -> httpStatus) (\s@StartPipelineExecutionResponse' {} a -> s {httpStatus = a} :: StartPipelineExecutionResponse)

instance Core.NFData StartPipelineExecutionResponse
