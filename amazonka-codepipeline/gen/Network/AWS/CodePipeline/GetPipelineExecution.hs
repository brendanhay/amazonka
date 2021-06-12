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
-- Module      : Network.AWS.CodePipeline.GetPipelineExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an execution of a pipeline, including details
-- about artifacts, the pipeline execution ID, and the name, version, and
-- status of the pipeline.
module Network.AWS.CodePipeline.GetPipelineExecution
  ( -- * Creating a Request
    GetPipelineExecution (..),
    newGetPipelineExecution,

    -- * Request Lenses
    getPipelineExecution_pipelineName,
    getPipelineExecution_pipelineExecutionId,

    -- * Destructuring the Response
    GetPipelineExecutionResponse (..),
    newGetPipelineExecutionResponse,

    -- * Response Lenses
    getPipelineExecutionResponse_pipelineExecution,
    getPipelineExecutionResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetPipelineExecution@ action.
--
-- /See:/ 'newGetPipelineExecution' smart constructor.
data GetPipelineExecution = GetPipelineExecution'
  { -- | The name of the pipeline about which you want to get execution details.
    pipelineName :: Core.Text,
    -- | The ID of the pipeline execution about which you want to get execution
    -- details.
    pipelineExecutionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPipelineExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineName', 'getPipelineExecution_pipelineName' - The name of the pipeline about which you want to get execution details.
--
-- 'pipelineExecutionId', 'getPipelineExecution_pipelineExecutionId' - The ID of the pipeline execution about which you want to get execution
-- details.
newGetPipelineExecution ::
  -- | 'pipelineName'
  Core.Text ->
  -- | 'pipelineExecutionId'
  Core.Text ->
  GetPipelineExecution
newGetPipelineExecution
  pPipelineName_
  pPipelineExecutionId_ =
    GetPipelineExecution'
      { pipelineName =
          pPipelineName_,
        pipelineExecutionId = pPipelineExecutionId_
      }

-- | The name of the pipeline about which you want to get execution details.
getPipelineExecution_pipelineName :: Lens.Lens' GetPipelineExecution Core.Text
getPipelineExecution_pipelineName = Lens.lens (\GetPipelineExecution' {pipelineName} -> pipelineName) (\s@GetPipelineExecution' {} a -> s {pipelineName = a} :: GetPipelineExecution)

-- | The ID of the pipeline execution about which you want to get execution
-- details.
getPipelineExecution_pipelineExecutionId :: Lens.Lens' GetPipelineExecution Core.Text
getPipelineExecution_pipelineExecutionId = Lens.lens (\GetPipelineExecution' {pipelineExecutionId} -> pipelineExecutionId) (\s@GetPipelineExecution' {} a -> s {pipelineExecutionId = a} :: GetPipelineExecution)

instance Core.AWSRequest GetPipelineExecution where
  type
    AWSResponse GetPipelineExecution =
      GetPipelineExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPipelineExecutionResponse'
            Core.<$> (x Core..?> "pipelineExecution")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetPipelineExecution

instance Core.NFData GetPipelineExecution

instance Core.ToHeaders GetPipelineExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.GetPipelineExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetPipelineExecution where
  toJSON GetPipelineExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineName" Core..= pipelineName),
            Core.Just
              ("pipelineExecutionId" Core..= pipelineExecutionId)
          ]
      )

instance Core.ToPath GetPipelineExecution where
  toPath = Core.const "/"

instance Core.ToQuery GetPipelineExecution where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @GetPipelineExecution@ action.
--
-- /See:/ 'newGetPipelineExecutionResponse' smart constructor.
data GetPipelineExecutionResponse = GetPipelineExecutionResponse'
  { -- | Represents information about the execution of a pipeline.
    pipelineExecution :: Core.Maybe PipelineExecution,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPipelineExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecution', 'getPipelineExecutionResponse_pipelineExecution' - Represents information about the execution of a pipeline.
--
-- 'httpStatus', 'getPipelineExecutionResponse_httpStatus' - The response's http status code.
newGetPipelineExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetPipelineExecutionResponse
newGetPipelineExecutionResponse pHttpStatus_ =
  GetPipelineExecutionResponse'
    { pipelineExecution =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents information about the execution of a pipeline.
getPipelineExecutionResponse_pipelineExecution :: Lens.Lens' GetPipelineExecutionResponse (Core.Maybe PipelineExecution)
getPipelineExecutionResponse_pipelineExecution = Lens.lens (\GetPipelineExecutionResponse' {pipelineExecution} -> pipelineExecution) (\s@GetPipelineExecutionResponse' {} a -> s {pipelineExecution = a} :: GetPipelineExecutionResponse)

-- | The response's http status code.
getPipelineExecutionResponse_httpStatus :: Lens.Lens' GetPipelineExecutionResponse Core.Int
getPipelineExecutionResponse_httpStatus = Lens.lens (\GetPipelineExecutionResponse' {httpStatus} -> httpStatus) (\s@GetPipelineExecutionResponse' {} a -> s {httpStatus = a} :: GetPipelineExecutionResponse)

instance Core.NFData GetPipelineExecutionResponse
