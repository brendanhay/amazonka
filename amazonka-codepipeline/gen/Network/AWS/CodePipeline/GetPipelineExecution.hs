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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetPipelineExecution@ action.
--
-- /See:/ 'newGetPipelineExecution' smart constructor.
data GetPipelineExecution = GetPipelineExecution'
  { -- | The name of the pipeline about which you want to get execution details.
    pipelineName :: Prelude.Text,
    -- | The ID of the pipeline execution about which you want to get execution
    -- details.
    pipelineExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'pipelineExecutionId'
  Prelude.Text ->
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
getPipelineExecution_pipelineName :: Lens.Lens' GetPipelineExecution Prelude.Text
getPipelineExecution_pipelineName = Lens.lens (\GetPipelineExecution' {pipelineName} -> pipelineName) (\s@GetPipelineExecution' {} a -> s {pipelineName = a} :: GetPipelineExecution)

-- | The ID of the pipeline execution about which you want to get execution
-- details.
getPipelineExecution_pipelineExecutionId :: Lens.Lens' GetPipelineExecution Prelude.Text
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
            Prelude.<$> (x Core..?> "pipelineExecution")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPipelineExecution

instance Prelude.NFData GetPipelineExecution

instance Core.ToHeaders GetPipelineExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.GetPipelineExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetPipelineExecution where
  toJSON GetPipelineExecution' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("pipelineName" Core..= pipelineName),
            Prelude.Just
              ("pipelineExecutionId" Core..= pipelineExecutionId)
          ]
      )

instance Core.ToPath GetPipelineExecution where
  toPath = Prelude.const "/"

instance Core.ToQuery GetPipelineExecution where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetPipelineExecution@ action.
--
-- /See:/ 'newGetPipelineExecutionResponse' smart constructor.
data GetPipelineExecutionResponse = GetPipelineExecutionResponse'
  { -- | Represents information about the execution of a pipeline.
    pipelineExecution :: Prelude.Maybe PipelineExecution,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetPipelineExecutionResponse
newGetPipelineExecutionResponse pHttpStatus_ =
  GetPipelineExecutionResponse'
    { pipelineExecution =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents information about the execution of a pipeline.
getPipelineExecutionResponse_pipelineExecution :: Lens.Lens' GetPipelineExecutionResponse (Prelude.Maybe PipelineExecution)
getPipelineExecutionResponse_pipelineExecution = Lens.lens (\GetPipelineExecutionResponse' {pipelineExecution} -> pipelineExecution) (\s@GetPipelineExecutionResponse' {} a -> s {pipelineExecution = a} :: GetPipelineExecutionResponse)

-- | The response's http status code.
getPipelineExecutionResponse_httpStatus :: Lens.Lens' GetPipelineExecutionResponse Prelude.Int
getPipelineExecutionResponse_httpStatus = Lens.lens (\GetPipelineExecutionResponse' {httpStatus} -> httpStatus) (\s@GetPipelineExecutionResponse' {} a -> s {httpStatus = a} :: GetPipelineExecutionResponse)

instance Prelude.NFData GetPipelineExecutionResponse
