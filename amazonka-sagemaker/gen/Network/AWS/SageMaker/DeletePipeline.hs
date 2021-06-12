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
-- Module      : Network.AWS.SageMaker.DeletePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a pipeline if there are no in-progress executions.
module Network.AWS.SageMaker.DeletePipeline
  ( -- * Creating a Request
    DeletePipeline (..),
    newDeletePipeline,

    -- * Request Lenses
    deletePipeline_pipelineName,
    deletePipeline_clientRequestToken,

    -- * Destructuring the Response
    DeletePipelineResponse (..),
    newDeletePipelineResponse,

    -- * Response Lenses
    deletePipelineResponse_pipelineArn,
    deletePipelineResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeletePipeline' smart constructor.
data DeletePipeline = DeletePipeline'
  { -- | The name of the pipeline to delete.
    pipelineName :: Core.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the operation. An idempotent operation completes no more
    -- than one time.
    clientRequestToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineName', 'deletePipeline_pipelineName' - The name of the pipeline to delete.
--
-- 'clientRequestToken', 'deletePipeline_clientRequestToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time.
newDeletePipeline ::
  -- | 'pipelineName'
  Core.Text ->
  -- | 'clientRequestToken'
  Core.Text ->
  DeletePipeline
newDeletePipeline pPipelineName_ pClientRequestToken_ =
  DeletePipeline'
    { pipelineName = pPipelineName_,
      clientRequestToken = pClientRequestToken_
    }

-- | The name of the pipeline to delete.
deletePipeline_pipelineName :: Lens.Lens' DeletePipeline Core.Text
deletePipeline_pipelineName = Lens.lens (\DeletePipeline' {pipelineName} -> pipelineName) (\s@DeletePipeline' {} a -> s {pipelineName = a} :: DeletePipeline)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time.
deletePipeline_clientRequestToken :: Lens.Lens' DeletePipeline Core.Text
deletePipeline_clientRequestToken = Lens.lens (\DeletePipeline' {clientRequestToken} -> clientRequestToken) (\s@DeletePipeline' {} a -> s {clientRequestToken = a} :: DeletePipeline)

instance Core.AWSRequest DeletePipeline where
  type
    AWSResponse DeletePipeline =
      DeletePipelineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePipelineResponse'
            Core.<$> (x Core..?> "PipelineArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeletePipeline

instance Core.NFData DeletePipeline

instance Core.ToHeaders DeletePipeline where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DeletePipeline" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeletePipeline where
  toJSON DeletePipeline' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PipelineName" Core..= pipelineName),
            Core.Just
              ("ClientRequestToken" Core..= clientRequestToken)
          ]
      )

instance Core.ToPath DeletePipeline where
  toPath = Core.const "/"

instance Core.ToQuery DeletePipeline where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeletePipelineResponse' smart constructor.
data DeletePipelineResponse = DeletePipelineResponse'
  { -- | The Amazon Resource Name (ARN) of the pipeline to delete.
    pipelineArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineArn', 'deletePipelineResponse_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline to delete.
--
-- 'httpStatus', 'deletePipelineResponse_httpStatus' - The response's http status code.
newDeletePipelineResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeletePipelineResponse
newDeletePipelineResponse pHttpStatus_ =
  DeletePipelineResponse'
    { pipelineArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the pipeline to delete.
deletePipelineResponse_pipelineArn :: Lens.Lens' DeletePipelineResponse (Core.Maybe Core.Text)
deletePipelineResponse_pipelineArn = Lens.lens (\DeletePipelineResponse' {pipelineArn} -> pipelineArn) (\s@DeletePipelineResponse' {} a -> s {pipelineArn = a} :: DeletePipelineResponse)

-- | The response's http status code.
deletePipelineResponse_httpStatus :: Lens.Lens' DeletePipelineResponse Core.Int
deletePipelineResponse_httpStatus = Lens.lens (\DeletePipelineResponse' {httpStatus} -> httpStatus) (\s@DeletePipelineResponse' {} a -> s {httpStatus = a} :: DeletePipelineResponse)

instance Core.NFData DeletePipelineResponse
