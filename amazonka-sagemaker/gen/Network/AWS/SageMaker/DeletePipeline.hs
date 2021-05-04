{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeletePipeline' smart constructor.
data DeletePipeline = DeletePipeline'
  { -- | The name of the pipeline to delete.
    pipelineName :: Prelude.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the operation. An idempotent operation completes no more
    -- than one time.
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  DeletePipeline
newDeletePipeline pPipelineName_ pClientRequestToken_ =
  DeletePipeline'
    { pipelineName = pPipelineName_,
      clientRequestToken = pClientRequestToken_
    }

-- | The name of the pipeline to delete.
deletePipeline_pipelineName :: Lens.Lens' DeletePipeline Prelude.Text
deletePipeline_pipelineName = Lens.lens (\DeletePipeline' {pipelineName} -> pipelineName) (\s@DeletePipeline' {} a -> s {pipelineName = a} :: DeletePipeline)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time.
deletePipeline_clientRequestToken :: Lens.Lens' DeletePipeline Prelude.Text
deletePipeline_clientRequestToken = Lens.lens (\DeletePipeline' {clientRequestToken} -> clientRequestToken) (\s@DeletePipeline' {} a -> s {clientRequestToken = a} :: DeletePipeline)

instance Prelude.AWSRequest DeletePipeline where
  type Rs DeletePipeline = DeletePipelineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePipelineResponse'
            Prelude.<$> (x Prelude..?> "PipelineArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePipeline

instance Prelude.NFData DeletePipeline

instance Prelude.ToHeaders DeletePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.DeletePipeline" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeletePipeline where
  toJSON DeletePipeline' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PipelineName" Prelude..= pipelineName),
            Prelude.Just
              ( "ClientRequestToken"
                  Prelude..= clientRequestToken
              )
          ]
      )

instance Prelude.ToPath DeletePipeline where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeletePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePipelineResponse' smart constructor.
data DeletePipelineResponse = DeletePipelineResponse'
  { -- | The Amazon Resource Name (ARN) of the pipeline to delete.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeletePipelineResponse
newDeletePipelineResponse pHttpStatus_ =
  DeletePipelineResponse'
    { pipelineArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the pipeline to delete.
deletePipelineResponse_pipelineArn :: Lens.Lens' DeletePipelineResponse (Prelude.Maybe Prelude.Text)
deletePipelineResponse_pipelineArn = Lens.lens (\DeletePipelineResponse' {pipelineArn} -> pipelineArn) (\s@DeletePipelineResponse' {} a -> s {pipelineArn = a} :: DeletePipelineResponse)

-- | The response's http status code.
deletePipelineResponse_httpStatus :: Lens.Lens' DeletePipelineResponse Prelude.Int
deletePipelineResponse_httpStatus = Lens.lens (\DeletePipelineResponse' {httpStatus} -> httpStatus) (\s@DeletePipelineResponse' {} a -> s {httpStatus = a} :: DeletePipelineResponse)

instance Prelude.NFData DeletePipelineResponse
