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
-- Module      : Network.AWS.CodePipeline.UpdatePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified pipeline with edits or changes to its structure. Use
-- a JSON file with the pipeline structure and @UpdatePipeline@ to provide
-- the full structure of the pipeline. Updating the pipeline increases the
-- version number of the pipeline by 1.
module Network.AWS.CodePipeline.UpdatePipeline
  ( -- * Creating a Request
    UpdatePipeline (..),
    newUpdatePipeline,

    -- * Request Lenses
    updatePipeline_pipeline,

    -- * Destructuring the Response
    UpdatePipelineResponse (..),
    newUpdatePipelineResponse,

    -- * Response Lenses
    updatePipelineResponse_pipeline,
    updatePipelineResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an @UpdatePipeline@ action.
--
-- /See:/ 'newUpdatePipeline' smart constructor.
data UpdatePipeline = UpdatePipeline'
  { -- | The name of the pipeline to be updated.
    pipeline :: PipelineDeclaration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipeline', 'updatePipeline_pipeline' - The name of the pipeline to be updated.
newUpdatePipeline ::
  -- | 'pipeline'
  PipelineDeclaration ->
  UpdatePipeline
newUpdatePipeline pPipeline_ =
  UpdatePipeline' {pipeline = pPipeline_}

-- | The name of the pipeline to be updated.
updatePipeline_pipeline :: Lens.Lens' UpdatePipeline PipelineDeclaration
updatePipeline_pipeline = Lens.lens (\UpdatePipeline' {pipeline} -> pipeline) (\s@UpdatePipeline' {} a -> s {pipeline = a} :: UpdatePipeline)

instance Core.AWSRequest UpdatePipeline where
  type
    AWSResponse UpdatePipeline =
      UpdatePipelineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePipelineResponse'
            Prelude.<$> (x Core..?> "pipeline")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePipeline

instance Prelude.NFData UpdatePipeline

instance Core.ToHeaders UpdatePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.UpdatePipeline" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdatePipeline where
  toJSON UpdatePipeline' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("pipeline" Core..= pipeline)]
      )

instance Core.ToPath UpdatePipeline where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdatePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of an @UpdatePipeline@ action.
--
-- /See:/ 'newUpdatePipelineResponse' smart constructor.
data UpdatePipelineResponse = UpdatePipelineResponse'
  { -- | The structure of the updated pipeline.
    pipeline :: Prelude.Maybe PipelineDeclaration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipeline', 'updatePipelineResponse_pipeline' - The structure of the updated pipeline.
--
-- 'httpStatus', 'updatePipelineResponse_httpStatus' - The response's http status code.
newUpdatePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePipelineResponse
newUpdatePipelineResponse pHttpStatus_ =
  UpdatePipelineResponse'
    { pipeline = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The structure of the updated pipeline.
updatePipelineResponse_pipeline :: Lens.Lens' UpdatePipelineResponse (Prelude.Maybe PipelineDeclaration)
updatePipelineResponse_pipeline = Lens.lens (\UpdatePipelineResponse' {pipeline} -> pipeline) (\s@UpdatePipelineResponse' {} a -> s {pipeline = a} :: UpdatePipelineResponse)

-- | The response's http status code.
updatePipelineResponse_httpStatus :: Lens.Lens' UpdatePipelineResponse Prelude.Int
updatePipelineResponse_httpStatus = Lens.lens (\UpdatePipelineResponse' {httpStatus} -> httpStatus) (\s@UpdatePipelineResponse' {} a -> s {httpStatus = a} :: UpdatePipelineResponse)

instance Prelude.NFData UpdatePipelineResponse
