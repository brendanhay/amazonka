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
-- Module      : Amazonka.CodePipeline.CreatePipeline
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a pipeline.
--
-- In the pipeline structure, you must include either @artifactStore@ or
-- @artifactStores@ in your pipeline, but you cannot use both. If you
-- create a cross-region action in your pipeline, you must use
-- @artifactStores@.
module Amazonka.CodePipeline.CreatePipeline
  ( -- * Creating a Request
    CreatePipeline (..),
    newCreatePipeline,

    -- * Request Lenses
    createPipeline_tags,
    createPipeline_pipeline,

    -- * Destructuring the Response
    CreatePipelineResponse (..),
    newCreatePipelineResponse,

    -- * Response Lenses
    createPipelineResponse_tags,
    createPipelineResponse_pipeline,
    createPipelineResponse_httpStatus,
  )
where

import Amazonka.CodePipeline.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @CreatePipeline@ action.
--
-- /See:/ 'newCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { -- | The tags for the pipeline.
    tags :: Prelude.Maybe [Tag],
    -- | Represents the structure of actions and stages to be performed in the
    -- pipeline.
    pipeline :: PipelineDeclaration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createPipeline_tags' - The tags for the pipeline.
--
-- 'pipeline', 'createPipeline_pipeline' - Represents the structure of actions and stages to be performed in the
-- pipeline.
newCreatePipeline ::
  -- | 'pipeline'
  PipelineDeclaration ->
  CreatePipeline
newCreatePipeline pPipeline_ =
  CreatePipeline'
    { tags = Prelude.Nothing,
      pipeline = pPipeline_
    }

-- | The tags for the pipeline.
createPipeline_tags :: Lens.Lens' CreatePipeline (Prelude.Maybe [Tag])
createPipeline_tags = Lens.lens (\CreatePipeline' {tags} -> tags) (\s@CreatePipeline' {} a -> s {tags = a} :: CreatePipeline) Prelude.. Lens.mapping Lens.coerced

-- | Represents the structure of actions and stages to be performed in the
-- pipeline.
createPipeline_pipeline :: Lens.Lens' CreatePipeline PipelineDeclaration
createPipeline_pipeline = Lens.lens (\CreatePipeline' {pipeline} -> pipeline) (\s@CreatePipeline' {} a -> s {pipeline = a} :: CreatePipeline)

instance Core.AWSRequest CreatePipeline where
  type
    AWSResponse CreatePipeline =
      CreatePipelineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePipelineResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "pipeline")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePipeline where
  hashWithSalt _salt CreatePipeline' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` pipeline

instance Prelude.NFData CreatePipeline where
  rnf CreatePipeline' {..} =
    Prelude.rnf tags `Prelude.seq` Prelude.rnf pipeline

instance Core.ToHeaders CreatePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.CreatePipeline" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreatePipeline where
  toJSON CreatePipeline' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("pipeline" Core..= pipeline)
          ]
      )

instance Core.ToPath CreatePipeline where
  toPath = Prelude.const "/"

instance Core.ToQuery CreatePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @CreatePipeline@ action.
--
-- /See:/ 'newCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { -- | Specifies the tags applied to the pipeline.
    tags :: Prelude.Maybe [Tag],
    -- | Represents the structure of actions and stages to be performed in the
    -- pipeline.
    pipeline :: Prelude.Maybe PipelineDeclaration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createPipelineResponse_tags' - Specifies the tags applied to the pipeline.
--
-- 'pipeline', 'createPipelineResponse_pipeline' - Represents the structure of actions and stages to be performed in the
-- pipeline.
--
-- 'httpStatus', 'createPipelineResponse_httpStatus' - The response's http status code.
newCreatePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePipelineResponse
newCreatePipelineResponse pHttpStatus_ =
  CreatePipelineResponse'
    { tags = Prelude.Nothing,
      pipeline = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies the tags applied to the pipeline.
createPipelineResponse_tags :: Lens.Lens' CreatePipelineResponse (Prelude.Maybe [Tag])
createPipelineResponse_tags = Lens.lens (\CreatePipelineResponse' {tags} -> tags) (\s@CreatePipelineResponse' {} a -> s {tags = a} :: CreatePipelineResponse) Prelude.. Lens.mapping Lens.coerced

-- | Represents the structure of actions and stages to be performed in the
-- pipeline.
createPipelineResponse_pipeline :: Lens.Lens' CreatePipelineResponse (Prelude.Maybe PipelineDeclaration)
createPipelineResponse_pipeline = Lens.lens (\CreatePipelineResponse' {pipeline} -> pipeline) (\s@CreatePipelineResponse' {} a -> s {pipeline = a} :: CreatePipelineResponse)

-- | The response's http status code.
createPipelineResponse_httpStatus :: Lens.Lens' CreatePipelineResponse Prelude.Int
createPipelineResponse_httpStatus = Lens.lens (\CreatePipelineResponse' {httpStatus} -> httpStatus) (\s@CreatePipelineResponse' {} a -> s {httpStatus = a} :: CreatePipelineResponse)

instance Prelude.NFData CreatePipelineResponse where
  rnf CreatePipelineResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf pipeline
      `Prelude.seq` Prelude.rnf httpStatus
