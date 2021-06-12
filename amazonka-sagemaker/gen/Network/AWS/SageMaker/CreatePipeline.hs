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
-- Module      : Network.AWS.SageMaker.CreatePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a pipeline using a JSON pipeline definition.
module Network.AWS.SageMaker.CreatePipeline
  ( -- * Creating a Request
    CreatePipeline (..),
    newCreatePipeline,

    -- * Request Lenses
    createPipeline_pipelineDescription,
    createPipeline_pipelineDisplayName,
    createPipeline_tags,
    createPipeline_pipelineName,
    createPipeline_pipelineDefinition,
    createPipeline_clientRequestToken,
    createPipeline_roleArn,

    -- * Destructuring the Response
    CreatePipelineResponse (..),
    newCreatePipelineResponse,

    -- * Response Lenses
    createPipelineResponse_pipelineArn,
    createPipelineResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { -- | A description of the pipeline.
    pipelineDescription :: Core.Maybe Core.Text,
    -- | The display name of the pipeline.
    pipelineDisplayName :: Core.Maybe Core.Text,
    -- | A list of tags to apply to the created pipeline.
    tags :: Core.Maybe [Tag],
    -- | The name of the pipeline.
    pipelineName :: Core.Text,
    -- | The JSON pipeline definition of the pipeline.
    pipelineDefinition :: Core.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the operation. An idempotent operation completes no more
    -- than one time.
    clientRequestToken :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the role used by the pipeline to
    -- access and create resources.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineDescription', 'createPipeline_pipelineDescription' - A description of the pipeline.
--
-- 'pipelineDisplayName', 'createPipeline_pipelineDisplayName' - The display name of the pipeline.
--
-- 'tags', 'createPipeline_tags' - A list of tags to apply to the created pipeline.
--
-- 'pipelineName', 'createPipeline_pipelineName' - The name of the pipeline.
--
-- 'pipelineDefinition', 'createPipeline_pipelineDefinition' - The JSON pipeline definition of the pipeline.
--
-- 'clientRequestToken', 'createPipeline_clientRequestToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time.
--
-- 'roleArn', 'createPipeline_roleArn' - The Amazon Resource Name (ARN) of the role used by the pipeline to
-- access and create resources.
newCreatePipeline ::
  -- | 'pipelineName'
  Core.Text ->
  -- | 'pipelineDefinition'
  Core.Text ->
  -- | 'clientRequestToken'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  CreatePipeline
newCreatePipeline
  pPipelineName_
  pPipelineDefinition_
  pClientRequestToken_
  pRoleArn_ =
    CreatePipeline'
      { pipelineDescription = Core.Nothing,
        pipelineDisplayName = Core.Nothing,
        tags = Core.Nothing,
        pipelineName = pPipelineName_,
        pipelineDefinition = pPipelineDefinition_,
        clientRequestToken = pClientRequestToken_,
        roleArn = pRoleArn_
      }

-- | A description of the pipeline.
createPipeline_pipelineDescription :: Lens.Lens' CreatePipeline (Core.Maybe Core.Text)
createPipeline_pipelineDescription = Lens.lens (\CreatePipeline' {pipelineDescription} -> pipelineDescription) (\s@CreatePipeline' {} a -> s {pipelineDescription = a} :: CreatePipeline)

-- | The display name of the pipeline.
createPipeline_pipelineDisplayName :: Lens.Lens' CreatePipeline (Core.Maybe Core.Text)
createPipeline_pipelineDisplayName = Lens.lens (\CreatePipeline' {pipelineDisplayName} -> pipelineDisplayName) (\s@CreatePipeline' {} a -> s {pipelineDisplayName = a} :: CreatePipeline)

-- | A list of tags to apply to the created pipeline.
createPipeline_tags :: Lens.Lens' CreatePipeline (Core.Maybe [Tag])
createPipeline_tags = Lens.lens (\CreatePipeline' {tags} -> tags) (\s@CreatePipeline' {} a -> s {tags = a} :: CreatePipeline) Core.. Lens.mapping Lens._Coerce

-- | The name of the pipeline.
createPipeline_pipelineName :: Lens.Lens' CreatePipeline Core.Text
createPipeline_pipelineName = Lens.lens (\CreatePipeline' {pipelineName} -> pipelineName) (\s@CreatePipeline' {} a -> s {pipelineName = a} :: CreatePipeline)

-- | The JSON pipeline definition of the pipeline.
createPipeline_pipelineDefinition :: Lens.Lens' CreatePipeline Core.Text
createPipeline_pipelineDefinition = Lens.lens (\CreatePipeline' {pipelineDefinition} -> pipelineDefinition) (\s@CreatePipeline' {} a -> s {pipelineDefinition = a} :: CreatePipeline)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time.
createPipeline_clientRequestToken :: Lens.Lens' CreatePipeline Core.Text
createPipeline_clientRequestToken = Lens.lens (\CreatePipeline' {clientRequestToken} -> clientRequestToken) (\s@CreatePipeline' {} a -> s {clientRequestToken = a} :: CreatePipeline)

-- | The Amazon Resource Name (ARN) of the role used by the pipeline to
-- access and create resources.
createPipeline_roleArn :: Lens.Lens' CreatePipeline Core.Text
createPipeline_roleArn = Lens.lens (\CreatePipeline' {roleArn} -> roleArn) (\s@CreatePipeline' {} a -> s {roleArn = a} :: CreatePipeline)

instance Core.AWSRequest CreatePipeline where
  type
    AWSResponse CreatePipeline =
      CreatePipelineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePipelineResponse'
            Core.<$> (x Core..?> "PipelineArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreatePipeline

instance Core.NFData CreatePipeline

instance Core.ToHeaders CreatePipeline where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreatePipeline" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreatePipeline where
  toJSON CreatePipeline' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PipelineDescription" Core..=)
              Core.<$> pipelineDescription,
            ("PipelineDisplayName" Core..=)
              Core.<$> pipelineDisplayName,
            ("Tags" Core..=) Core.<$> tags,
            Core.Just ("PipelineName" Core..= pipelineName),
            Core.Just
              ("PipelineDefinition" Core..= pipelineDefinition),
            Core.Just
              ("ClientRequestToken" Core..= clientRequestToken),
            Core.Just ("RoleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreatePipeline where
  toPath = Core.const "/"

instance Core.ToQuery CreatePipeline where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { -- | The Amazon Resource Name (ARN) of the created pipeline.
    pipelineArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineArn', 'createPipelineResponse_pipelineArn' - The Amazon Resource Name (ARN) of the created pipeline.
--
-- 'httpStatus', 'createPipelineResponse_httpStatus' - The response's http status code.
newCreatePipelineResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreatePipelineResponse
newCreatePipelineResponse pHttpStatus_ =
  CreatePipelineResponse'
    { pipelineArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the created pipeline.
createPipelineResponse_pipelineArn :: Lens.Lens' CreatePipelineResponse (Core.Maybe Core.Text)
createPipelineResponse_pipelineArn = Lens.lens (\CreatePipelineResponse' {pipelineArn} -> pipelineArn) (\s@CreatePipelineResponse' {} a -> s {pipelineArn = a} :: CreatePipelineResponse)

-- | The response's http status code.
createPipelineResponse_httpStatus :: Lens.Lens' CreatePipelineResponse Core.Int
createPipelineResponse_httpStatus = Lens.lens (\CreatePipelineResponse' {httpStatus} -> httpStatus) (\s@CreatePipelineResponse' {} a -> s {httpStatus = a} :: CreatePipelineResponse)

instance Core.NFData CreatePipelineResponse
