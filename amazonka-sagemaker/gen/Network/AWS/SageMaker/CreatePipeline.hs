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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { -- | A description of the pipeline.
    pipelineDescription :: Prelude.Maybe Prelude.Text,
    -- | The display name of the pipeline.
    pipelineDisplayName :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to apply to the created pipeline.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the pipeline.
    pipelineName :: Prelude.Text,
    -- | The JSON pipeline definition of the pipeline.
    pipelineDefinition :: Prelude.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the operation. An idempotent operation completes no more
    -- than one time.
    clientRequestToken :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role used by the pipeline to
    -- access and create resources.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'pipelineDefinition'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  CreatePipeline
newCreatePipeline
  pPipelineName_
  pPipelineDefinition_
  pClientRequestToken_
  pRoleArn_ =
    CreatePipeline'
      { pipelineDescription =
          Prelude.Nothing,
        pipelineDisplayName = Prelude.Nothing,
        tags = Prelude.Nothing,
        pipelineName = pPipelineName_,
        pipelineDefinition = pPipelineDefinition_,
        clientRequestToken = pClientRequestToken_,
        roleArn = pRoleArn_
      }

-- | A description of the pipeline.
createPipeline_pipelineDescription :: Lens.Lens' CreatePipeline (Prelude.Maybe Prelude.Text)
createPipeline_pipelineDescription = Lens.lens (\CreatePipeline' {pipelineDescription} -> pipelineDescription) (\s@CreatePipeline' {} a -> s {pipelineDescription = a} :: CreatePipeline)

-- | The display name of the pipeline.
createPipeline_pipelineDisplayName :: Lens.Lens' CreatePipeline (Prelude.Maybe Prelude.Text)
createPipeline_pipelineDisplayName = Lens.lens (\CreatePipeline' {pipelineDisplayName} -> pipelineDisplayName) (\s@CreatePipeline' {} a -> s {pipelineDisplayName = a} :: CreatePipeline)

-- | A list of tags to apply to the created pipeline.
createPipeline_tags :: Lens.Lens' CreatePipeline (Prelude.Maybe [Tag])
createPipeline_tags = Lens.lens (\CreatePipeline' {tags} -> tags) (\s@CreatePipeline' {} a -> s {tags = a} :: CreatePipeline) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the pipeline.
createPipeline_pipelineName :: Lens.Lens' CreatePipeline Prelude.Text
createPipeline_pipelineName = Lens.lens (\CreatePipeline' {pipelineName} -> pipelineName) (\s@CreatePipeline' {} a -> s {pipelineName = a} :: CreatePipeline)

-- | The JSON pipeline definition of the pipeline.
createPipeline_pipelineDefinition :: Lens.Lens' CreatePipeline Prelude.Text
createPipeline_pipelineDefinition = Lens.lens (\CreatePipeline' {pipelineDefinition} -> pipelineDefinition) (\s@CreatePipeline' {} a -> s {pipelineDefinition = a} :: CreatePipeline)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time.
createPipeline_clientRequestToken :: Lens.Lens' CreatePipeline Prelude.Text
createPipeline_clientRequestToken = Lens.lens (\CreatePipeline' {clientRequestToken} -> clientRequestToken) (\s@CreatePipeline' {} a -> s {clientRequestToken = a} :: CreatePipeline)

-- | The Amazon Resource Name (ARN) of the role used by the pipeline to
-- access and create resources.
createPipeline_roleArn :: Lens.Lens' CreatePipeline Prelude.Text
createPipeline_roleArn = Lens.lens (\CreatePipeline' {roleArn} -> roleArn) (\s@CreatePipeline' {} a -> s {roleArn = a} :: CreatePipeline)

instance Prelude.AWSRequest CreatePipeline where
  type Rs CreatePipeline = CreatePipelineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePipelineResponse'
            Prelude.<$> (x Prelude..?> "PipelineArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePipeline

instance Prelude.NFData CreatePipeline

instance Prelude.ToHeaders CreatePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.CreatePipeline" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreatePipeline where
  toJSON CreatePipeline' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PipelineDescription" Prelude..=)
              Prelude.<$> pipelineDescription,
            ("PipelineDisplayName" Prelude..=)
              Prelude.<$> pipelineDisplayName,
            ("Tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just
              ("PipelineName" Prelude..= pipelineName),
            Prelude.Just
              ("PipelineDefinition" Prelude..= pipelineDefinition),
            Prelude.Just
              ("ClientRequestToken" Prelude..= clientRequestToken),
            Prelude.Just ("RoleArn" Prelude..= roleArn)
          ]
      )

instance Prelude.ToPath CreatePipeline where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreatePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { -- | The Amazon Resource Name (ARN) of the created pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreatePipelineResponse
newCreatePipelineResponse pHttpStatus_ =
  CreatePipelineResponse'
    { pipelineArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the created pipeline.
createPipelineResponse_pipelineArn :: Lens.Lens' CreatePipelineResponse (Prelude.Maybe Prelude.Text)
createPipelineResponse_pipelineArn = Lens.lens (\CreatePipelineResponse' {pipelineArn} -> pipelineArn) (\s@CreatePipelineResponse' {} a -> s {pipelineArn = a} :: CreatePipelineResponse)

-- | The response's http status code.
createPipelineResponse_httpStatus :: Lens.Lens' CreatePipelineResponse Prelude.Int
createPipelineResponse_httpStatus = Lens.lens (\CreatePipelineResponse' {httpStatus} -> httpStatus) (\s@CreatePipelineResponse' {} a -> s {httpStatus = a} :: CreatePipelineResponse)

instance Prelude.NFData CreatePipelineResponse
