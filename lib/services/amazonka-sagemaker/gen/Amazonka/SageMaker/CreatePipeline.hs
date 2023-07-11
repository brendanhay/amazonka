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
-- Module      : Amazonka.SageMaker.CreatePipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a pipeline using a JSON pipeline definition.
module Amazonka.SageMaker.CreatePipeline
  ( -- * Creating a Request
    CreatePipeline (..),
    newCreatePipeline,

    -- * Request Lenses
    createPipeline_parallelismConfiguration,
    createPipeline_pipelineDefinition,
    createPipeline_pipelineDefinitionS3Location,
    createPipeline_pipelineDescription,
    createPipeline_pipelineDisplayName,
    createPipeline_tags,
    createPipeline_pipelineName,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { -- | This is the configuration that controls the parallelism of the pipeline.
    -- If specified, it applies to all runs of this pipeline by default.
    parallelismConfiguration :: Prelude.Maybe ParallelismConfiguration,
    -- | The JSON pipeline definition of the pipeline.
    pipelineDefinition :: Prelude.Maybe Prelude.Text,
    -- | The location of the pipeline definition stored in Amazon S3. If
    -- specified, SageMaker will retrieve the pipeline definition from this
    -- location.
    pipelineDefinitionS3Location :: Prelude.Maybe PipelineDefinitionS3Location,
    -- | A description of the pipeline.
    pipelineDescription :: Prelude.Maybe Prelude.Text,
    -- | The display name of the pipeline.
    pipelineDisplayName :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to apply to the created pipeline.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the pipeline.
    pipelineName :: Prelude.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the operation. An idempotent operation completes no more
    -- than one time.
    clientRequestToken :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role used by the pipeline to
    -- access and create resources.
    roleArn :: Prelude.Text
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
-- 'parallelismConfiguration', 'createPipeline_parallelismConfiguration' - This is the configuration that controls the parallelism of the pipeline.
-- If specified, it applies to all runs of this pipeline by default.
--
-- 'pipelineDefinition', 'createPipeline_pipelineDefinition' - The JSON pipeline definition of the pipeline.
--
-- 'pipelineDefinitionS3Location', 'createPipeline_pipelineDefinitionS3Location' - The location of the pipeline definition stored in Amazon S3. If
-- specified, SageMaker will retrieve the pipeline definition from this
-- location.
--
-- 'pipelineDescription', 'createPipeline_pipelineDescription' - A description of the pipeline.
--
-- 'pipelineDisplayName', 'createPipeline_pipelineDisplayName' - The display name of the pipeline.
--
-- 'tags', 'createPipeline_tags' - A list of tags to apply to the created pipeline.
--
-- 'pipelineName', 'createPipeline_pipelineName' - The name of the pipeline.
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
  -- | 'clientRequestToken'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  CreatePipeline
newCreatePipeline
  pPipelineName_
  pClientRequestToken_
  pRoleArn_ =
    CreatePipeline'
      { parallelismConfiguration =
          Prelude.Nothing,
        pipelineDefinition = Prelude.Nothing,
        pipelineDefinitionS3Location = Prelude.Nothing,
        pipelineDescription = Prelude.Nothing,
        pipelineDisplayName = Prelude.Nothing,
        tags = Prelude.Nothing,
        pipelineName = pPipelineName_,
        clientRequestToken = pClientRequestToken_,
        roleArn = pRoleArn_
      }

-- | This is the configuration that controls the parallelism of the pipeline.
-- If specified, it applies to all runs of this pipeline by default.
createPipeline_parallelismConfiguration :: Lens.Lens' CreatePipeline (Prelude.Maybe ParallelismConfiguration)
createPipeline_parallelismConfiguration = Lens.lens (\CreatePipeline' {parallelismConfiguration} -> parallelismConfiguration) (\s@CreatePipeline' {} a -> s {parallelismConfiguration = a} :: CreatePipeline)

-- | The JSON pipeline definition of the pipeline.
createPipeline_pipelineDefinition :: Lens.Lens' CreatePipeline (Prelude.Maybe Prelude.Text)
createPipeline_pipelineDefinition = Lens.lens (\CreatePipeline' {pipelineDefinition} -> pipelineDefinition) (\s@CreatePipeline' {} a -> s {pipelineDefinition = a} :: CreatePipeline)

-- | The location of the pipeline definition stored in Amazon S3. If
-- specified, SageMaker will retrieve the pipeline definition from this
-- location.
createPipeline_pipelineDefinitionS3Location :: Lens.Lens' CreatePipeline (Prelude.Maybe PipelineDefinitionS3Location)
createPipeline_pipelineDefinitionS3Location = Lens.lens (\CreatePipeline' {pipelineDefinitionS3Location} -> pipelineDefinitionS3Location) (\s@CreatePipeline' {} a -> s {pipelineDefinitionS3Location = a} :: CreatePipeline)

-- | A description of the pipeline.
createPipeline_pipelineDescription :: Lens.Lens' CreatePipeline (Prelude.Maybe Prelude.Text)
createPipeline_pipelineDescription = Lens.lens (\CreatePipeline' {pipelineDescription} -> pipelineDescription) (\s@CreatePipeline' {} a -> s {pipelineDescription = a} :: CreatePipeline)

-- | The display name of the pipeline.
createPipeline_pipelineDisplayName :: Lens.Lens' CreatePipeline (Prelude.Maybe Prelude.Text)
createPipeline_pipelineDisplayName = Lens.lens (\CreatePipeline' {pipelineDisplayName} -> pipelineDisplayName) (\s@CreatePipeline' {} a -> s {pipelineDisplayName = a} :: CreatePipeline)

-- | A list of tags to apply to the created pipeline.
createPipeline_tags :: Lens.Lens' CreatePipeline (Prelude.Maybe [Tag])
createPipeline_tags = Lens.lens (\CreatePipeline' {tags} -> tags) (\s@CreatePipeline' {} a -> s {tags = a} :: CreatePipeline) Prelude.. Lens.mapping Lens.coerced

-- | The name of the pipeline.
createPipeline_pipelineName :: Lens.Lens' CreatePipeline Prelude.Text
createPipeline_pipelineName = Lens.lens (\CreatePipeline' {pipelineName} -> pipelineName) (\s@CreatePipeline' {} a -> s {pipelineName = a} :: CreatePipeline)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time.
createPipeline_clientRequestToken :: Lens.Lens' CreatePipeline Prelude.Text
createPipeline_clientRequestToken = Lens.lens (\CreatePipeline' {clientRequestToken} -> clientRequestToken) (\s@CreatePipeline' {} a -> s {clientRequestToken = a} :: CreatePipeline)

-- | The Amazon Resource Name (ARN) of the role used by the pipeline to
-- access and create resources.
createPipeline_roleArn :: Lens.Lens' CreatePipeline Prelude.Text
createPipeline_roleArn = Lens.lens (\CreatePipeline' {roleArn} -> roleArn) (\s@CreatePipeline' {} a -> s {roleArn = a} :: CreatePipeline)

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
            Prelude.<$> (x Data..?> "PipelineArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePipeline where
  hashWithSalt _salt CreatePipeline' {..} =
    _salt
      `Prelude.hashWithSalt` parallelismConfiguration
      `Prelude.hashWithSalt` pipelineDefinition
      `Prelude.hashWithSalt` pipelineDefinitionS3Location
      `Prelude.hashWithSalt` pipelineDescription
      `Prelude.hashWithSalt` pipelineDisplayName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` pipelineName
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreatePipeline where
  rnf CreatePipeline' {..} =
    Prelude.rnf parallelismConfiguration
      `Prelude.seq` Prelude.rnf pipelineDefinition
      `Prelude.seq` Prelude.rnf pipelineDefinitionS3Location
      `Prelude.seq` Prelude.rnf pipelineDescription
      `Prelude.seq` Prelude.rnf pipelineDisplayName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf pipelineName
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders CreatePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.CreatePipeline" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePipeline where
  toJSON CreatePipeline' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ParallelismConfiguration" Data..=)
              Prelude.<$> parallelismConfiguration,
            ("PipelineDefinition" Data..=)
              Prelude.<$> pipelineDefinition,
            ("PipelineDefinitionS3Location" Data..=)
              Prelude.<$> pipelineDefinitionS3Location,
            ("PipelineDescription" Data..=)
              Prelude.<$> pipelineDescription,
            ("PipelineDisplayName" Data..=)
              Prelude.<$> pipelineDisplayName,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("PipelineName" Data..= pipelineName),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreatePipeline where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { -- | The Amazon Resource Name (ARN) of the created pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
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

instance Prelude.NFData CreatePipelineResponse where
  rnf CreatePipelineResponse' {..} =
    Prelude.rnf pipelineArn
      `Prelude.seq` Prelude.rnf httpStatus
