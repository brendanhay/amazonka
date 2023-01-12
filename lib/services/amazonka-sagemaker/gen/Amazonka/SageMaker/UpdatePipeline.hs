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
-- Module      : Amazonka.SageMaker.UpdatePipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a pipeline.
module Amazonka.SageMaker.UpdatePipeline
  ( -- * Creating a Request
    UpdatePipeline (..),
    newUpdatePipeline,

    -- * Request Lenses
    updatePipeline_parallelismConfiguration,
    updatePipeline_pipelineDefinition,
    updatePipeline_pipelineDefinitionS3Location,
    updatePipeline_pipelineDescription,
    updatePipeline_pipelineDisplayName,
    updatePipeline_roleArn,
    updatePipeline_pipelineName,

    -- * Destructuring the Response
    UpdatePipelineResponse (..),
    newUpdatePipelineResponse,

    -- * Response Lenses
    updatePipelineResponse_pipelineArn,
    updatePipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdatePipeline' smart constructor.
data UpdatePipeline = UpdatePipeline'
  { -- | If specified, it applies to all executions of this pipeline by default.
    parallelismConfiguration :: Prelude.Maybe ParallelismConfiguration,
    -- | The JSON pipeline definition.
    pipelineDefinition :: Prelude.Maybe Prelude.Text,
    -- | The location of the pipeline definition stored in Amazon S3. If
    -- specified, SageMaker will retrieve the pipeline definition from this
    -- location.
    pipelineDefinitionS3Location :: Prelude.Maybe PipelineDefinitionS3Location,
    -- | The description of the pipeline.
    pipelineDescription :: Prelude.Maybe Prelude.Text,
    -- | The display name of the pipeline.
    pipelineDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that the pipeline uses to execute.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the pipeline to update.
    pipelineName :: Prelude.Text
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
-- 'parallelismConfiguration', 'updatePipeline_parallelismConfiguration' - If specified, it applies to all executions of this pipeline by default.
--
-- 'pipelineDefinition', 'updatePipeline_pipelineDefinition' - The JSON pipeline definition.
--
-- 'pipelineDefinitionS3Location', 'updatePipeline_pipelineDefinitionS3Location' - The location of the pipeline definition stored in Amazon S3. If
-- specified, SageMaker will retrieve the pipeline definition from this
-- location.
--
-- 'pipelineDescription', 'updatePipeline_pipelineDescription' - The description of the pipeline.
--
-- 'pipelineDisplayName', 'updatePipeline_pipelineDisplayName' - The display name of the pipeline.
--
-- 'roleArn', 'updatePipeline_roleArn' - The Amazon Resource Name (ARN) that the pipeline uses to execute.
--
-- 'pipelineName', 'updatePipeline_pipelineName' - The name of the pipeline to update.
newUpdatePipeline ::
  -- | 'pipelineName'
  Prelude.Text ->
  UpdatePipeline
newUpdatePipeline pPipelineName_ =
  UpdatePipeline'
    { parallelismConfiguration =
        Prelude.Nothing,
      pipelineDefinition = Prelude.Nothing,
      pipelineDefinitionS3Location = Prelude.Nothing,
      pipelineDescription = Prelude.Nothing,
      pipelineDisplayName = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      pipelineName = pPipelineName_
    }

-- | If specified, it applies to all executions of this pipeline by default.
updatePipeline_parallelismConfiguration :: Lens.Lens' UpdatePipeline (Prelude.Maybe ParallelismConfiguration)
updatePipeline_parallelismConfiguration = Lens.lens (\UpdatePipeline' {parallelismConfiguration} -> parallelismConfiguration) (\s@UpdatePipeline' {} a -> s {parallelismConfiguration = a} :: UpdatePipeline)

-- | The JSON pipeline definition.
updatePipeline_pipelineDefinition :: Lens.Lens' UpdatePipeline (Prelude.Maybe Prelude.Text)
updatePipeline_pipelineDefinition = Lens.lens (\UpdatePipeline' {pipelineDefinition} -> pipelineDefinition) (\s@UpdatePipeline' {} a -> s {pipelineDefinition = a} :: UpdatePipeline)

-- | The location of the pipeline definition stored in Amazon S3. If
-- specified, SageMaker will retrieve the pipeline definition from this
-- location.
updatePipeline_pipelineDefinitionS3Location :: Lens.Lens' UpdatePipeline (Prelude.Maybe PipelineDefinitionS3Location)
updatePipeline_pipelineDefinitionS3Location = Lens.lens (\UpdatePipeline' {pipelineDefinitionS3Location} -> pipelineDefinitionS3Location) (\s@UpdatePipeline' {} a -> s {pipelineDefinitionS3Location = a} :: UpdatePipeline)

-- | The description of the pipeline.
updatePipeline_pipelineDescription :: Lens.Lens' UpdatePipeline (Prelude.Maybe Prelude.Text)
updatePipeline_pipelineDescription = Lens.lens (\UpdatePipeline' {pipelineDescription} -> pipelineDescription) (\s@UpdatePipeline' {} a -> s {pipelineDescription = a} :: UpdatePipeline)

-- | The display name of the pipeline.
updatePipeline_pipelineDisplayName :: Lens.Lens' UpdatePipeline (Prelude.Maybe Prelude.Text)
updatePipeline_pipelineDisplayName = Lens.lens (\UpdatePipeline' {pipelineDisplayName} -> pipelineDisplayName) (\s@UpdatePipeline' {} a -> s {pipelineDisplayName = a} :: UpdatePipeline)

-- | The Amazon Resource Name (ARN) that the pipeline uses to execute.
updatePipeline_roleArn :: Lens.Lens' UpdatePipeline (Prelude.Maybe Prelude.Text)
updatePipeline_roleArn = Lens.lens (\UpdatePipeline' {roleArn} -> roleArn) (\s@UpdatePipeline' {} a -> s {roleArn = a} :: UpdatePipeline)

-- | The name of the pipeline to update.
updatePipeline_pipelineName :: Lens.Lens' UpdatePipeline Prelude.Text
updatePipeline_pipelineName = Lens.lens (\UpdatePipeline' {pipelineName} -> pipelineName) (\s@UpdatePipeline' {} a -> s {pipelineName = a} :: UpdatePipeline)

instance Core.AWSRequest UpdatePipeline where
  type
    AWSResponse UpdatePipeline =
      UpdatePipelineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePipelineResponse'
            Prelude.<$> (x Data..?> "PipelineArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePipeline where
  hashWithSalt _salt UpdatePipeline' {..} =
    _salt
      `Prelude.hashWithSalt` parallelismConfiguration
      `Prelude.hashWithSalt` pipelineDefinition
      `Prelude.hashWithSalt` pipelineDefinitionS3Location
      `Prelude.hashWithSalt` pipelineDescription
      `Prelude.hashWithSalt` pipelineDisplayName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` pipelineName

instance Prelude.NFData UpdatePipeline where
  rnf UpdatePipeline' {..} =
    Prelude.rnf parallelismConfiguration
      `Prelude.seq` Prelude.rnf pipelineDefinition
      `Prelude.seq` Prelude.rnf pipelineDefinitionS3Location
      `Prelude.seq` Prelude.rnf pipelineDescription
      `Prelude.seq` Prelude.rnf pipelineDisplayName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf pipelineName

instance Data.ToHeaders UpdatePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.UpdatePipeline" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePipeline where
  toJSON UpdatePipeline' {..} =
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
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            Prelude.Just ("PipelineName" Data..= pipelineName)
          ]
      )

instance Data.ToPath UpdatePipeline where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePipelineResponse' smart constructor.
data UpdatePipelineResponse = UpdatePipelineResponse'
  { -- | The Amazon Resource Name (ARN) of the updated pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
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
-- 'pipelineArn', 'updatePipelineResponse_pipelineArn' - The Amazon Resource Name (ARN) of the updated pipeline.
--
-- 'httpStatus', 'updatePipelineResponse_httpStatus' - The response's http status code.
newUpdatePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePipelineResponse
newUpdatePipelineResponse pHttpStatus_ =
  UpdatePipelineResponse'
    { pipelineArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the updated pipeline.
updatePipelineResponse_pipelineArn :: Lens.Lens' UpdatePipelineResponse (Prelude.Maybe Prelude.Text)
updatePipelineResponse_pipelineArn = Lens.lens (\UpdatePipelineResponse' {pipelineArn} -> pipelineArn) (\s@UpdatePipelineResponse' {} a -> s {pipelineArn = a} :: UpdatePipelineResponse)

-- | The response's http status code.
updatePipelineResponse_httpStatus :: Lens.Lens' UpdatePipelineResponse Prelude.Int
updatePipelineResponse_httpStatus = Lens.lens (\UpdatePipelineResponse' {httpStatus} -> httpStatus) (\s@UpdatePipelineResponse' {} a -> s {httpStatus = a} :: UpdatePipelineResponse)

instance Prelude.NFData UpdatePipelineResponse where
  rnf UpdatePipelineResponse' {..} =
    Prelude.rnf pipelineArn
      `Prelude.seq` Prelude.rnf httpStatus
