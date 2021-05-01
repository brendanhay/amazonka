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
-- Module      : Network.AWS.SageMaker.UpdatePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a pipeline.
module Network.AWS.SageMaker.UpdatePipeline
  ( -- * Creating a Request
    UpdatePipeline (..),
    newUpdatePipeline,

    -- * Request Lenses
    updatePipeline_pipelineDescription,
    updatePipeline_roleArn,
    updatePipeline_pipelineDefinition,
    updatePipeline_pipelineDisplayName,
    updatePipeline_pipelineName,

    -- * Destructuring the Response
    UpdatePipelineResponse (..),
    newUpdatePipelineResponse,

    -- * Response Lenses
    updatePipelineResponse_pipelineArn,
    updatePipelineResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdatePipeline' smart constructor.
data UpdatePipeline = UpdatePipeline'
  { -- | The description of the pipeline.
    pipelineDescription :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that the pipeline uses to execute.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The JSON pipeline definition.
    pipelineDefinition :: Prelude.Maybe Prelude.Text,
    -- | The display name of the pipeline.
    pipelineDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the pipeline to update.
    pipelineName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineDescription', 'updatePipeline_pipelineDescription' - The description of the pipeline.
--
-- 'roleArn', 'updatePipeline_roleArn' - The Amazon Resource Name (ARN) that the pipeline uses to execute.
--
-- 'pipelineDefinition', 'updatePipeline_pipelineDefinition' - The JSON pipeline definition.
--
-- 'pipelineDisplayName', 'updatePipeline_pipelineDisplayName' - The display name of the pipeline.
--
-- 'pipelineName', 'updatePipeline_pipelineName' - The name of the pipeline to update.
newUpdatePipeline ::
  -- | 'pipelineName'
  Prelude.Text ->
  UpdatePipeline
newUpdatePipeline pPipelineName_ =
  UpdatePipeline'
    { pipelineDescription =
        Prelude.Nothing,
      roleArn = Prelude.Nothing,
      pipelineDefinition = Prelude.Nothing,
      pipelineDisplayName = Prelude.Nothing,
      pipelineName = pPipelineName_
    }

-- | The description of the pipeline.
updatePipeline_pipelineDescription :: Lens.Lens' UpdatePipeline (Prelude.Maybe Prelude.Text)
updatePipeline_pipelineDescription = Lens.lens (\UpdatePipeline' {pipelineDescription} -> pipelineDescription) (\s@UpdatePipeline' {} a -> s {pipelineDescription = a} :: UpdatePipeline)

-- | The Amazon Resource Name (ARN) that the pipeline uses to execute.
updatePipeline_roleArn :: Lens.Lens' UpdatePipeline (Prelude.Maybe Prelude.Text)
updatePipeline_roleArn = Lens.lens (\UpdatePipeline' {roleArn} -> roleArn) (\s@UpdatePipeline' {} a -> s {roleArn = a} :: UpdatePipeline)

-- | The JSON pipeline definition.
updatePipeline_pipelineDefinition :: Lens.Lens' UpdatePipeline (Prelude.Maybe Prelude.Text)
updatePipeline_pipelineDefinition = Lens.lens (\UpdatePipeline' {pipelineDefinition} -> pipelineDefinition) (\s@UpdatePipeline' {} a -> s {pipelineDefinition = a} :: UpdatePipeline)

-- | The display name of the pipeline.
updatePipeline_pipelineDisplayName :: Lens.Lens' UpdatePipeline (Prelude.Maybe Prelude.Text)
updatePipeline_pipelineDisplayName = Lens.lens (\UpdatePipeline' {pipelineDisplayName} -> pipelineDisplayName) (\s@UpdatePipeline' {} a -> s {pipelineDisplayName = a} :: UpdatePipeline)

-- | The name of the pipeline to update.
updatePipeline_pipelineName :: Lens.Lens' UpdatePipeline Prelude.Text
updatePipeline_pipelineName = Lens.lens (\UpdatePipeline' {pipelineName} -> pipelineName) (\s@UpdatePipeline' {} a -> s {pipelineName = a} :: UpdatePipeline)

instance Prelude.AWSRequest UpdatePipeline where
  type Rs UpdatePipeline = UpdatePipelineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePipelineResponse'
            Prelude.<$> (x Prelude..?> "PipelineArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePipeline

instance Prelude.NFData UpdatePipeline

instance Prelude.ToHeaders UpdatePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.UpdatePipeline" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdatePipeline where
  toJSON UpdatePipeline' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PipelineDescription" Prelude..=)
              Prelude.<$> pipelineDescription,
            ("RoleArn" Prelude..=) Prelude.<$> roleArn,
            ("PipelineDefinition" Prelude..=)
              Prelude.<$> pipelineDefinition,
            ("PipelineDisplayName" Prelude..=)
              Prelude.<$> pipelineDisplayName,
            Prelude.Just
              ("PipelineName" Prelude..= pipelineName)
          ]
      )

instance Prelude.ToPath UpdatePipeline where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdatePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePipelineResponse' smart constructor.
data UpdatePipelineResponse = UpdatePipelineResponse'
  { -- | The Amazon Resource Name (ARN) of the updated pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdatePipelineResponse
