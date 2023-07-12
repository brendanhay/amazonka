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
-- Module      : Amazonka.SageMaker.UpdatePipelineExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a pipeline execution.
module Amazonka.SageMaker.UpdatePipelineExecution
  ( -- * Creating a Request
    UpdatePipelineExecution (..),
    newUpdatePipelineExecution,

    -- * Request Lenses
    updatePipelineExecution_parallelismConfiguration,
    updatePipelineExecution_pipelineExecutionDescription,
    updatePipelineExecution_pipelineExecutionDisplayName,
    updatePipelineExecution_pipelineExecutionArn,

    -- * Destructuring the Response
    UpdatePipelineExecutionResponse (..),
    newUpdatePipelineExecutionResponse,

    -- * Response Lenses
    updatePipelineExecutionResponse_pipelineExecutionArn,
    updatePipelineExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdatePipelineExecution' smart constructor.
data UpdatePipelineExecution = UpdatePipelineExecution'
  { -- | This configuration, if specified, overrides the parallelism
    -- configuration of the parent pipeline for this specific run.
    parallelismConfiguration :: Prelude.Maybe ParallelismConfiguration,
    -- | The description of the pipeline execution.
    pipelineExecutionDescription :: Prelude.Maybe Prelude.Text,
    -- | The display name of the pipeline execution.
    pipelineExecutionDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipelineExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parallelismConfiguration', 'updatePipelineExecution_parallelismConfiguration' - This configuration, if specified, overrides the parallelism
-- configuration of the parent pipeline for this specific run.
--
-- 'pipelineExecutionDescription', 'updatePipelineExecution_pipelineExecutionDescription' - The description of the pipeline execution.
--
-- 'pipelineExecutionDisplayName', 'updatePipelineExecution_pipelineExecutionDisplayName' - The display name of the pipeline execution.
--
-- 'pipelineExecutionArn', 'updatePipelineExecution_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
newUpdatePipelineExecution ::
  -- | 'pipelineExecutionArn'
  Prelude.Text ->
  UpdatePipelineExecution
newUpdatePipelineExecution pPipelineExecutionArn_ =
  UpdatePipelineExecution'
    { parallelismConfiguration =
        Prelude.Nothing,
      pipelineExecutionDescription = Prelude.Nothing,
      pipelineExecutionDisplayName = Prelude.Nothing,
      pipelineExecutionArn = pPipelineExecutionArn_
    }

-- | This configuration, if specified, overrides the parallelism
-- configuration of the parent pipeline for this specific run.
updatePipelineExecution_parallelismConfiguration :: Lens.Lens' UpdatePipelineExecution (Prelude.Maybe ParallelismConfiguration)
updatePipelineExecution_parallelismConfiguration = Lens.lens (\UpdatePipelineExecution' {parallelismConfiguration} -> parallelismConfiguration) (\s@UpdatePipelineExecution' {} a -> s {parallelismConfiguration = a} :: UpdatePipelineExecution)

-- | The description of the pipeline execution.
updatePipelineExecution_pipelineExecutionDescription :: Lens.Lens' UpdatePipelineExecution (Prelude.Maybe Prelude.Text)
updatePipelineExecution_pipelineExecutionDescription = Lens.lens (\UpdatePipelineExecution' {pipelineExecutionDescription} -> pipelineExecutionDescription) (\s@UpdatePipelineExecution' {} a -> s {pipelineExecutionDescription = a} :: UpdatePipelineExecution)

-- | The display name of the pipeline execution.
updatePipelineExecution_pipelineExecutionDisplayName :: Lens.Lens' UpdatePipelineExecution (Prelude.Maybe Prelude.Text)
updatePipelineExecution_pipelineExecutionDisplayName = Lens.lens (\UpdatePipelineExecution' {pipelineExecutionDisplayName} -> pipelineExecutionDisplayName) (\s@UpdatePipelineExecution' {} a -> s {pipelineExecutionDisplayName = a} :: UpdatePipelineExecution)

-- | The Amazon Resource Name (ARN) of the pipeline execution.
updatePipelineExecution_pipelineExecutionArn :: Lens.Lens' UpdatePipelineExecution Prelude.Text
updatePipelineExecution_pipelineExecutionArn = Lens.lens (\UpdatePipelineExecution' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@UpdatePipelineExecution' {} a -> s {pipelineExecutionArn = a} :: UpdatePipelineExecution)

instance Core.AWSRequest UpdatePipelineExecution where
  type
    AWSResponse UpdatePipelineExecution =
      UpdatePipelineExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePipelineExecutionResponse'
            Prelude.<$> (x Data..?> "PipelineExecutionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePipelineExecution where
  hashWithSalt _salt UpdatePipelineExecution' {..} =
    _salt
      `Prelude.hashWithSalt` parallelismConfiguration
      `Prelude.hashWithSalt` pipelineExecutionDescription
      `Prelude.hashWithSalt` pipelineExecutionDisplayName
      `Prelude.hashWithSalt` pipelineExecutionArn

instance Prelude.NFData UpdatePipelineExecution where
  rnf UpdatePipelineExecution' {..} =
    Prelude.rnf parallelismConfiguration
      `Prelude.seq` Prelude.rnf pipelineExecutionDescription
      `Prelude.seq` Prelude.rnf pipelineExecutionDisplayName
      `Prelude.seq` Prelude.rnf pipelineExecutionArn

instance Data.ToHeaders UpdatePipelineExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.UpdatePipelineExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePipelineExecution where
  toJSON UpdatePipelineExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ParallelismConfiguration" Data..=)
              Prelude.<$> parallelismConfiguration,
            ("PipelineExecutionDescription" Data..=)
              Prelude.<$> pipelineExecutionDescription,
            ("PipelineExecutionDisplayName" Data..=)
              Prelude.<$> pipelineExecutionDisplayName,
            Prelude.Just
              ( "PipelineExecutionArn"
                  Data..= pipelineExecutionArn
              )
          ]
      )

instance Data.ToPath UpdatePipelineExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePipelineExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePipelineExecutionResponse' smart constructor.
data UpdatePipelineExecutionResponse = UpdatePipelineExecutionResponse'
  { -- | The Amazon Resource Name (ARN) of the updated pipeline execution.
    pipelineExecutionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipelineExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionArn', 'updatePipelineExecutionResponse_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the updated pipeline execution.
--
-- 'httpStatus', 'updatePipelineExecutionResponse_httpStatus' - The response's http status code.
newUpdatePipelineExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePipelineExecutionResponse
newUpdatePipelineExecutionResponse pHttpStatus_ =
  UpdatePipelineExecutionResponse'
    { pipelineExecutionArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the updated pipeline execution.
updatePipelineExecutionResponse_pipelineExecutionArn :: Lens.Lens' UpdatePipelineExecutionResponse (Prelude.Maybe Prelude.Text)
updatePipelineExecutionResponse_pipelineExecutionArn = Lens.lens (\UpdatePipelineExecutionResponse' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@UpdatePipelineExecutionResponse' {} a -> s {pipelineExecutionArn = a} :: UpdatePipelineExecutionResponse)

-- | The response's http status code.
updatePipelineExecutionResponse_httpStatus :: Lens.Lens' UpdatePipelineExecutionResponse Prelude.Int
updatePipelineExecutionResponse_httpStatus = Lens.lens (\UpdatePipelineExecutionResponse' {httpStatus} -> httpStatus) (\s@UpdatePipelineExecutionResponse' {} a -> s {httpStatus = a} :: UpdatePipelineExecutionResponse)

instance
  Prelude.NFData
    UpdatePipelineExecutionResponse
  where
  rnf UpdatePipelineExecutionResponse' {..} =
    Prelude.rnf pipelineExecutionArn
      `Prelude.seq` Prelude.rnf httpStatus
