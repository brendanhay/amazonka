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
-- Module      : Amazonka.SageMaker.StartPipelineExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a pipeline execution.
module Amazonka.SageMaker.StartPipelineExecution
  ( -- * Creating a Request
    StartPipelineExecution (..),
    newStartPipelineExecution,

    -- * Request Lenses
    startPipelineExecution_parallelismConfiguration,
    startPipelineExecution_pipelineExecutionDescription,
    startPipelineExecution_pipelineExecutionDisplayName,
    startPipelineExecution_pipelineParameters,
    startPipelineExecution_selectiveExecutionConfig,
    startPipelineExecution_pipelineName,
    startPipelineExecution_clientRequestToken,

    -- * Destructuring the Response
    StartPipelineExecutionResponse (..),
    newStartPipelineExecutionResponse,

    -- * Response Lenses
    startPipelineExecutionResponse_pipelineExecutionArn,
    startPipelineExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newStartPipelineExecution' smart constructor.
data StartPipelineExecution = StartPipelineExecution'
  { -- | This configuration, if specified, overrides the parallelism
    -- configuration of the parent pipeline for this specific run.
    parallelismConfiguration :: Prelude.Maybe ParallelismConfiguration,
    -- | The description of the pipeline execution.
    pipelineExecutionDescription :: Prelude.Maybe Prelude.Text,
    -- | The display name of the pipeline execution.
    pipelineExecutionDisplayName :: Prelude.Maybe Prelude.Text,
    -- | Contains a list of pipeline parameters. This list can be empty.
    pipelineParameters :: Prelude.Maybe [Parameter],
    -- | The selective execution configuration applied to the pipeline run.
    selectiveExecutionConfig :: Prelude.Maybe SelectiveExecutionConfig,
    -- | The name or Amazon Resource Name (ARN) of the pipeline.
    pipelineName :: Prelude.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the operation. An idempotent operation completes no more
    -- than once.
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartPipelineExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parallelismConfiguration', 'startPipelineExecution_parallelismConfiguration' - This configuration, if specified, overrides the parallelism
-- configuration of the parent pipeline for this specific run.
--
-- 'pipelineExecutionDescription', 'startPipelineExecution_pipelineExecutionDescription' - The description of the pipeline execution.
--
-- 'pipelineExecutionDisplayName', 'startPipelineExecution_pipelineExecutionDisplayName' - The display name of the pipeline execution.
--
-- 'pipelineParameters', 'startPipelineExecution_pipelineParameters' - Contains a list of pipeline parameters. This list can be empty.
--
-- 'selectiveExecutionConfig', 'startPipelineExecution_selectiveExecutionConfig' - The selective execution configuration applied to the pipeline run.
--
-- 'pipelineName', 'startPipelineExecution_pipelineName' - The name or Amazon Resource Name (ARN) of the pipeline.
--
-- 'clientRequestToken', 'startPipelineExecution_clientRequestToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than once.
newStartPipelineExecution ::
  -- | 'pipelineName'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  StartPipelineExecution
newStartPipelineExecution
  pPipelineName_
  pClientRequestToken_ =
    StartPipelineExecution'
      { parallelismConfiguration =
          Prelude.Nothing,
        pipelineExecutionDescription = Prelude.Nothing,
        pipelineExecutionDisplayName = Prelude.Nothing,
        pipelineParameters = Prelude.Nothing,
        selectiveExecutionConfig = Prelude.Nothing,
        pipelineName = pPipelineName_,
        clientRequestToken = pClientRequestToken_
      }

-- | This configuration, if specified, overrides the parallelism
-- configuration of the parent pipeline for this specific run.
startPipelineExecution_parallelismConfiguration :: Lens.Lens' StartPipelineExecution (Prelude.Maybe ParallelismConfiguration)
startPipelineExecution_parallelismConfiguration = Lens.lens (\StartPipelineExecution' {parallelismConfiguration} -> parallelismConfiguration) (\s@StartPipelineExecution' {} a -> s {parallelismConfiguration = a} :: StartPipelineExecution)

-- | The description of the pipeline execution.
startPipelineExecution_pipelineExecutionDescription :: Lens.Lens' StartPipelineExecution (Prelude.Maybe Prelude.Text)
startPipelineExecution_pipelineExecutionDescription = Lens.lens (\StartPipelineExecution' {pipelineExecutionDescription} -> pipelineExecutionDescription) (\s@StartPipelineExecution' {} a -> s {pipelineExecutionDescription = a} :: StartPipelineExecution)

-- | The display name of the pipeline execution.
startPipelineExecution_pipelineExecutionDisplayName :: Lens.Lens' StartPipelineExecution (Prelude.Maybe Prelude.Text)
startPipelineExecution_pipelineExecutionDisplayName = Lens.lens (\StartPipelineExecution' {pipelineExecutionDisplayName} -> pipelineExecutionDisplayName) (\s@StartPipelineExecution' {} a -> s {pipelineExecutionDisplayName = a} :: StartPipelineExecution)

-- | Contains a list of pipeline parameters. This list can be empty.
startPipelineExecution_pipelineParameters :: Lens.Lens' StartPipelineExecution (Prelude.Maybe [Parameter])
startPipelineExecution_pipelineParameters = Lens.lens (\StartPipelineExecution' {pipelineParameters} -> pipelineParameters) (\s@StartPipelineExecution' {} a -> s {pipelineParameters = a} :: StartPipelineExecution) Prelude.. Lens.mapping Lens.coerced

-- | The selective execution configuration applied to the pipeline run.
startPipelineExecution_selectiveExecutionConfig :: Lens.Lens' StartPipelineExecution (Prelude.Maybe SelectiveExecutionConfig)
startPipelineExecution_selectiveExecutionConfig = Lens.lens (\StartPipelineExecution' {selectiveExecutionConfig} -> selectiveExecutionConfig) (\s@StartPipelineExecution' {} a -> s {selectiveExecutionConfig = a} :: StartPipelineExecution)

-- | The name or Amazon Resource Name (ARN) of the pipeline.
startPipelineExecution_pipelineName :: Lens.Lens' StartPipelineExecution Prelude.Text
startPipelineExecution_pipelineName = Lens.lens (\StartPipelineExecution' {pipelineName} -> pipelineName) (\s@StartPipelineExecution' {} a -> s {pipelineName = a} :: StartPipelineExecution)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than once.
startPipelineExecution_clientRequestToken :: Lens.Lens' StartPipelineExecution Prelude.Text
startPipelineExecution_clientRequestToken = Lens.lens (\StartPipelineExecution' {clientRequestToken} -> clientRequestToken) (\s@StartPipelineExecution' {} a -> s {clientRequestToken = a} :: StartPipelineExecution)

instance Core.AWSRequest StartPipelineExecution where
  type
    AWSResponse StartPipelineExecution =
      StartPipelineExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartPipelineExecutionResponse'
            Prelude.<$> (x Data..?> "PipelineExecutionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartPipelineExecution where
  hashWithSalt _salt StartPipelineExecution' {..} =
    _salt
      `Prelude.hashWithSalt` parallelismConfiguration
      `Prelude.hashWithSalt` pipelineExecutionDescription
      `Prelude.hashWithSalt` pipelineExecutionDisplayName
      `Prelude.hashWithSalt` pipelineParameters
      `Prelude.hashWithSalt` selectiveExecutionConfig
      `Prelude.hashWithSalt` pipelineName
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData StartPipelineExecution where
  rnf StartPipelineExecution' {..} =
    Prelude.rnf parallelismConfiguration
      `Prelude.seq` Prelude.rnf pipelineExecutionDescription
      `Prelude.seq` Prelude.rnf pipelineExecutionDisplayName
      `Prelude.seq` Prelude.rnf pipelineParameters
      `Prelude.seq` Prelude.rnf selectiveExecutionConfig
      `Prelude.seq` Prelude.rnf pipelineName
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders StartPipelineExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.StartPipelineExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartPipelineExecution where
  toJSON StartPipelineExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ParallelismConfiguration" Data..=)
              Prelude.<$> parallelismConfiguration,
            ("PipelineExecutionDescription" Data..=)
              Prelude.<$> pipelineExecutionDescription,
            ("PipelineExecutionDisplayName" Data..=)
              Prelude.<$> pipelineExecutionDisplayName,
            ("PipelineParameters" Data..=)
              Prelude.<$> pipelineParameters,
            ("SelectiveExecutionConfig" Data..=)
              Prelude.<$> selectiveExecutionConfig,
            Prelude.Just ("PipelineName" Data..= pipelineName),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath StartPipelineExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery StartPipelineExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartPipelineExecutionResponse' smart constructor.
data StartPipelineExecutionResponse = StartPipelineExecutionResponse'
  { -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartPipelineExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionArn', 'startPipelineExecutionResponse_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
--
-- 'httpStatus', 'startPipelineExecutionResponse_httpStatus' - The response's http status code.
newStartPipelineExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartPipelineExecutionResponse
newStartPipelineExecutionResponse pHttpStatus_ =
  StartPipelineExecutionResponse'
    { pipelineExecutionArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the pipeline execution.
startPipelineExecutionResponse_pipelineExecutionArn :: Lens.Lens' StartPipelineExecutionResponse (Prelude.Maybe Prelude.Text)
startPipelineExecutionResponse_pipelineExecutionArn = Lens.lens (\StartPipelineExecutionResponse' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@StartPipelineExecutionResponse' {} a -> s {pipelineExecutionArn = a} :: StartPipelineExecutionResponse)

-- | The response's http status code.
startPipelineExecutionResponse_httpStatus :: Lens.Lens' StartPipelineExecutionResponse Prelude.Int
startPipelineExecutionResponse_httpStatus = Lens.lens (\StartPipelineExecutionResponse' {httpStatus} -> httpStatus) (\s@StartPipelineExecutionResponse' {} a -> s {httpStatus = a} :: StartPipelineExecutionResponse)

instance
  Prelude.NFData
    StartPipelineExecutionResponse
  where
  rnf StartPipelineExecutionResponse' {..} =
    Prelude.rnf pipelineExecutionArn
      `Prelude.seq` Prelude.rnf httpStatus
