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
-- Module      : Amazonka.SageMaker.DescribePipelineDefinitionForExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the details of an execution\'s pipeline definition.
module Amazonka.SageMaker.DescribePipelineDefinitionForExecution
  ( -- * Creating a Request
    DescribePipelineDefinitionForExecution (..),
    newDescribePipelineDefinitionForExecution,

    -- * Request Lenses
    describePipelineDefinitionForExecution_pipelineExecutionArn,

    -- * Destructuring the Response
    DescribePipelineDefinitionForExecutionResponse (..),
    newDescribePipelineDefinitionForExecutionResponse,

    -- * Response Lenses
    describePipelineDefinitionForExecutionResponse_creationTime,
    describePipelineDefinitionForExecutionResponse_pipelineDefinition,
    describePipelineDefinitionForExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribePipelineDefinitionForExecution' smart constructor.
data DescribePipelineDefinitionForExecution = DescribePipelineDefinitionForExecution'
  { -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePipelineDefinitionForExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionArn', 'describePipelineDefinitionForExecution_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
newDescribePipelineDefinitionForExecution ::
  -- | 'pipelineExecutionArn'
  Prelude.Text ->
  DescribePipelineDefinitionForExecution
newDescribePipelineDefinitionForExecution
  pPipelineExecutionArn_ =
    DescribePipelineDefinitionForExecution'
      { pipelineExecutionArn =
          pPipelineExecutionArn_
      }

-- | The Amazon Resource Name (ARN) of the pipeline execution.
describePipelineDefinitionForExecution_pipelineExecutionArn :: Lens.Lens' DescribePipelineDefinitionForExecution Prelude.Text
describePipelineDefinitionForExecution_pipelineExecutionArn = Lens.lens (\DescribePipelineDefinitionForExecution' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@DescribePipelineDefinitionForExecution' {} a -> s {pipelineExecutionArn = a} :: DescribePipelineDefinitionForExecution)

instance
  Core.AWSRequest
    DescribePipelineDefinitionForExecution
  where
  type
    AWSResponse
      DescribePipelineDefinitionForExecution =
      DescribePipelineDefinitionForExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePipelineDefinitionForExecutionResponse'
            Prelude.<$> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "PipelineDefinition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribePipelineDefinitionForExecution
  where
  hashWithSalt
    _salt
    DescribePipelineDefinitionForExecution' {..} =
      _salt `Prelude.hashWithSalt` pipelineExecutionArn

instance
  Prelude.NFData
    DescribePipelineDefinitionForExecution
  where
  rnf DescribePipelineDefinitionForExecution' {..} =
    Prelude.rnf pipelineExecutionArn

instance
  Data.ToHeaders
    DescribePipelineDefinitionForExecution
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribePipelineDefinitionForExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribePipelineDefinitionForExecution
  where
  toJSON DescribePipelineDefinitionForExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "PipelineExecutionArn"
                  Data..= pipelineExecutionArn
              )
          ]
      )

instance
  Data.ToPath
    DescribePipelineDefinitionForExecution
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribePipelineDefinitionForExecution
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePipelineDefinitionForExecutionResponse' smart constructor.
data DescribePipelineDefinitionForExecutionResponse = DescribePipelineDefinitionForExecutionResponse'
  { -- | The time when the pipeline was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The JSON pipeline definition.
    pipelineDefinition :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePipelineDefinitionForExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describePipelineDefinitionForExecutionResponse_creationTime' - The time when the pipeline was created.
--
-- 'pipelineDefinition', 'describePipelineDefinitionForExecutionResponse_pipelineDefinition' - The JSON pipeline definition.
--
-- 'httpStatus', 'describePipelineDefinitionForExecutionResponse_httpStatus' - The response's http status code.
newDescribePipelineDefinitionForExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePipelineDefinitionForExecutionResponse
newDescribePipelineDefinitionForExecutionResponse
  pHttpStatus_ =
    DescribePipelineDefinitionForExecutionResponse'
      { creationTime =
          Prelude.Nothing,
        pipelineDefinition =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The time when the pipeline was created.
describePipelineDefinitionForExecutionResponse_creationTime :: Lens.Lens' DescribePipelineDefinitionForExecutionResponse (Prelude.Maybe Prelude.UTCTime)
describePipelineDefinitionForExecutionResponse_creationTime = Lens.lens (\DescribePipelineDefinitionForExecutionResponse' {creationTime} -> creationTime) (\s@DescribePipelineDefinitionForExecutionResponse' {} a -> s {creationTime = a} :: DescribePipelineDefinitionForExecutionResponse) Prelude.. Lens.mapping Data._Time

-- | The JSON pipeline definition.
describePipelineDefinitionForExecutionResponse_pipelineDefinition :: Lens.Lens' DescribePipelineDefinitionForExecutionResponse (Prelude.Maybe Prelude.Text)
describePipelineDefinitionForExecutionResponse_pipelineDefinition = Lens.lens (\DescribePipelineDefinitionForExecutionResponse' {pipelineDefinition} -> pipelineDefinition) (\s@DescribePipelineDefinitionForExecutionResponse' {} a -> s {pipelineDefinition = a} :: DescribePipelineDefinitionForExecutionResponse)

-- | The response's http status code.
describePipelineDefinitionForExecutionResponse_httpStatus :: Lens.Lens' DescribePipelineDefinitionForExecutionResponse Prelude.Int
describePipelineDefinitionForExecutionResponse_httpStatus = Lens.lens (\DescribePipelineDefinitionForExecutionResponse' {httpStatus} -> httpStatus) (\s@DescribePipelineDefinitionForExecutionResponse' {} a -> s {httpStatus = a} :: DescribePipelineDefinitionForExecutionResponse)

instance
  Prelude.NFData
    DescribePipelineDefinitionForExecutionResponse
  where
  rnf
    DescribePipelineDefinitionForExecutionResponse' {..} =
      Prelude.rnf creationTime
        `Prelude.seq` Prelude.rnf pipelineDefinition
        `Prelude.seq` Prelude.rnf httpStatus
