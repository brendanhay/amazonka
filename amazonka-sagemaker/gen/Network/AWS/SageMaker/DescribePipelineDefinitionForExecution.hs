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
-- Module      : Network.AWS.SageMaker.DescribePipelineDefinitionForExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the details of an execution\'s pipeline definition.
module Network.AWS.SageMaker.DescribePipelineDefinitionForExecution
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribePipelineDefinitionForExecution' smart constructor.
data DescribePipelineDefinitionForExecution = DescribePipelineDefinitionForExecution'
  { -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribePipelineDefinitionForExecution
newDescribePipelineDefinitionForExecution
  pPipelineExecutionArn_ =
    DescribePipelineDefinitionForExecution'
      { pipelineExecutionArn =
          pPipelineExecutionArn_
      }

-- | The Amazon Resource Name (ARN) of the pipeline execution.
describePipelineDefinitionForExecution_pipelineExecutionArn :: Lens.Lens' DescribePipelineDefinitionForExecution Core.Text
describePipelineDefinitionForExecution_pipelineExecutionArn = Lens.lens (\DescribePipelineDefinitionForExecution' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@DescribePipelineDefinitionForExecution' {} a -> s {pipelineExecutionArn = a} :: DescribePipelineDefinitionForExecution)

instance
  Core.AWSRequest
    DescribePipelineDefinitionForExecution
  where
  type
    AWSResponse
      DescribePipelineDefinitionForExecution =
      DescribePipelineDefinitionForExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePipelineDefinitionForExecutionResponse'
            Core.<$> (x Core..?> "CreationTime")
              Core.<*> (x Core..?> "PipelineDefinition")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribePipelineDefinitionForExecution

instance
  Core.NFData
    DescribePipelineDefinitionForExecution

instance
  Core.ToHeaders
    DescribePipelineDefinitionForExecution
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribePipelineDefinitionForExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribePipelineDefinitionForExecution
  where
  toJSON DescribePipelineDefinitionForExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "PipelineExecutionArn"
                  Core..= pipelineExecutionArn
              )
          ]
      )

instance
  Core.ToPath
    DescribePipelineDefinitionForExecution
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribePipelineDefinitionForExecution
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribePipelineDefinitionForExecutionResponse' smart constructor.
data DescribePipelineDefinitionForExecutionResponse = DescribePipelineDefinitionForExecutionResponse'
  { -- | The time when the pipeline was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The JSON pipeline definition.
    pipelineDefinition :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribePipelineDefinitionForExecutionResponse
newDescribePipelineDefinitionForExecutionResponse
  pHttpStatus_ =
    DescribePipelineDefinitionForExecutionResponse'
      { creationTime =
          Core.Nothing,
        pipelineDefinition =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The time when the pipeline was created.
describePipelineDefinitionForExecutionResponse_creationTime :: Lens.Lens' DescribePipelineDefinitionForExecutionResponse (Core.Maybe Core.UTCTime)
describePipelineDefinitionForExecutionResponse_creationTime = Lens.lens (\DescribePipelineDefinitionForExecutionResponse' {creationTime} -> creationTime) (\s@DescribePipelineDefinitionForExecutionResponse' {} a -> s {creationTime = a} :: DescribePipelineDefinitionForExecutionResponse) Core.. Lens.mapping Core._Time

-- | The JSON pipeline definition.
describePipelineDefinitionForExecutionResponse_pipelineDefinition :: Lens.Lens' DescribePipelineDefinitionForExecutionResponse (Core.Maybe Core.Text)
describePipelineDefinitionForExecutionResponse_pipelineDefinition = Lens.lens (\DescribePipelineDefinitionForExecutionResponse' {pipelineDefinition} -> pipelineDefinition) (\s@DescribePipelineDefinitionForExecutionResponse' {} a -> s {pipelineDefinition = a} :: DescribePipelineDefinitionForExecutionResponse)

-- | The response's http status code.
describePipelineDefinitionForExecutionResponse_httpStatus :: Lens.Lens' DescribePipelineDefinitionForExecutionResponse Core.Int
describePipelineDefinitionForExecutionResponse_httpStatus = Lens.lens (\DescribePipelineDefinitionForExecutionResponse' {httpStatus} -> httpStatus) (\s@DescribePipelineDefinitionForExecutionResponse' {} a -> s {httpStatus = a} :: DescribePipelineDefinitionForExecutionResponse)

instance
  Core.NFData
    DescribePipelineDefinitionForExecutionResponse
