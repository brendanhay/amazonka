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
-- Module      : Network.AWS.CodePipeline.RetryStageExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resumes the pipeline execution by retrying the last failed actions in a
-- stage. You can retry a stage immediately if any of the actions in the
-- stage fail. When you retry, all actions that are still in progress
-- continue working, and failed actions are triggered again.
module Network.AWS.CodePipeline.RetryStageExecution
  ( -- * Creating a Request
    RetryStageExecution (..),
    newRetryStageExecution,

    -- * Request Lenses
    retryStageExecution_pipelineName,
    retryStageExecution_stageName,
    retryStageExecution_pipelineExecutionId,
    retryStageExecution_retryMode,

    -- * Destructuring the Response
    RetryStageExecutionResponse (..),
    newRetryStageExecutionResponse,

    -- * Response Lenses
    retryStageExecutionResponse_pipelineExecutionId,
    retryStageExecutionResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @RetryStageExecution@ action.
--
-- /See:/ 'newRetryStageExecution' smart constructor.
data RetryStageExecution = RetryStageExecution'
  { -- | The name of the pipeline that contains the failed stage.
    pipelineName :: Prelude.Text,
    -- | The name of the failed stage to be retried.
    stageName :: Prelude.Text,
    -- | The ID of the pipeline execution in the failed stage to be retried. Use
    -- the GetPipelineState action to retrieve the current pipelineExecutionId
    -- of the failed stage
    pipelineExecutionId :: Prelude.Text,
    -- | The scope of the retry attempt. Currently, the only supported value is
    -- FAILED_ACTIONS.
    retryMode :: StageRetryMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RetryStageExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineName', 'retryStageExecution_pipelineName' - The name of the pipeline that contains the failed stage.
--
-- 'stageName', 'retryStageExecution_stageName' - The name of the failed stage to be retried.
--
-- 'pipelineExecutionId', 'retryStageExecution_pipelineExecutionId' - The ID of the pipeline execution in the failed stage to be retried. Use
-- the GetPipelineState action to retrieve the current pipelineExecutionId
-- of the failed stage
--
-- 'retryMode', 'retryStageExecution_retryMode' - The scope of the retry attempt. Currently, the only supported value is
-- FAILED_ACTIONS.
newRetryStageExecution ::
  -- | 'pipelineName'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  -- | 'pipelineExecutionId'
  Prelude.Text ->
  -- | 'retryMode'
  StageRetryMode ->
  RetryStageExecution
newRetryStageExecution
  pPipelineName_
  pStageName_
  pPipelineExecutionId_
  pRetryMode_ =
    RetryStageExecution'
      { pipelineName = pPipelineName_,
        stageName = pStageName_,
        pipelineExecutionId = pPipelineExecutionId_,
        retryMode = pRetryMode_
      }

-- | The name of the pipeline that contains the failed stage.
retryStageExecution_pipelineName :: Lens.Lens' RetryStageExecution Prelude.Text
retryStageExecution_pipelineName = Lens.lens (\RetryStageExecution' {pipelineName} -> pipelineName) (\s@RetryStageExecution' {} a -> s {pipelineName = a} :: RetryStageExecution)

-- | The name of the failed stage to be retried.
retryStageExecution_stageName :: Lens.Lens' RetryStageExecution Prelude.Text
retryStageExecution_stageName = Lens.lens (\RetryStageExecution' {stageName} -> stageName) (\s@RetryStageExecution' {} a -> s {stageName = a} :: RetryStageExecution)

-- | The ID of the pipeline execution in the failed stage to be retried. Use
-- the GetPipelineState action to retrieve the current pipelineExecutionId
-- of the failed stage
retryStageExecution_pipelineExecutionId :: Lens.Lens' RetryStageExecution Prelude.Text
retryStageExecution_pipelineExecutionId = Lens.lens (\RetryStageExecution' {pipelineExecutionId} -> pipelineExecutionId) (\s@RetryStageExecution' {} a -> s {pipelineExecutionId = a} :: RetryStageExecution)

-- | The scope of the retry attempt. Currently, the only supported value is
-- FAILED_ACTIONS.
retryStageExecution_retryMode :: Lens.Lens' RetryStageExecution StageRetryMode
retryStageExecution_retryMode = Lens.lens (\RetryStageExecution' {retryMode} -> retryMode) (\s@RetryStageExecution' {} a -> s {retryMode = a} :: RetryStageExecution)

instance Prelude.AWSRequest RetryStageExecution where
  type
    Rs RetryStageExecution =
      RetryStageExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RetryStageExecutionResponse'
            Prelude.<$> (x Prelude..?> "pipelineExecutionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RetryStageExecution

instance Prelude.NFData RetryStageExecution

instance Prelude.ToHeaders RetryStageExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodePipeline_20150709.RetryStageExecution" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RetryStageExecution where
  toJSON RetryStageExecution' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("pipelineName" Prelude..= pipelineName),
            Prelude.Just ("stageName" Prelude..= stageName),
            Prelude.Just
              ( "pipelineExecutionId"
                  Prelude..= pipelineExecutionId
              ),
            Prelude.Just ("retryMode" Prelude..= retryMode)
          ]
      )

instance Prelude.ToPath RetryStageExecution where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RetryStageExecution where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @RetryStageExecution@ action.
--
-- /See:/ 'newRetryStageExecutionResponse' smart constructor.
data RetryStageExecutionResponse = RetryStageExecutionResponse'
  { -- | The ID of the current workflow execution in the failed stage.
    pipelineExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RetryStageExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionId', 'retryStageExecutionResponse_pipelineExecutionId' - The ID of the current workflow execution in the failed stage.
--
-- 'httpStatus', 'retryStageExecutionResponse_httpStatus' - The response's http status code.
newRetryStageExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RetryStageExecutionResponse
newRetryStageExecutionResponse pHttpStatus_ =
  RetryStageExecutionResponse'
    { pipelineExecutionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the current workflow execution in the failed stage.
retryStageExecutionResponse_pipelineExecutionId :: Lens.Lens' RetryStageExecutionResponse (Prelude.Maybe Prelude.Text)
retryStageExecutionResponse_pipelineExecutionId = Lens.lens (\RetryStageExecutionResponse' {pipelineExecutionId} -> pipelineExecutionId) (\s@RetryStageExecutionResponse' {} a -> s {pipelineExecutionId = a} :: RetryStageExecutionResponse)

-- | The response's http status code.
retryStageExecutionResponse_httpStatus :: Lens.Lens' RetryStageExecutionResponse Prelude.Int
retryStageExecutionResponse_httpStatus = Lens.lens (\RetryStageExecutionResponse' {httpStatus} -> httpStatus) (\s@RetryStageExecutionResponse' {} a -> s {httpStatus = a} :: RetryStageExecutionResponse)

instance Prelude.NFData RetryStageExecutionResponse
