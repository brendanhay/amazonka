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
-- Module      : Network.AWS.DataPipeline.EvaluateExpression
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @EvaluateExpression@ to evaluate a string in the
-- context of the specified object. For example, a task runner can evaluate
-- SQL queries stored in Amazon S3.
module Network.AWS.DataPipeline.EvaluateExpression
  ( -- * Creating a Request
    EvaluateExpression (..),
    newEvaluateExpression,

    -- * Request Lenses
    evaluateExpression_pipelineId,
    evaluateExpression_objectId,
    evaluateExpression_expression,

    -- * Destructuring the Response
    EvaluateExpressionResponse (..),
    newEvaluateExpressionResponse,

    -- * Response Lenses
    evaluateExpressionResponse_httpStatus,
    evaluateExpressionResponse_evaluatedExpression,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for EvaluateExpression.
--
-- /See:/ 'newEvaluateExpression' smart constructor.
data EvaluateExpression = EvaluateExpression'
  { -- | The ID of the pipeline.
    pipelineId :: Core.Text,
    -- | The ID of the object.
    objectId :: Core.Text,
    -- | The expression to evaluate.
    expression :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EvaluateExpression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineId', 'evaluateExpression_pipelineId' - The ID of the pipeline.
--
-- 'objectId', 'evaluateExpression_objectId' - The ID of the object.
--
-- 'expression', 'evaluateExpression_expression' - The expression to evaluate.
newEvaluateExpression ::
  -- | 'pipelineId'
  Core.Text ->
  -- | 'objectId'
  Core.Text ->
  -- | 'expression'
  Core.Text ->
  EvaluateExpression
newEvaluateExpression
  pPipelineId_
  pObjectId_
  pExpression_ =
    EvaluateExpression'
      { pipelineId = pPipelineId_,
        objectId = pObjectId_,
        expression = pExpression_
      }

-- | The ID of the pipeline.
evaluateExpression_pipelineId :: Lens.Lens' EvaluateExpression Core.Text
evaluateExpression_pipelineId = Lens.lens (\EvaluateExpression' {pipelineId} -> pipelineId) (\s@EvaluateExpression' {} a -> s {pipelineId = a} :: EvaluateExpression)

-- | The ID of the object.
evaluateExpression_objectId :: Lens.Lens' EvaluateExpression Core.Text
evaluateExpression_objectId = Lens.lens (\EvaluateExpression' {objectId} -> objectId) (\s@EvaluateExpression' {} a -> s {objectId = a} :: EvaluateExpression)

-- | The expression to evaluate.
evaluateExpression_expression :: Lens.Lens' EvaluateExpression Core.Text
evaluateExpression_expression = Lens.lens (\EvaluateExpression' {expression} -> expression) (\s@EvaluateExpression' {} a -> s {expression = a} :: EvaluateExpression)

instance Core.AWSRequest EvaluateExpression where
  type
    AWSResponse EvaluateExpression =
      EvaluateExpressionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          EvaluateExpressionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "evaluatedExpression")
      )

instance Core.Hashable EvaluateExpression

instance Core.NFData EvaluateExpression

instance Core.ToHeaders EvaluateExpression where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DataPipeline.EvaluateExpression" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON EvaluateExpression where
  toJSON EvaluateExpression' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineId" Core..= pipelineId),
            Core.Just ("objectId" Core..= objectId),
            Core.Just ("expression" Core..= expression)
          ]
      )

instance Core.ToPath EvaluateExpression where
  toPath = Core.const "/"

instance Core.ToQuery EvaluateExpression where
  toQuery = Core.const Core.mempty

-- | Contains the output of EvaluateExpression.
--
-- /See:/ 'newEvaluateExpressionResponse' smart constructor.
data EvaluateExpressionResponse = EvaluateExpressionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The evaluated expression.
    evaluatedExpression :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EvaluateExpressionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'evaluateExpressionResponse_httpStatus' - The response's http status code.
--
-- 'evaluatedExpression', 'evaluateExpressionResponse_evaluatedExpression' - The evaluated expression.
newEvaluateExpressionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'evaluatedExpression'
  Core.Text ->
  EvaluateExpressionResponse
newEvaluateExpressionResponse
  pHttpStatus_
  pEvaluatedExpression_ =
    EvaluateExpressionResponse'
      { httpStatus =
          pHttpStatus_,
        evaluatedExpression = pEvaluatedExpression_
      }

-- | The response's http status code.
evaluateExpressionResponse_httpStatus :: Lens.Lens' EvaluateExpressionResponse Core.Int
evaluateExpressionResponse_httpStatus = Lens.lens (\EvaluateExpressionResponse' {httpStatus} -> httpStatus) (\s@EvaluateExpressionResponse' {} a -> s {httpStatus = a} :: EvaluateExpressionResponse)

-- | The evaluated expression.
evaluateExpressionResponse_evaluatedExpression :: Lens.Lens' EvaluateExpressionResponse Core.Text
evaluateExpressionResponse_evaluatedExpression = Lens.lens (\EvaluateExpressionResponse' {evaluatedExpression} -> evaluatedExpression) (\s@EvaluateExpressionResponse' {} a -> s {evaluatedExpression = a} :: EvaluateExpressionResponse)

instance Core.NFData EvaluateExpressionResponse
