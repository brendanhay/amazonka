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

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for EvaluateExpression.
--
-- /See:/ 'newEvaluateExpression' smart constructor.
data EvaluateExpression = EvaluateExpression'
  { -- | The ID of the pipeline.
    pipelineId :: Prelude.Text,
    -- | The ID of the object.
    objectId :: Prelude.Text,
    -- | The expression to evaluate.
    expression :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'objectId'
  Prelude.Text ->
  -- | 'expression'
  Prelude.Text ->
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
evaluateExpression_pipelineId :: Lens.Lens' EvaluateExpression Prelude.Text
evaluateExpression_pipelineId = Lens.lens (\EvaluateExpression' {pipelineId} -> pipelineId) (\s@EvaluateExpression' {} a -> s {pipelineId = a} :: EvaluateExpression)

-- | The ID of the object.
evaluateExpression_objectId :: Lens.Lens' EvaluateExpression Prelude.Text
evaluateExpression_objectId = Lens.lens (\EvaluateExpression' {objectId} -> objectId) (\s@EvaluateExpression' {} a -> s {objectId = a} :: EvaluateExpression)

-- | The expression to evaluate.
evaluateExpression_expression :: Lens.Lens' EvaluateExpression Prelude.Text
evaluateExpression_expression = Lens.lens (\EvaluateExpression' {expression} -> expression) (\s@EvaluateExpression' {} a -> s {expression = a} :: EvaluateExpression)

instance Prelude.AWSRequest EvaluateExpression where
  type
    Rs EvaluateExpression =
      EvaluateExpressionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          EvaluateExpressionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "evaluatedExpression")
      )

instance Prelude.Hashable EvaluateExpression

instance Prelude.NFData EvaluateExpression

instance Prelude.ToHeaders EvaluateExpression where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DataPipeline.EvaluateExpression" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON EvaluateExpression where
  toJSON EvaluateExpression' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("pipelineId" Prelude..= pipelineId),
            Prelude.Just ("objectId" Prelude..= objectId),
            Prelude.Just ("expression" Prelude..= expression)
          ]
      )

instance Prelude.ToPath EvaluateExpression where
  toPath = Prelude.const "/"

instance Prelude.ToQuery EvaluateExpression where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of EvaluateExpression.
--
-- /See:/ 'newEvaluateExpressionResponse' smart constructor.
data EvaluateExpressionResponse = EvaluateExpressionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The evaluated expression.
    evaluatedExpression :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'evaluatedExpression'
  Prelude.Text ->
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
evaluateExpressionResponse_httpStatus :: Lens.Lens' EvaluateExpressionResponse Prelude.Int
evaluateExpressionResponse_httpStatus = Lens.lens (\EvaluateExpressionResponse' {httpStatus} -> httpStatus) (\s@EvaluateExpressionResponse' {} a -> s {httpStatus = a} :: EvaluateExpressionResponse)

-- | The evaluated expression.
evaluateExpressionResponse_evaluatedExpression :: Lens.Lens' EvaluateExpressionResponse Prelude.Text
evaluateExpressionResponse_evaluatedExpression = Lens.lens (\EvaluateExpressionResponse' {evaluatedExpression} -> evaluatedExpression) (\s@EvaluateExpressionResponse' {} a -> s {evaluatedExpression = a} :: EvaluateExpressionResponse)

instance Prelude.NFData EvaluateExpressionResponse
