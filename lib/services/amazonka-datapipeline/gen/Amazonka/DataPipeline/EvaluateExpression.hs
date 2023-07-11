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
-- Module      : Amazonka.DataPipeline.EvaluateExpression
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @EvaluateExpression@ to evaluate a string in the
-- context of the specified object. For example, a task runner can evaluate
-- SQL queries stored in Amazon S3.
module Amazonka.DataPipeline.EvaluateExpression
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataPipeline.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest EvaluateExpression where
  type
    AWSResponse EvaluateExpression =
      EvaluateExpressionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          EvaluateExpressionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "evaluatedExpression")
      )

instance Prelude.Hashable EvaluateExpression where
  hashWithSalt _salt EvaluateExpression' {..} =
    _salt
      `Prelude.hashWithSalt` pipelineId
      `Prelude.hashWithSalt` objectId
      `Prelude.hashWithSalt` expression

instance Prelude.NFData EvaluateExpression where
  rnf EvaluateExpression' {..} =
    Prelude.rnf pipelineId
      `Prelude.seq` Prelude.rnf objectId
      `Prelude.seq` Prelude.rnf expression

instance Data.ToHeaders EvaluateExpression where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DataPipeline.EvaluateExpression" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EvaluateExpression where
  toJSON EvaluateExpression' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("pipelineId" Data..= pipelineId),
            Prelude.Just ("objectId" Data..= objectId),
            Prelude.Just ("expression" Data..= expression)
          ]
      )

instance Data.ToPath EvaluateExpression where
  toPath = Prelude.const "/"

instance Data.ToQuery EvaluateExpression where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData EvaluateExpressionResponse where
  rnf EvaluateExpressionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf evaluatedExpression
