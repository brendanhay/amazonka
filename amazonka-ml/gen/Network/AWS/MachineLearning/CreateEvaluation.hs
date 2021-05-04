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
-- Module      : Network.AWS.MachineLearning.CreateEvaluation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new @Evaluation@ of an @MLModel@. An @MLModel@ is evaluated on
-- a set of observations associated to a @DataSource@. Like a @DataSource@
-- for an @MLModel@, the @DataSource@ for an @Evaluation@ contains values
-- for the @Target Variable@. The @Evaluation@ compares the predicted
-- result for each observation to the actual outcome and provides a summary
-- so that you know how effective the @MLModel@ functions on the test data.
-- Evaluation generates a relevant performance metric, such as BinaryAUC,
-- RegressionRMSE or MulticlassAvgFScore based on the corresponding
-- @MLModelType@: @BINARY@, @REGRESSION@ or @MULTICLASS@.
--
-- @CreateEvaluation@ is an asynchronous operation. In response to
-- @CreateEvaluation@, Amazon Machine Learning (Amazon ML) immediately
-- returns and sets the evaluation status to @PENDING@. After the
-- @Evaluation@ is created and ready for use, Amazon ML sets the status to
-- @COMPLETED@.
--
-- You can use the @GetEvaluation@ operation to check progress of the
-- evaluation during the creation operation.
module Network.AWS.MachineLearning.CreateEvaluation
  ( -- * Creating a Request
    CreateEvaluation (..),
    newCreateEvaluation,

    -- * Request Lenses
    createEvaluation_evaluationName,
    createEvaluation_evaluationId,
    createEvaluation_mLModelId,
    createEvaluation_evaluationDataSourceId,

    -- * Destructuring the Response
    CreateEvaluationResponse (..),
    newCreateEvaluationResponse,

    -- * Response Lenses
    createEvaluationResponse_evaluationId,
    createEvaluationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateEvaluation' smart constructor.
data CreateEvaluation = CreateEvaluation'
  { -- | A user-supplied name or description of the @Evaluation@.
    evaluationName :: Prelude.Maybe Prelude.Text,
    -- | A user-supplied ID that uniquely identifies the @Evaluation@.
    evaluationId :: Prelude.Text,
    -- | The ID of the @MLModel@ to evaluate.
    --
    -- The schema used in creating the @MLModel@ must match the schema of the
    -- @DataSource@ used in the @Evaluation@.
    mLModelId :: Prelude.Text,
    -- | The ID of the @DataSource@ for the evaluation. The schema of the
    -- @DataSource@ must match the schema used to create the @MLModel@.
    evaluationDataSourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateEvaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationName', 'createEvaluation_evaluationName' - A user-supplied name or description of the @Evaluation@.
--
-- 'evaluationId', 'createEvaluation_evaluationId' - A user-supplied ID that uniquely identifies the @Evaluation@.
--
-- 'mLModelId', 'createEvaluation_mLModelId' - The ID of the @MLModel@ to evaluate.
--
-- The schema used in creating the @MLModel@ must match the schema of the
-- @DataSource@ used in the @Evaluation@.
--
-- 'evaluationDataSourceId', 'createEvaluation_evaluationDataSourceId' - The ID of the @DataSource@ for the evaluation. The schema of the
-- @DataSource@ must match the schema used to create the @MLModel@.
newCreateEvaluation ::
  -- | 'evaluationId'
  Prelude.Text ->
  -- | 'mLModelId'
  Prelude.Text ->
  -- | 'evaluationDataSourceId'
  Prelude.Text ->
  CreateEvaluation
newCreateEvaluation
  pEvaluationId_
  pMLModelId_
  pEvaluationDataSourceId_ =
    CreateEvaluation'
      { evaluationName = Prelude.Nothing,
        evaluationId = pEvaluationId_,
        mLModelId = pMLModelId_,
        evaluationDataSourceId = pEvaluationDataSourceId_
      }

-- | A user-supplied name or description of the @Evaluation@.
createEvaluation_evaluationName :: Lens.Lens' CreateEvaluation (Prelude.Maybe Prelude.Text)
createEvaluation_evaluationName = Lens.lens (\CreateEvaluation' {evaluationName} -> evaluationName) (\s@CreateEvaluation' {} a -> s {evaluationName = a} :: CreateEvaluation)

-- | A user-supplied ID that uniquely identifies the @Evaluation@.
createEvaluation_evaluationId :: Lens.Lens' CreateEvaluation Prelude.Text
createEvaluation_evaluationId = Lens.lens (\CreateEvaluation' {evaluationId} -> evaluationId) (\s@CreateEvaluation' {} a -> s {evaluationId = a} :: CreateEvaluation)

-- | The ID of the @MLModel@ to evaluate.
--
-- The schema used in creating the @MLModel@ must match the schema of the
-- @DataSource@ used in the @Evaluation@.
createEvaluation_mLModelId :: Lens.Lens' CreateEvaluation Prelude.Text
createEvaluation_mLModelId = Lens.lens (\CreateEvaluation' {mLModelId} -> mLModelId) (\s@CreateEvaluation' {} a -> s {mLModelId = a} :: CreateEvaluation)

-- | The ID of the @DataSource@ for the evaluation. The schema of the
-- @DataSource@ must match the schema used to create the @MLModel@.
createEvaluation_evaluationDataSourceId :: Lens.Lens' CreateEvaluation Prelude.Text
createEvaluation_evaluationDataSourceId = Lens.lens (\CreateEvaluation' {evaluationDataSourceId} -> evaluationDataSourceId) (\s@CreateEvaluation' {} a -> s {evaluationDataSourceId = a} :: CreateEvaluation)

instance Prelude.AWSRequest CreateEvaluation where
  type Rs CreateEvaluation = CreateEvaluationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEvaluationResponse'
            Prelude.<$> (x Prelude..?> "EvaluationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEvaluation

instance Prelude.NFData CreateEvaluation

instance Prelude.ToHeaders CreateEvaluation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonML_20141212.CreateEvaluation" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateEvaluation where
  toJSON CreateEvaluation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EvaluationName" Prelude..=)
              Prelude.<$> evaluationName,
            Prelude.Just
              ("EvaluationId" Prelude..= evaluationId),
            Prelude.Just ("MLModelId" Prelude..= mLModelId),
            Prelude.Just
              ( "EvaluationDataSourceId"
                  Prelude..= evaluationDataSourceId
              )
          ]
      )

instance Prelude.ToPath CreateEvaluation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateEvaluation where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @CreateEvaluation@ operation, and is an
-- acknowledgement that Amazon ML received the request.
--
-- @CreateEvaluation@ operation is asynchronous. You can poll for status
-- updates by using the @GetEvcaluation@ operation and checking the
-- @Status@ parameter.
--
-- /See:/ 'newCreateEvaluationResponse' smart constructor.
data CreateEvaluationResponse = CreateEvaluationResponse'
  { -- | The user-supplied ID that uniquely identifies the @Evaluation@. This
    -- value should be identical to the value of the @EvaluationId@ in the
    -- request.
    evaluationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateEvaluationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationId', 'createEvaluationResponse_evaluationId' - The user-supplied ID that uniquely identifies the @Evaluation@. This
-- value should be identical to the value of the @EvaluationId@ in the
-- request.
--
-- 'httpStatus', 'createEvaluationResponse_httpStatus' - The response's http status code.
newCreateEvaluationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEvaluationResponse
newCreateEvaluationResponse pHttpStatus_ =
  CreateEvaluationResponse'
    { evaluationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user-supplied ID that uniquely identifies the @Evaluation@. This
-- value should be identical to the value of the @EvaluationId@ in the
-- request.
createEvaluationResponse_evaluationId :: Lens.Lens' CreateEvaluationResponse (Prelude.Maybe Prelude.Text)
createEvaluationResponse_evaluationId = Lens.lens (\CreateEvaluationResponse' {evaluationId} -> evaluationId) (\s@CreateEvaluationResponse' {} a -> s {evaluationId = a} :: CreateEvaluationResponse)

-- | The response's http status code.
createEvaluationResponse_httpStatus :: Lens.Lens' CreateEvaluationResponse Prelude.Int
createEvaluationResponse_httpStatus = Lens.lens (\CreateEvaluationResponse' {httpStatus} -> httpStatus) (\s@CreateEvaluationResponse' {} a -> s {httpStatus = a} :: CreateEvaluationResponse)

instance Prelude.NFData CreateEvaluationResponse
