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
-- Module      : Network.AWS.MachineLearning.GetEvaluation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an @Evaluation@ that includes metadata as well as the current
-- status of the @Evaluation@.
module Network.AWS.MachineLearning.GetEvaluation
  ( -- * Creating a Request
    GetEvaluation (..),
    newGetEvaluation,

    -- * Request Lenses
    getEvaluation_evaluationId,

    -- * Destructuring the Response
    GetEvaluationResponse (..),
    newGetEvaluationResponse,

    -- * Response Lenses
    getEvaluationResponse_performanceMetrics,
    getEvaluationResponse_status,
    getEvaluationResponse_startedAt,
    getEvaluationResponse_evaluationDataSourceId,
    getEvaluationResponse_message,
    getEvaluationResponse_createdAt,
    getEvaluationResponse_finishedAt,
    getEvaluationResponse_createdByIamUser,
    getEvaluationResponse_name,
    getEvaluationResponse_evaluationId,
    getEvaluationResponse_mLModelId,
    getEvaluationResponse_inputDataLocationS3,
    getEvaluationResponse_computeTime,
    getEvaluationResponse_lastUpdatedAt,
    getEvaluationResponse_logUri,
    getEvaluationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetEvaluation' smart constructor.
data GetEvaluation = GetEvaluation'
  { -- | The ID of the @Evaluation@ to retrieve. The evaluation of each @MLModel@
    -- is recorded and cataloged. The ID provides the means to access the
    -- information.
    evaluationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetEvaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationId', 'getEvaluation_evaluationId' - The ID of the @Evaluation@ to retrieve. The evaluation of each @MLModel@
-- is recorded and cataloged. The ID provides the means to access the
-- information.
newGetEvaluation ::
  -- | 'evaluationId'
  Prelude.Text ->
  GetEvaluation
newGetEvaluation pEvaluationId_ =
  GetEvaluation' {evaluationId = pEvaluationId_}

-- | The ID of the @Evaluation@ to retrieve. The evaluation of each @MLModel@
-- is recorded and cataloged. The ID provides the means to access the
-- information.
getEvaluation_evaluationId :: Lens.Lens' GetEvaluation Prelude.Text
getEvaluation_evaluationId = Lens.lens (\GetEvaluation' {evaluationId} -> evaluationId) (\s@GetEvaluation' {} a -> s {evaluationId = a} :: GetEvaluation)

instance Prelude.AWSRequest GetEvaluation where
  type Rs GetEvaluation = GetEvaluationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEvaluationResponse'
            Prelude.<$> (x Prelude..?> "PerformanceMetrics")
            Prelude.<*> (x Prelude..?> "Status")
            Prelude.<*> (x Prelude..?> "StartedAt")
            Prelude.<*> (x Prelude..?> "EvaluationDataSourceId")
            Prelude.<*> (x Prelude..?> "Message")
            Prelude.<*> (x Prelude..?> "CreatedAt")
            Prelude.<*> (x Prelude..?> "FinishedAt")
            Prelude.<*> (x Prelude..?> "CreatedByIamUser")
            Prelude.<*> (x Prelude..?> "Name")
            Prelude.<*> (x Prelude..?> "EvaluationId")
            Prelude.<*> (x Prelude..?> "MLModelId")
            Prelude.<*> (x Prelude..?> "InputDataLocationS3")
            Prelude.<*> (x Prelude..?> "ComputeTime")
            Prelude.<*> (x Prelude..?> "LastUpdatedAt")
            Prelude.<*> (x Prelude..?> "LogUri")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEvaluation

instance Prelude.NFData GetEvaluation

instance Prelude.ToHeaders GetEvaluation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonML_20141212.GetEvaluation" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetEvaluation where
  toJSON GetEvaluation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EvaluationId" Prelude..= evaluationId)
          ]
      )

instance Prelude.ToPath GetEvaluation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetEvaluation where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetEvaluation@ operation and describes an
-- @Evaluation@.
--
-- /See:/ 'newGetEvaluationResponse' smart constructor.
data GetEvaluationResponse = GetEvaluationResponse'
  { -- | Measurements of how well the @MLModel@ performed using observations
    -- referenced by the @DataSource@. One of the following metric is returned
    -- based on the type of the @MLModel@:
    --
    -- -   BinaryAUC: A binary @MLModel@ uses the Area Under the Curve (AUC)
    --     technique to measure performance.
    --
    -- -   RegressionRMSE: A regression @MLModel@ uses the Root Mean Square
    --     Error (RMSE) technique to measure performance. RMSE measures the
    --     difference between predicted and actual values for a single
    --     variable.
    --
    -- -   MulticlassAvgFScore: A multiclass @MLModel@ uses the F1 score
    --     technique to measure performance.
    --
    -- For more information about performance metrics, please see the
    -- <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
    performanceMetrics :: Prelude.Maybe PerformanceMetrics,
    -- | The status of the evaluation. This element can have one of the following
    -- values:
    --
    -- -   @PENDING@ - Amazon Machine Language (Amazon ML) submitted a request
    --     to evaluate an @MLModel@.
    -- -   @INPROGRESS@ - The evaluation is underway.
    -- -   @FAILED@ - The request to evaluate an @MLModel@ did not run to
    --     completion. It is not usable.
    -- -   @COMPLETED@ - The evaluation process completed successfully.
    -- -   @DELETED@ - The @Evaluation@ is marked as deleted. It is not usable.
    status :: Prelude.Maybe EntityStatus,
    -- | The epoch time when Amazon Machine Learning marked the @Evaluation@ as
    -- @INPROGRESS@. @StartedAt@ isn\'t available if the @Evaluation@ is in the
    -- @PENDING@ state.
    startedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The @DataSource@ used for this evaluation.
    evaluationDataSourceId :: Prelude.Maybe Prelude.Text,
    -- | A description of the most recent details about evaluating the @MLModel@.
    message :: Prelude.Maybe Prelude.Text,
    -- | The time that the @Evaluation@ was created. The time is expressed in
    -- epoch time.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The epoch time when Amazon Machine Learning marked the @Evaluation@ as
    -- @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
    -- @Evaluation@ is in the @COMPLETED@ or @FAILED@ state.
    finishedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The AWS user account that invoked the evaluation. The account type can
    -- be either an AWS root account or an AWS Identity and Access Management
    -- (IAM) user account.
    createdByIamUser :: Prelude.Maybe Prelude.Text,
    -- | A user-supplied name or description of the @Evaluation@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The evaluation ID which is same as the @EvaluationId@ in the request.
    evaluationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @MLModel@ that was the focus of the evaluation.
    mLModelId :: Prelude.Maybe Prelude.Text,
    -- | The location of the data file or directory in Amazon Simple Storage
    -- Service (Amazon S3).
    inputDataLocationS3 :: Prelude.Maybe Prelude.Text,
    -- | The approximate CPU time in milliseconds that Amazon Machine Learning
    -- spent processing the @Evaluation@, normalized and scaled on computation
    -- resources. @ComputeTime@ is only available if the @Evaluation@ is in the
    -- @COMPLETED@ state.
    computeTime :: Prelude.Maybe Prelude.Integer,
    -- | The time of the most recent edit to the @Evaluation@. The time is
    -- expressed in epoch time.
    lastUpdatedAt :: Prelude.Maybe Prelude.POSIX,
    -- | A link to the file that contains logs of the @CreateEvaluation@
    -- operation.
    logUri :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetEvaluationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'performanceMetrics', 'getEvaluationResponse_performanceMetrics' - Measurements of how well the @MLModel@ performed using observations
-- referenced by the @DataSource@. One of the following metric is returned
-- based on the type of the @MLModel@:
--
-- -   BinaryAUC: A binary @MLModel@ uses the Area Under the Curve (AUC)
--     technique to measure performance.
--
-- -   RegressionRMSE: A regression @MLModel@ uses the Root Mean Square
--     Error (RMSE) technique to measure performance. RMSE measures the
--     difference between predicted and actual values for a single
--     variable.
--
-- -   MulticlassAvgFScore: A multiclass @MLModel@ uses the F1 score
--     technique to measure performance.
--
-- For more information about performance metrics, please see the
-- <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
--
-- 'status', 'getEvaluationResponse_status' - The status of the evaluation. This element can have one of the following
-- values:
--
-- -   @PENDING@ - Amazon Machine Language (Amazon ML) submitted a request
--     to evaluate an @MLModel@.
-- -   @INPROGRESS@ - The evaluation is underway.
-- -   @FAILED@ - The request to evaluate an @MLModel@ did not run to
--     completion. It is not usable.
-- -   @COMPLETED@ - The evaluation process completed successfully.
-- -   @DELETED@ - The @Evaluation@ is marked as deleted. It is not usable.
--
-- 'startedAt', 'getEvaluationResponse_startedAt' - The epoch time when Amazon Machine Learning marked the @Evaluation@ as
-- @INPROGRESS@. @StartedAt@ isn\'t available if the @Evaluation@ is in the
-- @PENDING@ state.
--
-- 'evaluationDataSourceId', 'getEvaluationResponse_evaluationDataSourceId' - The @DataSource@ used for this evaluation.
--
-- 'message', 'getEvaluationResponse_message' - A description of the most recent details about evaluating the @MLModel@.
--
-- 'createdAt', 'getEvaluationResponse_createdAt' - The time that the @Evaluation@ was created. The time is expressed in
-- epoch time.
--
-- 'finishedAt', 'getEvaluationResponse_finishedAt' - The epoch time when Amazon Machine Learning marked the @Evaluation@ as
-- @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
-- @Evaluation@ is in the @COMPLETED@ or @FAILED@ state.
--
-- 'createdByIamUser', 'getEvaluationResponse_createdByIamUser' - The AWS user account that invoked the evaluation. The account type can
-- be either an AWS root account or an AWS Identity and Access Management
-- (IAM) user account.
--
-- 'name', 'getEvaluationResponse_name' - A user-supplied name or description of the @Evaluation@.
--
-- 'evaluationId', 'getEvaluationResponse_evaluationId' - The evaluation ID which is same as the @EvaluationId@ in the request.
--
-- 'mLModelId', 'getEvaluationResponse_mLModelId' - The ID of the @MLModel@ that was the focus of the evaluation.
--
-- 'inputDataLocationS3', 'getEvaluationResponse_inputDataLocationS3' - The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
--
-- 'computeTime', 'getEvaluationResponse_computeTime' - The approximate CPU time in milliseconds that Amazon Machine Learning
-- spent processing the @Evaluation@, normalized and scaled on computation
-- resources. @ComputeTime@ is only available if the @Evaluation@ is in the
-- @COMPLETED@ state.
--
-- 'lastUpdatedAt', 'getEvaluationResponse_lastUpdatedAt' - The time of the most recent edit to the @Evaluation@. The time is
-- expressed in epoch time.
--
-- 'logUri', 'getEvaluationResponse_logUri' - A link to the file that contains logs of the @CreateEvaluation@
-- operation.
--
-- 'httpStatus', 'getEvaluationResponse_httpStatus' - The response's http status code.
newGetEvaluationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEvaluationResponse
newGetEvaluationResponse pHttpStatus_ =
  GetEvaluationResponse'
    { performanceMetrics =
        Prelude.Nothing,
      status = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      evaluationDataSourceId = Prelude.Nothing,
      message = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      finishedAt = Prelude.Nothing,
      createdByIamUser = Prelude.Nothing,
      name = Prelude.Nothing,
      evaluationId = Prelude.Nothing,
      mLModelId = Prelude.Nothing,
      inputDataLocationS3 = Prelude.Nothing,
      computeTime = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      logUri = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Measurements of how well the @MLModel@ performed using observations
-- referenced by the @DataSource@. One of the following metric is returned
-- based on the type of the @MLModel@:
--
-- -   BinaryAUC: A binary @MLModel@ uses the Area Under the Curve (AUC)
--     technique to measure performance.
--
-- -   RegressionRMSE: A regression @MLModel@ uses the Root Mean Square
--     Error (RMSE) technique to measure performance. RMSE measures the
--     difference between predicted and actual values for a single
--     variable.
--
-- -   MulticlassAvgFScore: A multiclass @MLModel@ uses the F1 score
--     technique to measure performance.
--
-- For more information about performance metrics, please see the
-- <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
getEvaluationResponse_performanceMetrics :: Lens.Lens' GetEvaluationResponse (Prelude.Maybe PerformanceMetrics)
getEvaluationResponse_performanceMetrics = Lens.lens (\GetEvaluationResponse' {performanceMetrics} -> performanceMetrics) (\s@GetEvaluationResponse' {} a -> s {performanceMetrics = a} :: GetEvaluationResponse)

-- | The status of the evaluation. This element can have one of the following
-- values:
--
-- -   @PENDING@ - Amazon Machine Language (Amazon ML) submitted a request
--     to evaluate an @MLModel@.
-- -   @INPROGRESS@ - The evaluation is underway.
-- -   @FAILED@ - The request to evaluate an @MLModel@ did not run to
--     completion. It is not usable.
-- -   @COMPLETED@ - The evaluation process completed successfully.
-- -   @DELETED@ - The @Evaluation@ is marked as deleted. It is not usable.
getEvaluationResponse_status :: Lens.Lens' GetEvaluationResponse (Prelude.Maybe EntityStatus)
getEvaluationResponse_status = Lens.lens (\GetEvaluationResponse' {status} -> status) (\s@GetEvaluationResponse' {} a -> s {status = a} :: GetEvaluationResponse)

-- | The epoch time when Amazon Machine Learning marked the @Evaluation@ as
-- @INPROGRESS@. @StartedAt@ isn\'t available if the @Evaluation@ is in the
-- @PENDING@ state.
getEvaluationResponse_startedAt :: Lens.Lens' GetEvaluationResponse (Prelude.Maybe Prelude.UTCTime)
getEvaluationResponse_startedAt = Lens.lens (\GetEvaluationResponse' {startedAt} -> startedAt) (\s@GetEvaluationResponse' {} a -> s {startedAt = a} :: GetEvaluationResponse) Prelude.. Lens.mapping Prelude._Time

-- | The @DataSource@ used for this evaluation.
getEvaluationResponse_evaluationDataSourceId :: Lens.Lens' GetEvaluationResponse (Prelude.Maybe Prelude.Text)
getEvaluationResponse_evaluationDataSourceId = Lens.lens (\GetEvaluationResponse' {evaluationDataSourceId} -> evaluationDataSourceId) (\s@GetEvaluationResponse' {} a -> s {evaluationDataSourceId = a} :: GetEvaluationResponse)

-- | A description of the most recent details about evaluating the @MLModel@.
getEvaluationResponse_message :: Lens.Lens' GetEvaluationResponse (Prelude.Maybe Prelude.Text)
getEvaluationResponse_message = Lens.lens (\GetEvaluationResponse' {message} -> message) (\s@GetEvaluationResponse' {} a -> s {message = a} :: GetEvaluationResponse)

-- | The time that the @Evaluation@ was created. The time is expressed in
-- epoch time.
getEvaluationResponse_createdAt :: Lens.Lens' GetEvaluationResponse (Prelude.Maybe Prelude.UTCTime)
getEvaluationResponse_createdAt = Lens.lens (\GetEvaluationResponse' {createdAt} -> createdAt) (\s@GetEvaluationResponse' {} a -> s {createdAt = a} :: GetEvaluationResponse) Prelude.. Lens.mapping Prelude._Time

-- | The epoch time when Amazon Machine Learning marked the @Evaluation@ as
-- @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
-- @Evaluation@ is in the @COMPLETED@ or @FAILED@ state.
getEvaluationResponse_finishedAt :: Lens.Lens' GetEvaluationResponse (Prelude.Maybe Prelude.UTCTime)
getEvaluationResponse_finishedAt = Lens.lens (\GetEvaluationResponse' {finishedAt} -> finishedAt) (\s@GetEvaluationResponse' {} a -> s {finishedAt = a} :: GetEvaluationResponse) Prelude.. Lens.mapping Prelude._Time

-- | The AWS user account that invoked the evaluation. The account type can
-- be either an AWS root account or an AWS Identity and Access Management
-- (IAM) user account.
getEvaluationResponse_createdByIamUser :: Lens.Lens' GetEvaluationResponse (Prelude.Maybe Prelude.Text)
getEvaluationResponse_createdByIamUser = Lens.lens (\GetEvaluationResponse' {createdByIamUser} -> createdByIamUser) (\s@GetEvaluationResponse' {} a -> s {createdByIamUser = a} :: GetEvaluationResponse)

-- | A user-supplied name or description of the @Evaluation@.
getEvaluationResponse_name :: Lens.Lens' GetEvaluationResponse (Prelude.Maybe Prelude.Text)
getEvaluationResponse_name = Lens.lens (\GetEvaluationResponse' {name} -> name) (\s@GetEvaluationResponse' {} a -> s {name = a} :: GetEvaluationResponse)

-- | The evaluation ID which is same as the @EvaluationId@ in the request.
getEvaluationResponse_evaluationId :: Lens.Lens' GetEvaluationResponse (Prelude.Maybe Prelude.Text)
getEvaluationResponse_evaluationId = Lens.lens (\GetEvaluationResponse' {evaluationId} -> evaluationId) (\s@GetEvaluationResponse' {} a -> s {evaluationId = a} :: GetEvaluationResponse)

-- | The ID of the @MLModel@ that was the focus of the evaluation.
getEvaluationResponse_mLModelId :: Lens.Lens' GetEvaluationResponse (Prelude.Maybe Prelude.Text)
getEvaluationResponse_mLModelId = Lens.lens (\GetEvaluationResponse' {mLModelId} -> mLModelId) (\s@GetEvaluationResponse' {} a -> s {mLModelId = a} :: GetEvaluationResponse)

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
getEvaluationResponse_inputDataLocationS3 :: Lens.Lens' GetEvaluationResponse (Prelude.Maybe Prelude.Text)
getEvaluationResponse_inputDataLocationS3 = Lens.lens (\GetEvaluationResponse' {inputDataLocationS3} -> inputDataLocationS3) (\s@GetEvaluationResponse' {} a -> s {inputDataLocationS3 = a} :: GetEvaluationResponse)

-- | The approximate CPU time in milliseconds that Amazon Machine Learning
-- spent processing the @Evaluation@, normalized and scaled on computation
-- resources. @ComputeTime@ is only available if the @Evaluation@ is in the
-- @COMPLETED@ state.
getEvaluationResponse_computeTime :: Lens.Lens' GetEvaluationResponse (Prelude.Maybe Prelude.Integer)
getEvaluationResponse_computeTime = Lens.lens (\GetEvaluationResponse' {computeTime} -> computeTime) (\s@GetEvaluationResponse' {} a -> s {computeTime = a} :: GetEvaluationResponse)

-- | The time of the most recent edit to the @Evaluation@. The time is
-- expressed in epoch time.
getEvaluationResponse_lastUpdatedAt :: Lens.Lens' GetEvaluationResponse (Prelude.Maybe Prelude.UTCTime)
getEvaluationResponse_lastUpdatedAt = Lens.lens (\GetEvaluationResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetEvaluationResponse' {} a -> s {lastUpdatedAt = a} :: GetEvaluationResponse) Prelude.. Lens.mapping Prelude._Time

-- | A link to the file that contains logs of the @CreateEvaluation@
-- operation.
getEvaluationResponse_logUri :: Lens.Lens' GetEvaluationResponse (Prelude.Maybe Prelude.Text)
getEvaluationResponse_logUri = Lens.lens (\GetEvaluationResponse' {logUri} -> logUri) (\s@GetEvaluationResponse' {} a -> s {logUri = a} :: GetEvaluationResponse)

-- | The response's http status code.
getEvaluationResponse_httpStatus :: Lens.Lens' GetEvaluationResponse Prelude.Int
getEvaluationResponse_httpStatus = Lens.lens (\GetEvaluationResponse' {httpStatus} -> httpStatus) (\s@GetEvaluationResponse' {} a -> s {httpStatus = a} :: GetEvaluationResponse)

instance Prelude.NFData GetEvaluationResponse
