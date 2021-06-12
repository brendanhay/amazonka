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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetEvaluation' smart constructor.
data GetEvaluation = GetEvaluation'
  { -- | The ID of the @Evaluation@ to retrieve. The evaluation of each @MLModel@
    -- is recorded and cataloged. The ID provides the means to access the
    -- information.
    evaluationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetEvaluation
newGetEvaluation pEvaluationId_ =
  GetEvaluation' {evaluationId = pEvaluationId_}

-- | The ID of the @Evaluation@ to retrieve. The evaluation of each @MLModel@
-- is recorded and cataloged. The ID provides the means to access the
-- information.
getEvaluation_evaluationId :: Lens.Lens' GetEvaluation Core.Text
getEvaluation_evaluationId = Lens.lens (\GetEvaluation' {evaluationId} -> evaluationId) (\s@GetEvaluation' {} a -> s {evaluationId = a} :: GetEvaluation)

instance Core.AWSRequest GetEvaluation where
  type
    AWSResponse GetEvaluation =
      GetEvaluationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEvaluationResponse'
            Core.<$> (x Core..?> "PerformanceMetrics")
            Core.<*> (x Core..?> "Status")
            Core.<*> (x Core..?> "StartedAt")
            Core.<*> (x Core..?> "EvaluationDataSourceId")
            Core.<*> (x Core..?> "Message")
            Core.<*> (x Core..?> "CreatedAt")
            Core.<*> (x Core..?> "FinishedAt")
            Core.<*> (x Core..?> "CreatedByIamUser")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "EvaluationId")
            Core.<*> (x Core..?> "MLModelId")
            Core.<*> (x Core..?> "InputDataLocationS3")
            Core.<*> (x Core..?> "ComputeTime")
            Core.<*> (x Core..?> "LastUpdatedAt")
            Core.<*> (x Core..?> "LogUri")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetEvaluation

instance Core.NFData GetEvaluation

instance Core.ToHeaders GetEvaluation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonML_20141212.GetEvaluation" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetEvaluation where
  toJSON GetEvaluation' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("EvaluationId" Core..= evaluationId)]
      )

instance Core.ToPath GetEvaluation where
  toPath = Core.const "/"

instance Core.ToQuery GetEvaluation where
  toQuery = Core.const Core.mempty

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
    performanceMetrics :: Core.Maybe PerformanceMetrics,
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
    status :: Core.Maybe EntityStatus,
    -- | The epoch time when Amazon Machine Learning marked the @Evaluation@ as
    -- @INPROGRESS@. @StartedAt@ isn\'t available if the @Evaluation@ is in the
    -- @PENDING@ state.
    startedAt :: Core.Maybe Core.POSIX,
    -- | The @DataSource@ used for this evaluation.
    evaluationDataSourceId :: Core.Maybe Core.Text,
    -- | A description of the most recent details about evaluating the @MLModel@.
    message :: Core.Maybe Core.Text,
    -- | The time that the @Evaluation@ was created. The time is expressed in
    -- epoch time.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The epoch time when Amazon Machine Learning marked the @Evaluation@ as
    -- @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
    -- @Evaluation@ is in the @COMPLETED@ or @FAILED@ state.
    finishedAt :: Core.Maybe Core.POSIX,
    -- | The AWS user account that invoked the evaluation. The account type can
    -- be either an AWS root account or an AWS Identity and Access Management
    -- (IAM) user account.
    createdByIamUser :: Core.Maybe Core.Text,
    -- | A user-supplied name or description of the @Evaluation@.
    name :: Core.Maybe Core.Text,
    -- | The evaluation ID which is same as the @EvaluationId@ in the request.
    evaluationId :: Core.Maybe Core.Text,
    -- | The ID of the @MLModel@ that was the focus of the evaluation.
    mLModelId :: Core.Maybe Core.Text,
    -- | The location of the data file or directory in Amazon Simple Storage
    -- Service (Amazon S3).
    inputDataLocationS3 :: Core.Maybe Core.Text,
    -- | The approximate CPU time in milliseconds that Amazon Machine Learning
    -- spent processing the @Evaluation@, normalized and scaled on computation
    -- resources. @ComputeTime@ is only available if the @Evaluation@ is in the
    -- @COMPLETED@ state.
    computeTime :: Core.Maybe Core.Integer,
    -- | The time of the most recent edit to the @Evaluation@. The time is
    -- expressed in epoch time.
    lastUpdatedAt :: Core.Maybe Core.POSIX,
    -- | A link to the file that contains logs of the @CreateEvaluation@
    -- operation.
    logUri :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetEvaluationResponse
newGetEvaluationResponse pHttpStatus_ =
  GetEvaluationResponse'
    { performanceMetrics =
        Core.Nothing,
      status = Core.Nothing,
      startedAt = Core.Nothing,
      evaluationDataSourceId = Core.Nothing,
      message = Core.Nothing,
      createdAt = Core.Nothing,
      finishedAt = Core.Nothing,
      createdByIamUser = Core.Nothing,
      name = Core.Nothing,
      evaluationId = Core.Nothing,
      mLModelId = Core.Nothing,
      inputDataLocationS3 = Core.Nothing,
      computeTime = Core.Nothing,
      lastUpdatedAt = Core.Nothing,
      logUri = Core.Nothing,
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
getEvaluationResponse_performanceMetrics :: Lens.Lens' GetEvaluationResponse (Core.Maybe PerformanceMetrics)
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
getEvaluationResponse_status :: Lens.Lens' GetEvaluationResponse (Core.Maybe EntityStatus)
getEvaluationResponse_status = Lens.lens (\GetEvaluationResponse' {status} -> status) (\s@GetEvaluationResponse' {} a -> s {status = a} :: GetEvaluationResponse)

-- | The epoch time when Amazon Machine Learning marked the @Evaluation@ as
-- @INPROGRESS@. @StartedAt@ isn\'t available if the @Evaluation@ is in the
-- @PENDING@ state.
getEvaluationResponse_startedAt :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.UTCTime)
getEvaluationResponse_startedAt = Lens.lens (\GetEvaluationResponse' {startedAt} -> startedAt) (\s@GetEvaluationResponse' {} a -> s {startedAt = a} :: GetEvaluationResponse) Core.. Lens.mapping Core._Time

-- | The @DataSource@ used for this evaluation.
getEvaluationResponse_evaluationDataSourceId :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.Text)
getEvaluationResponse_evaluationDataSourceId = Lens.lens (\GetEvaluationResponse' {evaluationDataSourceId} -> evaluationDataSourceId) (\s@GetEvaluationResponse' {} a -> s {evaluationDataSourceId = a} :: GetEvaluationResponse)

-- | A description of the most recent details about evaluating the @MLModel@.
getEvaluationResponse_message :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.Text)
getEvaluationResponse_message = Lens.lens (\GetEvaluationResponse' {message} -> message) (\s@GetEvaluationResponse' {} a -> s {message = a} :: GetEvaluationResponse)

-- | The time that the @Evaluation@ was created. The time is expressed in
-- epoch time.
getEvaluationResponse_createdAt :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.UTCTime)
getEvaluationResponse_createdAt = Lens.lens (\GetEvaluationResponse' {createdAt} -> createdAt) (\s@GetEvaluationResponse' {} a -> s {createdAt = a} :: GetEvaluationResponse) Core.. Lens.mapping Core._Time

-- | The epoch time when Amazon Machine Learning marked the @Evaluation@ as
-- @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
-- @Evaluation@ is in the @COMPLETED@ or @FAILED@ state.
getEvaluationResponse_finishedAt :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.UTCTime)
getEvaluationResponse_finishedAt = Lens.lens (\GetEvaluationResponse' {finishedAt} -> finishedAt) (\s@GetEvaluationResponse' {} a -> s {finishedAt = a} :: GetEvaluationResponse) Core.. Lens.mapping Core._Time

-- | The AWS user account that invoked the evaluation. The account type can
-- be either an AWS root account or an AWS Identity and Access Management
-- (IAM) user account.
getEvaluationResponse_createdByIamUser :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.Text)
getEvaluationResponse_createdByIamUser = Lens.lens (\GetEvaluationResponse' {createdByIamUser} -> createdByIamUser) (\s@GetEvaluationResponse' {} a -> s {createdByIamUser = a} :: GetEvaluationResponse)

-- | A user-supplied name or description of the @Evaluation@.
getEvaluationResponse_name :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.Text)
getEvaluationResponse_name = Lens.lens (\GetEvaluationResponse' {name} -> name) (\s@GetEvaluationResponse' {} a -> s {name = a} :: GetEvaluationResponse)

-- | The evaluation ID which is same as the @EvaluationId@ in the request.
getEvaluationResponse_evaluationId :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.Text)
getEvaluationResponse_evaluationId = Lens.lens (\GetEvaluationResponse' {evaluationId} -> evaluationId) (\s@GetEvaluationResponse' {} a -> s {evaluationId = a} :: GetEvaluationResponse)

-- | The ID of the @MLModel@ that was the focus of the evaluation.
getEvaluationResponse_mLModelId :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.Text)
getEvaluationResponse_mLModelId = Lens.lens (\GetEvaluationResponse' {mLModelId} -> mLModelId) (\s@GetEvaluationResponse' {} a -> s {mLModelId = a} :: GetEvaluationResponse)

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
getEvaluationResponse_inputDataLocationS3 :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.Text)
getEvaluationResponse_inputDataLocationS3 = Lens.lens (\GetEvaluationResponse' {inputDataLocationS3} -> inputDataLocationS3) (\s@GetEvaluationResponse' {} a -> s {inputDataLocationS3 = a} :: GetEvaluationResponse)

-- | The approximate CPU time in milliseconds that Amazon Machine Learning
-- spent processing the @Evaluation@, normalized and scaled on computation
-- resources. @ComputeTime@ is only available if the @Evaluation@ is in the
-- @COMPLETED@ state.
getEvaluationResponse_computeTime :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.Integer)
getEvaluationResponse_computeTime = Lens.lens (\GetEvaluationResponse' {computeTime} -> computeTime) (\s@GetEvaluationResponse' {} a -> s {computeTime = a} :: GetEvaluationResponse)

-- | The time of the most recent edit to the @Evaluation@. The time is
-- expressed in epoch time.
getEvaluationResponse_lastUpdatedAt :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.UTCTime)
getEvaluationResponse_lastUpdatedAt = Lens.lens (\GetEvaluationResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetEvaluationResponse' {} a -> s {lastUpdatedAt = a} :: GetEvaluationResponse) Core.. Lens.mapping Core._Time

-- | A link to the file that contains logs of the @CreateEvaluation@
-- operation.
getEvaluationResponse_logUri :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.Text)
getEvaluationResponse_logUri = Lens.lens (\GetEvaluationResponse' {logUri} -> logUri) (\s@GetEvaluationResponse' {} a -> s {logUri = a} :: GetEvaluationResponse)

-- | The response's http status code.
getEvaluationResponse_httpStatus :: Lens.Lens' GetEvaluationResponse Core.Int
getEvaluationResponse_httpStatus = Lens.lens (\GetEvaluationResponse' {httpStatus} -> httpStatus) (\s@GetEvaluationResponse' {} a -> s {httpStatus = a} :: GetEvaluationResponse)

instance Core.NFData GetEvaluationResponse
