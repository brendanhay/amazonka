{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.GetEvaluation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an @Evaluation@ that includes metadata as well as the current status of the @Evaluation@ .
module Network.AWS.MachineLearning.GetEvaluation
  ( -- * Creating a request
    GetEvaluation (..),
    mkGetEvaluation,

    -- ** Request lenses
    geEvaluationId,

    -- * Destructuring the response
    GetEvaluationResponse (..),
    mkGetEvaluationResponse,

    -- ** Response lenses
    gersStatus,
    gersPerformanceMetrics,
    gersLastUpdatedAt,
    gersCreatedAt,
    gersComputeTime,
    gersInputDataLocationS3,
    gersMLModelId,
    gersStartedAt,
    gersFinishedAt,
    gersCreatedByIAMUser,
    gersName,
    gersLogURI,
    gersEvaluationId,
    gersMessage,
    gersEvaluationDataSourceId,
    gersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetEvaluation' smart constructor.
newtype GetEvaluation = GetEvaluation'
  { -- | The ID of the @Evaluation@ to retrieve. The evaluation of each @MLModel@ is recorded and cataloged. The ID provides the means to access the information.
    evaluationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEvaluation' with the minimum fields required to make a request.
--
-- * 'evaluationId' - The ID of the @Evaluation@ to retrieve. The evaluation of each @MLModel@ is recorded and cataloged. The ID provides the means to access the information.
mkGetEvaluation ::
  -- | 'evaluationId'
  Lude.Text ->
  GetEvaluation
mkGetEvaluation pEvaluationId_ =
  GetEvaluation' {evaluationId = pEvaluationId_}

-- | The ID of the @Evaluation@ to retrieve. The evaluation of each @MLModel@ is recorded and cataloged. The ID provides the means to access the information.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geEvaluationId :: Lens.Lens' GetEvaluation Lude.Text
geEvaluationId = Lens.lens (evaluationId :: GetEvaluation -> Lude.Text) (\s a -> s {evaluationId = a} :: GetEvaluation)
{-# DEPRECATED geEvaluationId "Use generic-lens or generic-optics with 'evaluationId' instead." #-}

instance Lude.AWSRequest GetEvaluation where
  type Rs GetEvaluation = GetEvaluationResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetEvaluationResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "PerformanceMetrics")
            Lude.<*> (x Lude..?> "LastUpdatedAt")
            Lude.<*> (x Lude..?> "CreatedAt")
            Lude.<*> (x Lude..?> "ComputeTime")
            Lude.<*> (x Lude..?> "InputDataLocationS3")
            Lude.<*> (x Lude..?> "MLModelId")
            Lude.<*> (x Lude..?> "StartedAt")
            Lude.<*> (x Lude..?> "FinishedAt")
            Lude.<*> (x Lude..?> "CreatedByIamUser")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "LogUri")
            Lude.<*> (x Lude..?> "EvaluationId")
            Lude.<*> (x Lude..?> "Message")
            Lude.<*> (x Lude..?> "EvaluationDataSourceId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetEvaluation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.GetEvaluation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetEvaluation where
  toJSON GetEvaluation' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("EvaluationId" Lude..= evaluationId)])

instance Lude.ToPath GetEvaluation where
  toPath = Lude.const "/"

instance Lude.ToQuery GetEvaluation where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetEvaluation@ operation and describes an @Evaluation@ .
--
-- /See:/ 'mkGetEvaluationResponse' smart constructor.
data GetEvaluationResponse = GetEvaluationResponse'
  { -- | The status of the evaluation. This element can have one of the following values:
    --
    --
    --     * @PENDING@ - Amazon Machine Language (Amazon ML) submitted a request to evaluate an @MLModel@ .
    --
    --     * @INPROGRESS@ - The evaluation is underway.
    --
    --     * @FAILED@ - The request to evaluate an @MLModel@ did not run to completion. It is not usable.
    --
    --     * @COMPLETED@ - The evaluation process completed successfully.
    --
    --     * @DELETED@ - The @Evaluation@ is marked as deleted. It is not usable.
    status :: Lude.Maybe EntityStatus,
    -- | Measurements of how well the @MLModel@ performed using observations referenced by the @DataSource@ . One of the following metric is returned based on the type of the @MLModel@ :
    --
    --
    --     * BinaryAUC: A binary @MLModel@ uses the Area Under the Curve (AUC) technique to measure performance.
    --
    --
    --     * RegressionRMSE: A regression @MLModel@ uses the Root Mean Square Error (RMSE) technique to measure performance. RMSE measures the difference between predicted and actual values for a single variable.
    --
    --
    --     * MulticlassAvgFScore: A multiclass @MLModel@ uses the F1 score technique to measure performance.
    --
    --
    -- For more information about performance metrics, please see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide> .
    performanceMetrics :: Lude.Maybe PerformanceMetrics,
    -- | The time of the most recent edit to the @Evaluation@ . The time is expressed in epoch time.
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    -- | The time that the @Evaluation@ was created. The time is expressed in epoch time.
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @Evaluation@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @Evaluation@ is in the @COMPLETED@ state.
    computeTime :: Lude.Maybe Lude.Integer,
    -- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
    inputDataLocationS3 :: Lude.Maybe Lude.Text,
    -- | The ID of the @MLModel@ that was the focus of the evaluation.
    mLModelId :: Lude.Maybe Lude.Text,
    -- | The epoch time when Amazon Machine Learning marked the @Evaluation@ as @INPROGRESS@ . @StartedAt@ isn't available if the @Evaluation@ is in the @PENDING@ state.
    startedAt :: Lude.Maybe Lude.Timestamp,
    -- | The epoch time when Amazon Machine Learning marked the @Evaluation@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @Evaluation@ is in the @COMPLETED@ or @FAILED@ state.
    finishedAt :: Lude.Maybe Lude.Timestamp,
    -- | The AWS user account that invoked the evaluation. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
    createdByIAMUser :: Lude.Maybe Lude.Text,
    -- | A user-supplied name or description of the @Evaluation@ .
    name :: Lude.Maybe Lude.Text,
    -- | A link to the file that contains logs of the @CreateEvaluation@ operation.
    logURI :: Lude.Maybe Lude.Text,
    -- | The evaluation ID which is same as the @EvaluationId@ in the request.
    evaluationId :: Lude.Maybe Lude.Text,
    -- | A description of the most recent details about evaluating the @MLModel@ .
    message :: Lude.Maybe Lude.Text,
    -- | The @DataSource@ used for this evaluation.
    evaluationDataSourceId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEvaluationResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the evaluation. This element can have one of the following values:
--
--
--     * @PENDING@ - Amazon Machine Language (Amazon ML) submitted a request to evaluate an @MLModel@ .
--
--     * @INPROGRESS@ - The evaluation is underway.
--
--     * @FAILED@ - The request to evaluate an @MLModel@ did not run to completion. It is not usable.
--
--     * @COMPLETED@ - The evaluation process completed successfully.
--
--     * @DELETED@ - The @Evaluation@ is marked as deleted. It is not usable.
--
-- * 'performanceMetrics' - Measurements of how well the @MLModel@ performed using observations referenced by the @DataSource@ . One of the following metric is returned based on the type of the @MLModel@ :
--
--
--     * BinaryAUC: A binary @MLModel@ uses the Area Under the Curve (AUC) technique to measure performance.
--
--
--     * RegressionRMSE: A regression @MLModel@ uses the Root Mean Square Error (RMSE) technique to measure performance. RMSE measures the difference between predicted and actual values for a single variable.
--
--
--     * MulticlassAvgFScore: A multiclass @MLModel@ uses the F1 score technique to measure performance.
--
--
-- For more information about performance metrics, please see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide> .
-- * 'lastUpdatedAt' - The time of the most recent edit to the @Evaluation@ . The time is expressed in epoch time.
-- * 'createdAt' - The time that the @Evaluation@ was created. The time is expressed in epoch time.
-- * 'computeTime' - The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @Evaluation@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @Evaluation@ is in the @COMPLETED@ state.
-- * 'inputDataLocationS3' - The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
-- * 'mLModelId' - The ID of the @MLModel@ that was the focus of the evaluation.
-- * 'startedAt' - The epoch time when Amazon Machine Learning marked the @Evaluation@ as @INPROGRESS@ . @StartedAt@ isn't available if the @Evaluation@ is in the @PENDING@ state.
-- * 'finishedAt' - The epoch time when Amazon Machine Learning marked the @Evaluation@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @Evaluation@ is in the @COMPLETED@ or @FAILED@ state.
-- * 'createdByIAMUser' - The AWS user account that invoked the evaluation. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
-- * 'name' - A user-supplied name or description of the @Evaluation@ .
-- * 'logURI' - A link to the file that contains logs of the @CreateEvaluation@ operation.
-- * 'evaluationId' - The evaluation ID which is same as the @EvaluationId@ in the request.
-- * 'message' - A description of the most recent details about evaluating the @MLModel@ .
-- * 'evaluationDataSourceId' - The @DataSource@ used for this evaluation.
-- * 'responseStatus' - The response status code.
mkGetEvaluationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetEvaluationResponse
mkGetEvaluationResponse pResponseStatus_ =
  GetEvaluationResponse'
    { status = Lude.Nothing,
      performanceMetrics = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      createdAt = Lude.Nothing,
      computeTime = Lude.Nothing,
      inputDataLocationS3 = Lude.Nothing,
      mLModelId = Lude.Nothing,
      startedAt = Lude.Nothing,
      finishedAt = Lude.Nothing,
      createdByIAMUser = Lude.Nothing,
      name = Lude.Nothing,
      logURI = Lude.Nothing,
      evaluationId = Lude.Nothing,
      message = Lude.Nothing,
      evaluationDataSourceId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the evaluation. This element can have one of the following values:
--
--
--     * @PENDING@ - Amazon Machine Language (Amazon ML) submitted a request to evaluate an @MLModel@ .
--
--     * @INPROGRESS@ - The evaluation is underway.
--
--     * @FAILED@ - The request to evaluate an @MLModel@ did not run to completion. It is not usable.
--
--     * @COMPLETED@ - The evaluation process completed successfully.
--
--     * @DELETED@ - The @Evaluation@ is marked as deleted. It is not usable.
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersStatus :: Lens.Lens' GetEvaluationResponse (Lude.Maybe EntityStatus)
gersStatus = Lens.lens (status :: GetEvaluationResponse -> Lude.Maybe EntityStatus) (\s a -> s {status = a} :: GetEvaluationResponse)
{-# DEPRECATED gersStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Measurements of how well the @MLModel@ performed using observations referenced by the @DataSource@ . One of the following metric is returned based on the type of the @MLModel@ :
--
--
--     * BinaryAUC: A binary @MLModel@ uses the Area Under the Curve (AUC) technique to measure performance.
--
--
--     * RegressionRMSE: A regression @MLModel@ uses the Root Mean Square Error (RMSE) technique to measure performance. RMSE measures the difference between predicted and actual values for a single variable.
--
--
--     * MulticlassAvgFScore: A multiclass @MLModel@ uses the F1 score technique to measure performance.
--
--
-- For more information about performance metrics, please see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide> .
--
-- /Note:/ Consider using 'performanceMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersPerformanceMetrics :: Lens.Lens' GetEvaluationResponse (Lude.Maybe PerformanceMetrics)
gersPerformanceMetrics = Lens.lens (performanceMetrics :: GetEvaluationResponse -> Lude.Maybe PerformanceMetrics) (\s a -> s {performanceMetrics = a} :: GetEvaluationResponse)
{-# DEPRECATED gersPerformanceMetrics "Use generic-lens or generic-optics with 'performanceMetrics' instead." #-}

-- | The time of the most recent edit to the @Evaluation@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersLastUpdatedAt :: Lens.Lens' GetEvaluationResponse (Lude.Maybe Lude.Timestamp)
gersLastUpdatedAt = Lens.lens (lastUpdatedAt :: GetEvaluationResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: GetEvaluationResponse)
{-# DEPRECATED gersLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The time that the @Evaluation@ was created. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersCreatedAt :: Lens.Lens' GetEvaluationResponse (Lude.Maybe Lude.Timestamp)
gersCreatedAt = Lens.lens (createdAt :: GetEvaluationResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: GetEvaluationResponse)
{-# DEPRECATED gersCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @Evaluation@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @Evaluation@ is in the @COMPLETED@ state.
--
-- /Note:/ Consider using 'computeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersComputeTime :: Lens.Lens' GetEvaluationResponse (Lude.Maybe Lude.Integer)
gersComputeTime = Lens.lens (computeTime :: GetEvaluationResponse -> Lude.Maybe Lude.Integer) (\s a -> s {computeTime = a} :: GetEvaluationResponse)
{-# DEPRECATED gersComputeTime "Use generic-lens or generic-optics with 'computeTime' instead." #-}

-- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
--
-- /Note:/ Consider using 'inputDataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersInputDataLocationS3 :: Lens.Lens' GetEvaluationResponse (Lude.Maybe Lude.Text)
gersInputDataLocationS3 = Lens.lens (inputDataLocationS3 :: GetEvaluationResponse -> Lude.Maybe Lude.Text) (\s a -> s {inputDataLocationS3 = a} :: GetEvaluationResponse)
{-# DEPRECATED gersInputDataLocationS3 "Use generic-lens or generic-optics with 'inputDataLocationS3' instead." #-}

-- | The ID of the @MLModel@ that was the focus of the evaluation.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersMLModelId :: Lens.Lens' GetEvaluationResponse (Lude.Maybe Lude.Text)
gersMLModelId = Lens.lens (mLModelId :: GetEvaluationResponse -> Lude.Maybe Lude.Text) (\s a -> s {mLModelId = a} :: GetEvaluationResponse)
{-# DEPRECATED gersMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | The epoch time when Amazon Machine Learning marked the @Evaluation@ as @INPROGRESS@ . @StartedAt@ isn't available if the @Evaluation@ is in the @PENDING@ state.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersStartedAt :: Lens.Lens' GetEvaluationResponse (Lude.Maybe Lude.Timestamp)
gersStartedAt = Lens.lens (startedAt :: GetEvaluationResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedAt = a} :: GetEvaluationResponse)
{-# DEPRECATED gersStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | The epoch time when Amazon Machine Learning marked the @Evaluation@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @Evaluation@ is in the @COMPLETED@ or @FAILED@ state.
--
-- /Note:/ Consider using 'finishedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersFinishedAt :: Lens.Lens' GetEvaluationResponse (Lude.Maybe Lude.Timestamp)
gersFinishedAt = Lens.lens (finishedAt :: GetEvaluationResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {finishedAt = a} :: GetEvaluationResponse)
{-# DEPRECATED gersFinishedAt "Use generic-lens or generic-optics with 'finishedAt' instead." #-}

-- | The AWS user account that invoked the evaluation. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- /Note:/ Consider using 'createdByIAMUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersCreatedByIAMUser :: Lens.Lens' GetEvaluationResponse (Lude.Maybe Lude.Text)
gersCreatedByIAMUser = Lens.lens (createdByIAMUser :: GetEvaluationResponse -> Lude.Maybe Lude.Text) (\s a -> s {createdByIAMUser = a} :: GetEvaluationResponse)
{-# DEPRECATED gersCreatedByIAMUser "Use generic-lens or generic-optics with 'createdByIAMUser' instead." #-}

-- | A user-supplied name or description of the @Evaluation@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersName :: Lens.Lens' GetEvaluationResponse (Lude.Maybe Lude.Text)
gersName = Lens.lens (name :: GetEvaluationResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetEvaluationResponse)
{-# DEPRECATED gersName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A link to the file that contains logs of the @CreateEvaluation@ operation.
--
-- /Note:/ Consider using 'logURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersLogURI :: Lens.Lens' GetEvaluationResponse (Lude.Maybe Lude.Text)
gersLogURI = Lens.lens (logURI :: GetEvaluationResponse -> Lude.Maybe Lude.Text) (\s a -> s {logURI = a} :: GetEvaluationResponse)
{-# DEPRECATED gersLogURI "Use generic-lens or generic-optics with 'logURI' instead." #-}

-- | The evaluation ID which is same as the @EvaluationId@ in the request.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersEvaluationId :: Lens.Lens' GetEvaluationResponse (Lude.Maybe Lude.Text)
gersEvaluationId = Lens.lens (evaluationId :: GetEvaluationResponse -> Lude.Maybe Lude.Text) (\s a -> s {evaluationId = a} :: GetEvaluationResponse)
{-# DEPRECATED gersEvaluationId "Use generic-lens or generic-optics with 'evaluationId' instead." #-}

-- | A description of the most recent details about evaluating the @MLModel@ .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersMessage :: Lens.Lens' GetEvaluationResponse (Lude.Maybe Lude.Text)
gersMessage = Lens.lens (message :: GetEvaluationResponse -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: GetEvaluationResponse)
{-# DEPRECATED gersMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The @DataSource@ used for this evaluation.
--
-- /Note:/ Consider using 'evaluationDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersEvaluationDataSourceId :: Lens.Lens' GetEvaluationResponse (Lude.Maybe Lude.Text)
gersEvaluationDataSourceId = Lens.lens (evaluationDataSourceId :: GetEvaluationResponse -> Lude.Maybe Lude.Text) (\s a -> s {evaluationDataSourceId = a} :: GetEvaluationResponse)
{-# DEPRECATED gersEvaluationDataSourceId "Use generic-lens or generic-optics with 'evaluationDataSourceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersResponseStatus :: Lens.Lens' GetEvaluationResponse Lude.Int
gersResponseStatus = Lens.lens (responseStatus :: GetEvaluationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetEvaluationResponse)
{-# DEPRECATED gersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
