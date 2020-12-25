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
    gerrsComputeTime,
    gerrsCreatedAt,
    gerrsCreatedByIamUser,
    gerrsEvaluationDataSourceId,
    gerrsEvaluationId,
    gerrsFinishedAt,
    gerrsInputDataLocationS3,
    gerrsLastUpdatedAt,
    gerrsLogUri,
    gerrsMLModelId,
    gerrsMessage,
    gerrsName,
    gerrsPerformanceMetrics,
    gerrsStartedAt,
    gerrsStatus,
    gerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetEvaluation' smart constructor.
newtype GetEvaluation = GetEvaluation'
  { -- | The ID of the @Evaluation@ to retrieve. The evaluation of each @MLModel@ is recorded and cataloged. The ID provides the means to access the information.
    evaluationId :: Types.EvaluationId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetEvaluation' value with any optional fields omitted.
mkGetEvaluation ::
  -- | 'evaluationId'
  Types.EvaluationId ->
  GetEvaluation
mkGetEvaluation evaluationId = GetEvaluation' {evaluationId}

-- | The ID of the @Evaluation@ to retrieve. The evaluation of each @MLModel@ is recorded and cataloged. The ID provides the means to access the information.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geEvaluationId :: Lens.Lens' GetEvaluation Types.EvaluationId
geEvaluationId = Lens.field @"evaluationId"
{-# DEPRECATED geEvaluationId "Use generic-lens or generic-optics with 'evaluationId' instead." #-}

instance Core.FromJSON GetEvaluation where
  toJSON GetEvaluation {..} =
    Core.object
      (Core.catMaybes [Core.Just ("EvaluationId" Core..= evaluationId)])

instance Core.AWSRequest GetEvaluation where
  type Rs GetEvaluation = GetEvaluationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonML_20141212.GetEvaluation")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEvaluationResponse'
            Core.<$> (x Core..:? "ComputeTime")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "CreatedByIamUser")
            Core.<*> (x Core..:? "EvaluationDataSourceId")
            Core.<*> (x Core..:? "EvaluationId")
            Core.<*> (x Core..:? "FinishedAt")
            Core.<*> (x Core..:? "InputDataLocationS3")
            Core.<*> (x Core..:? "LastUpdatedAt")
            Core.<*> (x Core..:? "LogUri")
            Core.<*> (x Core..:? "MLModelId")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "PerformanceMetrics")
            Core.<*> (x Core..:? "StartedAt")
            Core.<*> (x Core..:? "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @GetEvaluation@ operation and describes an @Evaluation@ .
--
-- /See:/ 'mkGetEvaluationResponse' smart constructor.
data GetEvaluationResponse = GetEvaluationResponse'
  { -- | The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @Evaluation@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @Evaluation@ is in the @COMPLETED@ state.
    computeTime :: Core.Maybe Core.Integer,
    -- | The time that the @Evaluation@ was created. The time is expressed in epoch time.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | The AWS user account that invoked the evaluation. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
    createdByIamUser :: Core.Maybe Types.CreatedByIamUser,
    -- | The @DataSource@ used for this evaluation.
    evaluationDataSourceId :: Core.Maybe Types.EvaluationDataSourceId,
    -- | The evaluation ID which is same as the @EvaluationId@ in the request.
    evaluationId :: Core.Maybe Types.EvaluationId,
    -- | The epoch time when Amazon Machine Learning marked the @Evaluation@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @Evaluation@ is in the @COMPLETED@ or @FAILED@ state.
    finishedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
    inputDataLocationS3 :: Core.Maybe Types.InputDataLocationS3,
    -- | The time of the most recent edit to the @Evaluation@ . The time is expressed in epoch time.
    lastUpdatedAt :: Core.Maybe Core.NominalDiffTime,
    -- | A link to the file that contains logs of the @CreateEvaluation@ operation.
    logUri :: Core.Maybe Types.LogUri,
    -- | The ID of the @MLModel@ that was the focus of the evaluation.
    mLModelId :: Core.Maybe Types.MLModelId,
    -- | A description of the most recent details about evaluating the @MLModel@ .
    message :: Core.Maybe Types.Message,
    -- | A user-supplied name or description of the @Evaluation@ .
    name :: Core.Maybe Types.EntityName,
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
    performanceMetrics :: Core.Maybe Types.PerformanceMetrics,
    -- | The epoch time when Amazon Machine Learning marked the @Evaluation@ as @INPROGRESS@ . @StartedAt@ isn't available if the @Evaluation@ is in the @PENDING@ state.
    startedAt :: Core.Maybe Core.NominalDiffTime,
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
    status :: Core.Maybe Types.EntityStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetEvaluationResponse' value with any optional fields omitted.
mkGetEvaluationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetEvaluationResponse
mkGetEvaluationResponse responseStatus =
  GetEvaluationResponse'
    { computeTime = Core.Nothing,
      createdAt = Core.Nothing,
      createdByIamUser = Core.Nothing,
      evaluationDataSourceId = Core.Nothing,
      evaluationId = Core.Nothing,
      finishedAt = Core.Nothing,
      inputDataLocationS3 = Core.Nothing,
      lastUpdatedAt = Core.Nothing,
      logUri = Core.Nothing,
      mLModelId = Core.Nothing,
      message = Core.Nothing,
      name = Core.Nothing,
      performanceMetrics = Core.Nothing,
      startedAt = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @Evaluation@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @Evaluation@ is in the @COMPLETED@ state.
--
-- /Note:/ Consider using 'computeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsComputeTime :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.Integer)
gerrsComputeTime = Lens.field @"computeTime"
{-# DEPRECATED gerrsComputeTime "Use generic-lens or generic-optics with 'computeTime' instead." #-}

-- | The time that the @Evaluation@ was created. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsCreatedAt :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.NominalDiffTime)
gerrsCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED gerrsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The AWS user account that invoked the evaluation. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- /Note:/ Consider using 'createdByIamUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsCreatedByIamUser :: Lens.Lens' GetEvaluationResponse (Core.Maybe Types.CreatedByIamUser)
gerrsCreatedByIamUser = Lens.field @"createdByIamUser"
{-# DEPRECATED gerrsCreatedByIamUser "Use generic-lens or generic-optics with 'createdByIamUser' instead." #-}

-- | The @DataSource@ used for this evaluation.
--
-- /Note:/ Consider using 'evaluationDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsEvaluationDataSourceId :: Lens.Lens' GetEvaluationResponse (Core.Maybe Types.EvaluationDataSourceId)
gerrsEvaluationDataSourceId = Lens.field @"evaluationDataSourceId"
{-# DEPRECATED gerrsEvaluationDataSourceId "Use generic-lens or generic-optics with 'evaluationDataSourceId' instead." #-}

-- | The evaluation ID which is same as the @EvaluationId@ in the request.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsEvaluationId :: Lens.Lens' GetEvaluationResponse (Core.Maybe Types.EvaluationId)
gerrsEvaluationId = Lens.field @"evaluationId"
{-# DEPRECATED gerrsEvaluationId "Use generic-lens or generic-optics with 'evaluationId' instead." #-}

-- | The epoch time when Amazon Machine Learning marked the @Evaluation@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @Evaluation@ is in the @COMPLETED@ or @FAILED@ state.
--
-- /Note:/ Consider using 'finishedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsFinishedAt :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.NominalDiffTime)
gerrsFinishedAt = Lens.field @"finishedAt"
{-# DEPRECATED gerrsFinishedAt "Use generic-lens or generic-optics with 'finishedAt' instead." #-}

-- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
--
-- /Note:/ Consider using 'inputDataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsInputDataLocationS3 :: Lens.Lens' GetEvaluationResponse (Core.Maybe Types.InputDataLocationS3)
gerrsInputDataLocationS3 = Lens.field @"inputDataLocationS3"
{-# DEPRECATED gerrsInputDataLocationS3 "Use generic-lens or generic-optics with 'inputDataLocationS3' instead." #-}

-- | The time of the most recent edit to the @Evaluation@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsLastUpdatedAt :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.NominalDiffTime)
gerrsLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# DEPRECATED gerrsLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | A link to the file that contains logs of the @CreateEvaluation@ operation.
--
-- /Note:/ Consider using 'logUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsLogUri :: Lens.Lens' GetEvaluationResponse (Core.Maybe Types.LogUri)
gerrsLogUri = Lens.field @"logUri"
{-# DEPRECATED gerrsLogUri "Use generic-lens or generic-optics with 'logUri' instead." #-}

-- | The ID of the @MLModel@ that was the focus of the evaluation.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsMLModelId :: Lens.Lens' GetEvaluationResponse (Core.Maybe Types.MLModelId)
gerrsMLModelId = Lens.field @"mLModelId"
{-# DEPRECATED gerrsMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | A description of the most recent details about evaluating the @MLModel@ .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsMessage :: Lens.Lens' GetEvaluationResponse (Core.Maybe Types.Message)
gerrsMessage = Lens.field @"message"
{-# DEPRECATED gerrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | A user-supplied name or description of the @Evaluation@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsName :: Lens.Lens' GetEvaluationResponse (Core.Maybe Types.EntityName)
gerrsName = Lens.field @"name"
{-# DEPRECATED gerrsName "Use generic-lens or generic-optics with 'name' instead." #-}

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
gerrsPerformanceMetrics :: Lens.Lens' GetEvaluationResponse (Core.Maybe Types.PerformanceMetrics)
gerrsPerformanceMetrics = Lens.field @"performanceMetrics"
{-# DEPRECATED gerrsPerformanceMetrics "Use generic-lens or generic-optics with 'performanceMetrics' instead." #-}

-- | The epoch time when Amazon Machine Learning marked the @Evaluation@ as @INPROGRESS@ . @StartedAt@ isn't available if the @Evaluation@ is in the @PENDING@ state.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsStartedAt :: Lens.Lens' GetEvaluationResponse (Core.Maybe Core.NominalDiffTime)
gerrsStartedAt = Lens.field @"startedAt"
{-# DEPRECATED gerrsStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

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
gerrsStatus :: Lens.Lens' GetEvaluationResponse (Core.Maybe Types.EntityStatus)
gerrsStatus = Lens.field @"status"
{-# DEPRECATED gerrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsResponseStatus :: Lens.Lens' GetEvaluationResponse Core.Int
gerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
