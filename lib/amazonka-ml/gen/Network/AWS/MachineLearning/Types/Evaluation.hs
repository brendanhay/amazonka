{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.Evaluation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.Evaluation
  ( Evaluation (..),

    -- * Smart constructor
    mkEvaluation,

    -- * Lenses
    eComputeTime,
    eCreatedAt,
    eCreatedByIamUser,
    eEvaluationDataSourceId,
    eEvaluationId,
    eFinishedAt,
    eInputDataLocationS3,
    eLastUpdatedAt,
    eMLModelId,
    eMessage,
    eName,
    ePerformanceMetrics,
    eStartedAt,
    eStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types.AwsUserArn as Types
import qualified Network.AWS.MachineLearning.Types.EntityId as Types
import qualified Network.AWS.MachineLearning.Types.EntityName as Types
import qualified Network.AWS.MachineLearning.Types.EntityStatus as Types
import qualified Network.AWS.MachineLearning.Types.Message as Types
import qualified Network.AWS.MachineLearning.Types.PerformanceMetrics as Types
import qualified Network.AWS.MachineLearning.Types.S3Url as Types
import qualified Network.AWS.Prelude as Core

-- | Represents the output of @GetEvaluation@ operation.
--
-- The content consists of the detailed metadata and data file information and the current status of the @Evaluation@ .
--
-- /See:/ 'mkEvaluation' smart constructor.
data Evaluation = Evaluation'
  { computeTime :: Core.Maybe Core.Integer,
    -- | The time that the @Evaluation@ was created. The time is expressed in epoch time.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | The AWS user account that invoked the evaluation. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
    createdByIamUser :: Core.Maybe Types.AwsUserArn,
    -- | The ID of the @DataSource@ that is used to evaluate the @MLModel@ .
    evaluationDataSourceId :: Core.Maybe Types.EntityId,
    -- | The ID that is assigned to the @Evaluation@ at creation.
    evaluationId :: Core.Maybe Types.EntityId,
    finishedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The location and name of the data in Amazon Simple Storage Server (Amazon S3) that is used in the evaluation.
    inputDataLocationS3 :: Core.Maybe Types.S3Url,
    -- | The time of the most recent edit to the @Evaluation@ . The time is expressed in epoch time.
    lastUpdatedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the @MLModel@ that is the focus of the evaluation.
    mLModelId :: Core.Maybe Types.EntityId,
    -- | A description of the most recent details about evaluating the @MLModel@ .
    message :: Core.Maybe Types.Message,
    -- | A user-supplied name or description of the @Evaluation@ .
    name :: Core.Maybe Types.EntityName,
    -- | Measurements of how well the @MLModel@ performed, using observations referenced by the @DataSource@ . One of the following metrics is returned, based on the type of the @MLModel@ :
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
    startedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The status of the evaluation. This element can have one of the following values:
    --
    --
    --     * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to evaluate an @MLModel@ .
    --
    --     * @INPROGRESS@ - The evaluation is underway.
    --
    --     * @FAILED@ - The request to evaluate an @MLModel@ did not run to completion. It is not usable.
    --
    --     * @COMPLETED@ - The evaluation process completed successfully.
    --
    --     * @DELETED@ - The @Evaluation@ is marked as deleted. It is not usable.
    status :: Core.Maybe Types.EntityStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Evaluation' value with any optional fields omitted.
mkEvaluation ::
  Evaluation
mkEvaluation =
  Evaluation'
    { computeTime = Core.Nothing,
      createdAt = Core.Nothing,
      createdByIamUser = Core.Nothing,
      evaluationDataSourceId = Core.Nothing,
      evaluationId = Core.Nothing,
      finishedAt = Core.Nothing,
      inputDataLocationS3 = Core.Nothing,
      lastUpdatedAt = Core.Nothing,
      mLModelId = Core.Nothing,
      message = Core.Nothing,
      name = Core.Nothing,
      performanceMetrics = Core.Nothing,
      startedAt = Core.Nothing,
      status = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'computeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eComputeTime :: Lens.Lens' Evaluation (Core.Maybe Core.Integer)
eComputeTime = Lens.field @"computeTime"
{-# DEPRECATED eComputeTime "Use generic-lens or generic-optics with 'computeTime' instead." #-}

-- | The time that the @Evaluation@ was created. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCreatedAt :: Lens.Lens' Evaluation (Core.Maybe Core.NominalDiffTime)
eCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED eCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The AWS user account that invoked the evaluation. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- /Note:/ Consider using 'createdByIamUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCreatedByIamUser :: Lens.Lens' Evaluation (Core.Maybe Types.AwsUserArn)
eCreatedByIamUser = Lens.field @"createdByIamUser"
{-# DEPRECATED eCreatedByIamUser "Use generic-lens or generic-optics with 'createdByIamUser' instead." #-}

-- | The ID of the @DataSource@ that is used to evaluate the @MLModel@ .
--
-- /Note:/ Consider using 'evaluationDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEvaluationDataSourceId :: Lens.Lens' Evaluation (Core.Maybe Types.EntityId)
eEvaluationDataSourceId = Lens.field @"evaluationDataSourceId"
{-# DEPRECATED eEvaluationDataSourceId "Use generic-lens or generic-optics with 'evaluationDataSourceId' instead." #-}

-- | The ID that is assigned to the @Evaluation@ at creation.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEvaluationId :: Lens.Lens' Evaluation (Core.Maybe Types.EntityId)
eEvaluationId = Lens.field @"evaluationId"
{-# DEPRECATED eEvaluationId "Use generic-lens or generic-optics with 'evaluationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'finishedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eFinishedAt :: Lens.Lens' Evaluation (Core.Maybe Core.NominalDiffTime)
eFinishedAt = Lens.field @"finishedAt"
{-# DEPRECATED eFinishedAt "Use generic-lens or generic-optics with 'finishedAt' instead." #-}

-- | The location and name of the data in Amazon Simple Storage Server (Amazon S3) that is used in the evaluation.
--
-- /Note:/ Consider using 'inputDataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eInputDataLocationS3 :: Lens.Lens' Evaluation (Core.Maybe Types.S3Url)
eInputDataLocationS3 = Lens.field @"inputDataLocationS3"
{-# DEPRECATED eInputDataLocationS3 "Use generic-lens or generic-optics with 'inputDataLocationS3' instead." #-}

-- | The time of the most recent edit to the @Evaluation@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eLastUpdatedAt :: Lens.Lens' Evaluation (Core.Maybe Core.NominalDiffTime)
eLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# DEPRECATED eLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The ID of the @MLModel@ that is the focus of the evaluation.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMLModelId :: Lens.Lens' Evaluation (Core.Maybe Types.EntityId)
eMLModelId = Lens.field @"mLModelId"
{-# DEPRECATED eMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | A description of the most recent details about evaluating the @MLModel@ .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' Evaluation (Core.Maybe Types.Message)
eMessage = Lens.field @"message"
{-# DEPRECATED eMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | A user-supplied name or description of the @Evaluation@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eName :: Lens.Lens' Evaluation (Core.Maybe Types.EntityName)
eName = Lens.field @"name"
{-# DEPRECATED eName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Measurements of how well the @MLModel@ performed, using observations referenced by the @DataSource@ . One of the following metrics is returned, based on the type of the @MLModel@ :
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
ePerformanceMetrics :: Lens.Lens' Evaluation (Core.Maybe Types.PerformanceMetrics)
ePerformanceMetrics = Lens.field @"performanceMetrics"
{-# DEPRECATED ePerformanceMetrics "Use generic-lens or generic-optics with 'performanceMetrics' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eStartedAt :: Lens.Lens' Evaluation (Core.Maybe Core.NominalDiffTime)
eStartedAt = Lens.field @"startedAt"
{-# DEPRECATED eStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | The status of the evaluation. This element can have one of the following values:
--
--
--     * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to evaluate an @MLModel@ .
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
eStatus :: Lens.Lens' Evaluation (Core.Maybe Types.EntityStatus)
eStatus = Lens.field @"status"
{-# DEPRECATED eStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON Evaluation where
  parseJSON =
    Core.withObject "Evaluation" Core.$
      \x ->
        Evaluation'
          Core.<$> (x Core..:? "ComputeTime")
          Core.<*> (x Core..:? "CreatedAt")
          Core.<*> (x Core..:? "CreatedByIamUser")
          Core.<*> (x Core..:? "EvaluationDataSourceId")
          Core.<*> (x Core..:? "EvaluationId")
          Core.<*> (x Core..:? "FinishedAt")
          Core.<*> (x Core..:? "InputDataLocationS3")
          Core.<*> (x Core..:? "LastUpdatedAt")
          Core.<*> (x Core..:? "MLModelId")
          Core.<*> (x Core..:? "Message")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "PerformanceMetrics")
          Core.<*> (x Core..:? "StartedAt")
          Core.<*> (x Core..:? "Status")
