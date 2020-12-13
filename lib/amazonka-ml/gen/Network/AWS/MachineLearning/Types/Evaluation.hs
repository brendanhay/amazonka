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
    eStatus,
    ePerformanceMetrics,
    eLastUpdatedAt,
    eCreatedAt,
    eComputeTime,
    eInputDataLocationS3,
    eMLModelId,
    eStartedAt,
    eFinishedAt,
    eCreatedByIAMUser,
    eName,
    eEvaluationId,
    eMessage,
    eEvaluationDataSourceId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.EntityStatus
import Network.AWS.MachineLearning.Types.PerformanceMetrics
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of @GetEvaluation@ operation.
--
-- The content consists of the detailed metadata and data file information and the current status of the @Evaluation@ .
--
-- /See:/ 'mkEvaluation' smart constructor.
data Evaluation = Evaluation'
  { -- | The status of the evaluation. This element can have one of the following values:
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
    status :: Lude.Maybe EntityStatus,
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
    performanceMetrics :: Lude.Maybe PerformanceMetrics,
    -- | The time of the most recent edit to the @Evaluation@ . The time is expressed in epoch time.
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    -- | The time that the @Evaluation@ was created. The time is expressed in epoch time.
    createdAt :: Lude.Maybe Lude.Timestamp,
    computeTime :: Lude.Maybe Lude.Integer,
    -- | The location and name of the data in Amazon Simple Storage Server (Amazon S3) that is used in the evaluation.
    inputDataLocationS3 :: Lude.Maybe Lude.Text,
    -- | The ID of the @MLModel@ that is the focus of the evaluation.
    mLModelId :: Lude.Maybe Lude.Text,
    startedAt :: Lude.Maybe Lude.Timestamp,
    finishedAt :: Lude.Maybe Lude.Timestamp,
    -- | The AWS user account that invoked the evaluation. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
    createdByIAMUser :: Lude.Maybe Lude.Text,
    -- | A user-supplied name or description of the @Evaluation@ .
    name :: Lude.Maybe Lude.Text,
    -- | The ID that is assigned to the @Evaluation@ at creation.
    evaluationId :: Lude.Maybe Lude.Text,
    -- | A description of the most recent details about evaluating the @MLModel@ .
    message :: Lude.Maybe Lude.Text,
    -- | The ID of the @DataSource@ that is used to evaluate the @MLModel@ .
    evaluationDataSourceId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Evaluation' with the minimum fields required to make a request.
--
-- * 'status' - The status of the evaluation. This element can have one of the following values:
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
-- * 'performanceMetrics' - Measurements of how well the @MLModel@ performed, using observations referenced by the @DataSource@ . One of the following metrics is returned, based on the type of the @MLModel@ :
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
-- * 'computeTime' -
-- * 'inputDataLocationS3' - The location and name of the data in Amazon Simple Storage Server (Amazon S3) that is used in the evaluation.
-- * 'mLModelId' - The ID of the @MLModel@ that is the focus of the evaluation.
-- * 'startedAt' -
-- * 'finishedAt' -
-- * 'createdByIAMUser' - The AWS user account that invoked the evaluation. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
-- * 'name' - A user-supplied name or description of the @Evaluation@ .
-- * 'evaluationId' - The ID that is assigned to the @Evaluation@ at creation.
-- * 'message' - A description of the most recent details about evaluating the @MLModel@ .
-- * 'evaluationDataSourceId' - The ID of the @DataSource@ that is used to evaluate the @MLModel@ .
mkEvaluation ::
  Evaluation
mkEvaluation =
  Evaluation'
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
      evaluationId = Lude.Nothing,
      message = Lude.Nothing,
      evaluationDataSourceId = Lude.Nothing
    }

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
eStatus :: Lens.Lens' Evaluation (Lude.Maybe EntityStatus)
eStatus = Lens.lens (status :: Evaluation -> Lude.Maybe EntityStatus) (\s a -> s {status = a} :: Evaluation)
{-# DEPRECATED eStatus "Use generic-lens or generic-optics with 'status' instead." #-}

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
ePerformanceMetrics :: Lens.Lens' Evaluation (Lude.Maybe PerformanceMetrics)
ePerformanceMetrics = Lens.lens (performanceMetrics :: Evaluation -> Lude.Maybe PerformanceMetrics) (\s a -> s {performanceMetrics = a} :: Evaluation)
{-# DEPRECATED ePerformanceMetrics "Use generic-lens or generic-optics with 'performanceMetrics' instead." #-}

-- | The time of the most recent edit to the @Evaluation@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eLastUpdatedAt :: Lens.Lens' Evaluation (Lude.Maybe Lude.Timestamp)
eLastUpdatedAt = Lens.lens (lastUpdatedAt :: Evaluation -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: Evaluation)
{-# DEPRECATED eLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The time that the @Evaluation@ was created. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCreatedAt :: Lens.Lens' Evaluation (Lude.Maybe Lude.Timestamp)
eCreatedAt = Lens.lens (createdAt :: Evaluation -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Evaluation)
{-# DEPRECATED eCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'computeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eComputeTime :: Lens.Lens' Evaluation (Lude.Maybe Lude.Integer)
eComputeTime = Lens.lens (computeTime :: Evaluation -> Lude.Maybe Lude.Integer) (\s a -> s {computeTime = a} :: Evaluation)
{-# DEPRECATED eComputeTime "Use generic-lens or generic-optics with 'computeTime' instead." #-}

-- | The location and name of the data in Amazon Simple Storage Server (Amazon S3) that is used in the evaluation.
--
-- /Note:/ Consider using 'inputDataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eInputDataLocationS3 :: Lens.Lens' Evaluation (Lude.Maybe Lude.Text)
eInputDataLocationS3 = Lens.lens (inputDataLocationS3 :: Evaluation -> Lude.Maybe Lude.Text) (\s a -> s {inputDataLocationS3 = a} :: Evaluation)
{-# DEPRECATED eInputDataLocationS3 "Use generic-lens or generic-optics with 'inputDataLocationS3' instead." #-}

-- | The ID of the @MLModel@ that is the focus of the evaluation.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMLModelId :: Lens.Lens' Evaluation (Lude.Maybe Lude.Text)
eMLModelId = Lens.lens (mLModelId :: Evaluation -> Lude.Maybe Lude.Text) (\s a -> s {mLModelId = a} :: Evaluation)
{-# DEPRECATED eMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eStartedAt :: Lens.Lens' Evaluation (Lude.Maybe Lude.Timestamp)
eStartedAt = Lens.lens (startedAt :: Evaluation -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedAt = a} :: Evaluation)
{-# DEPRECATED eStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'finishedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eFinishedAt :: Lens.Lens' Evaluation (Lude.Maybe Lude.Timestamp)
eFinishedAt = Lens.lens (finishedAt :: Evaluation -> Lude.Maybe Lude.Timestamp) (\s a -> s {finishedAt = a} :: Evaluation)
{-# DEPRECATED eFinishedAt "Use generic-lens or generic-optics with 'finishedAt' instead." #-}

-- | The AWS user account that invoked the evaluation. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- /Note:/ Consider using 'createdByIAMUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCreatedByIAMUser :: Lens.Lens' Evaluation (Lude.Maybe Lude.Text)
eCreatedByIAMUser = Lens.lens (createdByIAMUser :: Evaluation -> Lude.Maybe Lude.Text) (\s a -> s {createdByIAMUser = a} :: Evaluation)
{-# DEPRECATED eCreatedByIAMUser "Use generic-lens or generic-optics with 'createdByIAMUser' instead." #-}

-- | A user-supplied name or description of the @Evaluation@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eName :: Lens.Lens' Evaluation (Lude.Maybe Lude.Text)
eName = Lens.lens (name :: Evaluation -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Evaluation)
{-# DEPRECATED eName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID that is assigned to the @Evaluation@ at creation.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEvaluationId :: Lens.Lens' Evaluation (Lude.Maybe Lude.Text)
eEvaluationId = Lens.lens (evaluationId :: Evaluation -> Lude.Maybe Lude.Text) (\s a -> s {evaluationId = a} :: Evaluation)
{-# DEPRECATED eEvaluationId "Use generic-lens or generic-optics with 'evaluationId' instead." #-}

-- | A description of the most recent details about evaluating the @MLModel@ .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' Evaluation (Lude.Maybe Lude.Text)
eMessage = Lens.lens (message :: Evaluation -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: Evaluation)
{-# DEPRECATED eMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The ID of the @DataSource@ that is used to evaluate the @MLModel@ .
--
-- /Note:/ Consider using 'evaluationDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEvaluationDataSourceId :: Lens.Lens' Evaluation (Lude.Maybe Lude.Text)
eEvaluationDataSourceId = Lens.lens (evaluationDataSourceId :: Evaluation -> Lude.Maybe Lude.Text) (\s a -> s {evaluationDataSourceId = a} :: Evaluation)
{-# DEPRECATED eEvaluationDataSourceId "Use generic-lens or generic-optics with 'evaluationDataSourceId' instead." #-}

instance Lude.FromJSON Evaluation where
  parseJSON =
    Lude.withObject
      "Evaluation"
      ( \x ->
          Evaluation'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "PerformanceMetrics")
            Lude.<*> (x Lude..:? "LastUpdatedAt")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "ComputeTime")
            Lude.<*> (x Lude..:? "InputDataLocationS3")
            Lude.<*> (x Lude..:? "MLModelId")
            Lude.<*> (x Lude..:? "StartedAt")
            Lude.<*> (x Lude..:? "FinishedAt")
            Lude.<*> (x Lude..:? "CreatedByIamUser")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "EvaluationId")
            Lude.<*> (x Lude..:? "Message")
            Lude.<*> (x Lude..:? "EvaluationDataSourceId")
      )
