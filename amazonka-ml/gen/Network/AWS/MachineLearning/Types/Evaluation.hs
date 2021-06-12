{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.Evaluation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.Evaluation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.EntityStatus
import Network.AWS.MachineLearning.Types.PerformanceMetrics

-- | Represents the output of @GetEvaluation@ operation.
--
-- The content consists of the detailed metadata and data file information
-- and the current status of the @Evaluation@.
--
-- /See:/ 'newEvaluation' smart constructor.
data Evaluation = Evaluation'
  { -- | Measurements of how well the @MLModel@ performed, using observations
    -- referenced by the @DataSource@. One of the following metrics is
    -- returned, based on the type of the @MLModel@:
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
    -- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
    --     to evaluate an @MLModel@.
    -- -   @INPROGRESS@ - The evaluation is underway.
    -- -   @FAILED@ - The request to evaluate an @MLModel@ did not run to
    --     completion. It is not usable.
    -- -   @COMPLETED@ - The evaluation process completed successfully.
    -- -   @DELETED@ - The @Evaluation@ is marked as deleted. It is not usable.
    status :: Core.Maybe EntityStatus,
    startedAt :: Core.Maybe Core.POSIX,
    -- | The ID of the @DataSource@ that is used to evaluate the @MLModel@.
    evaluationDataSourceId :: Core.Maybe Core.Text,
    -- | A description of the most recent details about evaluating the @MLModel@.
    message :: Core.Maybe Core.Text,
    -- | The time that the @Evaluation@ was created. The time is expressed in
    -- epoch time.
    createdAt :: Core.Maybe Core.POSIX,
    finishedAt :: Core.Maybe Core.POSIX,
    -- | The AWS user account that invoked the evaluation. The account type can
    -- be either an AWS root account or an AWS Identity and Access Management
    -- (IAM) user account.
    createdByIamUser :: Core.Maybe Core.Text,
    -- | A user-supplied name or description of the @Evaluation@.
    name :: Core.Maybe Core.Text,
    -- | The ID that is assigned to the @Evaluation@ at creation.
    evaluationId :: Core.Maybe Core.Text,
    -- | The ID of the @MLModel@ that is the focus of the evaluation.
    mLModelId :: Core.Maybe Core.Text,
    -- | The location and name of the data in Amazon Simple Storage Server
    -- (Amazon S3) that is used in the evaluation.
    inputDataLocationS3 :: Core.Maybe Core.Text,
    computeTime :: Core.Maybe Core.Integer,
    -- | The time of the most recent edit to the @Evaluation@. The time is
    -- expressed in epoch time.
    lastUpdatedAt :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Evaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'performanceMetrics', 'evaluation_performanceMetrics' - Measurements of how well the @MLModel@ performed, using observations
-- referenced by the @DataSource@. One of the following metrics is
-- returned, based on the type of the @MLModel@:
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
-- 'status', 'evaluation_status' - The status of the evaluation. This element can have one of the following
-- values:
--
-- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
--     to evaluate an @MLModel@.
-- -   @INPROGRESS@ - The evaluation is underway.
-- -   @FAILED@ - The request to evaluate an @MLModel@ did not run to
--     completion. It is not usable.
-- -   @COMPLETED@ - The evaluation process completed successfully.
-- -   @DELETED@ - The @Evaluation@ is marked as deleted. It is not usable.
--
-- 'startedAt', 'evaluation_startedAt' - Undocumented member.
--
-- 'evaluationDataSourceId', 'evaluation_evaluationDataSourceId' - The ID of the @DataSource@ that is used to evaluate the @MLModel@.
--
-- 'message', 'evaluation_message' - A description of the most recent details about evaluating the @MLModel@.
--
-- 'createdAt', 'evaluation_createdAt' - The time that the @Evaluation@ was created. The time is expressed in
-- epoch time.
--
-- 'finishedAt', 'evaluation_finishedAt' - Undocumented member.
--
-- 'createdByIamUser', 'evaluation_createdByIamUser' - The AWS user account that invoked the evaluation. The account type can
-- be either an AWS root account or an AWS Identity and Access Management
-- (IAM) user account.
--
-- 'name', 'evaluation_name' - A user-supplied name or description of the @Evaluation@.
--
-- 'evaluationId', 'evaluation_evaluationId' - The ID that is assigned to the @Evaluation@ at creation.
--
-- 'mLModelId', 'evaluation_mLModelId' - The ID of the @MLModel@ that is the focus of the evaluation.
--
-- 'inputDataLocationS3', 'evaluation_inputDataLocationS3' - The location and name of the data in Amazon Simple Storage Server
-- (Amazon S3) that is used in the evaluation.
--
-- 'computeTime', 'evaluation_computeTime' - Undocumented member.
--
-- 'lastUpdatedAt', 'evaluation_lastUpdatedAt' - The time of the most recent edit to the @Evaluation@. The time is
-- expressed in epoch time.
newEvaluation ::
  Evaluation
newEvaluation =
  Evaluation'
    { performanceMetrics = Core.Nothing,
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
      lastUpdatedAt = Core.Nothing
    }

-- | Measurements of how well the @MLModel@ performed, using observations
-- referenced by the @DataSource@. One of the following metrics is
-- returned, based on the type of the @MLModel@:
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
evaluation_performanceMetrics :: Lens.Lens' Evaluation (Core.Maybe PerformanceMetrics)
evaluation_performanceMetrics = Lens.lens (\Evaluation' {performanceMetrics} -> performanceMetrics) (\s@Evaluation' {} a -> s {performanceMetrics = a} :: Evaluation)

-- | The status of the evaluation. This element can have one of the following
-- values:
--
-- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
--     to evaluate an @MLModel@.
-- -   @INPROGRESS@ - The evaluation is underway.
-- -   @FAILED@ - The request to evaluate an @MLModel@ did not run to
--     completion. It is not usable.
-- -   @COMPLETED@ - The evaluation process completed successfully.
-- -   @DELETED@ - The @Evaluation@ is marked as deleted. It is not usable.
evaluation_status :: Lens.Lens' Evaluation (Core.Maybe EntityStatus)
evaluation_status = Lens.lens (\Evaluation' {status} -> status) (\s@Evaluation' {} a -> s {status = a} :: Evaluation)

-- | Undocumented member.
evaluation_startedAt :: Lens.Lens' Evaluation (Core.Maybe Core.UTCTime)
evaluation_startedAt = Lens.lens (\Evaluation' {startedAt} -> startedAt) (\s@Evaluation' {} a -> s {startedAt = a} :: Evaluation) Core.. Lens.mapping Core._Time

-- | The ID of the @DataSource@ that is used to evaluate the @MLModel@.
evaluation_evaluationDataSourceId :: Lens.Lens' Evaluation (Core.Maybe Core.Text)
evaluation_evaluationDataSourceId = Lens.lens (\Evaluation' {evaluationDataSourceId} -> evaluationDataSourceId) (\s@Evaluation' {} a -> s {evaluationDataSourceId = a} :: Evaluation)

-- | A description of the most recent details about evaluating the @MLModel@.
evaluation_message :: Lens.Lens' Evaluation (Core.Maybe Core.Text)
evaluation_message = Lens.lens (\Evaluation' {message} -> message) (\s@Evaluation' {} a -> s {message = a} :: Evaluation)

-- | The time that the @Evaluation@ was created. The time is expressed in
-- epoch time.
evaluation_createdAt :: Lens.Lens' Evaluation (Core.Maybe Core.UTCTime)
evaluation_createdAt = Lens.lens (\Evaluation' {createdAt} -> createdAt) (\s@Evaluation' {} a -> s {createdAt = a} :: Evaluation) Core.. Lens.mapping Core._Time

-- | Undocumented member.
evaluation_finishedAt :: Lens.Lens' Evaluation (Core.Maybe Core.UTCTime)
evaluation_finishedAt = Lens.lens (\Evaluation' {finishedAt} -> finishedAt) (\s@Evaluation' {} a -> s {finishedAt = a} :: Evaluation) Core.. Lens.mapping Core._Time

-- | The AWS user account that invoked the evaluation. The account type can
-- be either an AWS root account or an AWS Identity and Access Management
-- (IAM) user account.
evaluation_createdByIamUser :: Lens.Lens' Evaluation (Core.Maybe Core.Text)
evaluation_createdByIamUser = Lens.lens (\Evaluation' {createdByIamUser} -> createdByIamUser) (\s@Evaluation' {} a -> s {createdByIamUser = a} :: Evaluation)

-- | A user-supplied name or description of the @Evaluation@.
evaluation_name :: Lens.Lens' Evaluation (Core.Maybe Core.Text)
evaluation_name = Lens.lens (\Evaluation' {name} -> name) (\s@Evaluation' {} a -> s {name = a} :: Evaluation)

-- | The ID that is assigned to the @Evaluation@ at creation.
evaluation_evaluationId :: Lens.Lens' Evaluation (Core.Maybe Core.Text)
evaluation_evaluationId = Lens.lens (\Evaluation' {evaluationId} -> evaluationId) (\s@Evaluation' {} a -> s {evaluationId = a} :: Evaluation)

-- | The ID of the @MLModel@ that is the focus of the evaluation.
evaluation_mLModelId :: Lens.Lens' Evaluation (Core.Maybe Core.Text)
evaluation_mLModelId = Lens.lens (\Evaluation' {mLModelId} -> mLModelId) (\s@Evaluation' {} a -> s {mLModelId = a} :: Evaluation)

-- | The location and name of the data in Amazon Simple Storage Server
-- (Amazon S3) that is used in the evaluation.
evaluation_inputDataLocationS3 :: Lens.Lens' Evaluation (Core.Maybe Core.Text)
evaluation_inputDataLocationS3 = Lens.lens (\Evaluation' {inputDataLocationS3} -> inputDataLocationS3) (\s@Evaluation' {} a -> s {inputDataLocationS3 = a} :: Evaluation)

-- | Undocumented member.
evaluation_computeTime :: Lens.Lens' Evaluation (Core.Maybe Core.Integer)
evaluation_computeTime = Lens.lens (\Evaluation' {computeTime} -> computeTime) (\s@Evaluation' {} a -> s {computeTime = a} :: Evaluation)

-- | The time of the most recent edit to the @Evaluation@. The time is
-- expressed in epoch time.
evaluation_lastUpdatedAt :: Lens.Lens' Evaluation (Core.Maybe Core.UTCTime)
evaluation_lastUpdatedAt = Lens.lens (\Evaluation' {lastUpdatedAt} -> lastUpdatedAt) (\s@Evaluation' {} a -> s {lastUpdatedAt = a} :: Evaluation) Core.. Lens.mapping Core._Time

instance Core.FromJSON Evaluation where
  parseJSON =
    Core.withObject
      "Evaluation"
      ( \x ->
          Evaluation'
            Core.<$> (x Core..:? "PerformanceMetrics")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "StartedAt")
            Core.<*> (x Core..:? "EvaluationDataSourceId")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "FinishedAt")
            Core.<*> (x Core..:? "CreatedByIamUser")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "EvaluationId")
            Core.<*> (x Core..:? "MLModelId")
            Core.<*> (x Core..:? "InputDataLocationS3")
            Core.<*> (x Core..:? "ComputeTime")
            Core.<*> (x Core..:? "LastUpdatedAt")
      )

instance Core.Hashable Evaluation

instance Core.NFData Evaluation
