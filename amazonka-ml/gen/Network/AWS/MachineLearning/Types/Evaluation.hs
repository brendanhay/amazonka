{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.EntityStatus
import Network.AWS.MachineLearning.Types.PerformanceMetrics
import qualified Network.AWS.Prelude as Prelude

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
    performanceMetrics :: Prelude.Maybe PerformanceMetrics,
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
    status :: Prelude.Maybe EntityStatus,
    startedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The ID of the @DataSource@ that is used to evaluate the @MLModel@.
    evaluationDataSourceId :: Prelude.Maybe Prelude.Text,
    -- | A description of the most recent details about evaluating the @MLModel@.
    message :: Prelude.Maybe Prelude.Text,
    -- | The time that the @Evaluation@ was created. The time is expressed in
    -- epoch time.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    finishedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The AWS user account that invoked the evaluation. The account type can
    -- be either an AWS root account or an AWS Identity and Access Management
    -- (IAM) user account.
    createdByIamUser :: Prelude.Maybe Prelude.Text,
    -- | A user-supplied name or description of the @Evaluation@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID that is assigned to the @Evaluation@ at creation.
    evaluationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @MLModel@ that is the focus of the evaluation.
    mLModelId :: Prelude.Maybe Prelude.Text,
    -- | The location and name of the data in Amazon Simple Storage Server
    -- (Amazon S3) that is used in the evaluation.
    inputDataLocationS3 :: Prelude.Maybe Prelude.Text,
    computeTime :: Prelude.Maybe Prelude.Integer,
    -- | The time of the most recent edit to the @Evaluation@. The time is
    -- expressed in epoch time.
    lastUpdatedAt :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { performanceMetrics = Prelude.Nothing,
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
      lastUpdatedAt = Prelude.Nothing
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
evaluation_performanceMetrics :: Lens.Lens' Evaluation (Prelude.Maybe PerformanceMetrics)
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
evaluation_status :: Lens.Lens' Evaluation (Prelude.Maybe EntityStatus)
evaluation_status = Lens.lens (\Evaluation' {status} -> status) (\s@Evaluation' {} a -> s {status = a} :: Evaluation)

-- | Undocumented member.
evaluation_startedAt :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.UTCTime)
evaluation_startedAt = Lens.lens (\Evaluation' {startedAt} -> startedAt) (\s@Evaluation' {} a -> s {startedAt = a} :: Evaluation) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the @DataSource@ that is used to evaluate the @MLModel@.
evaluation_evaluationDataSourceId :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Text)
evaluation_evaluationDataSourceId = Lens.lens (\Evaluation' {evaluationDataSourceId} -> evaluationDataSourceId) (\s@Evaluation' {} a -> s {evaluationDataSourceId = a} :: Evaluation)

-- | A description of the most recent details about evaluating the @MLModel@.
evaluation_message :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Text)
evaluation_message = Lens.lens (\Evaluation' {message} -> message) (\s@Evaluation' {} a -> s {message = a} :: Evaluation)

-- | The time that the @Evaluation@ was created. The time is expressed in
-- epoch time.
evaluation_createdAt :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.UTCTime)
evaluation_createdAt = Lens.lens (\Evaluation' {createdAt} -> createdAt) (\s@Evaluation' {} a -> s {createdAt = a} :: Evaluation) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
evaluation_finishedAt :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.UTCTime)
evaluation_finishedAt = Lens.lens (\Evaluation' {finishedAt} -> finishedAt) (\s@Evaluation' {} a -> s {finishedAt = a} :: Evaluation) Prelude.. Lens.mapping Prelude._Time

-- | The AWS user account that invoked the evaluation. The account type can
-- be either an AWS root account or an AWS Identity and Access Management
-- (IAM) user account.
evaluation_createdByIamUser :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Text)
evaluation_createdByIamUser = Lens.lens (\Evaluation' {createdByIamUser} -> createdByIamUser) (\s@Evaluation' {} a -> s {createdByIamUser = a} :: Evaluation)

-- | A user-supplied name or description of the @Evaluation@.
evaluation_name :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Text)
evaluation_name = Lens.lens (\Evaluation' {name} -> name) (\s@Evaluation' {} a -> s {name = a} :: Evaluation)

-- | The ID that is assigned to the @Evaluation@ at creation.
evaluation_evaluationId :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Text)
evaluation_evaluationId = Lens.lens (\Evaluation' {evaluationId} -> evaluationId) (\s@Evaluation' {} a -> s {evaluationId = a} :: Evaluation)

-- | The ID of the @MLModel@ that is the focus of the evaluation.
evaluation_mLModelId :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Text)
evaluation_mLModelId = Lens.lens (\Evaluation' {mLModelId} -> mLModelId) (\s@Evaluation' {} a -> s {mLModelId = a} :: Evaluation)

-- | The location and name of the data in Amazon Simple Storage Server
-- (Amazon S3) that is used in the evaluation.
evaluation_inputDataLocationS3 :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Text)
evaluation_inputDataLocationS3 = Lens.lens (\Evaluation' {inputDataLocationS3} -> inputDataLocationS3) (\s@Evaluation' {} a -> s {inputDataLocationS3 = a} :: Evaluation)

-- | Undocumented member.
evaluation_computeTime :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Integer)
evaluation_computeTime = Lens.lens (\Evaluation' {computeTime} -> computeTime) (\s@Evaluation' {} a -> s {computeTime = a} :: Evaluation)

-- | The time of the most recent edit to the @Evaluation@. The time is
-- expressed in epoch time.
evaluation_lastUpdatedAt :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.UTCTime)
evaluation_lastUpdatedAt = Lens.lens (\Evaluation' {lastUpdatedAt} -> lastUpdatedAt) (\s@Evaluation' {} a -> s {lastUpdatedAt = a} :: Evaluation) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON Evaluation where
  parseJSON =
    Prelude.withObject
      "Evaluation"
      ( \x ->
          Evaluation'
            Prelude.<$> (x Prelude..:? "PerformanceMetrics")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "StartedAt")
            Prelude.<*> (x Prelude..:? "EvaluationDataSourceId")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "CreatedAt")
            Prelude.<*> (x Prelude..:? "FinishedAt")
            Prelude.<*> (x Prelude..:? "CreatedByIamUser")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "EvaluationId")
            Prelude.<*> (x Prelude..:? "MLModelId")
            Prelude.<*> (x Prelude..:? "InputDataLocationS3")
            Prelude.<*> (x Prelude..:? "ComputeTime")
            Prelude.<*> (x Prelude..:? "LastUpdatedAt")
      )

instance Prelude.Hashable Evaluation

instance Prelude.NFData Evaluation
