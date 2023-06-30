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
-- Module      : Amazonka.MachineLearning.Types.Evaluation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.Evaluation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types.EntityStatus
import Amazonka.MachineLearning.Types.PerformanceMetrics
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of @GetEvaluation@ operation.
--
-- The content consists of the detailed metadata and data file information
-- and the current status of the @Evaluation@.
--
-- /See:/ 'newEvaluation' smart constructor.
data Evaluation = Evaluation'
  { computeTime :: Prelude.Maybe Prelude.Integer,
    -- | The time that the @Evaluation@ was created. The time is expressed in
    -- epoch time.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The AWS user account that invoked the evaluation. The account type can
    -- be either an AWS root account or an AWS Identity and Access Management
    -- (IAM) user account.
    createdByIamUser :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @DataSource@ that is used to evaluate the @MLModel@.
    evaluationDataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The ID that is assigned to the @Evaluation@ at creation.
    evaluationId :: Prelude.Maybe Prelude.Text,
    finishedAt :: Prelude.Maybe Data.POSIX,
    -- | The location and name of the data in Amazon Simple Storage Server
    -- (Amazon S3) that is used in the evaluation.
    inputDataLocationS3 :: Prelude.Maybe Prelude.Text,
    -- | The time of the most recent edit to the @Evaluation@. The time is
    -- expressed in epoch time.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The ID of the @MLModel@ that is the focus of the evaluation.
    mLModelId :: Prelude.Maybe Prelude.Text,
    -- | A description of the most recent details about evaluating the @MLModel@.
    message :: Prelude.Maybe Prelude.Text,
    -- | A user-supplied name or description of the @Evaluation@.
    name :: Prelude.Maybe Prelude.Text,
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
    -- <https://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
    performanceMetrics :: Prelude.Maybe PerformanceMetrics,
    startedAt :: Prelude.Maybe Data.POSIX,
    -- | The status of the evaluation. This element can have one of the following
    -- values:
    --
    -- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
    --     to evaluate an @MLModel@.
    --
    -- -   @INPROGRESS@ - The evaluation is underway.
    --
    -- -   @FAILED@ - The request to evaluate an @MLModel@ did not run to
    --     completion. It is not usable.
    --
    -- -   @COMPLETED@ - The evaluation process completed successfully.
    --
    -- -   @DELETED@ - The @Evaluation@ is marked as deleted. It is not usable.
    status :: Prelude.Maybe EntityStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Evaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeTime', 'evaluation_computeTime' - Undocumented member.
--
-- 'createdAt', 'evaluation_createdAt' - The time that the @Evaluation@ was created. The time is expressed in
-- epoch time.
--
-- 'createdByIamUser', 'evaluation_createdByIamUser' - The AWS user account that invoked the evaluation. The account type can
-- be either an AWS root account or an AWS Identity and Access Management
-- (IAM) user account.
--
-- 'evaluationDataSourceId', 'evaluation_evaluationDataSourceId' - The ID of the @DataSource@ that is used to evaluate the @MLModel@.
--
-- 'evaluationId', 'evaluation_evaluationId' - The ID that is assigned to the @Evaluation@ at creation.
--
-- 'finishedAt', 'evaluation_finishedAt' - Undocumented member.
--
-- 'inputDataLocationS3', 'evaluation_inputDataLocationS3' - The location and name of the data in Amazon Simple Storage Server
-- (Amazon S3) that is used in the evaluation.
--
-- 'lastUpdatedAt', 'evaluation_lastUpdatedAt' - The time of the most recent edit to the @Evaluation@. The time is
-- expressed in epoch time.
--
-- 'mLModelId', 'evaluation_mLModelId' - The ID of the @MLModel@ that is the focus of the evaluation.
--
-- 'message', 'evaluation_message' - A description of the most recent details about evaluating the @MLModel@.
--
-- 'name', 'evaluation_name' - A user-supplied name or description of the @Evaluation@.
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
-- <https://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
--
-- 'startedAt', 'evaluation_startedAt' - Undocumented member.
--
-- 'status', 'evaluation_status' - The status of the evaluation. This element can have one of the following
-- values:
--
-- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
--     to evaluate an @MLModel@.
--
-- -   @INPROGRESS@ - The evaluation is underway.
--
-- -   @FAILED@ - The request to evaluate an @MLModel@ did not run to
--     completion. It is not usable.
--
-- -   @COMPLETED@ - The evaluation process completed successfully.
--
-- -   @DELETED@ - The @Evaluation@ is marked as deleted. It is not usable.
newEvaluation ::
  Evaluation
newEvaluation =
  Evaluation'
    { computeTime = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      createdByIamUser = Prelude.Nothing,
      evaluationDataSourceId = Prelude.Nothing,
      evaluationId = Prelude.Nothing,
      finishedAt = Prelude.Nothing,
      inputDataLocationS3 = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      mLModelId = Prelude.Nothing,
      message = Prelude.Nothing,
      name = Prelude.Nothing,
      performanceMetrics = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Undocumented member.
evaluation_computeTime :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Integer)
evaluation_computeTime = Lens.lens (\Evaluation' {computeTime} -> computeTime) (\s@Evaluation' {} a -> s {computeTime = a} :: Evaluation)

-- | The time that the @Evaluation@ was created. The time is expressed in
-- epoch time.
evaluation_createdAt :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.UTCTime)
evaluation_createdAt = Lens.lens (\Evaluation' {createdAt} -> createdAt) (\s@Evaluation' {} a -> s {createdAt = a} :: Evaluation) Prelude.. Lens.mapping Data._Time

-- | The AWS user account that invoked the evaluation. The account type can
-- be either an AWS root account or an AWS Identity and Access Management
-- (IAM) user account.
evaluation_createdByIamUser :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Text)
evaluation_createdByIamUser = Lens.lens (\Evaluation' {createdByIamUser} -> createdByIamUser) (\s@Evaluation' {} a -> s {createdByIamUser = a} :: Evaluation)

-- | The ID of the @DataSource@ that is used to evaluate the @MLModel@.
evaluation_evaluationDataSourceId :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Text)
evaluation_evaluationDataSourceId = Lens.lens (\Evaluation' {evaluationDataSourceId} -> evaluationDataSourceId) (\s@Evaluation' {} a -> s {evaluationDataSourceId = a} :: Evaluation)

-- | The ID that is assigned to the @Evaluation@ at creation.
evaluation_evaluationId :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Text)
evaluation_evaluationId = Lens.lens (\Evaluation' {evaluationId} -> evaluationId) (\s@Evaluation' {} a -> s {evaluationId = a} :: Evaluation)

-- | Undocumented member.
evaluation_finishedAt :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.UTCTime)
evaluation_finishedAt = Lens.lens (\Evaluation' {finishedAt} -> finishedAt) (\s@Evaluation' {} a -> s {finishedAt = a} :: Evaluation) Prelude.. Lens.mapping Data._Time

-- | The location and name of the data in Amazon Simple Storage Server
-- (Amazon S3) that is used in the evaluation.
evaluation_inputDataLocationS3 :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Text)
evaluation_inputDataLocationS3 = Lens.lens (\Evaluation' {inputDataLocationS3} -> inputDataLocationS3) (\s@Evaluation' {} a -> s {inputDataLocationS3 = a} :: Evaluation)

-- | The time of the most recent edit to the @Evaluation@. The time is
-- expressed in epoch time.
evaluation_lastUpdatedAt :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.UTCTime)
evaluation_lastUpdatedAt = Lens.lens (\Evaluation' {lastUpdatedAt} -> lastUpdatedAt) (\s@Evaluation' {} a -> s {lastUpdatedAt = a} :: Evaluation) Prelude.. Lens.mapping Data._Time

-- | The ID of the @MLModel@ that is the focus of the evaluation.
evaluation_mLModelId :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Text)
evaluation_mLModelId = Lens.lens (\Evaluation' {mLModelId} -> mLModelId) (\s@Evaluation' {} a -> s {mLModelId = a} :: Evaluation)

-- | A description of the most recent details about evaluating the @MLModel@.
evaluation_message :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Text)
evaluation_message = Lens.lens (\Evaluation' {message} -> message) (\s@Evaluation' {} a -> s {message = a} :: Evaluation)

-- | A user-supplied name or description of the @Evaluation@.
evaluation_name :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Text)
evaluation_name = Lens.lens (\Evaluation' {name} -> name) (\s@Evaluation' {} a -> s {name = a} :: Evaluation)

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
-- <https://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
evaluation_performanceMetrics :: Lens.Lens' Evaluation (Prelude.Maybe PerformanceMetrics)
evaluation_performanceMetrics = Lens.lens (\Evaluation' {performanceMetrics} -> performanceMetrics) (\s@Evaluation' {} a -> s {performanceMetrics = a} :: Evaluation)

-- | Undocumented member.
evaluation_startedAt :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.UTCTime)
evaluation_startedAt = Lens.lens (\Evaluation' {startedAt} -> startedAt) (\s@Evaluation' {} a -> s {startedAt = a} :: Evaluation) Prelude.. Lens.mapping Data._Time

-- | The status of the evaluation. This element can have one of the following
-- values:
--
-- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
--     to evaluate an @MLModel@.
--
-- -   @INPROGRESS@ - The evaluation is underway.
--
-- -   @FAILED@ - The request to evaluate an @MLModel@ did not run to
--     completion. It is not usable.
--
-- -   @COMPLETED@ - The evaluation process completed successfully.
--
-- -   @DELETED@ - The @Evaluation@ is marked as deleted. It is not usable.
evaluation_status :: Lens.Lens' Evaluation (Prelude.Maybe EntityStatus)
evaluation_status = Lens.lens (\Evaluation' {status} -> status) (\s@Evaluation' {} a -> s {status = a} :: Evaluation)

instance Data.FromJSON Evaluation where
  parseJSON =
    Data.withObject
      "Evaluation"
      ( \x ->
          Evaluation'
            Prelude.<$> (x Data..:? "ComputeTime")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "CreatedByIamUser")
            Prelude.<*> (x Data..:? "EvaluationDataSourceId")
            Prelude.<*> (x Data..:? "EvaluationId")
            Prelude.<*> (x Data..:? "FinishedAt")
            Prelude.<*> (x Data..:? "InputDataLocationS3")
            Prelude.<*> (x Data..:? "LastUpdatedAt")
            Prelude.<*> (x Data..:? "MLModelId")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PerformanceMetrics")
            Prelude.<*> (x Data..:? "StartedAt")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable Evaluation where
  hashWithSalt _salt Evaluation' {..} =
    _salt
      `Prelude.hashWithSalt` computeTime
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` createdByIamUser
      `Prelude.hashWithSalt` evaluationDataSourceId
      `Prelude.hashWithSalt` evaluationId
      `Prelude.hashWithSalt` finishedAt
      `Prelude.hashWithSalt` inputDataLocationS3
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` mLModelId
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` performanceMetrics
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` status

instance Prelude.NFData Evaluation where
  rnf Evaluation' {..} =
    Prelude.rnf computeTime
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf createdByIamUser
      `Prelude.seq` Prelude.rnf evaluationDataSourceId
      `Prelude.seq` Prelude.rnf evaluationId
      `Prelude.seq` Prelude.rnf finishedAt
      `Prelude.seq` Prelude.rnf inputDataLocationS3
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf mLModelId
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf performanceMetrics
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf status
