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
-- Module      : Amazonka.SageMaker.Types.HyperParameterTrainingJobSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTrainingJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric
import Amazonka.SageMaker.Types.ObjectiveStatus
import Amazonka.SageMaker.Types.TrainingJobStatus

-- | The container for the summary information about a training job.
--
-- /See:/ 'newHyperParameterTrainingJobSummary' smart constructor.
data HyperParameterTrainingJobSummary = HyperParameterTrainingJobSummary'
  { -- | The date and time that the training job started.
    trainingStartTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the objective metric for the training job:
    --
    -- -   Succeeded: The final objective metric for the training job was
    --     evaluated by the hyperparameter tuning job and used in the
    --     hyperparameter tuning process.
    --
    -- -   Pending: The training job is in progress and evaluation of its final
    --     objective metric is pending.
    --
    -- -   Failed: The final objective metric for the training job was not
    --     evaluated, and was not used in the hyperparameter tuning process.
    --     This typically occurs when the training job failed or did not emit
    --     an objective metric.
    objectiveStatus :: Prelude.Maybe ObjectiveStatus,
    -- | The HyperParameter tuning job that launched the training job.
    tuningJobName :: Prelude.Maybe Prelude.Text,
    -- | The training job definition name.
    trainingJobDefinitionName :: Prelude.Maybe Prelude.Text,
    -- | The FinalHyperParameterTuningJobObjectiveMetric object that specifies
    -- the value of the objective metric of the tuning job that launched this
    -- training job.
    finalHyperParameterTuningJobObjectiveMetric :: Prelude.Maybe FinalHyperParameterTuningJobObjectiveMetric,
    -- | The reason that the training job failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time when the training job ends on training instances. You
    -- are billed for the time interval between the value of
    -- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
    -- this is the time after model artifacts are uploaded. For failed jobs,
    -- this is the time when SageMaker detects a job failure.
    trainingEndTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the training job.
    trainingJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Prelude.Text,
    -- | The date and time that the training job was created.
    creationTime :: Core.POSIX,
    -- | The status of the training job.
    trainingJobStatus :: TrainingJobStatus,
    -- | A list of the hyperparameters for which you specified ranges to search.
    tunedHyperParameters :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterTrainingJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trainingStartTime', 'hyperParameterTrainingJobSummary_trainingStartTime' - The date and time that the training job started.
--
-- 'objectiveStatus', 'hyperParameterTrainingJobSummary_objectiveStatus' - The status of the objective metric for the training job:
--
-- -   Succeeded: The final objective metric for the training job was
--     evaluated by the hyperparameter tuning job and used in the
--     hyperparameter tuning process.
--
-- -   Pending: The training job is in progress and evaluation of its final
--     objective metric is pending.
--
-- -   Failed: The final objective metric for the training job was not
--     evaluated, and was not used in the hyperparameter tuning process.
--     This typically occurs when the training job failed or did not emit
--     an objective metric.
--
-- 'tuningJobName', 'hyperParameterTrainingJobSummary_tuningJobName' - The HyperParameter tuning job that launched the training job.
--
-- 'trainingJobDefinitionName', 'hyperParameterTrainingJobSummary_trainingJobDefinitionName' - The training job definition name.
--
-- 'finalHyperParameterTuningJobObjectiveMetric', 'hyperParameterTrainingJobSummary_finalHyperParameterTuningJobObjectiveMetric' - The FinalHyperParameterTuningJobObjectiveMetric object that specifies
-- the value of the objective metric of the tuning job that launched this
-- training job.
--
-- 'failureReason', 'hyperParameterTrainingJobSummary_failureReason' - The reason that the training job failed.
--
-- 'trainingEndTime', 'hyperParameterTrainingJobSummary_trainingEndTime' - Specifies the time when the training job ends on training instances. You
-- are billed for the time interval between the value of
-- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
-- this is the time after model artifacts are uploaded. For failed jobs,
-- this is the time when SageMaker detects a job failure.
--
-- 'trainingJobName', 'hyperParameterTrainingJobSummary_trainingJobName' - The name of the training job.
--
-- 'trainingJobArn', 'hyperParameterTrainingJobSummary_trainingJobArn' - The Amazon Resource Name (ARN) of the training job.
--
-- 'creationTime', 'hyperParameterTrainingJobSummary_creationTime' - The date and time that the training job was created.
--
-- 'trainingJobStatus', 'hyperParameterTrainingJobSummary_trainingJobStatus' - The status of the training job.
--
-- 'tunedHyperParameters', 'hyperParameterTrainingJobSummary_tunedHyperParameters' - A list of the hyperparameters for which you specified ranges to search.
newHyperParameterTrainingJobSummary ::
  -- | 'trainingJobName'
  Prelude.Text ->
  -- | 'trainingJobArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'trainingJobStatus'
  TrainingJobStatus ->
  HyperParameterTrainingJobSummary
newHyperParameterTrainingJobSummary
  pTrainingJobName_
  pTrainingJobArn_
  pCreationTime_
  pTrainingJobStatus_ =
    HyperParameterTrainingJobSummary'
      { trainingStartTime =
          Prelude.Nothing,
        objectiveStatus = Prelude.Nothing,
        tuningJobName = Prelude.Nothing,
        trainingJobDefinitionName =
          Prelude.Nothing,
        finalHyperParameterTuningJobObjectiveMetric =
          Prelude.Nothing,
        failureReason = Prelude.Nothing,
        trainingEndTime = Prelude.Nothing,
        trainingJobName = pTrainingJobName_,
        trainingJobArn = pTrainingJobArn_,
        creationTime =
          Core._Time Lens.# pCreationTime_,
        trainingJobStatus = pTrainingJobStatus_,
        tunedHyperParameters = Prelude.mempty
      }

-- | The date and time that the training job started.
hyperParameterTrainingJobSummary_trainingStartTime :: Lens.Lens' HyperParameterTrainingJobSummary (Prelude.Maybe Prelude.UTCTime)
hyperParameterTrainingJobSummary_trainingStartTime = Lens.lens (\HyperParameterTrainingJobSummary' {trainingStartTime} -> trainingStartTime) (\s@HyperParameterTrainingJobSummary' {} a -> s {trainingStartTime = a} :: HyperParameterTrainingJobSummary) Prelude.. Lens.mapping Core._Time

-- | The status of the objective metric for the training job:
--
-- -   Succeeded: The final objective metric for the training job was
--     evaluated by the hyperparameter tuning job and used in the
--     hyperparameter tuning process.
--
-- -   Pending: The training job is in progress and evaluation of its final
--     objective metric is pending.
--
-- -   Failed: The final objective metric for the training job was not
--     evaluated, and was not used in the hyperparameter tuning process.
--     This typically occurs when the training job failed or did not emit
--     an objective metric.
hyperParameterTrainingJobSummary_objectiveStatus :: Lens.Lens' HyperParameterTrainingJobSummary (Prelude.Maybe ObjectiveStatus)
hyperParameterTrainingJobSummary_objectiveStatus = Lens.lens (\HyperParameterTrainingJobSummary' {objectiveStatus} -> objectiveStatus) (\s@HyperParameterTrainingJobSummary' {} a -> s {objectiveStatus = a} :: HyperParameterTrainingJobSummary)

-- | The HyperParameter tuning job that launched the training job.
hyperParameterTrainingJobSummary_tuningJobName :: Lens.Lens' HyperParameterTrainingJobSummary (Prelude.Maybe Prelude.Text)
hyperParameterTrainingJobSummary_tuningJobName = Lens.lens (\HyperParameterTrainingJobSummary' {tuningJobName} -> tuningJobName) (\s@HyperParameterTrainingJobSummary' {} a -> s {tuningJobName = a} :: HyperParameterTrainingJobSummary)

-- | The training job definition name.
hyperParameterTrainingJobSummary_trainingJobDefinitionName :: Lens.Lens' HyperParameterTrainingJobSummary (Prelude.Maybe Prelude.Text)
hyperParameterTrainingJobSummary_trainingJobDefinitionName = Lens.lens (\HyperParameterTrainingJobSummary' {trainingJobDefinitionName} -> trainingJobDefinitionName) (\s@HyperParameterTrainingJobSummary' {} a -> s {trainingJobDefinitionName = a} :: HyperParameterTrainingJobSummary)

-- | The FinalHyperParameterTuningJobObjectiveMetric object that specifies
-- the value of the objective metric of the tuning job that launched this
-- training job.
hyperParameterTrainingJobSummary_finalHyperParameterTuningJobObjectiveMetric :: Lens.Lens' HyperParameterTrainingJobSummary (Prelude.Maybe FinalHyperParameterTuningJobObjectiveMetric)
hyperParameterTrainingJobSummary_finalHyperParameterTuningJobObjectiveMetric = Lens.lens (\HyperParameterTrainingJobSummary' {finalHyperParameterTuningJobObjectiveMetric} -> finalHyperParameterTuningJobObjectiveMetric) (\s@HyperParameterTrainingJobSummary' {} a -> s {finalHyperParameterTuningJobObjectiveMetric = a} :: HyperParameterTrainingJobSummary)

-- | The reason that the training job failed.
hyperParameterTrainingJobSummary_failureReason :: Lens.Lens' HyperParameterTrainingJobSummary (Prelude.Maybe Prelude.Text)
hyperParameterTrainingJobSummary_failureReason = Lens.lens (\HyperParameterTrainingJobSummary' {failureReason} -> failureReason) (\s@HyperParameterTrainingJobSummary' {} a -> s {failureReason = a} :: HyperParameterTrainingJobSummary)

-- | Specifies the time when the training job ends on training instances. You
-- are billed for the time interval between the value of
-- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
-- this is the time after model artifacts are uploaded. For failed jobs,
-- this is the time when SageMaker detects a job failure.
hyperParameterTrainingJobSummary_trainingEndTime :: Lens.Lens' HyperParameterTrainingJobSummary (Prelude.Maybe Prelude.UTCTime)
hyperParameterTrainingJobSummary_trainingEndTime = Lens.lens (\HyperParameterTrainingJobSummary' {trainingEndTime} -> trainingEndTime) (\s@HyperParameterTrainingJobSummary' {} a -> s {trainingEndTime = a} :: HyperParameterTrainingJobSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the training job.
hyperParameterTrainingJobSummary_trainingJobName :: Lens.Lens' HyperParameterTrainingJobSummary Prelude.Text
hyperParameterTrainingJobSummary_trainingJobName = Lens.lens (\HyperParameterTrainingJobSummary' {trainingJobName} -> trainingJobName) (\s@HyperParameterTrainingJobSummary' {} a -> s {trainingJobName = a} :: HyperParameterTrainingJobSummary)

-- | The Amazon Resource Name (ARN) of the training job.
hyperParameterTrainingJobSummary_trainingJobArn :: Lens.Lens' HyperParameterTrainingJobSummary Prelude.Text
hyperParameterTrainingJobSummary_trainingJobArn = Lens.lens (\HyperParameterTrainingJobSummary' {trainingJobArn} -> trainingJobArn) (\s@HyperParameterTrainingJobSummary' {} a -> s {trainingJobArn = a} :: HyperParameterTrainingJobSummary)

-- | The date and time that the training job was created.
hyperParameterTrainingJobSummary_creationTime :: Lens.Lens' HyperParameterTrainingJobSummary Prelude.UTCTime
hyperParameterTrainingJobSummary_creationTime = Lens.lens (\HyperParameterTrainingJobSummary' {creationTime} -> creationTime) (\s@HyperParameterTrainingJobSummary' {} a -> s {creationTime = a} :: HyperParameterTrainingJobSummary) Prelude.. Core._Time

-- | The status of the training job.
hyperParameterTrainingJobSummary_trainingJobStatus :: Lens.Lens' HyperParameterTrainingJobSummary TrainingJobStatus
hyperParameterTrainingJobSummary_trainingJobStatus = Lens.lens (\HyperParameterTrainingJobSummary' {trainingJobStatus} -> trainingJobStatus) (\s@HyperParameterTrainingJobSummary' {} a -> s {trainingJobStatus = a} :: HyperParameterTrainingJobSummary)

-- | A list of the hyperparameters for which you specified ranges to search.
hyperParameterTrainingJobSummary_tunedHyperParameters :: Lens.Lens' HyperParameterTrainingJobSummary (Prelude.HashMap Prelude.Text Prelude.Text)
hyperParameterTrainingJobSummary_tunedHyperParameters = Lens.lens (\HyperParameterTrainingJobSummary' {tunedHyperParameters} -> tunedHyperParameters) (\s@HyperParameterTrainingJobSummary' {} a -> s {tunedHyperParameters = a} :: HyperParameterTrainingJobSummary) Prelude.. Lens.coerced

instance
  Core.FromJSON
    HyperParameterTrainingJobSummary
  where
  parseJSON =
    Core.withObject
      "HyperParameterTrainingJobSummary"
      ( \x ->
          HyperParameterTrainingJobSummary'
            Prelude.<$> (x Core..:? "TrainingStartTime")
            Prelude.<*> (x Core..:? "ObjectiveStatus")
            Prelude.<*> (x Core..:? "TuningJobName")
            Prelude.<*> (x Core..:? "TrainingJobDefinitionName")
            Prelude.<*> ( x
                            Core..:? "FinalHyperParameterTuningJobObjectiveMetric"
                        )
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "TrainingEndTime")
            Prelude.<*> (x Core..: "TrainingJobName")
            Prelude.<*> (x Core..: "TrainingJobArn")
            Prelude.<*> (x Core..: "CreationTime")
            Prelude.<*> (x Core..: "TrainingJobStatus")
            Prelude.<*> ( x Core..:? "TunedHyperParameters"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    HyperParameterTrainingJobSummary
  where
  hashWithSalt
    _salt
    HyperParameterTrainingJobSummary' {..} =
      _salt `Prelude.hashWithSalt` trainingStartTime
        `Prelude.hashWithSalt` objectiveStatus
        `Prelude.hashWithSalt` tuningJobName
        `Prelude.hashWithSalt` trainingJobDefinitionName
        `Prelude.hashWithSalt` finalHyperParameterTuningJobObjectiveMetric
        `Prelude.hashWithSalt` failureReason
        `Prelude.hashWithSalt` trainingEndTime
        `Prelude.hashWithSalt` trainingJobName
        `Prelude.hashWithSalt` trainingJobArn
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` trainingJobStatus
        `Prelude.hashWithSalt` tunedHyperParameters

instance
  Prelude.NFData
    HyperParameterTrainingJobSummary
  where
  rnf HyperParameterTrainingJobSummary' {..} =
    Prelude.rnf trainingStartTime
      `Prelude.seq` Prelude.rnf objectiveStatus
      `Prelude.seq` Prelude.rnf tuningJobName
      `Prelude.seq` Prelude.rnf trainingJobDefinitionName
      `Prelude.seq` Prelude.rnf
        finalHyperParameterTuningJobObjectiveMetric
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf trainingEndTime
      `Prelude.seq` Prelude.rnf trainingJobName
      `Prelude.seq` Prelude.rnf trainingJobArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf trainingJobStatus
      `Prelude.seq` Prelude.rnf tunedHyperParameters
