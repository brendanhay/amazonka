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
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTrainingJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTrainingJobSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric
import Network.AWS.SageMaker.Types.ObjectiveStatus
import Network.AWS.SageMaker.Types.TrainingJobStatus

-- | Specifies summary information about a training job.
--
-- /See:/ 'newHyperParameterTrainingJobSummary' smart constructor.
data HyperParameterTrainingJobSummary = HyperParameterTrainingJobSummary'
  { -- | The FinalHyperParameterTuningJobObjectiveMetric object that specifies
    -- the value of the objective metric of the tuning job that launched this
    -- training job.
    finalHyperParameterTuningJobObjectiveMetric :: Core.Maybe FinalHyperParameterTuningJobObjectiveMetric,
    -- | The HyperParameter tuning job that launched the training job.
    tuningJobName :: Core.Maybe Core.Text,
    -- | The reason that the training job failed.
    failureReason :: Core.Maybe Core.Text,
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
    objectiveStatus :: Core.Maybe ObjectiveStatus,
    -- | The training job definition name.
    trainingJobDefinitionName :: Core.Maybe Core.Text,
    -- | The date and time that the training job started.
    trainingStartTime :: Core.Maybe Core.POSIX,
    -- | Specifies the time when the training job ends on training instances. You
    -- are billed for the time interval between the value of
    -- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
    -- this is the time after model artifacts are uploaded. For failed jobs,
    -- this is the time when Amazon SageMaker detects a job failure.
    trainingEndTime :: Core.Maybe Core.POSIX,
    -- | The name of the training job.
    trainingJobName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Core.Text,
    -- | The date and time that the training job was created.
    creationTime :: Core.POSIX,
    -- | The status of the training job.
    trainingJobStatus :: TrainingJobStatus,
    -- | A list of the hyperparameters for which you specified ranges to search.
    tunedHyperParameters :: Core.HashMap Core.Text Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HyperParameterTrainingJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalHyperParameterTuningJobObjectiveMetric', 'hyperParameterTrainingJobSummary_finalHyperParameterTuningJobObjectiveMetric' - The FinalHyperParameterTuningJobObjectiveMetric object that specifies
-- the value of the objective metric of the tuning job that launched this
-- training job.
--
-- 'tuningJobName', 'hyperParameterTrainingJobSummary_tuningJobName' - The HyperParameter tuning job that launched the training job.
--
-- 'failureReason', 'hyperParameterTrainingJobSummary_failureReason' - The reason that the training job failed.
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
-- 'trainingJobDefinitionName', 'hyperParameterTrainingJobSummary_trainingJobDefinitionName' - The training job definition name.
--
-- 'trainingStartTime', 'hyperParameterTrainingJobSummary_trainingStartTime' - The date and time that the training job started.
--
-- 'trainingEndTime', 'hyperParameterTrainingJobSummary_trainingEndTime' - Specifies the time when the training job ends on training instances. You
-- are billed for the time interval between the value of
-- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
-- this is the time after model artifacts are uploaded. For failed jobs,
-- this is the time when Amazon SageMaker detects a job failure.
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
  Core.Text ->
  -- | 'trainingJobArn'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'trainingJobStatus'
  TrainingJobStatus ->
  HyperParameterTrainingJobSummary
newHyperParameterTrainingJobSummary
  pTrainingJobName_
  pTrainingJobArn_
  pCreationTime_
  pTrainingJobStatus_ =
    HyperParameterTrainingJobSummary'
      { finalHyperParameterTuningJobObjectiveMetric =
          Core.Nothing,
        tuningJobName = Core.Nothing,
        failureReason = Core.Nothing,
        objectiveStatus = Core.Nothing,
        trainingJobDefinitionName = Core.Nothing,
        trainingStartTime = Core.Nothing,
        trainingEndTime = Core.Nothing,
        trainingJobName = pTrainingJobName_,
        trainingJobArn = pTrainingJobArn_,
        creationTime =
          Core._Time Lens.# pCreationTime_,
        trainingJobStatus = pTrainingJobStatus_,
        tunedHyperParameters = Core.mempty
      }

-- | The FinalHyperParameterTuningJobObjectiveMetric object that specifies
-- the value of the objective metric of the tuning job that launched this
-- training job.
hyperParameterTrainingJobSummary_finalHyperParameterTuningJobObjectiveMetric :: Lens.Lens' HyperParameterTrainingJobSummary (Core.Maybe FinalHyperParameterTuningJobObjectiveMetric)
hyperParameterTrainingJobSummary_finalHyperParameterTuningJobObjectiveMetric = Lens.lens (\HyperParameterTrainingJobSummary' {finalHyperParameterTuningJobObjectiveMetric} -> finalHyperParameterTuningJobObjectiveMetric) (\s@HyperParameterTrainingJobSummary' {} a -> s {finalHyperParameterTuningJobObjectiveMetric = a} :: HyperParameterTrainingJobSummary)

-- | The HyperParameter tuning job that launched the training job.
hyperParameterTrainingJobSummary_tuningJobName :: Lens.Lens' HyperParameterTrainingJobSummary (Core.Maybe Core.Text)
hyperParameterTrainingJobSummary_tuningJobName = Lens.lens (\HyperParameterTrainingJobSummary' {tuningJobName} -> tuningJobName) (\s@HyperParameterTrainingJobSummary' {} a -> s {tuningJobName = a} :: HyperParameterTrainingJobSummary)

-- | The reason that the training job failed.
hyperParameterTrainingJobSummary_failureReason :: Lens.Lens' HyperParameterTrainingJobSummary (Core.Maybe Core.Text)
hyperParameterTrainingJobSummary_failureReason = Lens.lens (\HyperParameterTrainingJobSummary' {failureReason} -> failureReason) (\s@HyperParameterTrainingJobSummary' {} a -> s {failureReason = a} :: HyperParameterTrainingJobSummary)

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
hyperParameterTrainingJobSummary_objectiveStatus :: Lens.Lens' HyperParameterTrainingJobSummary (Core.Maybe ObjectiveStatus)
hyperParameterTrainingJobSummary_objectiveStatus = Lens.lens (\HyperParameterTrainingJobSummary' {objectiveStatus} -> objectiveStatus) (\s@HyperParameterTrainingJobSummary' {} a -> s {objectiveStatus = a} :: HyperParameterTrainingJobSummary)

-- | The training job definition name.
hyperParameterTrainingJobSummary_trainingJobDefinitionName :: Lens.Lens' HyperParameterTrainingJobSummary (Core.Maybe Core.Text)
hyperParameterTrainingJobSummary_trainingJobDefinitionName = Lens.lens (\HyperParameterTrainingJobSummary' {trainingJobDefinitionName} -> trainingJobDefinitionName) (\s@HyperParameterTrainingJobSummary' {} a -> s {trainingJobDefinitionName = a} :: HyperParameterTrainingJobSummary)

-- | The date and time that the training job started.
hyperParameterTrainingJobSummary_trainingStartTime :: Lens.Lens' HyperParameterTrainingJobSummary (Core.Maybe Core.UTCTime)
hyperParameterTrainingJobSummary_trainingStartTime = Lens.lens (\HyperParameterTrainingJobSummary' {trainingStartTime} -> trainingStartTime) (\s@HyperParameterTrainingJobSummary' {} a -> s {trainingStartTime = a} :: HyperParameterTrainingJobSummary) Core.. Lens.mapping Core._Time

-- | Specifies the time when the training job ends on training instances. You
-- are billed for the time interval between the value of
-- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
-- this is the time after model artifacts are uploaded. For failed jobs,
-- this is the time when Amazon SageMaker detects a job failure.
hyperParameterTrainingJobSummary_trainingEndTime :: Lens.Lens' HyperParameterTrainingJobSummary (Core.Maybe Core.UTCTime)
hyperParameterTrainingJobSummary_trainingEndTime = Lens.lens (\HyperParameterTrainingJobSummary' {trainingEndTime} -> trainingEndTime) (\s@HyperParameterTrainingJobSummary' {} a -> s {trainingEndTime = a} :: HyperParameterTrainingJobSummary) Core.. Lens.mapping Core._Time

-- | The name of the training job.
hyperParameterTrainingJobSummary_trainingJobName :: Lens.Lens' HyperParameterTrainingJobSummary Core.Text
hyperParameterTrainingJobSummary_trainingJobName = Lens.lens (\HyperParameterTrainingJobSummary' {trainingJobName} -> trainingJobName) (\s@HyperParameterTrainingJobSummary' {} a -> s {trainingJobName = a} :: HyperParameterTrainingJobSummary)

-- | The Amazon Resource Name (ARN) of the training job.
hyperParameterTrainingJobSummary_trainingJobArn :: Lens.Lens' HyperParameterTrainingJobSummary Core.Text
hyperParameterTrainingJobSummary_trainingJobArn = Lens.lens (\HyperParameterTrainingJobSummary' {trainingJobArn} -> trainingJobArn) (\s@HyperParameterTrainingJobSummary' {} a -> s {trainingJobArn = a} :: HyperParameterTrainingJobSummary)

-- | The date and time that the training job was created.
hyperParameterTrainingJobSummary_creationTime :: Lens.Lens' HyperParameterTrainingJobSummary Core.UTCTime
hyperParameterTrainingJobSummary_creationTime = Lens.lens (\HyperParameterTrainingJobSummary' {creationTime} -> creationTime) (\s@HyperParameterTrainingJobSummary' {} a -> s {creationTime = a} :: HyperParameterTrainingJobSummary) Core.. Core._Time

-- | The status of the training job.
hyperParameterTrainingJobSummary_trainingJobStatus :: Lens.Lens' HyperParameterTrainingJobSummary TrainingJobStatus
hyperParameterTrainingJobSummary_trainingJobStatus = Lens.lens (\HyperParameterTrainingJobSummary' {trainingJobStatus} -> trainingJobStatus) (\s@HyperParameterTrainingJobSummary' {} a -> s {trainingJobStatus = a} :: HyperParameterTrainingJobSummary)

-- | A list of the hyperparameters for which you specified ranges to search.
hyperParameterTrainingJobSummary_tunedHyperParameters :: Lens.Lens' HyperParameterTrainingJobSummary (Core.HashMap Core.Text Core.Text)
hyperParameterTrainingJobSummary_tunedHyperParameters = Lens.lens (\HyperParameterTrainingJobSummary' {tunedHyperParameters} -> tunedHyperParameters) (\s@HyperParameterTrainingJobSummary' {} a -> s {tunedHyperParameters = a} :: HyperParameterTrainingJobSummary) Core.. Lens._Coerce

instance
  Core.FromJSON
    HyperParameterTrainingJobSummary
  where
  parseJSON =
    Core.withObject
      "HyperParameterTrainingJobSummary"
      ( \x ->
          HyperParameterTrainingJobSummary'
            Core.<$> ( x
                         Core..:? "FinalHyperParameterTuningJobObjectiveMetric"
                     )
            Core.<*> (x Core..:? "TuningJobName")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "ObjectiveStatus")
            Core.<*> (x Core..:? "TrainingJobDefinitionName")
            Core.<*> (x Core..:? "TrainingStartTime")
            Core.<*> (x Core..:? "TrainingEndTime")
            Core.<*> (x Core..: "TrainingJobName")
            Core.<*> (x Core..: "TrainingJobArn")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "TrainingJobStatus")
            Core.<*> ( x Core..:? "TunedHyperParameters"
                         Core..!= Core.mempty
                     )
      )

instance
  Core.Hashable
    HyperParameterTrainingJobSummary

instance Core.NFData HyperParameterTrainingJobSummary
