{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTrainingJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTrainingJobSummary
  ( HyperParameterTrainingJobSummary (..),

    -- * Smart constructor
    mkHyperParameterTrainingJobSummary,

    -- * Lenses
    hptjsTrainingJobName,
    hptjsTrainingJobArn,
    hptjsCreationTime,
    hptjsTrainingJobStatus,
    hptjsTunedHyperParameters,
    hptjsFailureReason,
    hptjsFinalHyperParameterTuningJobObjectiveMetric,
    hptjsObjectiveStatus,
    hptjsTrainingEndTime,
    hptjsTrainingJobDefinitionName,
    hptjsTrainingStartTime,
    hptjsTuningJobName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.FailureReason as Types
import qualified Network.AWS.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric as Types
import qualified Network.AWS.SageMaker.Types.HyperParameterKey as Types
import qualified Network.AWS.SageMaker.Types.HyperParameterValue as Types
import qualified Network.AWS.SageMaker.Types.ObjectiveStatus as Types
import qualified Network.AWS.SageMaker.Types.TrainingJobArn as Types
import qualified Network.AWS.SageMaker.Types.TrainingJobDefinitionName as Types
import qualified Network.AWS.SageMaker.Types.TrainingJobName as Types
import qualified Network.AWS.SageMaker.Types.TrainingJobStatus as Types
import qualified Network.AWS.SageMaker.Types.TuningJobName as Types

-- | Specifies summary information about a training job.
--
-- /See:/ 'mkHyperParameterTrainingJobSummary' smart constructor.
data HyperParameterTrainingJobSummary = HyperParameterTrainingJobSummary'
  { -- | The name of the training job.
    trainingJobName :: Types.TrainingJobName,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Types.TrainingJobArn,
    -- | The date and time that the training job was created.
    creationTime :: Core.NominalDiffTime,
    -- | The status of the training job.
    trainingJobStatus :: Types.TrainingJobStatus,
    -- | A list of the hyperparameters for which you specified ranges to search.
    tunedHyperParameters :: Core.HashMap Types.HyperParameterKey Types.HyperParameterValue,
    -- | The reason that the training job failed.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | The 'FinalHyperParameterTuningJobObjectiveMetric' object that specifies the value of the objective metric of the tuning job that launched this training job.
    finalHyperParameterTuningJobObjectiveMetric :: Core.Maybe Types.FinalHyperParameterTuningJobObjectiveMetric,
    -- | The status of the objective metric for the training job:
    --
    --
    --     * Succeeded: The final objective metric for the training job was evaluated by the hyperparameter tuning job and used in the hyperparameter tuning process.
    --
    --
    --
    --     * Pending: The training job is in progress and evaluation of its final objective metric is pending.
    --
    --
    --
    --     * Failed: The final objective metric for the training job was not evaluated, and was not used in the hyperparameter tuning process. This typically occurs when the training job failed or did not emit an objective metric.
    objectiveStatus :: Core.Maybe Types.ObjectiveStatus,
    -- | Specifies the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
    trainingEndTime :: Core.Maybe Core.NominalDiffTime,
    -- | The training job definition name.
    trainingJobDefinitionName :: Core.Maybe Types.TrainingJobDefinitionName,
    -- | The date and time that the training job started.
    trainingStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | The HyperParameter tuning job that launched the training job.
    tuningJobName :: Core.Maybe Types.TuningJobName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'HyperParameterTrainingJobSummary' value with any optional fields omitted.
mkHyperParameterTrainingJobSummary ::
  -- | 'trainingJobName'
  Types.TrainingJobName ->
  -- | 'trainingJobArn'
  Types.TrainingJobArn ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'trainingJobStatus'
  Types.TrainingJobStatus ->
  HyperParameterTrainingJobSummary
mkHyperParameterTrainingJobSummary
  trainingJobName
  trainingJobArn
  creationTime
  trainingJobStatus =
    HyperParameterTrainingJobSummary'
      { trainingJobName,
        trainingJobArn,
        creationTime,
        trainingJobStatus,
        tunedHyperParameters = Core.mempty,
        failureReason = Core.Nothing,
        finalHyperParameterTuningJobObjectiveMetric = Core.Nothing,
        objectiveStatus = Core.Nothing,
        trainingEndTime = Core.Nothing,
        trainingJobDefinitionName = Core.Nothing,
        trainingStartTime = Core.Nothing,
        tuningJobName = Core.Nothing
      }

-- | The name of the training job.
--
-- /Note:/ Consider using 'trainingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjsTrainingJobName :: Lens.Lens' HyperParameterTrainingJobSummary Types.TrainingJobName
hptjsTrainingJobName = Lens.field @"trainingJobName"
{-# DEPRECATED hptjsTrainingJobName "Use generic-lens or generic-optics with 'trainingJobName' instead." #-}

-- | The Amazon Resource Name (ARN) of the training job.
--
-- /Note:/ Consider using 'trainingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjsTrainingJobArn :: Lens.Lens' HyperParameterTrainingJobSummary Types.TrainingJobArn
hptjsTrainingJobArn = Lens.field @"trainingJobArn"
{-# DEPRECATED hptjsTrainingJobArn "Use generic-lens or generic-optics with 'trainingJobArn' instead." #-}

-- | The date and time that the training job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjsCreationTime :: Lens.Lens' HyperParameterTrainingJobSummary Core.NominalDiffTime
hptjsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED hptjsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of the training job.
--
-- /Note:/ Consider using 'trainingJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjsTrainingJobStatus :: Lens.Lens' HyperParameterTrainingJobSummary Types.TrainingJobStatus
hptjsTrainingJobStatus = Lens.field @"trainingJobStatus"
{-# DEPRECATED hptjsTrainingJobStatus "Use generic-lens or generic-optics with 'trainingJobStatus' instead." #-}

-- | A list of the hyperparameters for which you specified ranges to search.
--
-- /Note:/ Consider using 'tunedHyperParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjsTunedHyperParameters :: Lens.Lens' HyperParameterTrainingJobSummary (Core.HashMap Types.HyperParameterKey Types.HyperParameterValue)
hptjsTunedHyperParameters = Lens.field @"tunedHyperParameters"
{-# DEPRECATED hptjsTunedHyperParameters "Use generic-lens or generic-optics with 'tunedHyperParameters' instead." #-}

-- | The reason that the training job failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjsFailureReason :: Lens.Lens' HyperParameterTrainingJobSummary (Core.Maybe Types.FailureReason)
hptjsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED hptjsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The 'FinalHyperParameterTuningJobObjectiveMetric' object that specifies the value of the objective metric of the tuning job that launched this training job.
--
-- /Note:/ Consider using 'finalHyperParameterTuningJobObjectiveMetric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjsFinalHyperParameterTuningJobObjectiveMetric :: Lens.Lens' HyperParameterTrainingJobSummary (Core.Maybe Types.FinalHyperParameterTuningJobObjectiveMetric)
hptjsFinalHyperParameterTuningJobObjectiveMetric = Lens.field @"finalHyperParameterTuningJobObjectiveMetric"
{-# DEPRECATED hptjsFinalHyperParameterTuningJobObjectiveMetric "Use generic-lens or generic-optics with 'finalHyperParameterTuningJobObjectiveMetric' instead." #-}

-- | The status of the objective metric for the training job:
--
--
--     * Succeeded: The final objective metric for the training job was evaluated by the hyperparameter tuning job and used in the hyperparameter tuning process.
--
--
--
--     * Pending: The training job is in progress and evaluation of its final objective metric is pending.
--
--
--
--     * Failed: The final objective metric for the training job was not evaluated, and was not used in the hyperparameter tuning process. This typically occurs when the training job failed or did not emit an objective metric.
--
--
--
-- /Note:/ Consider using 'objectiveStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjsObjectiveStatus :: Lens.Lens' HyperParameterTrainingJobSummary (Core.Maybe Types.ObjectiveStatus)
hptjsObjectiveStatus = Lens.field @"objectiveStatus"
{-# DEPRECATED hptjsObjectiveStatus "Use generic-lens or generic-optics with 'objectiveStatus' instead." #-}

-- | Specifies the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
--
-- /Note:/ Consider using 'trainingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjsTrainingEndTime :: Lens.Lens' HyperParameterTrainingJobSummary (Core.Maybe Core.NominalDiffTime)
hptjsTrainingEndTime = Lens.field @"trainingEndTime"
{-# DEPRECATED hptjsTrainingEndTime "Use generic-lens or generic-optics with 'trainingEndTime' instead." #-}

-- | The training job definition name.
--
-- /Note:/ Consider using 'trainingJobDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjsTrainingJobDefinitionName :: Lens.Lens' HyperParameterTrainingJobSummary (Core.Maybe Types.TrainingJobDefinitionName)
hptjsTrainingJobDefinitionName = Lens.field @"trainingJobDefinitionName"
{-# DEPRECATED hptjsTrainingJobDefinitionName "Use generic-lens or generic-optics with 'trainingJobDefinitionName' instead." #-}

-- | The date and time that the training job started.
--
-- /Note:/ Consider using 'trainingStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjsTrainingStartTime :: Lens.Lens' HyperParameterTrainingJobSummary (Core.Maybe Core.NominalDiffTime)
hptjsTrainingStartTime = Lens.field @"trainingStartTime"
{-# DEPRECATED hptjsTrainingStartTime "Use generic-lens or generic-optics with 'trainingStartTime' instead." #-}

-- | The HyperParameter tuning job that launched the training job.
--
-- /Note:/ Consider using 'tuningJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjsTuningJobName :: Lens.Lens' HyperParameterTrainingJobSummary (Core.Maybe Types.TuningJobName)
hptjsTuningJobName = Lens.field @"tuningJobName"
{-# DEPRECATED hptjsTuningJobName "Use generic-lens or generic-optics with 'tuningJobName' instead." #-}

instance Core.FromJSON HyperParameterTrainingJobSummary where
  parseJSON =
    Core.withObject "HyperParameterTrainingJobSummary" Core.$
      \x ->
        HyperParameterTrainingJobSummary'
          Core.<$> (x Core..: "TrainingJobName")
          Core.<*> (x Core..: "TrainingJobArn")
          Core.<*> (x Core..: "CreationTime")
          Core.<*> (x Core..: "TrainingJobStatus")
          Core.<*> (x Core..:? "TunedHyperParameters" Core..!= Core.mempty)
          Core.<*> (x Core..:? "FailureReason")
          Core.<*> (x Core..:? "FinalHyperParameterTuningJobObjectiveMetric")
          Core.<*> (x Core..:? "ObjectiveStatus")
          Core.<*> (x Core..:? "TrainingEndTime")
          Core.<*> (x Core..:? "TrainingJobDefinitionName")
          Core.<*> (x Core..:? "TrainingStartTime")
          Core.<*> (x Core..:? "TuningJobName")
