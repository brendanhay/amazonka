{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobSummary
  ( HyperParameterTuningJobSummary (..),

    -- * Smart constructor
    mkHyperParameterTuningJobSummary,

    -- * Lenses
    hHyperParameterTuningJobName,
    hHyperParameterTuningJobArn,
    hHyperParameterTuningJobStatus,
    hStrategy,
    hCreationTime,
    hTrainingJobStatusCounters,
    hObjectiveStatusCounters,
    hHyperParameterTuningEndTime,
    hLastModifiedTime,
    hResourceLimits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.HyperParameterTuningJobArn as Types
import qualified Network.AWS.SageMaker.Types.HyperParameterTuningJobName as Types
import qualified Network.AWS.SageMaker.Types.HyperParameterTuningJobStatus as Types
import qualified Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType as Types
import qualified Network.AWS.SageMaker.Types.ObjectiveStatusCounters as Types
import qualified Network.AWS.SageMaker.Types.ResourceLimits as Types
import qualified Network.AWS.SageMaker.Types.TrainingJobStatusCounters as Types

-- | Provides summary information about a hyperparameter tuning job.
--
-- /See:/ 'mkHyperParameterTuningJobSummary' smart constructor.
data HyperParameterTuningJobSummary = HyperParameterTuningJobSummary'
  { -- | The name of the tuning job.
    hyperParameterTuningJobName :: Types.HyperParameterTuningJobName,
    -- | The Amazon Resource Name (ARN) of the tuning job.
    hyperParameterTuningJobArn :: Types.HyperParameterTuningJobArn,
    -- | The status of the tuning job.
    hyperParameterTuningJobStatus :: Types.HyperParameterTuningJobStatus,
    -- | Specifies the search strategy hyperparameter tuning uses to choose which hyperparameters to use for each iteration. Currently, the only valid value is Bayesian.
    strategy :: Types.HyperParameterTuningJobStrategyType,
    -- | The date and time that the tuning job was created.
    creationTime :: Core.NominalDiffTime,
    -- | The 'TrainingJobStatusCounters' object that specifies the numbers of training jobs, categorized by status, that this tuning job launched.
    trainingJobStatusCounters :: Types.TrainingJobStatusCounters,
    -- | The 'ObjectiveStatusCounters' object that specifies the numbers of training jobs, categorized by objective metric status, that this tuning job launched.
    objectiveStatusCounters :: Types.ObjectiveStatusCounters,
    -- | The date and time that the tuning job ended.
    hyperParameterTuningEndTime :: Core.Maybe Core.NominalDiffTime,
    -- | The date and time that the tuning job was modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs allowed for this tuning job.
    resourceLimits :: Core.Maybe Types.ResourceLimits
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'HyperParameterTuningJobSummary' value with any optional fields omitted.
mkHyperParameterTuningJobSummary ::
  -- | 'hyperParameterTuningJobName'
  Types.HyperParameterTuningJobName ->
  -- | 'hyperParameterTuningJobArn'
  Types.HyperParameterTuningJobArn ->
  -- | 'hyperParameterTuningJobStatus'
  Types.HyperParameterTuningJobStatus ->
  -- | 'strategy'
  Types.HyperParameterTuningJobStrategyType ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'trainingJobStatusCounters'
  Types.TrainingJobStatusCounters ->
  -- | 'objectiveStatusCounters'
  Types.ObjectiveStatusCounters ->
  HyperParameterTuningJobSummary
mkHyperParameterTuningJobSummary
  hyperParameterTuningJobName
  hyperParameterTuningJobArn
  hyperParameterTuningJobStatus
  strategy
  creationTime
  trainingJobStatusCounters
  objectiveStatusCounters =
    HyperParameterTuningJobSummary'
      { hyperParameterTuningJobName,
        hyperParameterTuningJobArn,
        hyperParameterTuningJobStatus,
        strategy,
        creationTime,
        trainingJobStatusCounters,
        objectiveStatusCounters,
        hyperParameterTuningEndTime = Core.Nothing,
        lastModifiedTime = Core.Nothing,
        resourceLimits = Core.Nothing
      }

-- | The name of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHyperParameterTuningJobName :: Lens.Lens' HyperParameterTuningJobSummary Types.HyperParameterTuningJobName
hHyperParameterTuningJobName = Lens.field @"hyperParameterTuningJobName"
{-# DEPRECATED hHyperParameterTuningJobName "Use generic-lens or generic-optics with 'hyperParameterTuningJobName' instead." #-}

-- | The Amazon Resource Name (ARN) of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHyperParameterTuningJobArn :: Lens.Lens' HyperParameterTuningJobSummary Types.HyperParameterTuningJobArn
hHyperParameterTuningJobArn = Lens.field @"hyperParameterTuningJobArn"
{-# DEPRECATED hHyperParameterTuningJobArn "Use generic-lens or generic-optics with 'hyperParameterTuningJobArn' instead." #-}

-- | The status of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHyperParameterTuningJobStatus :: Lens.Lens' HyperParameterTuningJobSummary Types.HyperParameterTuningJobStatus
hHyperParameterTuningJobStatus = Lens.field @"hyperParameterTuningJobStatus"
{-# DEPRECATED hHyperParameterTuningJobStatus "Use generic-lens or generic-optics with 'hyperParameterTuningJobStatus' instead." #-}

-- | Specifies the search strategy hyperparameter tuning uses to choose which hyperparameters to use for each iteration. Currently, the only valid value is Bayesian.
--
-- /Note:/ Consider using 'strategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hStrategy :: Lens.Lens' HyperParameterTuningJobSummary Types.HyperParameterTuningJobStrategyType
hStrategy = Lens.field @"strategy"
{-# DEPRECATED hStrategy "Use generic-lens or generic-optics with 'strategy' instead." #-}

-- | The date and time that the tuning job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hCreationTime :: Lens.Lens' HyperParameterTuningJobSummary Core.NominalDiffTime
hCreationTime = Lens.field @"creationTime"
{-# DEPRECATED hCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The 'TrainingJobStatusCounters' object that specifies the numbers of training jobs, categorized by status, that this tuning job launched.
--
-- /Note:/ Consider using 'trainingJobStatusCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hTrainingJobStatusCounters :: Lens.Lens' HyperParameterTuningJobSummary Types.TrainingJobStatusCounters
hTrainingJobStatusCounters = Lens.field @"trainingJobStatusCounters"
{-# DEPRECATED hTrainingJobStatusCounters "Use generic-lens or generic-optics with 'trainingJobStatusCounters' instead." #-}

-- | The 'ObjectiveStatusCounters' object that specifies the numbers of training jobs, categorized by objective metric status, that this tuning job launched.
--
-- /Note:/ Consider using 'objectiveStatusCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hObjectiveStatusCounters :: Lens.Lens' HyperParameterTuningJobSummary Types.ObjectiveStatusCounters
hObjectiveStatusCounters = Lens.field @"objectiveStatusCounters"
{-# DEPRECATED hObjectiveStatusCounters "Use generic-lens or generic-optics with 'objectiveStatusCounters' instead." #-}

-- | The date and time that the tuning job ended.
--
-- /Note:/ Consider using 'hyperParameterTuningEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHyperParameterTuningEndTime :: Lens.Lens' HyperParameterTuningJobSummary (Core.Maybe Core.NominalDiffTime)
hHyperParameterTuningEndTime = Lens.field @"hyperParameterTuningEndTime"
{-# DEPRECATED hHyperParameterTuningEndTime "Use generic-lens or generic-optics with 'hyperParameterTuningEndTime' instead." #-}

-- | The date and time that the tuning job was modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hLastModifiedTime :: Lens.Lens' HyperParameterTuningJobSummary (Core.Maybe Core.NominalDiffTime)
hLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED hLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs allowed for this tuning job.
--
-- /Note:/ Consider using 'resourceLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hResourceLimits :: Lens.Lens' HyperParameterTuningJobSummary (Core.Maybe Types.ResourceLimits)
hResourceLimits = Lens.field @"resourceLimits"
{-# DEPRECATED hResourceLimits "Use generic-lens or generic-optics with 'resourceLimits' instead." #-}

instance Core.FromJSON HyperParameterTuningJobSummary where
  parseJSON =
    Core.withObject "HyperParameterTuningJobSummary" Core.$
      \x ->
        HyperParameterTuningJobSummary'
          Core.<$> (x Core..: "HyperParameterTuningJobName")
          Core.<*> (x Core..: "HyperParameterTuningJobArn")
          Core.<*> (x Core..: "HyperParameterTuningJobStatus")
          Core.<*> (x Core..: "Strategy")
          Core.<*> (x Core..: "CreationTime")
          Core.<*> (x Core..: "TrainingJobStatusCounters")
          Core.<*> (x Core..: "ObjectiveStatusCounters")
          Core.<*> (x Core..:? "HyperParameterTuningEndTime")
          Core.<*> (x Core..:? "LastModifiedTime")
          Core.<*> (x Core..:? "ResourceLimits")
