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
    hCreationTime,
    hTrainingJobStatusCounters,
    hResourceLimits,
    hObjectiveStatusCounters,
    hHyperParameterTuningJobARN,
    hHyperParameterTuningJobName,
    hStrategy,
    hLastModifiedTime,
    hHyperParameterTuningEndTime,
    hHyperParameterTuningJobStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.HyperParameterTuningJobStatus
import Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType
import Network.AWS.SageMaker.Types.ObjectiveStatusCounters
import Network.AWS.SageMaker.Types.ResourceLimits
import Network.AWS.SageMaker.Types.TrainingJobStatusCounters

-- | Provides summary information about a hyperparameter tuning job.
--
-- /See:/ 'mkHyperParameterTuningJobSummary' smart constructor.
data HyperParameterTuningJobSummary = HyperParameterTuningJobSummary'
  { -- | The date and time that the tuning job was created.
    creationTime :: Lude.Timestamp,
    -- | The 'TrainingJobStatusCounters' object that specifies the numbers of training jobs, categorized by status, that this tuning job launched.
    trainingJobStatusCounters :: TrainingJobStatusCounters,
    -- | The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs allowed for this tuning job.
    resourceLimits :: Lude.Maybe ResourceLimits,
    -- | The 'ObjectiveStatusCounters' object that specifies the numbers of training jobs, categorized by objective metric status, that this tuning job launched.
    objectiveStatusCounters :: ObjectiveStatusCounters,
    -- | The Amazon Resource Name (ARN) of the tuning job.
    hyperParameterTuningJobARN :: Lude.Text,
    -- | The name of the tuning job.
    hyperParameterTuningJobName :: Lude.Text,
    -- | Specifies the search strategy hyperparameter tuning uses to choose which hyperparameters to use for each iteration. Currently, the only valid value is Bayesian.
    strategy :: HyperParameterTuningJobStrategyType,
    -- | The date and time that the tuning job was modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The date and time that the tuning job ended.
    hyperParameterTuningEndTime :: Lude.Maybe Lude.Timestamp,
    -- | The status of the tuning job.
    hyperParameterTuningJobStatus :: HyperParameterTuningJobStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HyperParameterTuningJobSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - The date and time that the tuning job was created.
-- * 'trainingJobStatusCounters' - The 'TrainingJobStatusCounters' object that specifies the numbers of training jobs, categorized by status, that this tuning job launched.
-- * 'resourceLimits' - The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs allowed for this tuning job.
-- * 'objectiveStatusCounters' - The 'ObjectiveStatusCounters' object that specifies the numbers of training jobs, categorized by objective metric status, that this tuning job launched.
-- * 'hyperParameterTuningJobARN' - The Amazon Resource Name (ARN) of the tuning job.
-- * 'hyperParameterTuningJobName' - The name of the tuning job.
-- * 'strategy' - Specifies the search strategy hyperparameter tuning uses to choose which hyperparameters to use for each iteration. Currently, the only valid value is Bayesian.
-- * 'lastModifiedTime' - The date and time that the tuning job was modified.
-- * 'hyperParameterTuningEndTime' - The date and time that the tuning job ended.
-- * 'hyperParameterTuningJobStatus' - The status of the tuning job.
mkHyperParameterTuningJobSummary ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'trainingJobStatusCounters'
  TrainingJobStatusCounters ->
  -- | 'objectiveStatusCounters'
  ObjectiveStatusCounters ->
  -- | 'hyperParameterTuningJobARN'
  Lude.Text ->
  -- | 'hyperParameterTuningJobName'
  Lude.Text ->
  -- | 'strategy'
  HyperParameterTuningJobStrategyType ->
  -- | 'hyperParameterTuningJobStatus'
  HyperParameterTuningJobStatus ->
  HyperParameterTuningJobSummary
mkHyperParameterTuningJobSummary
  pCreationTime_
  pTrainingJobStatusCounters_
  pObjectiveStatusCounters_
  pHyperParameterTuningJobARN_
  pHyperParameterTuningJobName_
  pStrategy_
  pHyperParameterTuningJobStatus_ =
    HyperParameterTuningJobSummary'
      { creationTime = pCreationTime_,
        trainingJobStatusCounters = pTrainingJobStatusCounters_,
        resourceLimits = Lude.Nothing,
        objectiveStatusCounters = pObjectiveStatusCounters_,
        hyperParameterTuningJobARN = pHyperParameterTuningJobARN_,
        hyperParameterTuningJobName = pHyperParameterTuningJobName_,
        strategy = pStrategy_,
        lastModifiedTime = Lude.Nothing,
        hyperParameterTuningEndTime = Lude.Nothing,
        hyperParameterTuningJobStatus = pHyperParameterTuningJobStatus_
      }

-- | The date and time that the tuning job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hCreationTime :: Lens.Lens' HyperParameterTuningJobSummary Lude.Timestamp
hCreationTime = Lens.lens (creationTime :: HyperParameterTuningJobSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: HyperParameterTuningJobSummary)
{-# DEPRECATED hCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The 'TrainingJobStatusCounters' object that specifies the numbers of training jobs, categorized by status, that this tuning job launched.
--
-- /Note:/ Consider using 'trainingJobStatusCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hTrainingJobStatusCounters :: Lens.Lens' HyperParameterTuningJobSummary TrainingJobStatusCounters
hTrainingJobStatusCounters = Lens.lens (trainingJobStatusCounters :: HyperParameterTuningJobSummary -> TrainingJobStatusCounters) (\s a -> s {trainingJobStatusCounters = a} :: HyperParameterTuningJobSummary)
{-# DEPRECATED hTrainingJobStatusCounters "Use generic-lens or generic-optics with 'trainingJobStatusCounters' instead." #-}

-- | The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs allowed for this tuning job.
--
-- /Note:/ Consider using 'resourceLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hResourceLimits :: Lens.Lens' HyperParameterTuningJobSummary (Lude.Maybe ResourceLimits)
hResourceLimits = Lens.lens (resourceLimits :: HyperParameterTuningJobSummary -> Lude.Maybe ResourceLimits) (\s a -> s {resourceLimits = a} :: HyperParameterTuningJobSummary)
{-# DEPRECATED hResourceLimits "Use generic-lens or generic-optics with 'resourceLimits' instead." #-}

-- | The 'ObjectiveStatusCounters' object that specifies the numbers of training jobs, categorized by objective metric status, that this tuning job launched.
--
-- /Note:/ Consider using 'objectiveStatusCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hObjectiveStatusCounters :: Lens.Lens' HyperParameterTuningJobSummary ObjectiveStatusCounters
hObjectiveStatusCounters = Lens.lens (objectiveStatusCounters :: HyperParameterTuningJobSummary -> ObjectiveStatusCounters) (\s a -> s {objectiveStatusCounters = a} :: HyperParameterTuningJobSummary)
{-# DEPRECATED hObjectiveStatusCounters "Use generic-lens or generic-optics with 'objectiveStatusCounters' instead." #-}

-- | The Amazon Resource Name (ARN) of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHyperParameterTuningJobARN :: Lens.Lens' HyperParameterTuningJobSummary Lude.Text
hHyperParameterTuningJobARN = Lens.lens (hyperParameterTuningJobARN :: HyperParameterTuningJobSummary -> Lude.Text) (\s a -> s {hyperParameterTuningJobARN = a} :: HyperParameterTuningJobSummary)
{-# DEPRECATED hHyperParameterTuningJobARN "Use generic-lens or generic-optics with 'hyperParameterTuningJobARN' instead." #-}

-- | The name of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHyperParameterTuningJobName :: Lens.Lens' HyperParameterTuningJobSummary Lude.Text
hHyperParameterTuningJobName = Lens.lens (hyperParameterTuningJobName :: HyperParameterTuningJobSummary -> Lude.Text) (\s a -> s {hyperParameterTuningJobName = a} :: HyperParameterTuningJobSummary)
{-# DEPRECATED hHyperParameterTuningJobName "Use generic-lens or generic-optics with 'hyperParameterTuningJobName' instead." #-}

-- | Specifies the search strategy hyperparameter tuning uses to choose which hyperparameters to use for each iteration. Currently, the only valid value is Bayesian.
--
-- /Note:/ Consider using 'strategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hStrategy :: Lens.Lens' HyperParameterTuningJobSummary HyperParameterTuningJobStrategyType
hStrategy = Lens.lens (strategy :: HyperParameterTuningJobSummary -> HyperParameterTuningJobStrategyType) (\s a -> s {strategy = a} :: HyperParameterTuningJobSummary)
{-# DEPRECATED hStrategy "Use generic-lens or generic-optics with 'strategy' instead." #-}

-- | The date and time that the tuning job was modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hLastModifiedTime :: Lens.Lens' HyperParameterTuningJobSummary (Lude.Maybe Lude.Timestamp)
hLastModifiedTime = Lens.lens (lastModifiedTime :: HyperParameterTuningJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: HyperParameterTuningJobSummary)
{-# DEPRECATED hLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The date and time that the tuning job ended.
--
-- /Note:/ Consider using 'hyperParameterTuningEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHyperParameterTuningEndTime :: Lens.Lens' HyperParameterTuningJobSummary (Lude.Maybe Lude.Timestamp)
hHyperParameterTuningEndTime = Lens.lens (hyperParameterTuningEndTime :: HyperParameterTuningJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {hyperParameterTuningEndTime = a} :: HyperParameterTuningJobSummary)
{-# DEPRECATED hHyperParameterTuningEndTime "Use generic-lens or generic-optics with 'hyperParameterTuningEndTime' instead." #-}

-- | The status of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHyperParameterTuningJobStatus :: Lens.Lens' HyperParameterTuningJobSummary HyperParameterTuningJobStatus
hHyperParameterTuningJobStatus = Lens.lens (hyperParameterTuningJobStatus :: HyperParameterTuningJobSummary -> HyperParameterTuningJobStatus) (\s a -> s {hyperParameterTuningJobStatus = a} :: HyperParameterTuningJobSummary)
{-# DEPRECATED hHyperParameterTuningJobStatus "Use generic-lens or generic-optics with 'hyperParameterTuningJobStatus' instead." #-}

instance Lude.FromJSON HyperParameterTuningJobSummary where
  parseJSON =
    Lude.withObject
      "HyperParameterTuningJobSummary"
      ( \x ->
          HyperParameterTuningJobSummary'
            Lude.<$> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..: "TrainingJobStatusCounters")
            Lude.<*> (x Lude..:? "ResourceLimits")
            Lude.<*> (x Lude..: "ObjectiveStatusCounters")
            Lude.<*> (x Lude..: "HyperParameterTuningJobArn")
            Lude.<*> (x Lude..: "HyperParameterTuningJobName")
            Lude.<*> (x Lude..: "Strategy")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "HyperParameterTuningEndTime")
            Lude.<*> (x Lude..: "HyperParameterTuningJobStatus")
      )
