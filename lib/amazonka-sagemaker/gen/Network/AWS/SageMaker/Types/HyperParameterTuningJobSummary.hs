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
    hResourceLimits,
    hLastModifiedTime,
    hHyperParameterTuningEndTime,
    hHyperParameterTuningJobName,
    hHyperParameterTuningJobARN,
    hHyperParameterTuningJobStatus,
    hStrategy,
    hCreationTime,
    hTrainingJobStatusCounters,
    hObjectiveStatusCounters,
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
  { resourceLimits ::
      Lude.Maybe ResourceLimits,
    lastModifiedTime ::
      Lude.Maybe Lude.Timestamp,
    hyperParameterTuningEndTime ::
      Lude.Maybe Lude.Timestamp,
    hyperParameterTuningJobName ::
      Lude.Text,
    hyperParameterTuningJobARN ::
      Lude.Text,
    hyperParameterTuningJobStatus ::
      HyperParameterTuningJobStatus,
    strategy ::
      HyperParameterTuningJobStrategyType,
    creationTime ::
      Lude.Timestamp,
    trainingJobStatusCounters ::
      TrainingJobStatusCounters,
    objectiveStatusCounters ::
      ObjectiveStatusCounters
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HyperParameterTuningJobSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - The date and time that the tuning job was created.
-- * 'hyperParameterTuningEndTime' - The date and time that the tuning job ended.
-- * 'hyperParameterTuningJobARN' - The Amazon Resource Name (ARN) of the tuning job.
-- * 'hyperParameterTuningJobName' - The name of the tuning job.
-- * 'hyperParameterTuningJobStatus' - The status of the tuning job.
-- * 'lastModifiedTime' - The date and time that the tuning job was modified.
-- * 'objectiveStatusCounters' - The 'ObjectiveStatusCounters' object that specifies the numbers of training jobs, categorized by objective metric status, that this tuning job launched.
-- * 'resourceLimits' - The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs allowed for this tuning job.
-- * 'strategy' - Specifies the search strategy hyperparameter tuning uses to choose which hyperparameters to use for each iteration. Currently, the only valid value is Bayesian.
-- * 'trainingJobStatusCounters' - The 'TrainingJobStatusCounters' object that specifies the numbers of training jobs, categorized by status, that this tuning job launched.
mkHyperParameterTuningJobSummary ::
  -- | 'hyperParameterTuningJobName'
  Lude.Text ->
  -- | 'hyperParameterTuningJobARN'
  Lude.Text ->
  -- | 'hyperParameterTuningJobStatus'
  HyperParameterTuningJobStatus ->
  -- | 'strategy'
  HyperParameterTuningJobStrategyType ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'trainingJobStatusCounters'
  TrainingJobStatusCounters ->
  -- | 'objectiveStatusCounters'
  ObjectiveStatusCounters ->
  HyperParameterTuningJobSummary
mkHyperParameterTuningJobSummary
  pHyperParameterTuningJobName_
  pHyperParameterTuningJobARN_
  pHyperParameterTuningJobStatus_
  pStrategy_
  pCreationTime_
  pTrainingJobStatusCounters_
  pObjectiveStatusCounters_ =
    HyperParameterTuningJobSummary'
      { resourceLimits = Lude.Nothing,
        lastModifiedTime = Lude.Nothing,
        hyperParameterTuningEndTime = Lude.Nothing,
        hyperParameterTuningJobName = pHyperParameterTuningJobName_,
        hyperParameterTuningJobARN = pHyperParameterTuningJobARN_,
        hyperParameterTuningJobStatus = pHyperParameterTuningJobStatus_,
        strategy = pStrategy_,
        creationTime = pCreationTime_,
        trainingJobStatusCounters = pTrainingJobStatusCounters_,
        objectiveStatusCounters = pObjectiveStatusCounters_
      }

-- | The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs allowed for this tuning job.
--
-- /Note:/ Consider using 'resourceLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hResourceLimits :: Lens.Lens' HyperParameterTuningJobSummary (Lude.Maybe ResourceLimits)
hResourceLimits = Lens.lens (resourceLimits :: HyperParameterTuningJobSummary -> Lude.Maybe ResourceLimits) (\s a -> s {resourceLimits = a} :: HyperParameterTuningJobSummary)
{-# DEPRECATED hResourceLimits "Use generic-lens or generic-optics with 'resourceLimits' instead." #-}

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

-- | The name of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHyperParameterTuningJobName :: Lens.Lens' HyperParameterTuningJobSummary Lude.Text
hHyperParameterTuningJobName = Lens.lens (hyperParameterTuningJobName :: HyperParameterTuningJobSummary -> Lude.Text) (\s a -> s {hyperParameterTuningJobName = a} :: HyperParameterTuningJobSummary)
{-# DEPRECATED hHyperParameterTuningJobName "Use generic-lens or generic-optics with 'hyperParameterTuningJobName' instead." #-}

-- | The Amazon Resource Name (ARN) of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHyperParameterTuningJobARN :: Lens.Lens' HyperParameterTuningJobSummary Lude.Text
hHyperParameterTuningJobARN = Lens.lens (hyperParameterTuningJobARN :: HyperParameterTuningJobSummary -> Lude.Text) (\s a -> s {hyperParameterTuningJobARN = a} :: HyperParameterTuningJobSummary)
{-# DEPRECATED hHyperParameterTuningJobARN "Use generic-lens or generic-optics with 'hyperParameterTuningJobARN' instead." #-}

-- | The status of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHyperParameterTuningJobStatus :: Lens.Lens' HyperParameterTuningJobSummary HyperParameterTuningJobStatus
hHyperParameterTuningJobStatus = Lens.lens (hyperParameterTuningJobStatus :: HyperParameterTuningJobSummary -> HyperParameterTuningJobStatus) (\s a -> s {hyperParameterTuningJobStatus = a} :: HyperParameterTuningJobSummary)
{-# DEPRECATED hHyperParameterTuningJobStatus "Use generic-lens or generic-optics with 'hyperParameterTuningJobStatus' instead." #-}

-- | Specifies the search strategy hyperparameter tuning uses to choose which hyperparameters to use for each iteration. Currently, the only valid value is Bayesian.
--
-- /Note:/ Consider using 'strategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hStrategy :: Lens.Lens' HyperParameterTuningJobSummary HyperParameterTuningJobStrategyType
hStrategy = Lens.lens (strategy :: HyperParameterTuningJobSummary -> HyperParameterTuningJobStrategyType) (\s a -> s {strategy = a} :: HyperParameterTuningJobSummary)
{-# DEPRECATED hStrategy "Use generic-lens or generic-optics with 'strategy' instead." #-}

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

-- | The 'ObjectiveStatusCounters' object that specifies the numbers of training jobs, categorized by objective metric status, that this tuning job launched.
--
-- /Note:/ Consider using 'objectiveStatusCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hObjectiveStatusCounters :: Lens.Lens' HyperParameterTuningJobSummary ObjectiveStatusCounters
hObjectiveStatusCounters = Lens.lens (objectiveStatusCounters :: HyperParameterTuningJobSummary -> ObjectiveStatusCounters) (\s a -> s {objectiveStatusCounters = a} :: HyperParameterTuningJobSummary)
{-# DEPRECATED hObjectiveStatusCounters "Use generic-lens or generic-optics with 'objectiveStatusCounters' instead." #-}

instance Lude.FromJSON HyperParameterTuningJobSummary where
  parseJSON =
    Lude.withObject
      "HyperParameterTuningJobSummary"
      ( \x ->
          HyperParameterTuningJobSummary'
            Lude.<$> (x Lude..:? "ResourceLimits")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "HyperParameterTuningEndTime")
            Lude.<*> (x Lude..: "HyperParameterTuningJobName")
            Lude.<*> (x Lude..: "HyperParameterTuningJobArn")
            Lude.<*> (x Lude..: "HyperParameterTuningJobStatus")
            Lude.<*> (x Lude..: "Strategy")
            Lude.<*> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..: "TrainingJobStatusCounters")
            Lude.<*> (x Lude..: "ObjectiveStatusCounters")
      )
