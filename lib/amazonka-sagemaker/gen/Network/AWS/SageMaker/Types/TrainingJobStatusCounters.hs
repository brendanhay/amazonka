-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingJobStatusCounters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobStatusCounters
  ( TrainingJobStatusCounters (..),

    -- * Smart constructor
    mkTrainingJobStatusCounters,

    -- * Lenses
    tjscStopped,
    tjscRetryableError,
    tjscInProgress,
    tjscNonRetryableError,
    tjscCompleted,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The numbers of training jobs launched by a hyperparameter tuning job, categorized by status.
--
-- /See:/ 'mkTrainingJobStatusCounters' smart constructor.
data TrainingJobStatusCounters = TrainingJobStatusCounters'
  { stopped ::
      Lude.Maybe Lude.Natural,
    retryableError ::
      Lude.Maybe Lude.Natural,
    inProgress :: Lude.Maybe Lude.Natural,
    nonRetryableError ::
      Lude.Maybe Lude.Natural,
    completed :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrainingJobStatusCounters' with the minimum fields required to make a request.
--
-- * 'completed' - The number of completed training jobs launched by the hyperparameter tuning job.
-- * 'inProgress' - The number of in-progress training jobs launched by a hyperparameter tuning job.
-- * 'nonRetryableError' - The number of training jobs that failed and can't be retried. A failed training job can't be retried if it failed because a client error occurred.
-- * 'retryableError' - The number of training jobs that failed, but can be retried. A failed training job can be retried only if it failed because an internal service error occurred.
-- * 'stopped' - The number of training jobs launched by a hyperparameter tuning job that were manually stopped.
mkTrainingJobStatusCounters ::
  TrainingJobStatusCounters
mkTrainingJobStatusCounters =
  TrainingJobStatusCounters'
    { stopped = Lude.Nothing,
      retryableError = Lude.Nothing,
      inProgress = Lude.Nothing,
      nonRetryableError = Lude.Nothing,
      completed = Lude.Nothing
    }

-- | The number of training jobs launched by a hyperparameter tuning job that were manually stopped.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjscStopped :: Lens.Lens' TrainingJobStatusCounters (Lude.Maybe Lude.Natural)
tjscStopped = Lens.lens (stopped :: TrainingJobStatusCounters -> Lude.Maybe Lude.Natural) (\s a -> s {stopped = a} :: TrainingJobStatusCounters)
{-# DEPRECATED tjscStopped "Use generic-lens or generic-optics with 'stopped' instead." #-}

-- | The number of training jobs that failed, but can be retried. A failed training job can be retried only if it failed because an internal service error occurred.
--
-- /Note:/ Consider using 'retryableError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjscRetryableError :: Lens.Lens' TrainingJobStatusCounters (Lude.Maybe Lude.Natural)
tjscRetryableError = Lens.lens (retryableError :: TrainingJobStatusCounters -> Lude.Maybe Lude.Natural) (\s a -> s {retryableError = a} :: TrainingJobStatusCounters)
{-# DEPRECATED tjscRetryableError "Use generic-lens or generic-optics with 'retryableError' instead." #-}

-- | The number of in-progress training jobs launched by a hyperparameter tuning job.
--
-- /Note:/ Consider using 'inProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjscInProgress :: Lens.Lens' TrainingJobStatusCounters (Lude.Maybe Lude.Natural)
tjscInProgress = Lens.lens (inProgress :: TrainingJobStatusCounters -> Lude.Maybe Lude.Natural) (\s a -> s {inProgress = a} :: TrainingJobStatusCounters)
{-# DEPRECATED tjscInProgress "Use generic-lens or generic-optics with 'inProgress' instead." #-}

-- | The number of training jobs that failed and can't be retried. A failed training job can't be retried if it failed because a client error occurred.
--
-- /Note:/ Consider using 'nonRetryableError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjscNonRetryableError :: Lens.Lens' TrainingJobStatusCounters (Lude.Maybe Lude.Natural)
tjscNonRetryableError = Lens.lens (nonRetryableError :: TrainingJobStatusCounters -> Lude.Maybe Lude.Natural) (\s a -> s {nonRetryableError = a} :: TrainingJobStatusCounters)
{-# DEPRECATED tjscNonRetryableError "Use generic-lens or generic-optics with 'nonRetryableError' instead." #-}

-- | The number of completed training jobs launched by the hyperparameter tuning job.
--
-- /Note:/ Consider using 'completed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjscCompleted :: Lens.Lens' TrainingJobStatusCounters (Lude.Maybe Lude.Natural)
tjscCompleted = Lens.lens (completed :: TrainingJobStatusCounters -> Lude.Maybe Lude.Natural) (\s a -> s {completed = a} :: TrainingJobStatusCounters)
{-# DEPRECATED tjscCompleted "Use generic-lens or generic-optics with 'completed' instead." #-}

instance Lude.FromJSON TrainingJobStatusCounters where
  parseJSON =
    Lude.withObject
      "TrainingJobStatusCounters"
      ( \x ->
          TrainingJobStatusCounters'
            Lude.<$> (x Lude..:? "Stopped")
            Lude.<*> (x Lude..:? "RetryableError")
            Lude.<*> (x Lude..:? "InProgress")
            Lude.<*> (x Lude..:? "NonRetryableError")
            Lude.<*> (x Lude..:? "Completed")
      )
