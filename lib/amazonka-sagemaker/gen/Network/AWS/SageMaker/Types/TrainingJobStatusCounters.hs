{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    tjscCompleted,
    tjscInProgress,
    tjscNonRetryableError,
    tjscRetryableError,
    tjscStopped,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The numbers of training jobs launched by a hyperparameter tuning job, categorized by status.
--
-- /See:/ 'mkTrainingJobStatusCounters' smart constructor.
data TrainingJobStatusCounters = TrainingJobStatusCounters'
  { -- | The number of completed training jobs launched by the hyperparameter tuning job.
    completed :: Core.Maybe Core.Natural,
    -- | The number of in-progress training jobs launched by a hyperparameter tuning job.
    inProgress :: Core.Maybe Core.Natural,
    -- | The number of training jobs that failed and can't be retried. A failed training job can't be retried if it failed because a client error occurred.
    nonRetryableError :: Core.Maybe Core.Natural,
    -- | The number of training jobs that failed, but can be retried. A failed training job can be retried only if it failed because an internal service error occurred.
    retryableError :: Core.Maybe Core.Natural,
    -- | The number of training jobs launched by a hyperparameter tuning job that were manually stopped.
    stopped :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrainingJobStatusCounters' value with any optional fields omitted.
mkTrainingJobStatusCounters ::
  TrainingJobStatusCounters
mkTrainingJobStatusCounters =
  TrainingJobStatusCounters'
    { completed = Core.Nothing,
      inProgress = Core.Nothing,
      nonRetryableError = Core.Nothing,
      retryableError = Core.Nothing,
      stopped = Core.Nothing
    }

-- | The number of completed training jobs launched by the hyperparameter tuning job.
--
-- /Note:/ Consider using 'completed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjscCompleted :: Lens.Lens' TrainingJobStatusCounters (Core.Maybe Core.Natural)
tjscCompleted = Lens.field @"completed"
{-# DEPRECATED tjscCompleted "Use generic-lens or generic-optics with 'completed' instead." #-}

-- | The number of in-progress training jobs launched by a hyperparameter tuning job.
--
-- /Note:/ Consider using 'inProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjscInProgress :: Lens.Lens' TrainingJobStatusCounters (Core.Maybe Core.Natural)
tjscInProgress = Lens.field @"inProgress"
{-# DEPRECATED tjscInProgress "Use generic-lens or generic-optics with 'inProgress' instead." #-}

-- | The number of training jobs that failed and can't be retried. A failed training job can't be retried if it failed because a client error occurred.
--
-- /Note:/ Consider using 'nonRetryableError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjscNonRetryableError :: Lens.Lens' TrainingJobStatusCounters (Core.Maybe Core.Natural)
tjscNonRetryableError = Lens.field @"nonRetryableError"
{-# DEPRECATED tjscNonRetryableError "Use generic-lens or generic-optics with 'nonRetryableError' instead." #-}

-- | The number of training jobs that failed, but can be retried. A failed training job can be retried only if it failed because an internal service error occurred.
--
-- /Note:/ Consider using 'retryableError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjscRetryableError :: Lens.Lens' TrainingJobStatusCounters (Core.Maybe Core.Natural)
tjscRetryableError = Lens.field @"retryableError"
{-# DEPRECATED tjscRetryableError "Use generic-lens or generic-optics with 'retryableError' instead." #-}

-- | The number of training jobs launched by a hyperparameter tuning job that were manually stopped.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjscStopped :: Lens.Lens' TrainingJobStatusCounters (Core.Maybe Core.Natural)
tjscStopped = Lens.field @"stopped"
{-# DEPRECATED tjscStopped "Use generic-lens or generic-optics with 'stopped' instead." #-}

instance Core.FromJSON TrainingJobStatusCounters where
  parseJSON =
    Core.withObject "TrainingJobStatusCounters" Core.$
      \x ->
        TrainingJobStatusCounters'
          Core.<$> (x Core..:? "Completed")
          Core.<*> (x Core..:? "InProgress")
          Core.<*> (x Core..:? "NonRetryableError")
          Core.<*> (x Core..:? "RetryableError")
          Core.<*> (x Core..:? "Stopped")
