{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ObjectiveStatusCounters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ObjectiveStatusCounters
  ( ObjectiveStatusCounters (..),

    -- * Smart constructor
    mkObjectiveStatusCounters,

    -- * Lenses
    oscPending,
    oscSucceeded,
    oscFailed,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the number of training jobs that this hyperparameter tuning job launched, categorized by the status of their objective metric. The objective metric status shows whether the final objective metric for the training job has been evaluated by the tuning job and used in the hyperparameter tuning process.
--
-- /See:/ 'mkObjectiveStatusCounters' smart constructor.
data ObjectiveStatusCounters = ObjectiveStatusCounters'
  { -- | The number of training jobs that are in progress and pending evaluation of their final objective metric.
    pending :: Lude.Maybe Lude.Natural,
    -- | The number of training jobs whose final objective metric was evaluated by the hyperparameter tuning job and used in the hyperparameter tuning process.
    succeeded :: Lude.Maybe Lude.Natural,
    -- | The number of training jobs whose final objective metric was not evaluated and used in the hyperparameter tuning process. This typically occurs when the training job failed or did not emit an objective metric.
    failed :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ObjectiveStatusCounters' with the minimum fields required to make a request.
--
-- * 'pending' - The number of training jobs that are in progress and pending evaluation of their final objective metric.
-- * 'succeeded' - The number of training jobs whose final objective metric was evaluated by the hyperparameter tuning job and used in the hyperparameter tuning process.
-- * 'failed' - The number of training jobs whose final objective metric was not evaluated and used in the hyperparameter tuning process. This typically occurs when the training job failed or did not emit an objective metric.
mkObjectiveStatusCounters ::
  ObjectiveStatusCounters
mkObjectiveStatusCounters =
  ObjectiveStatusCounters'
    { pending = Lude.Nothing,
      succeeded = Lude.Nothing,
      failed = Lude.Nothing
    }

-- | The number of training jobs that are in progress and pending evaluation of their final objective metric.
--
-- /Note:/ Consider using 'pending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oscPending :: Lens.Lens' ObjectiveStatusCounters (Lude.Maybe Lude.Natural)
oscPending = Lens.lens (pending :: ObjectiveStatusCounters -> Lude.Maybe Lude.Natural) (\s a -> s {pending = a} :: ObjectiveStatusCounters)
{-# DEPRECATED oscPending "Use generic-lens or generic-optics with 'pending' instead." #-}

-- | The number of training jobs whose final objective metric was evaluated by the hyperparameter tuning job and used in the hyperparameter tuning process.
--
-- /Note:/ Consider using 'succeeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oscSucceeded :: Lens.Lens' ObjectiveStatusCounters (Lude.Maybe Lude.Natural)
oscSucceeded = Lens.lens (succeeded :: ObjectiveStatusCounters -> Lude.Maybe Lude.Natural) (\s a -> s {succeeded = a} :: ObjectiveStatusCounters)
{-# DEPRECATED oscSucceeded "Use generic-lens or generic-optics with 'succeeded' instead." #-}

-- | The number of training jobs whose final objective metric was not evaluated and used in the hyperparameter tuning process. This typically occurs when the training job failed or did not emit an objective metric.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oscFailed :: Lens.Lens' ObjectiveStatusCounters (Lude.Maybe Lude.Natural)
oscFailed = Lens.lens (failed :: ObjectiveStatusCounters -> Lude.Maybe Lude.Natural) (\s a -> s {failed = a} :: ObjectiveStatusCounters)
{-# DEPRECATED oscFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

instance Lude.FromJSON ObjectiveStatusCounters where
  parseJSON =
    Lude.withObject
      "ObjectiveStatusCounters"
      ( \x ->
          ObjectiveStatusCounters'
            Lude.<$> (x Lude..:? "Pending")
            Lude.<*> (x Lude..:? "Succeeded")
            Lude.<*> (x Lude..:? "Failed")
      )
