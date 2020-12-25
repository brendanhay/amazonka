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
    oscFailed,
    oscPending,
    oscSucceeded,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the number of training jobs that this hyperparameter tuning job launched, categorized by the status of their objective metric. The objective metric status shows whether the final objective metric for the training job has been evaluated by the tuning job and used in the hyperparameter tuning process.
--
-- /See:/ 'mkObjectiveStatusCounters' smart constructor.
data ObjectiveStatusCounters = ObjectiveStatusCounters'
  { -- | The number of training jobs whose final objective metric was not evaluated and used in the hyperparameter tuning process. This typically occurs when the training job failed or did not emit an objective metric.
    failed :: Core.Maybe Core.Natural,
    -- | The number of training jobs that are in progress and pending evaluation of their final objective metric.
    pending :: Core.Maybe Core.Natural,
    -- | The number of training jobs whose final objective metric was evaluated by the hyperparameter tuning job and used in the hyperparameter tuning process.
    succeeded :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ObjectiveStatusCounters' value with any optional fields omitted.
mkObjectiveStatusCounters ::
  ObjectiveStatusCounters
mkObjectiveStatusCounters =
  ObjectiveStatusCounters'
    { failed = Core.Nothing,
      pending = Core.Nothing,
      succeeded = Core.Nothing
    }

-- | The number of training jobs whose final objective metric was not evaluated and used in the hyperparameter tuning process. This typically occurs when the training job failed or did not emit an objective metric.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oscFailed :: Lens.Lens' ObjectiveStatusCounters (Core.Maybe Core.Natural)
oscFailed = Lens.field @"failed"
{-# DEPRECATED oscFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | The number of training jobs that are in progress and pending evaluation of their final objective metric.
--
-- /Note:/ Consider using 'pending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oscPending :: Lens.Lens' ObjectiveStatusCounters (Core.Maybe Core.Natural)
oscPending = Lens.field @"pending"
{-# DEPRECATED oscPending "Use generic-lens or generic-optics with 'pending' instead." #-}

-- | The number of training jobs whose final objective metric was evaluated by the hyperparameter tuning job and used in the hyperparameter tuning process.
--
-- /Note:/ Consider using 'succeeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oscSucceeded :: Lens.Lens' ObjectiveStatusCounters (Core.Maybe Core.Natural)
oscSucceeded = Lens.field @"succeeded"
{-# DEPRECATED oscSucceeded "Use generic-lens or generic-optics with 'succeeded' instead." #-}

instance Core.FromJSON ObjectiveStatusCounters where
  parseJSON =
    Core.withObject "ObjectiveStatusCounters" Core.$
      \x ->
        ObjectiveStatusCounters'
          Core.<$> (x Core..:? "Failed")
          Core.<*> (x Core..:? "Pending")
          Core.<*> (x Core..:? "Succeeded")
