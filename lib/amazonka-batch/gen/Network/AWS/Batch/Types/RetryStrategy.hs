{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.RetryStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.RetryStrategy
  ( RetryStrategy (..),

    -- * Smart constructor
    mkRetryStrategy,

    -- * Lenses
    rsAttempts,
    rsEvaluateOnExit,
  )
where

import qualified Network.AWS.Batch.Types.EvaluateOnExit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The retry strategy associated with a job.
--
-- /See:/ 'mkRetryStrategy' smart constructor.
data RetryStrategy = RetryStrategy'
  { -- | The number of times to move a job to the @RUNNABLE@ status. You may specify between 1 and 10 attempts. If the value of @attempts@ is greater than one, the job is retried on failure the same number of attempts as the value.
    attempts :: Core.Maybe Core.Int,
    -- | Array of up to 5 objects that specify conditions under which the job should be retried or failed. If this parameter is specified, then the @attempts@ parameter must also be specified.
    evaluateOnExit :: Core.Maybe [Types.EvaluateOnExit]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetryStrategy' value with any optional fields omitted.
mkRetryStrategy ::
  RetryStrategy
mkRetryStrategy =
  RetryStrategy'
    { attempts = Core.Nothing,
      evaluateOnExit = Core.Nothing
    }

-- | The number of times to move a job to the @RUNNABLE@ status. You may specify between 1 and 10 attempts. If the value of @attempts@ is greater than one, the job is retried on failure the same number of attempts as the value.
--
-- /Note:/ Consider using 'attempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsAttempts :: Lens.Lens' RetryStrategy (Core.Maybe Core.Int)
rsAttempts = Lens.field @"attempts"
{-# DEPRECATED rsAttempts "Use generic-lens or generic-optics with 'attempts' instead." #-}

-- | Array of up to 5 objects that specify conditions under which the job should be retried or failed. If this parameter is specified, then the @attempts@ parameter must also be specified.
--
-- /Note:/ Consider using 'evaluateOnExit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsEvaluateOnExit :: Lens.Lens' RetryStrategy (Core.Maybe [Types.EvaluateOnExit])
rsEvaluateOnExit = Lens.field @"evaluateOnExit"
{-# DEPRECATED rsEvaluateOnExit "Use generic-lens or generic-optics with 'evaluateOnExit' instead." #-}

instance Core.FromJSON RetryStrategy where
  toJSON RetryStrategy {..} =
    Core.object
      ( Core.catMaybes
          [ ("attempts" Core..=) Core.<$> attempts,
            ("evaluateOnExit" Core..=) Core.<$> evaluateOnExit
          ]
      )

instance Core.FromJSON RetryStrategy where
  parseJSON =
    Core.withObject "RetryStrategy" Core.$
      \x ->
        RetryStrategy'
          Core.<$> (x Core..:? "attempts") Core.<*> (x Core..:? "evaluateOnExit")
