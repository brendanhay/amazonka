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
    rsEvaluateOnExit,
    rsAttempts,
  )
where

import Network.AWS.Batch.Types.EvaluateOnExit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The retry strategy associated with a job.
--
-- /See:/ 'mkRetryStrategy' smart constructor.
data RetryStrategy = RetryStrategy'
  { evaluateOnExit ::
      Lude.Maybe [EvaluateOnExit],
    attempts :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetryStrategy' with the minimum fields required to make a request.
--
-- * 'attempts' - The number of times to move a job to the @RUNNABLE@ status. You may specify between 1 and 10 attempts. If the value of @attempts@ is greater than one, the job is retried on failure the same number of attempts as the value.
-- * 'evaluateOnExit' - Array of up to 5 objects that specify conditions under which the job should be retried or failed. If this parameter is specified, then the @attempts@ parameter must also be specified.
mkRetryStrategy ::
  RetryStrategy
mkRetryStrategy =
  RetryStrategy'
    { evaluateOnExit = Lude.Nothing,
      attempts = Lude.Nothing
    }

-- | Array of up to 5 objects that specify conditions under which the job should be retried or failed. If this parameter is specified, then the @attempts@ parameter must also be specified.
--
-- /Note:/ Consider using 'evaluateOnExit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsEvaluateOnExit :: Lens.Lens' RetryStrategy (Lude.Maybe [EvaluateOnExit])
rsEvaluateOnExit = Lens.lens (evaluateOnExit :: RetryStrategy -> Lude.Maybe [EvaluateOnExit]) (\s a -> s {evaluateOnExit = a} :: RetryStrategy)
{-# DEPRECATED rsEvaluateOnExit "Use generic-lens or generic-optics with 'evaluateOnExit' instead." #-}

-- | The number of times to move a job to the @RUNNABLE@ status. You may specify between 1 and 10 attempts. If the value of @attempts@ is greater than one, the job is retried on failure the same number of attempts as the value.
--
-- /Note:/ Consider using 'attempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsAttempts :: Lens.Lens' RetryStrategy (Lude.Maybe Lude.Int)
rsAttempts = Lens.lens (attempts :: RetryStrategy -> Lude.Maybe Lude.Int) (\s a -> s {attempts = a} :: RetryStrategy)
{-# DEPRECATED rsAttempts "Use generic-lens or generic-optics with 'attempts' instead." #-}

instance Lude.FromJSON RetryStrategy where
  parseJSON =
    Lude.withObject
      "RetryStrategy"
      ( \x ->
          RetryStrategy'
            Lude.<$> (x Lude..:? "evaluateOnExit" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "attempts")
      )

instance Lude.ToJSON RetryStrategy where
  toJSON RetryStrategy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("evaluateOnExit" Lude..=) Lude.<$> evaluateOnExit,
            ("attempts" Lude..=) Lude.<$> attempts
          ]
      )
