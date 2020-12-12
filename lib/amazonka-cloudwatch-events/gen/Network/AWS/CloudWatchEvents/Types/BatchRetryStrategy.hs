{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy
  ( BatchRetryStrategy (..),

    -- * Smart constructor
    mkBatchRetryStrategy,

    -- * Lenses
    brsAttempts,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The retry strategy to use for failed jobs, if the target is an AWS Batch job. If you specify a retry strategy here, it overrides the retry strategy defined in the job definition.
--
-- /See:/ 'mkBatchRetryStrategy' smart constructor.
newtype BatchRetryStrategy = BatchRetryStrategy'
  { attempts ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchRetryStrategy' with the minimum fields required to make a request.
--
-- * 'attempts' - The number of times to attempt to retry, if the job fails. Valid values are 1–10.
mkBatchRetryStrategy ::
  BatchRetryStrategy
mkBatchRetryStrategy = BatchRetryStrategy' {attempts = Lude.Nothing}

-- | The number of times to attempt to retry, if the job fails. Valid values are 1–10.
--
-- /Note:/ Consider using 'attempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsAttempts :: Lens.Lens' BatchRetryStrategy (Lude.Maybe Lude.Int)
brsAttempts = Lens.lens (attempts :: BatchRetryStrategy -> Lude.Maybe Lude.Int) (\s a -> s {attempts = a} :: BatchRetryStrategy)
{-# DEPRECATED brsAttempts "Use generic-lens or generic-optics with 'attempts' instead." #-}

instance Lude.FromJSON BatchRetryStrategy where
  parseJSON =
    Lude.withObject
      "BatchRetryStrategy"
      (\x -> BatchRetryStrategy' Lude.<$> (x Lude..:? "Attempts"))

instance Lude.ToJSON BatchRetryStrategy where
  toJSON BatchRetryStrategy' {..} =
    Lude.object
      (Lude.catMaybes [("Attempts" Lude..=) Lude.<$> attempts])
