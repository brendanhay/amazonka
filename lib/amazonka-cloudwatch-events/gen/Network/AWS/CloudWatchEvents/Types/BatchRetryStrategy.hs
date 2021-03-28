{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy
  ( BatchRetryStrategy (..)
  -- * Smart constructor
  , mkBatchRetryStrategy
  -- * Lenses
  , brsAttempts
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The retry strategy to use for failed jobs, if the target is an AWS Batch job. If you specify a retry strategy here, it overrides the retry strategy defined in the job definition.
--
-- /See:/ 'mkBatchRetryStrategy' smart constructor.
newtype BatchRetryStrategy = BatchRetryStrategy'
  { attempts :: Core.Maybe Core.Int
    -- ^ The number of times to attempt to retry, if the job fails. Valid values are 1–10.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchRetryStrategy' value with any optional fields omitted.
mkBatchRetryStrategy
    :: BatchRetryStrategy
mkBatchRetryStrategy = BatchRetryStrategy'{attempts = Core.Nothing}

-- | The number of times to attempt to retry, if the job fails. Valid values are 1–10.
--
-- /Note:/ Consider using 'attempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsAttempts :: Lens.Lens' BatchRetryStrategy (Core.Maybe Core.Int)
brsAttempts = Lens.field @"attempts"
{-# INLINEABLE brsAttempts #-}
{-# DEPRECATED attempts "Use generic-lens or generic-optics with 'attempts' instead"  #-}

instance Core.FromJSON BatchRetryStrategy where
        toJSON BatchRetryStrategy{..}
          = Core.object
              (Core.catMaybes [("Attempts" Core..=) Core.<$> attempts])

instance Core.FromJSON BatchRetryStrategy where
        parseJSON
          = Core.withObject "BatchRetryStrategy" Core.$
              \ x -> BatchRetryStrategy' Core.<$> (x Core..:? "Attempts")
