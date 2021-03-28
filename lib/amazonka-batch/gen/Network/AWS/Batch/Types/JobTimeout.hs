{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobTimeout
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.JobTimeout
  ( JobTimeout (..)
  -- * Smart constructor
  , mkJobTimeout
  -- * Lenses
  , jtAttemptDurationSeconds
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing a job timeout configuration.
--
-- /See:/ 'mkJobTimeout' smart constructor.
newtype JobTimeout = JobTimeout'
  { attemptDurationSeconds :: Core.Maybe Core.Int
    -- ^ The time duration in seconds (measured from the job attempt's @startedAt@ timestamp) after which AWS Batch terminates your jobs if they have not finished.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'JobTimeout' value with any optional fields omitted.
mkJobTimeout
    :: JobTimeout
mkJobTimeout = JobTimeout'{attemptDurationSeconds = Core.Nothing}

-- | The time duration in seconds (measured from the job attempt's @startedAt@ timestamp) after which AWS Batch terminates your jobs if they have not finished.
--
-- /Note:/ Consider using 'attemptDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtAttemptDurationSeconds :: Lens.Lens' JobTimeout (Core.Maybe Core.Int)
jtAttemptDurationSeconds = Lens.field @"attemptDurationSeconds"
{-# INLINEABLE jtAttemptDurationSeconds #-}
{-# DEPRECATED attemptDurationSeconds "Use generic-lens or generic-optics with 'attemptDurationSeconds' instead"  #-}

instance Core.FromJSON JobTimeout where
        toJSON JobTimeout{..}
          = Core.object
              (Core.catMaybes
                 [("attemptDurationSeconds" Core..=) Core.<$>
                    attemptDurationSeconds])

instance Core.FromJSON JobTimeout where
        parseJSON
          = Core.withObject "JobTimeout" Core.$
              \ x -> JobTimeout' Core.<$> (x Core..:? "attemptDurationSeconds")
