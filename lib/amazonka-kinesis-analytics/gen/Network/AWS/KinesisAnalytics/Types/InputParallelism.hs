{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputParallelism
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.InputParallelism
  ( InputParallelism (..)
  -- * Smart constructor
  , mkInputParallelism
  -- * Lenses
  , ipCount
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the number of in-application streams to create for a given streaming source. For information about parallelism, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> . 
--
-- /See:/ 'mkInputParallelism' smart constructor.
newtype InputParallelism = InputParallelism'
  { count :: Core.Maybe Core.Natural
    -- ^ Number of in-application streams to create. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InputParallelism' value with any optional fields omitted.
mkInputParallelism
    :: InputParallelism
mkInputParallelism = InputParallelism'{count = Core.Nothing}

-- | Number of in-application streams to create. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits> . 
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipCount :: Lens.Lens' InputParallelism (Core.Maybe Core.Natural)
ipCount = Lens.field @"count"
{-# INLINEABLE ipCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

instance Core.FromJSON InputParallelism where
        toJSON InputParallelism{..}
          = Core.object (Core.catMaybes [("Count" Core..=) Core.<$> count])

instance Core.FromJSON InputParallelism where
        parseJSON
          = Core.withObject "InputParallelism" Core.$
              \ x -> InputParallelism' Core.<$> (x Core..:? "Count")
