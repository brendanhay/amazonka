{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.TimestampRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.TimestampRange
  ( TimestampRange (..),

    -- * Smart constructor
    mkTimestampRange,

    -- * Lenses
    trStartTimestamp,
    trEndTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The range of timestamps for which to return fragments.
--
-- /See:/ 'mkTimestampRange' smart constructor.
data TimestampRange = TimestampRange'
  { -- | The starting timestamp in the range of timestamps for which to return fragments.
    startTimestamp :: Core.NominalDiffTime,
    -- | The ending timestamp in the range of timestamps for which to return fragments.
    endTimestamp :: Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TimestampRange' value with any optional fields omitted.
mkTimestampRange ::
  -- | 'startTimestamp'
  Core.NominalDiffTime ->
  -- | 'endTimestamp'
  Core.NominalDiffTime ->
  TimestampRange
mkTimestampRange startTimestamp endTimestamp =
  TimestampRange' {startTimestamp, endTimestamp}

-- | The starting timestamp in the range of timestamps for which to return fragments.
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trStartTimestamp :: Lens.Lens' TimestampRange Core.NominalDiffTime
trStartTimestamp = Lens.field @"startTimestamp"
{-# DEPRECATED trStartTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead." #-}

-- | The ending timestamp in the range of timestamps for which to return fragments.
--
-- /Note:/ Consider using 'endTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trEndTimestamp :: Lens.Lens' TimestampRange Core.NominalDiffTime
trEndTimestamp = Lens.field @"endTimestamp"
{-# DEPRECATED trEndTimestamp "Use generic-lens or generic-optics with 'endTimestamp' instead." #-}

instance Core.FromJSON TimestampRange where
  toJSON TimestampRange {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StartTimestamp" Core..= startTimestamp),
            Core.Just ("EndTimestamp" Core..= endTimestamp)
          ]
      )
