{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.TimeRangeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.TimeRangeFilter
  ( TimeRangeFilter (..),

    -- * Smart constructor
    mkTimeRangeFilter,

    -- * Lenses
    trfEndTime,
    trfStartTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Filters update actions from the service updates that are in available status during the time range.
--
-- /See:/ 'mkTimeRangeFilter' smart constructor.
data TimeRangeFilter = TimeRangeFilter'
  { -- | The end time of the time range filter
    endTime :: Core.Maybe Core.UTCTime,
    -- | The start time of the time range filter
    startTime :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TimeRangeFilter' value with any optional fields omitted.
mkTimeRangeFilter ::
  TimeRangeFilter
mkTimeRangeFilter =
  TimeRangeFilter'
    { endTime = Core.Nothing,
      startTime = Core.Nothing
    }

-- | The end time of the time range filter
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trfEndTime :: Lens.Lens' TimeRangeFilter (Core.Maybe Core.UTCTime)
trfEndTime = Lens.field @"endTime"
{-# DEPRECATED trfEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The start time of the time range filter
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trfStartTime :: Lens.Lens' TimeRangeFilter (Core.Maybe Core.UTCTime)
trfStartTime = Lens.field @"startTime"
{-# DEPRECATED trfStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}
