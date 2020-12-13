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
    trfStartTime,
    trfEndTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Filters update actions from the service updates that are in available status during the time range.
--
-- /See:/ 'mkTimeRangeFilter' smart constructor.
data TimeRangeFilter = TimeRangeFilter'
  { -- | The start time of the time range filter
    startTime :: Lude.Maybe Lude.DateTime,
    -- | The end time of the time range filter
    endTime :: Lude.Maybe Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimeRangeFilter' with the minimum fields required to make a request.
--
-- * 'startTime' - The start time of the time range filter
-- * 'endTime' - The end time of the time range filter
mkTimeRangeFilter ::
  TimeRangeFilter
mkTimeRangeFilter =
  TimeRangeFilter'
    { startTime = Lude.Nothing,
      endTime = Lude.Nothing
    }

-- | The start time of the time range filter
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trfStartTime :: Lens.Lens' TimeRangeFilter (Lude.Maybe Lude.DateTime)
trfStartTime = Lens.lens (startTime :: TimeRangeFilter -> Lude.Maybe Lude.DateTime) (\s a -> s {startTime = a} :: TimeRangeFilter)
{-# DEPRECATED trfStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The end time of the time range filter
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trfEndTime :: Lens.Lens' TimeRangeFilter (Lude.Maybe Lude.DateTime)
trfEndTime = Lens.lens (endTime :: TimeRangeFilter -> Lude.Maybe Lude.DateTime) (\s a -> s {endTime = a} :: TimeRangeFilter)
{-# DEPRECATED trfEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Lude.ToQuery TimeRangeFilter where
  toQuery TimeRangeFilter' {..} =
    Lude.mconcat
      ["StartTime" Lude.=: startTime, "EndTime" Lude.=: endTime]
