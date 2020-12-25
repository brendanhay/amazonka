{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Range
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.Range
  ( Range (..),

    -- * Smart constructor
    mkRange,

    -- * Lenses
    rStartTime,
    rEndTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies one range of days or times to exclude from use for training an anomaly detection model.
--
-- /See:/ 'mkRange' smart constructor.
data Range = Range'
  { -- | The start time of the range to exclude. The format is @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
    startTime :: Core.UTCTime,
    -- | The end time of the range to exclude. The format is @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
    endTime :: Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Range' value with any optional fields omitted.
mkRange ::
  -- | 'startTime'
  Core.UTCTime ->
  -- | 'endTime'
  Core.UTCTime ->
  Range
mkRange startTime endTime = Range' {startTime, endTime}

-- | The start time of the range to exclude. The format is @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStartTime :: Lens.Lens' Range Core.UTCTime
rStartTime = Lens.field @"startTime"
{-# DEPRECATED rStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The end time of the range to exclude. The format is @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEndTime :: Lens.Lens' Range Core.UTCTime
rEndTime = Lens.field @"endTime"
{-# DEPRECATED rEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Core.FromXML Range where
  parseXML x =
    Range'
      Core.<$> (x Core..@ "StartTime") Core.<*> (x Core..@ "EndTime")
