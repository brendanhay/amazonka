{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.BandwidthRateLimitInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.BandwidthRateLimitInterval
  ( BandwidthRateLimitInterval (..)
  -- * Smart constructor
  , mkBandwidthRateLimitInterval
  -- * Lenses
  , brliStartHourOfDay
  , brliStartMinuteOfHour
  , brliEndHourOfDay
  , brliEndMinuteOfHour
  , brliDaysOfWeek
  , brliAverageDownloadRateLimitInBitsPerSec
  , brliAverageUploadRateLimitInBitsPerSec
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a bandwidth rate limit interval for a gateway. A bandwidth rate limit schedule consists of one or more bandwidth rate limit intervals. A bandwidth rate limit interval defines a period of time on one or more days of the week, during which bandwidth rate limits are specified for uploading, downloading, or both. 
--
-- /See:/ 'mkBandwidthRateLimitInterval' smart constructor.
data BandwidthRateLimitInterval = BandwidthRateLimitInterval'
  { startHourOfDay :: Core.Natural
    -- ^ The hour of the day to start the bandwidth rate limit interval. 
  , startMinuteOfHour :: Core.Natural
    -- ^ The minute of the hour to start the bandwidth rate limit interval. The interval begins at the start of that minute. To begin an interval exactly at the start of the hour, use the value @0@ . 
  , endHourOfDay :: Core.Natural
    -- ^ The hour of the day to end the bandwidth rate limit interval. 
  , endMinuteOfHour :: Core.Natural
    -- ^ The minute of the hour to end the bandwidth rate limit interval. 
--
-- /Important:/ The bandwidth rate limit interval ends at the end of the minute. To end an interval at the end of an hour, use the value @59@ . 
  , daysOfWeek :: Core.NonEmpty Core.Natural
    -- ^ The days of the week component of the bandwidth rate limit interval, represented as ordinal numbers from 0 to 6, where 0 represents Sunday and 6 Saturday. 
  , averageDownloadRateLimitInBitsPerSec :: Core.Maybe Core.Natural
    -- ^ The average download rate limit component of the bandwidth rate limit interval, in bits per second. This field does not appear in the response if the download rate limit is not set. 
  , averageUploadRateLimitInBitsPerSec :: Core.Maybe Core.Natural
    -- ^ The average upload rate limit component of the bandwidth rate limit interval, in bits per second. This field does not appear in the response if the upload rate limit is not set. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BandwidthRateLimitInterval' value with any optional fields omitted.
mkBandwidthRateLimitInterval
    :: Core.Natural -- ^ 'startHourOfDay'
    -> Core.Natural -- ^ 'startMinuteOfHour'
    -> Core.Natural -- ^ 'endHourOfDay'
    -> Core.Natural -- ^ 'endMinuteOfHour'
    -> Core.NonEmpty Core.Natural -- ^ 'daysOfWeek'
    -> BandwidthRateLimitInterval
mkBandwidthRateLimitInterval startHourOfDay startMinuteOfHour
  endHourOfDay endMinuteOfHour daysOfWeek
  = BandwidthRateLimitInterval'{startHourOfDay, startMinuteOfHour,
                                endHourOfDay, endMinuteOfHour, daysOfWeek,
                                averageDownloadRateLimitInBitsPerSec = Core.Nothing,
                                averageUploadRateLimitInBitsPerSec = Core.Nothing}

-- | The hour of the day to start the bandwidth rate limit interval. 
--
-- /Note:/ Consider using 'startHourOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliStartHourOfDay :: Lens.Lens' BandwidthRateLimitInterval Core.Natural
brliStartHourOfDay = Lens.field @"startHourOfDay"
{-# INLINEABLE brliStartHourOfDay #-}
{-# DEPRECATED startHourOfDay "Use generic-lens or generic-optics with 'startHourOfDay' instead"  #-}

-- | The minute of the hour to start the bandwidth rate limit interval. The interval begins at the start of that minute. To begin an interval exactly at the start of the hour, use the value @0@ . 
--
-- /Note:/ Consider using 'startMinuteOfHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliStartMinuteOfHour :: Lens.Lens' BandwidthRateLimitInterval Core.Natural
brliStartMinuteOfHour = Lens.field @"startMinuteOfHour"
{-# INLINEABLE brliStartMinuteOfHour #-}
{-# DEPRECATED startMinuteOfHour "Use generic-lens or generic-optics with 'startMinuteOfHour' instead"  #-}

-- | The hour of the day to end the bandwidth rate limit interval. 
--
-- /Note:/ Consider using 'endHourOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliEndHourOfDay :: Lens.Lens' BandwidthRateLimitInterval Core.Natural
brliEndHourOfDay = Lens.field @"endHourOfDay"
{-# INLINEABLE brliEndHourOfDay #-}
{-# DEPRECATED endHourOfDay "Use generic-lens or generic-optics with 'endHourOfDay' instead"  #-}

-- | The minute of the hour to end the bandwidth rate limit interval. 
--
-- /Important:/ The bandwidth rate limit interval ends at the end of the minute. To end an interval at the end of an hour, use the value @59@ . 
--
-- /Note:/ Consider using 'endMinuteOfHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliEndMinuteOfHour :: Lens.Lens' BandwidthRateLimitInterval Core.Natural
brliEndMinuteOfHour = Lens.field @"endMinuteOfHour"
{-# INLINEABLE brliEndMinuteOfHour #-}
{-# DEPRECATED endMinuteOfHour "Use generic-lens or generic-optics with 'endMinuteOfHour' instead"  #-}

-- | The days of the week component of the bandwidth rate limit interval, represented as ordinal numbers from 0 to 6, where 0 represents Sunday and 6 Saturday. 
--
-- /Note:/ Consider using 'daysOfWeek' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliDaysOfWeek :: Lens.Lens' BandwidthRateLimitInterval (Core.NonEmpty Core.Natural)
brliDaysOfWeek = Lens.field @"daysOfWeek"
{-# INLINEABLE brliDaysOfWeek #-}
{-# DEPRECATED daysOfWeek "Use generic-lens or generic-optics with 'daysOfWeek' instead"  #-}

-- | The average download rate limit component of the bandwidth rate limit interval, in bits per second. This field does not appear in the response if the download rate limit is not set. 
--
-- /Note:/ Consider using 'averageDownloadRateLimitInBitsPerSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliAverageDownloadRateLimitInBitsPerSec :: Lens.Lens' BandwidthRateLimitInterval (Core.Maybe Core.Natural)
brliAverageDownloadRateLimitInBitsPerSec = Lens.field @"averageDownloadRateLimitInBitsPerSec"
{-# INLINEABLE brliAverageDownloadRateLimitInBitsPerSec #-}
{-# DEPRECATED averageDownloadRateLimitInBitsPerSec "Use generic-lens or generic-optics with 'averageDownloadRateLimitInBitsPerSec' instead"  #-}

-- | The average upload rate limit component of the bandwidth rate limit interval, in bits per second. This field does not appear in the response if the upload rate limit is not set. 
--
-- /Note:/ Consider using 'averageUploadRateLimitInBitsPerSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliAverageUploadRateLimitInBitsPerSec :: Lens.Lens' BandwidthRateLimitInterval (Core.Maybe Core.Natural)
brliAverageUploadRateLimitInBitsPerSec = Lens.field @"averageUploadRateLimitInBitsPerSec"
{-# INLINEABLE brliAverageUploadRateLimitInBitsPerSec #-}
{-# DEPRECATED averageUploadRateLimitInBitsPerSec "Use generic-lens or generic-optics with 'averageUploadRateLimitInBitsPerSec' instead"  #-}

instance Core.FromJSON BandwidthRateLimitInterval where
        toJSON BandwidthRateLimitInterval{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StartHourOfDay" Core..= startHourOfDay),
                  Core.Just ("StartMinuteOfHour" Core..= startMinuteOfHour),
                  Core.Just ("EndHourOfDay" Core..= endHourOfDay),
                  Core.Just ("EndMinuteOfHour" Core..= endMinuteOfHour),
                  Core.Just ("DaysOfWeek" Core..= daysOfWeek),
                  ("AverageDownloadRateLimitInBitsPerSec" Core..=) Core.<$>
                    averageDownloadRateLimitInBitsPerSec,
                  ("AverageUploadRateLimitInBitsPerSec" Core..=) Core.<$>
                    averageUploadRateLimitInBitsPerSec])

instance Core.FromJSON BandwidthRateLimitInterval where
        parseJSON
          = Core.withObject "BandwidthRateLimitInterval" Core.$
              \ x ->
                BandwidthRateLimitInterval' Core.<$>
                  (x Core..: "StartHourOfDay") Core.<*> x Core..: "StartMinuteOfHour"
                    Core.<*> x Core..: "EndHourOfDay"
                    Core.<*> x Core..: "EndMinuteOfHour"
                    Core.<*> x Core..: "DaysOfWeek"
                    Core.<*> x Core..:? "AverageDownloadRateLimitInBitsPerSec"
                    Core.<*> x Core..:? "AverageUploadRateLimitInBitsPerSec"
