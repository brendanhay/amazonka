-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.BandwidthRateLimitInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.BandwidthRateLimitInterval
  ( BandwidthRateLimitInterval (..),

    -- * Smart constructor
    mkBandwidthRateLimitInterval,

    -- * Lenses
    brliAverageUploadRateLimitInBitsPerSec,
    brliAverageDownloadRateLimitInBitsPerSec,
    brliStartHourOfDay,
    brliStartMinuteOfHour,
    brliEndHourOfDay,
    brliEndMinuteOfHour,
    brliDaysOfWeek,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a bandwidth rate limit interval for a gateway. A bandwidth rate limit schedule consists of one or more bandwidth rate limit intervals. A bandwidth rate limit interval defines a period of time on one or more days of the week, during which bandwidth rate limits are specified for uploading, downloading, or both.
--
-- /See:/ 'mkBandwidthRateLimitInterval' smart constructor.
data BandwidthRateLimitInterval = BandwidthRateLimitInterval'
  { averageUploadRateLimitInBitsPerSec ::
      Lude.Maybe Lude.Natural,
    averageDownloadRateLimitInBitsPerSec ::
      Lude.Maybe Lude.Natural,
    startHourOfDay :: Lude.Natural,
    startMinuteOfHour :: Lude.Natural,
    endHourOfDay :: Lude.Natural,
    endMinuteOfHour :: Lude.Natural,
    daysOfWeek ::
      Lude.NonEmpty Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BandwidthRateLimitInterval' with the minimum fields required to make a request.
--
-- * 'averageDownloadRateLimitInBitsPerSec' - The average download rate limit component of the bandwidth rate limit interval, in bits per second. This field does not appear in the response if the download rate limit is not set.
-- * 'averageUploadRateLimitInBitsPerSec' - The average upload rate limit component of the bandwidth rate limit interval, in bits per second. This field does not appear in the response if the upload rate limit is not set.
-- * 'daysOfWeek' - The days of the week component of the bandwidth rate limit interval, represented as ordinal numbers from 0 to 6, where 0 represents Sunday and 6 Saturday.
-- * 'endHourOfDay' - The hour of the day to end the bandwidth rate limit interval.
-- * 'endMinuteOfHour' - The minute of the hour to end the bandwidth rate limit interval.
--
-- /Important:/ The bandwidth rate limit interval ends at the end of the minute. To end an interval at the end of an hour, use the value @59@ .
-- * 'startHourOfDay' - The hour of the day to start the bandwidth rate limit interval.
-- * 'startMinuteOfHour' - The minute of the hour to start the bandwidth rate limit interval. The interval begins at the start of that minute. To begin an interval exactly at the start of the hour, use the value @0@ .
mkBandwidthRateLimitInterval ::
  -- | 'startHourOfDay'
  Lude.Natural ->
  -- | 'startMinuteOfHour'
  Lude.Natural ->
  -- | 'endHourOfDay'
  Lude.Natural ->
  -- | 'endMinuteOfHour'
  Lude.Natural ->
  -- | 'daysOfWeek'
  Lude.NonEmpty Lude.Natural ->
  BandwidthRateLimitInterval
mkBandwidthRateLimitInterval
  pStartHourOfDay_
  pStartMinuteOfHour_
  pEndHourOfDay_
  pEndMinuteOfHour_
  pDaysOfWeek_ =
    BandwidthRateLimitInterval'
      { averageUploadRateLimitInBitsPerSec =
          Lude.Nothing,
        averageDownloadRateLimitInBitsPerSec = Lude.Nothing,
        startHourOfDay = pStartHourOfDay_,
        startMinuteOfHour = pStartMinuteOfHour_,
        endHourOfDay = pEndHourOfDay_,
        endMinuteOfHour = pEndMinuteOfHour_,
        daysOfWeek = pDaysOfWeek_
      }

-- | The average upload rate limit component of the bandwidth rate limit interval, in bits per second. This field does not appear in the response if the upload rate limit is not set.
--
-- /Note:/ Consider using 'averageUploadRateLimitInBitsPerSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliAverageUploadRateLimitInBitsPerSec :: Lens.Lens' BandwidthRateLimitInterval (Lude.Maybe Lude.Natural)
brliAverageUploadRateLimitInBitsPerSec = Lens.lens (averageUploadRateLimitInBitsPerSec :: BandwidthRateLimitInterval -> Lude.Maybe Lude.Natural) (\s a -> s {averageUploadRateLimitInBitsPerSec = a} :: BandwidthRateLimitInterval)
{-# DEPRECATED brliAverageUploadRateLimitInBitsPerSec "Use generic-lens or generic-optics with 'averageUploadRateLimitInBitsPerSec' instead." #-}

-- | The average download rate limit component of the bandwidth rate limit interval, in bits per second. This field does not appear in the response if the download rate limit is not set.
--
-- /Note:/ Consider using 'averageDownloadRateLimitInBitsPerSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliAverageDownloadRateLimitInBitsPerSec :: Lens.Lens' BandwidthRateLimitInterval (Lude.Maybe Lude.Natural)
brliAverageDownloadRateLimitInBitsPerSec = Lens.lens (averageDownloadRateLimitInBitsPerSec :: BandwidthRateLimitInterval -> Lude.Maybe Lude.Natural) (\s a -> s {averageDownloadRateLimitInBitsPerSec = a} :: BandwidthRateLimitInterval)
{-# DEPRECATED brliAverageDownloadRateLimitInBitsPerSec "Use generic-lens or generic-optics with 'averageDownloadRateLimitInBitsPerSec' instead." #-}

-- | The hour of the day to start the bandwidth rate limit interval.
--
-- /Note:/ Consider using 'startHourOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliStartHourOfDay :: Lens.Lens' BandwidthRateLimitInterval Lude.Natural
brliStartHourOfDay = Lens.lens (startHourOfDay :: BandwidthRateLimitInterval -> Lude.Natural) (\s a -> s {startHourOfDay = a} :: BandwidthRateLimitInterval)
{-# DEPRECATED brliStartHourOfDay "Use generic-lens or generic-optics with 'startHourOfDay' instead." #-}

-- | The minute of the hour to start the bandwidth rate limit interval. The interval begins at the start of that minute. To begin an interval exactly at the start of the hour, use the value @0@ .
--
-- /Note:/ Consider using 'startMinuteOfHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliStartMinuteOfHour :: Lens.Lens' BandwidthRateLimitInterval Lude.Natural
brliStartMinuteOfHour = Lens.lens (startMinuteOfHour :: BandwidthRateLimitInterval -> Lude.Natural) (\s a -> s {startMinuteOfHour = a} :: BandwidthRateLimitInterval)
{-# DEPRECATED brliStartMinuteOfHour "Use generic-lens or generic-optics with 'startMinuteOfHour' instead." #-}

-- | The hour of the day to end the bandwidth rate limit interval.
--
-- /Note:/ Consider using 'endHourOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliEndHourOfDay :: Lens.Lens' BandwidthRateLimitInterval Lude.Natural
brliEndHourOfDay = Lens.lens (endHourOfDay :: BandwidthRateLimitInterval -> Lude.Natural) (\s a -> s {endHourOfDay = a} :: BandwidthRateLimitInterval)
{-# DEPRECATED brliEndHourOfDay "Use generic-lens or generic-optics with 'endHourOfDay' instead." #-}

-- | The minute of the hour to end the bandwidth rate limit interval.
--
-- /Important:/ The bandwidth rate limit interval ends at the end of the minute. To end an interval at the end of an hour, use the value @59@ .
--
-- /Note:/ Consider using 'endMinuteOfHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliEndMinuteOfHour :: Lens.Lens' BandwidthRateLimitInterval Lude.Natural
brliEndMinuteOfHour = Lens.lens (endMinuteOfHour :: BandwidthRateLimitInterval -> Lude.Natural) (\s a -> s {endMinuteOfHour = a} :: BandwidthRateLimitInterval)
{-# DEPRECATED brliEndMinuteOfHour "Use generic-lens or generic-optics with 'endMinuteOfHour' instead." #-}

-- | The days of the week component of the bandwidth rate limit interval, represented as ordinal numbers from 0 to 6, where 0 represents Sunday and 6 Saturday.
--
-- /Note:/ Consider using 'daysOfWeek' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliDaysOfWeek :: Lens.Lens' BandwidthRateLimitInterval (Lude.NonEmpty Lude.Natural)
brliDaysOfWeek = Lens.lens (daysOfWeek :: BandwidthRateLimitInterval -> Lude.NonEmpty Lude.Natural) (\s a -> s {daysOfWeek = a} :: BandwidthRateLimitInterval)
{-# DEPRECATED brliDaysOfWeek "Use generic-lens or generic-optics with 'daysOfWeek' instead." #-}

instance Lude.FromJSON BandwidthRateLimitInterval where
  parseJSON =
    Lude.withObject
      "BandwidthRateLimitInterval"
      ( \x ->
          BandwidthRateLimitInterval'
            Lude.<$> (x Lude..:? "AverageUploadRateLimitInBitsPerSec")
            Lude.<*> (x Lude..:? "AverageDownloadRateLimitInBitsPerSec")
            Lude.<*> (x Lude..: "StartHourOfDay")
            Lude.<*> (x Lude..: "StartMinuteOfHour")
            Lude.<*> (x Lude..: "EndHourOfDay")
            Lude.<*> (x Lude..: "EndMinuteOfHour")
            Lude.<*> (x Lude..: "DaysOfWeek")
      )

instance Lude.ToJSON BandwidthRateLimitInterval where
  toJSON BandwidthRateLimitInterval' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AverageUploadRateLimitInBitsPerSec" Lude..=)
              Lude.<$> averageUploadRateLimitInBitsPerSec,
            ("AverageDownloadRateLimitInBitsPerSec" Lude..=)
              Lude.<$> averageDownloadRateLimitInBitsPerSec,
            Lude.Just ("StartHourOfDay" Lude..= startHourOfDay),
            Lude.Just ("StartMinuteOfHour" Lude..= startMinuteOfHour),
            Lude.Just ("EndHourOfDay" Lude..= endHourOfDay),
            Lude.Just ("EndMinuteOfHour" Lude..= endMinuteOfHour),
            Lude.Just ("DaysOfWeek" Lude..= daysOfWeek)
          ]
      )
