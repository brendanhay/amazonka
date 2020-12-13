{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    brliDaysOfWeek,
    brliEndMinuteOfHour,
    brliStartMinuteOfHour,
    brliEndHourOfDay,
    brliStartHourOfDay,
    brliAverageUploadRateLimitInBitsPerSec,
    brliAverageDownloadRateLimitInBitsPerSec,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a bandwidth rate limit interval for a gateway. A bandwidth rate limit schedule consists of one or more bandwidth rate limit intervals. A bandwidth rate limit interval defines a period of time on one or more days of the week, during which bandwidth rate limits are specified for uploading, downloading, or both.
--
-- /See:/ 'mkBandwidthRateLimitInterval' smart constructor.
data BandwidthRateLimitInterval = BandwidthRateLimitInterval'
  { -- | The days of the week component of the bandwidth rate limit interval, represented as ordinal numbers from 0 to 6, where 0 represents Sunday and 6 Saturday.
    daysOfWeek :: Lude.NonEmpty Lude.Natural,
    -- | The minute of the hour to end the bandwidth rate limit interval.
    --
    -- /Important:/ The bandwidth rate limit interval ends at the end of the minute. To end an interval at the end of an hour, use the value @59@ .
    endMinuteOfHour :: Lude.Natural,
    -- | The minute of the hour to start the bandwidth rate limit interval. The interval begins at the start of that minute. To begin an interval exactly at the start of the hour, use the value @0@ .
    startMinuteOfHour :: Lude.Natural,
    -- | The hour of the day to end the bandwidth rate limit interval.
    endHourOfDay :: Lude.Natural,
    -- | The hour of the day to start the bandwidth rate limit interval.
    startHourOfDay :: Lude.Natural,
    -- | The average upload rate limit component of the bandwidth rate limit interval, in bits per second. This field does not appear in the response if the upload rate limit is not set.
    averageUploadRateLimitInBitsPerSec :: Lude.Maybe Lude.Natural,
    -- | The average download rate limit component of the bandwidth rate limit interval, in bits per second. This field does not appear in the response if the download rate limit is not set.
    averageDownloadRateLimitInBitsPerSec :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BandwidthRateLimitInterval' with the minimum fields required to make a request.
--
-- * 'daysOfWeek' - The days of the week component of the bandwidth rate limit interval, represented as ordinal numbers from 0 to 6, where 0 represents Sunday and 6 Saturday.
-- * 'endMinuteOfHour' - The minute of the hour to end the bandwidth rate limit interval.
--
-- /Important:/ The bandwidth rate limit interval ends at the end of the minute. To end an interval at the end of an hour, use the value @59@ .
-- * 'startMinuteOfHour' - The minute of the hour to start the bandwidth rate limit interval. The interval begins at the start of that minute. To begin an interval exactly at the start of the hour, use the value @0@ .
-- * 'endHourOfDay' - The hour of the day to end the bandwidth rate limit interval.
-- * 'startHourOfDay' - The hour of the day to start the bandwidth rate limit interval.
-- * 'averageUploadRateLimitInBitsPerSec' - The average upload rate limit component of the bandwidth rate limit interval, in bits per second. This field does not appear in the response if the upload rate limit is not set.
-- * 'averageDownloadRateLimitInBitsPerSec' - The average download rate limit component of the bandwidth rate limit interval, in bits per second. This field does not appear in the response if the download rate limit is not set.
mkBandwidthRateLimitInterval ::
  -- | 'daysOfWeek'
  Lude.NonEmpty Lude.Natural ->
  -- | 'endMinuteOfHour'
  Lude.Natural ->
  -- | 'startMinuteOfHour'
  Lude.Natural ->
  -- | 'endHourOfDay'
  Lude.Natural ->
  -- | 'startHourOfDay'
  Lude.Natural ->
  BandwidthRateLimitInterval
mkBandwidthRateLimitInterval
  pDaysOfWeek_
  pEndMinuteOfHour_
  pStartMinuteOfHour_
  pEndHourOfDay_
  pStartHourOfDay_ =
    BandwidthRateLimitInterval'
      { daysOfWeek = pDaysOfWeek_,
        endMinuteOfHour = pEndMinuteOfHour_,
        startMinuteOfHour = pStartMinuteOfHour_,
        endHourOfDay = pEndHourOfDay_,
        startHourOfDay = pStartHourOfDay_,
        averageUploadRateLimitInBitsPerSec = Lude.Nothing,
        averageDownloadRateLimitInBitsPerSec = Lude.Nothing
      }

-- | The days of the week component of the bandwidth rate limit interval, represented as ordinal numbers from 0 to 6, where 0 represents Sunday and 6 Saturday.
--
-- /Note:/ Consider using 'daysOfWeek' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliDaysOfWeek :: Lens.Lens' BandwidthRateLimitInterval (Lude.NonEmpty Lude.Natural)
brliDaysOfWeek = Lens.lens (daysOfWeek :: BandwidthRateLimitInterval -> Lude.NonEmpty Lude.Natural) (\s a -> s {daysOfWeek = a} :: BandwidthRateLimitInterval)
{-# DEPRECATED brliDaysOfWeek "Use generic-lens or generic-optics with 'daysOfWeek' instead." #-}

-- | The minute of the hour to end the bandwidth rate limit interval.
--
-- /Important:/ The bandwidth rate limit interval ends at the end of the minute. To end an interval at the end of an hour, use the value @59@ .
--
-- /Note:/ Consider using 'endMinuteOfHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliEndMinuteOfHour :: Lens.Lens' BandwidthRateLimitInterval Lude.Natural
brliEndMinuteOfHour = Lens.lens (endMinuteOfHour :: BandwidthRateLimitInterval -> Lude.Natural) (\s a -> s {endMinuteOfHour = a} :: BandwidthRateLimitInterval)
{-# DEPRECATED brliEndMinuteOfHour "Use generic-lens or generic-optics with 'endMinuteOfHour' instead." #-}

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

-- | The hour of the day to start the bandwidth rate limit interval.
--
-- /Note:/ Consider using 'startHourOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brliStartHourOfDay :: Lens.Lens' BandwidthRateLimitInterval Lude.Natural
brliStartHourOfDay = Lens.lens (startHourOfDay :: BandwidthRateLimitInterval -> Lude.Natural) (\s a -> s {startHourOfDay = a} :: BandwidthRateLimitInterval)
{-# DEPRECATED brliStartHourOfDay "Use generic-lens or generic-optics with 'startHourOfDay' instead." #-}

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

instance Lude.FromJSON BandwidthRateLimitInterval where
  parseJSON =
    Lude.withObject
      "BandwidthRateLimitInterval"
      ( \x ->
          BandwidthRateLimitInterval'
            Lude.<$> (x Lude..: "DaysOfWeek")
            Lude.<*> (x Lude..: "EndMinuteOfHour")
            Lude.<*> (x Lude..: "StartMinuteOfHour")
            Lude.<*> (x Lude..: "EndHourOfDay")
            Lude.<*> (x Lude..: "StartHourOfDay")
            Lude.<*> (x Lude..:? "AverageUploadRateLimitInBitsPerSec")
            Lude.<*> (x Lude..:? "AverageDownloadRateLimitInBitsPerSec")
      )

instance Lude.ToJSON BandwidthRateLimitInterval where
  toJSON BandwidthRateLimitInterval' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DaysOfWeek" Lude..= daysOfWeek),
            Lude.Just ("EndMinuteOfHour" Lude..= endMinuteOfHour),
            Lude.Just ("StartMinuteOfHour" Lude..= startMinuteOfHour),
            Lude.Just ("EndHourOfDay" Lude..= endHourOfDay),
            Lude.Just ("StartHourOfDay" Lude..= startHourOfDay),
            ("AverageUploadRateLimitInBitsPerSec" Lude..=)
              Lude.<$> averageUploadRateLimitInBitsPerSec,
            ("AverageDownloadRateLimitInBitsPerSec" Lude..=)
              Lude.<$> averageDownloadRateLimitInBitsPerSec
          ]
      )
