{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.StorageGateway.Types.BandwidthRateLimitInterval
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.BandwidthRateLimitInterval where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a bandwidth rate limit interval for a gateway. A bandwidth
-- rate limit schedule consists of one or more bandwidth rate limit
-- intervals. A bandwidth rate limit interval defines a period of time on
-- one or more days of the week, during which bandwidth rate limits are
-- specified for uploading, downloading, or both.
--
-- /See:/ 'newBandwidthRateLimitInterval' smart constructor.
data BandwidthRateLimitInterval = BandwidthRateLimitInterval'
  { -- | The average download rate limit component of the bandwidth rate limit
    -- interval, in bits per second. This field does not appear in the response
    -- if the download rate limit is not set.
    averageDownloadRateLimitInBitsPerSec :: Prelude.Maybe Prelude.Natural,
    -- | The average upload rate limit component of the bandwidth rate limit
    -- interval, in bits per second. This field does not appear in the response
    -- if the upload rate limit is not set.
    averageUploadRateLimitInBitsPerSec :: Prelude.Maybe Prelude.Natural,
    -- | The hour of the day to start the bandwidth rate limit interval.
    startHourOfDay :: Prelude.Natural,
    -- | The minute of the hour to start the bandwidth rate limit interval. The
    -- interval begins at the start of that minute. To begin an interval
    -- exactly at the start of the hour, use the value @0@.
    startMinuteOfHour :: Prelude.Natural,
    -- | The hour of the day to end the bandwidth rate limit interval.
    endHourOfDay :: Prelude.Natural,
    -- | The minute of the hour to end the bandwidth rate limit interval.
    --
    -- The bandwidth rate limit interval ends at the end of the minute. To end
    -- an interval at the end of an hour, use the value @59@.
    endMinuteOfHour :: Prelude.Natural,
    -- | The days of the week component of the bandwidth rate limit interval,
    -- represented as ordinal numbers from 0 to 6, where 0 represents Sunday
    -- and 6 represents Saturday.
    daysOfWeek :: Prelude.NonEmpty Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BandwidthRateLimitInterval' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'averageDownloadRateLimitInBitsPerSec', 'bandwidthRateLimitInterval_averageDownloadRateLimitInBitsPerSec' - The average download rate limit component of the bandwidth rate limit
-- interval, in bits per second. This field does not appear in the response
-- if the download rate limit is not set.
--
-- 'averageUploadRateLimitInBitsPerSec', 'bandwidthRateLimitInterval_averageUploadRateLimitInBitsPerSec' - The average upload rate limit component of the bandwidth rate limit
-- interval, in bits per second. This field does not appear in the response
-- if the upload rate limit is not set.
--
-- 'startHourOfDay', 'bandwidthRateLimitInterval_startHourOfDay' - The hour of the day to start the bandwidth rate limit interval.
--
-- 'startMinuteOfHour', 'bandwidthRateLimitInterval_startMinuteOfHour' - The minute of the hour to start the bandwidth rate limit interval. The
-- interval begins at the start of that minute. To begin an interval
-- exactly at the start of the hour, use the value @0@.
--
-- 'endHourOfDay', 'bandwidthRateLimitInterval_endHourOfDay' - The hour of the day to end the bandwidth rate limit interval.
--
-- 'endMinuteOfHour', 'bandwidthRateLimitInterval_endMinuteOfHour' - The minute of the hour to end the bandwidth rate limit interval.
--
-- The bandwidth rate limit interval ends at the end of the minute. To end
-- an interval at the end of an hour, use the value @59@.
--
-- 'daysOfWeek', 'bandwidthRateLimitInterval_daysOfWeek' - The days of the week component of the bandwidth rate limit interval,
-- represented as ordinal numbers from 0 to 6, where 0 represents Sunday
-- and 6 represents Saturday.
newBandwidthRateLimitInterval ::
  -- | 'startHourOfDay'
  Prelude.Natural ->
  -- | 'startMinuteOfHour'
  Prelude.Natural ->
  -- | 'endHourOfDay'
  Prelude.Natural ->
  -- | 'endMinuteOfHour'
  Prelude.Natural ->
  -- | 'daysOfWeek'
  Prelude.NonEmpty Prelude.Natural ->
  BandwidthRateLimitInterval
newBandwidthRateLimitInterval
  pStartHourOfDay_
  pStartMinuteOfHour_
  pEndHourOfDay_
  pEndMinuteOfHour_
  pDaysOfWeek_ =
    BandwidthRateLimitInterval'
      { averageDownloadRateLimitInBitsPerSec =
          Prelude.Nothing,
        averageUploadRateLimitInBitsPerSec =
          Prelude.Nothing,
        startHourOfDay = pStartHourOfDay_,
        startMinuteOfHour = pStartMinuteOfHour_,
        endHourOfDay = pEndHourOfDay_,
        endMinuteOfHour = pEndMinuteOfHour_,
        daysOfWeek = Lens.coerced Lens.# pDaysOfWeek_
      }

-- | The average download rate limit component of the bandwidth rate limit
-- interval, in bits per second. This field does not appear in the response
-- if the download rate limit is not set.
bandwidthRateLimitInterval_averageDownloadRateLimitInBitsPerSec :: Lens.Lens' BandwidthRateLimitInterval (Prelude.Maybe Prelude.Natural)
bandwidthRateLimitInterval_averageDownloadRateLimitInBitsPerSec = Lens.lens (\BandwidthRateLimitInterval' {averageDownloadRateLimitInBitsPerSec} -> averageDownloadRateLimitInBitsPerSec) (\s@BandwidthRateLimitInterval' {} a -> s {averageDownloadRateLimitInBitsPerSec = a} :: BandwidthRateLimitInterval)

-- | The average upload rate limit component of the bandwidth rate limit
-- interval, in bits per second. This field does not appear in the response
-- if the upload rate limit is not set.
bandwidthRateLimitInterval_averageUploadRateLimitInBitsPerSec :: Lens.Lens' BandwidthRateLimitInterval (Prelude.Maybe Prelude.Natural)
bandwidthRateLimitInterval_averageUploadRateLimitInBitsPerSec = Lens.lens (\BandwidthRateLimitInterval' {averageUploadRateLimitInBitsPerSec} -> averageUploadRateLimitInBitsPerSec) (\s@BandwidthRateLimitInterval' {} a -> s {averageUploadRateLimitInBitsPerSec = a} :: BandwidthRateLimitInterval)

-- | The hour of the day to start the bandwidth rate limit interval.
bandwidthRateLimitInterval_startHourOfDay :: Lens.Lens' BandwidthRateLimitInterval Prelude.Natural
bandwidthRateLimitInterval_startHourOfDay = Lens.lens (\BandwidthRateLimitInterval' {startHourOfDay} -> startHourOfDay) (\s@BandwidthRateLimitInterval' {} a -> s {startHourOfDay = a} :: BandwidthRateLimitInterval)

-- | The minute of the hour to start the bandwidth rate limit interval. The
-- interval begins at the start of that minute. To begin an interval
-- exactly at the start of the hour, use the value @0@.
bandwidthRateLimitInterval_startMinuteOfHour :: Lens.Lens' BandwidthRateLimitInterval Prelude.Natural
bandwidthRateLimitInterval_startMinuteOfHour = Lens.lens (\BandwidthRateLimitInterval' {startMinuteOfHour} -> startMinuteOfHour) (\s@BandwidthRateLimitInterval' {} a -> s {startMinuteOfHour = a} :: BandwidthRateLimitInterval)

-- | The hour of the day to end the bandwidth rate limit interval.
bandwidthRateLimitInterval_endHourOfDay :: Lens.Lens' BandwidthRateLimitInterval Prelude.Natural
bandwidthRateLimitInterval_endHourOfDay = Lens.lens (\BandwidthRateLimitInterval' {endHourOfDay} -> endHourOfDay) (\s@BandwidthRateLimitInterval' {} a -> s {endHourOfDay = a} :: BandwidthRateLimitInterval)

-- | The minute of the hour to end the bandwidth rate limit interval.
--
-- The bandwidth rate limit interval ends at the end of the minute. To end
-- an interval at the end of an hour, use the value @59@.
bandwidthRateLimitInterval_endMinuteOfHour :: Lens.Lens' BandwidthRateLimitInterval Prelude.Natural
bandwidthRateLimitInterval_endMinuteOfHour = Lens.lens (\BandwidthRateLimitInterval' {endMinuteOfHour} -> endMinuteOfHour) (\s@BandwidthRateLimitInterval' {} a -> s {endMinuteOfHour = a} :: BandwidthRateLimitInterval)

-- | The days of the week component of the bandwidth rate limit interval,
-- represented as ordinal numbers from 0 to 6, where 0 represents Sunday
-- and 6 represents Saturday.
bandwidthRateLimitInterval_daysOfWeek :: Lens.Lens' BandwidthRateLimitInterval (Prelude.NonEmpty Prelude.Natural)
bandwidthRateLimitInterval_daysOfWeek = Lens.lens (\BandwidthRateLimitInterval' {daysOfWeek} -> daysOfWeek) (\s@BandwidthRateLimitInterval' {} a -> s {daysOfWeek = a} :: BandwidthRateLimitInterval) Prelude.. Lens.coerced

instance Data.FromJSON BandwidthRateLimitInterval where
  parseJSON =
    Data.withObject
      "BandwidthRateLimitInterval"
      ( \x ->
          BandwidthRateLimitInterval'
            Prelude.<$> (x Data..:? "AverageDownloadRateLimitInBitsPerSec")
            Prelude.<*> (x Data..:? "AverageUploadRateLimitInBitsPerSec")
            Prelude.<*> (x Data..: "StartHourOfDay")
            Prelude.<*> (x Data..: "StartMinuteOfHour")
            Prelude.<*> (x Data..: "EndHourOfDay")
            Prelude.<*> (x Data..: "EndMinuteOfHour")
            Prelude.<*> (x Data..: "DaysOfWeek")
      )

instance Prelude.Hashable BandwidthRateLimitInterval where
  hashWithSalt _salt BandwidthRateLimitInterval' {..} =
    _salt
      `Prelude.hashWithSalt` averageDownloadRateLimitInBitsPerSec
      `Prelude.hashWithSalt` averageUploadRateLimitInBitsPerSec
      `Prelude.hashWithSalt` startHourOfDay
      `Prelude.hashWithSalt` startMinuteOfHour
      `Prelude.hashWithSalt` endHourOfDay
      `Prelude.hashWithSalt` endMinuteOfHour
      `Prelude.hashWithSalt` daysOfWeek

instance Prelude.NFData BandwidthRateLimitInterval where
  rnf BandwidthRateLimitInterval' {..} =
    Prelude.rnf averageDownloadRateLimitInBitsPerSec `Prelude.seq`
      Prelude.rnf averageUploadRateLimitInBitsPerSec `Prelude.seq`
        Prelude.rnf startHourOfDay `Prelude.seq`
          Prelude.rnf startMinuteOfHour `Prelude.seq`
            Prelude.rnf endHourOfDay `Prelude.seq`
              Prelude.rnf endMinuteOfHour `Prelude.seq`
                Prelude.rnf daysOfWeek

instance Data.ToJSON BandwidthRateLimitInterval where
  toJSON BandwidthRateLimitInterval' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AverageDownloadRateLimitInBitsPerSec" Data..=)
              Prelude.<$> averageDownloadRateLimitInBitsPerSec,
            ("AverageUploadRateLimitInBitsPerSec" Data..=)
              Prelude.<$> averageUploadRateLimitInBitsPerSec,
            Prelude.Just
              ("StartHourOfDay" Data..= startHourOfDay),
            Prelude.Just
              ("StartMinuteOfHour" Data..= startMinuteOfHour),
            Prelude.Just ("EndHourOfDay" Data..= endHourOfDay),
            Prelude.Just
              ("EndMinuteOfHour" Data..= endMinuteOfHour),
            Prelude.Just ("DaysOfWeek" Data..= daysOfWeek)
          ]
      )
