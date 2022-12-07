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
-- Module      : Amazonka.Forecast.Types.TimeAlignmentBoundary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.TimeAlignmentBoundary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.DayOfWeek
import Amazonka.Forecast.Types.Month
import qualified Amazonka.Prelude as Prelude

-- | The time boundary Forecast uses to align and aggregate your data to
-- match your forecast frequency. Provide the unit of time and the time
-- boundary as a key value pair. If you don\'t provide a time boundary,
-- Forecast uses a set of
-- <https://docs.aws.amazon.com/forecast/latest/dg/data-aggregation.html#default-time-boundaries Default Time Boundaries>.
--
-- For more information about aggregation, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/data-aggregation.html Data Aggregation for Different Forecast Frequencies>.
-- For more information setting a custom time boundary, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/data-aggregation.html#specifying-time-boundary Specifying a Time Boundary>.
--
-- /See:/ 'newTimeAlignmentBoundary' smart constructor.
data TimeAlignmentBoundary = TimeAlignmentBoundary'
  { -- | The day of week to use for time alignment during aggregation. The day
    -- must be in uppercase.
    dayOfWeek :: Prelude.Maybe DayOfWeek,
    -- | The month to use for time alignment during aggregation. The month must
    -- be in uppercase.
    month :: Prelude.Maybe Month,
    -- | The hour of day to use for time alignment during aggregation.
    hour :: Prelude.Maybe Prelude.Natural,
    -- | The day of the month to use for time alignment during aggregation.
    dayOfMonth :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeAlignmentBoundary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dayOfWeek', 'timeAlignmentBoundary_dayOfWeek' - The day of week to use for time alignment during aggregation. The day
-- must be in uppercase.
--
-- 'month', 'timeAlignmentBoundary_month' - The month to use for time alignment during aggregation. The month must
-- be in uppercase.
--
-- 'hour', 'timeAlignmentBoundary_hour' - The hour of day to use for time alignment during aggregation.
--
-- 'dayOfMonth', 'timeAlignmentBoundary_dayOfMonth' - The day of the month to use for time alignment during aggregation.
newTimeAlignmentBoundary ::
  TimeAlignmentBoundary
newTimeAlignmentBoundary =
  TimeAlignmentBoundary'
    { dayOfWeek = Prelude.Nothing,
      month = Prelude.Nothing,
      hour = Prelude.Nothing,
      dayOfMonth = Prelude.Nothing
    }

-- | The day of week to use for time alignment during aggregation. The day
-- must be in uppercase.
timeAlignmentBoundary_dayOfWeek :: Lens.Lens' TimeAlignmentBoundary (Prelude.Maybe DayOfWeek)
timeAlignmentBoundary_dayOfWeek = Lens.lens (\TimeAlignmentBoundary' {dayOfWeek} -> dayOfWeek) (\s@TimeAlignmentBoundary' {} a -> s {dayOfWeek = a} :: TimeAlignmentBoundary)

-- | The month to use for time alignment during aggregation. The month must
-- be in uppercase.
timeAlignmentBoundary_month :: Lens.Lens' TimeAlignmentBoundary (Prelude.Maybe Month)
timeAlignmentBoundary_month = Lens.lens (\TimeAlignmentBoundary' {month} -> month) (\s@TimeAlignmentBoundary' {} a -> s {month = a} :: TimeAlignmentBoundary)

-- | The hour of day to use for time alignment during aggregation.
timeAlignmentBoundary_hour :: Lens.Lens' TimeAlignmentBoundary (Prelude.Maybe Prelude.Natural)
timeAlignmentBoundary_hour = Lens.lens (\TimeAlignmentBoundary' {hour} -> hour) (\s@TimeAlignmentBoundary' {} a -> s {hour = a} :: TimeAlignmentBoundary)

-- | The day of the month to use for time alignment during aggregation.
timeAlignmentBoundary_dayOfMonth :: Lens.Lens' TimeAlignmentBoundary (Prelude.Maybe Prelude.Natural)
timeAlignmentBoundary_dayOfMonth = Lens.lens (\TimeAlignmentBoundary' {dayOfMonth} -> dayOfMonth) (\s@TimeAlignmentBoundary' {} a -> s {dayOfMonth = a} :: TimeAlignmentBoundary)

instance Data.FromJSON TimeAlignmentBoundary where
  parseJSON =
    Data.withObject
      "TimeAlignmentBoundary"
      ( \x ->
          TimeAlignmentBoundary'
            Prelude.<$> (x Data..:? "DayOfWeek")
            Prelude.<*> (x Data..:? "Month")
            Prelude.<*> (x Data..:? "Hour")
            Prelude.<*> (x Data..:? "DayOfMonth")
      )

instance Prelude.Hashable TimeAlignmentBoundary where
  hashWithSalt _salt TimeAlignmentBoundary' {..} =
    _salt `Prelude.hashWithSalt` dayOfWeek
      `Prelude.hashWithSalt` month
      `Prelude.hashWithSalt` hour
      `Prelude.hashWithSalt` dayOfMonth

instance Prelude.NFData TimeAlignmentBoundary where
  rnf TimeAlignmentBoundary' {..} =
    Prelude.rnf dayOfWeek
      `Prelude.seq` Prelude.rnf month
      `Prelude.seq` Prelude.rnf hour
      `Prelude.seq` Prelude.rnf dayOfMonth

instance Data.ToJSON TimeAlignmentBoundary where
  toJSON TimeAlignmentBoundary' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DayOfWeek" Data..=) Prelude.<$> dayOfWeek,
            ("Month" Data..=) Prelude.<$> month,
            ("Hour" Data..=) Prelude.<$> hour,
            ("DayOfMonth" Data..=) Prelude.<$> dayOfMonth
          ]
      )
