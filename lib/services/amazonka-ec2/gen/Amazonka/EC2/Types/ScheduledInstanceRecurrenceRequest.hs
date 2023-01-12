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
-- Module      : Amazonka.EC2.Types.ScheduledInstanceRecurrenceRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ScheduledInstanceRecurrenceRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the recurring schedule for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstanceRecurrenceRequest' smart constructor.
data ScheduledInstanceRecurrenceRequest = ScheduledInstanceRecurrenceRequest'
  { -- | The frequency (@Daily@, @Weekly@, or @Monthly@).
    frequency :: Prelude.Maybe Prelude.Text,
    -- | The interval quantity. The interval unit depends on the value of
    -- @Frequency@. For example, every 2 weeks or every 2 months.
    interval :: Prelude.Maybe Prelude.Int,
    -- | The days. For a monthly schedule, this is one or more days of the month
    -- (1-31). For a weekly schedule, this is one or more days of the week
    -- (1-7, where 1 is Sunday). You can\'t specify this value with a daily
    -- schedule. If the occurrence is relative to the end of the month, you can
    -- specify only a single day.
    occurrenceDays :: Prelude.Maybe [Prelude.Int],
    -- | Indicates whether the occurrence is relative to the end of the specified
    -- week or month. You can\'t specify this value with a daily schedule.
    occurrenceRelativeToEnd :: Prelude.Maybe Prelude.Bool,
    -- | The unit for @OccurrenceDays@ (@DayOfWeek@ or @DayOfMonth@). This value
    -- is required for a monthly schedule. You can\'t specify @DayOfWeek@ with
    -- a weekly schedule. You can\'t specify this value with a daily schedule.
    occurrenceUnit :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledInstanceRecurrenceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frequency', 'scheduledInstanceRecurrenceRequest_frequency' - The frequency (@Daily@, @Weekly@, or @Monthly@).
--
-- 'interval', 'scheduledInstanceRecurrenceRequest_interval' - The interval quantity. The interval unit depends on the value of
-- @Frequency@. For example, every 2 weeks or every 2 months.
--
-- 'occurrenceDays', 'scheduledInstanceRecurrenceRequest_occurrenceDays' - The days. For a monthly schedule, this is one or more days of the month
-- (1-31). For a weekly schedule, this is one or more days of the week
-- (1-7, where 1 is Sunday). You can\'t specify this value with a daily
-- schedule. If the occurrence is relative to the end of the month, you can
-- specify only a single day.
--
-- 'occurrenceRelativeToEnd', 'scheduledInstanceRecurrenceRequest_occurrenceRelativeToEnd' - Indicates whether the occurrence is relative to the end of the specified
-- week or month. You can\'t specify this value with a daily schedule.
--
-- 'occurrenceUnit', 'scheduledInstanceRecurrenceRequest_occurrenceUnit' - The unit for @OccurrenceDays@ (@DayOfWeek@ or @DayOfMonth@). This value
-- is required for a monthly schedule. You can\'t specify @DayOfWeek@ with
-- a weekly schedule. You can\'t specify this value with a daily schedule.
newScheduledInstanceRecurrenceRequest ::
  ScheduledInstanceRecurrenceRequest
newScheduledInstanceRecurrenceRequest =
  ScheduledInstanceRecurrenceRequest'
    { frequency =
        Prelude.Nothing,
      interval = Prelude.Nothing,
      occurrenceDays = Prelude.Nothing,
      occurrenceRelativeToEnd =
        Prelude.Nothing,
      occurrenceUnit = Prelude.Nothing
    }

-- | The frequency (@Daily@, @Weekly@, or @Monthly@).
scheduledInstanceRecurrenceRequest_frequency :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Prelude.Maybe Prelude.Text)
scheduledInstanceRecurrenceRequest_frequency = Lens.lens (\ScheduledInstanceRecurrenceRequest' {frequency} -> frequency) (\s@ScheduledInstanceRecurrenceRequest' {} a -> s {frequency = a} :: ScheduledInstanceRecurrenceRequest)

-- | The interval quantity. The interval unit depends on the value of
-- @Frequency@. For example, every 2 weeks or every 2 months.
scheduledInstanceRecurrenceRequest_interval :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Prelude.Maybe Prelude.Int)
scheduledInstanceRecurrenceRequest_interval = Lens.lens (\ScheduledInstanceRecurrenceRequest' {interval} -> interval) (\s@ScheduledInstanceRecurrenceRequest' {} a -> s {interval = a} :: ScheduledInstanceRecurrenceRequest)

-- | The days. For a monthly schedule, this is one or more days of the month
-- (1-31). For a weekly schedule, this is one or more days of the week
-- (1-7, where 1 is Sunday). You can\'t specify this value with a daily
-- schedule. If the occurrence is relative to the end of the month, you can
-- specify only a single day.
scheduledInstanceRecurrenceRequest_occurrenceDays :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Prelude.Maybe [Prelude.Int])
scheduledInstanceRecurrenceRequest_occurrenceDays = Lens.lens (\ScheduledInstanceRecurrenceRequest' {occurrenceDays} -> occurrenceDays) (\s@ScheduledInstanceRecurrenceRequest' {} a -> s {occurrenceDays = a} :: ScheduledInstanceRecurrenceRequest) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the occurrence is relative to the end of the specified
-- week or month. You can\'t specify this value with a daily schedule.
scheduledInstanceRecurrenceRequest_occurrenceRelativeToEnd :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Prelude.Maybe Prelude.Bool)
scheduledInstanceRecurrenceRequest_occurrenceRelativeToEnd = Lens.lens (\ScheduledInstanceRecurrenceRequest' {occurrenceRelativeToEnd} -> occurrenceRelativeToEnd) (\s@ScheduledInstanceRecurrenceRequest' {} a -> s {occurrenceRelativeToEnd = a} :: ScheduledInstanceRecurrenceRequest)

-- | The unit for @OccurrenceDays@ (@DayOfWeek@ or @DayOfMonth@). This value
-- is required for a monthly schedule. You can\'t specify @DayOfWeek@ with
-- a weekly schedule. You can\'t specify this value with a daily schedule.
scheduledInstanceRecurrenceRequest_occurrenceUnit :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Prelude.Maybe Prelude.Text)
scheduledInstanceRecurrenceRequest_occurrenceUnit = Lens.lens (\ScheduledInstanceRecurrenceRequest' {occurrenceUnit} -> occurrenceUnit) (\s@ScheduledInstanceRecurrenceRequest' {} a -> s {occurrenceUnit = a} :: ScheduledInstanceRecurrenceRequest)

instance
  Prelude.Hashable
    ScheduledInstanceRecurrenceRequest
  where
  hashWithSalt
    _salt
    ScheduledInstanceRecurrenceRequest' {..} =
      _salt `Prelude.hashWithSalt` frequency
        `Prelude.hashWithSalt` interval
        `Prelude.hashWithSalt` occurrenceDays
        `Prelude.hashWithSalt` occurrenceRelativeToEnd
        `Prelude.hashWithSalt` occurrenceUnit

instance
  Prelude.NFData
    ScheduledInstanceRecurrenceRequest
  where
  rnf ScheduledInstanceRecurrenceRequest' {..} =
    Prelude.rnf frequency
      `Prelude.seq` Prelude.rnf interval
      `Prelude.seq` Prelude.rnf occurrenceDays
      `Prelude.seq` Prelude.rnf occurrenceRelativeToEnd
      `Prelude.seq` Prelude.rnf occurrenceUnit

instance
  Data.ToQuery
    ScheduledInstanceRecurrenceRequest
  where
  toQuery ScheduledInstanceRecurrenceRequest' {..} =
    Prelude.mconcat
      [ "Frequency" Data.=: frequency,
        "Interval" Data.=: interval,
        Data.toQuery
          ( Data.toQueryList "OccurrenceDay"
              Prelude.<$> occurrenceDays
          ),
        "OccurrenceRelativeToEnd"
          Data.=: occurrenceRelativeToEnd,
        "OccurrenceUnit" Data.=: occurrenceUnit
      ]
