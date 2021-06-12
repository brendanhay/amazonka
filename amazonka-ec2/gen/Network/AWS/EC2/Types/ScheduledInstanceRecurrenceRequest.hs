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
-- Module      : Network.AWS.EC2.Types.ScheduledInstanceRecurrenceRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstanceRecurrenceRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the recurring schedule for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstanceRecurrenceRequest' smart constructor.
data ScheduledInstanceRecurrenceRequest = ScheduledInstanceRecurrenceRequest'
  { -- | The unit for @OccurrenceDays@ (@DayOfWeek@ or @DayOfMonth@). This value
    -- is required for a monthly schedule. You can\'t specify @DayOfWeek@ with
    -- a weekly schedule. You can\'t specify this value with a daily schedule.
    occurrenceUnit :: Core.Maybe Core.Text,
    -- | The days. For a monthly schedule, this is one or more days of the month
    -- (1-31). For a weekly schedule, this is one or more days of the week
    -- (1-7, where 1 is Sunday). You can\'t specify this value with a daily
    -- schedule. If the occurrence is relative to the end of the month, you can
    -- specify only a single day.
    occurrenceDays :: Core.Maybe [Core.Int],
    -- | The interval quantity. The interval unit depends on the value of
    -- @Frequency@. For example, every 2 weeks or every 2 months.
    interval :: Core.Maybe Core.Int,
    -- | Indicates whether the occurrence is relative to the end of the specified
    -- week or month. You can\'t specify this value with a daily schedule.
    occurrenceRelativeToEnd :: Core.Maybe Core.Bool,
    -- | The frequency (@Daily@, @Weekly@, or @Monthly@).
    frequency :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScheduledInstanceRecurrenceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'occurrenceUnit', 'scheduledInstanceRecurrenceRequest_occurrenceUnit' - The unit for @OccurrenceDays@ (@DayOfWeek@ or @DayOfMonth@). This value
-- is required for a monthly schedule. You can\'t specify @DayOfWeek@ with
-- a weekly schedule. You can\'t specify this value with a daily schedule.
--
-- 'occurrenceDays', 'scheduledInstanceRecurrenceRequest_occurrenceDays' - The days. For a monthly schedule, this is one or more days of the month
-- (1-31). For a weekly schedule, this is one or more days of the week
-- (1-7, where 1 is Sunday). You can\'t specify this value with a daily
-- schedule. If the occurrence is relative to the end of the month, you can
-- specify only a single day.
--
-- 'interval', 'scheduledInstanceRecurrenceRequest_interval' - The interval quantity. The interval unit depends on the value of
-- @Frequency@. For example, every 2 weeks or every 2 months.
--
-- 'occurrenceRelativeToEnd', 'scheduledInstanceRecurrenceRequest_occurrenceRelativeToEnd' - Indicates whether the occurrence is relative to the end of the specified
-- week or month. You can\'t specify this value with a daily schedule.
--
-- 'frequency', 'scheduledInstanceRecurrenceRequest_frequency' - The frequency (@Daily@, @Weekly@, or @Monthly@).
newScheduledInstanceRecurrenceRequest ::
  ScheduledInstanceRecurrenceRequest
newScheduledInstanceRecurrenceRequest =
  ScheduledInstanceRecurrenceRequest'
    { occurrenceUnit =
        Core.Nothing,
      occurrenceDays = Core.Nothing,
      interval = Core.Nothing,
      occurrenceRelativeToEnd = Core.Nothing,
      frequency = Core.Nothing
    }

-- | The unit for @OccurrenceDays@ (@DayOfWeek@ or @DayOfMonth@). This value
-- is required for a monthly schedule. You can\'t specify @DayOfWeek@ with
-- a weekly schedule. You can\'t specify this value with a daily schedule.
scheduledInstanceRecurrenceRequest_occurrenceUnit :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Core.Maybe Core.Text)
scheduledInstanceRecurrenceRequest_occurrenceUnit = Lens.lens (\ScheduledInstanceRecurrenceRequest' {occurrenceUnit} -> occurrenceUnit) (\s@ScheduledInstanceRecurrenceRequest' {} a -> s {occurrenceUnit = a} :: ScheduledInstanceRecurrenceRequest)

-- | The days. For a monthly schedule, this is one or more days of the month
-- (1-31). For a weekly schedule, this is one or more days of the week
-- (1-7, where 1 is Sunday). You can\'t specify this value with a daily
-- schedule. If the occurrence is relative to the end of the month, you can
-- specify only a single day.
scheduledInstanceRecurrenceRequest_occurrenceDays :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Core.Maybe [Core.Int])
scheduledInstanceRecurrenceRequest_occurrenceDays = Lens.lens (\ScheduledInstanceRecurrenceRequest' {occurrenceDays} -> occurrenceDays) (\s@ScheduledInstanceRecurrenceRequest' {} a -> s {occurrenceDays = a} :: ScheduledInstanceRecurrenceRequest) Core.. Lens.mapping Lens._Coerce

-- | The interval quantity. The interval unit depends on the value of
-- @Frequency@. For example, every 2 weeks or every 2 months.
scheduledInstanceRecurrenceRequest_interval :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Core.Maybe Core.Int)
scheduledInstanceRecurrenceRequest_interval = Lens.lens (\ScheduledInstanceRecurrenceRequest' {interval} -> interval) (\s@ScheduledInstanceRecurrenceRequest' {} a -> s {interval = a} :: ScheduledInstanceRecurrenceRequest)

-- | Indicates whether the occurrence is relative to the end of the specified
-- week or month. You can\'t specify this value with a daily schedule.
scheduledInstanceRecurrenceRequest_occurrenceRelativeToEnd :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Core.Maybe Core.Bool)
scheduledInstanceRecurrenceRequest_occurrenceRelativeToEnd = Lens.lens (\ScheduledInstanceRecurrenceRequest' {occurrenceRelativeToEnd} -> occurrenceRelativeToEnd) (\s@ScheduledInstanceRecurrenceRequest' {} a -> s {occurrenceRelativeToEnd = a} :: ScheduledInstanceRecurrenceRequest)

-- | The frequency (@Daily@, @Weekly@, or @Monthly@).
scheduledInstanceRecurrenceRequest_frequency :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Core.Maybe Core.Text)
scheduledInstanceRecurrenceRequest_frequency = Lens.lens (\ScheduledInstanceRecurrenceRequest' {frequency} -> frequency) (\s@ScheduledInstanceRecurrenceRequest' {} a -> s {frequency = a} :: ScheduledInstanceRecurrenceRequest)

instance
  Core.Hashable
    ScheduledInstanceRecurrenceRequest

instance
  Core.NFData
    ScheduledInstanceRecurrenceRequest

instance
  Core.ToQuery
    ScheduledInstanceRecurrenceRequest
  where
  toQuery ScheduledInstanceRecurrenceRequest' {..} =
    Core.mconcat
      [ "OccurrenceUnit" Core.=: occurrenceUnit,
        Core.toQuery
          ( Core.toQueryList "OccurrenceDay"
              Core.<$> occurrenceDays
          ),
        "Interval" Core.=: interval,
        "OccurrenceRelativeToEnd"
          Core.=: occurrenceRelativeToEnd,
        "Frequency" Core.=: frequency
      ]
