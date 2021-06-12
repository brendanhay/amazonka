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
-- Module      : Network.AWS.EC2.Types.ScheduledInstanceRecurrence
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstanceRecurrence where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the recurring schedule for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstanceRecurrence' smart constructor.
data ScheduledInstanceRecurrence = ScheduledInstanceRecurrence'
  { -- | The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@).
    occurrenceUnit :: Core.Maybe Core.Text,
    -- | The interval quantity. The interval unit depends on the value of
    -- @frequency@. For example, every 2 weeks or every 2 months.
    interval :: Core.Maybe Core.Int,
    -- | Indicates whether the occurrence is relative to the end of the specified
    -- week or month.
    occurrenceRelativeToEnd :: Core.Maybe Core.Bool,
    -- | The frequency (@Daily@, @Weekly@, or @Monthly@).
    frequency :: Core.Maybe Core.Text,
    -- | The days. For a monthly schedule, this is one or more days of the month
    -- (1-31). For a weekly schedule, this is one or more days of the week
    -- (1-7, where 1 is Sunday).
    occurrenceDaySet :: Core.Maybe [Core.Int]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScheduledInstanceRecurrence' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'occurrenceUnit', 'scheduledInstanceRecurrence_occurrenceUnit' - The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@).
--
-- 'interval', 'scheduledInstanceRecurrence_interval' - The interval quantity. The interval unit depends on the value of
-- @frequency@. For example, every 2 weeks or every 2 months.
--
-- 'occurrenceRelativeToEnd', 'scheduledInstanceRecurrence_occurrenceRelativeToEnd' - Indicates whether the occurrence is relative to the end of the specified
-- week or month.
--
-- 'frequency', 'scheduledInstanceRecurrence_frequency' - The frequency (@Daily@, @Weekly@, or @Monthly@).
--
-- 'occurrenceDaySet', 'scheduledInstanceRecurrence_occurrenceDaySet' - The days. For a monthly schedule, this is one or more days of the month
-- (1-31). For a weekly schedule, this is one or more days of the week
-- (1-7, where 1 is Sunday).
newScheduledInstanceRecurrence ::
  ScheduledInstanceRecurrence
newScheduledInstanceRecurrence =
  ScheduledInstanceRecurrence'
    { occurrenceUnit =
        Core.Nothing,
      interval = Core.Nothing,
      occurrenceRelativeToEnd = Core.Nothing,
      frequency = Core.Nothing,
      occurrenceDaySet = Core.Nothing
    }

-- | The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@).
scheduledInstanceRecurrence_occurrenceUnit :: Lens.Lens' ScheduledInstanceRecurrence (Core.Maybe Core.Text)
scheduledInstanceRecurrence_occurrenceUnit = Lens.lens (\ScheduledInstanceRecurrence' {occurrenceUnit} -> occurrenceUnit) (\s@ScheduledInstanceRecurrence' {} a -> s {occurrenceUnit = a} :: ScheduledInstanceRecurrence)

-- | The interval quantity. The interval unit depends on the value of
-- @frequency@. For example, every 2 weeks or every 2 months.
scheduledInstanceRecurrence_interval :: Lens.Lens' ScheduledInstanceRecurrence (Core.Maybe Core.Int)
scheduledInstanceRecurrence_interval = Lens.lens (\ScheduledInstanceRecurrence' {interval} -> interval) (\s@ScheduledInstanceRecurrence' {} a -> s {interval = a} :: ScheduledInstanceRecurrence)

-- | Indicates whether the occurrence is relative to the end of the specified
-- week or month.
scheduledInstanceRecurrence_occurrenceRelativeToEnd :: Lens.Lens' ScheduledInstanceRecurrence (Core.Maybe Core.Bool)
scheduledInstanceRecurrence_occurrenceRelativeToEnd = Lens.lens (\ScheduledInstanceRecurrence' {occurrenceRelativeToEnd} -> occurrenceRelativeToEnd) (\s@ScheduledInstanceRecurrence' {} a -> s {occurrenceRelativeToEnd = a} :: ScheduledInstanceRecurrence)

-- | The frequency (@Daily@, @Weekly@, or @Monthly@).
scheduledInstanceRecurrence_frequency :: Lens.Lens' ScheduledInstanceRecurrence (Core.Maybe Core.Text)
scheduledInstanceRecurrence_frequency = Lens.lens (\ScheduledInstanceRecurrence' {frequency} -> frequency) (\s@ScheduledInstanceRecurrence' {} a -> s {frequency = a} :: ScheduledInstanceRecurrence)

-- | The days. For a monthly schedule, this is one or more days of the month
-- (1-31). For a weekly schedule, this is one or more days of the week
-- (1-7, where 1 is Sunday).
scheduledInstanceRecurrence_occurrenceDaySet :: Lens.Lens' ScheduledInstanceRecurrence (Core.Maybe [Core.Int])
scheduledInstanceRecurrence_occurrenceDaySet = Lens.lens (\ScheduledInstanceRecurrence' {occurrenceDaySet} -> occurrenceDaySet) (\s@ScheduledInstanceRecurrence' {} a -> s {occurrenceDaySet = a} :: ScheduledInstanceRecurrence) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML ScheduledInstanceRecurrence where
  parseXML x =
    ScheduledInstanceRecurrence'
      Core.<$> (x Core..@? "occurrenceUnit")
      Core.<*> (x Core..@? "interval")
      Core.<*> (x Core..@? "occurrenceRelativeToEnd")
      Core.<*> (x Core..@? "frequency")
      Core.<*> ( x Core..@? "occurrenceDaySet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable ScheduledInstanceRecurrence

instance Core.NFData ScheduledInstanceRecurrence
