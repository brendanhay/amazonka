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
-- Module      : Amazonka.EC2.Types.ScheduledInstanceRecurrence
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ScheduledInstanceRecurrence where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the recurring schedule for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstanceRecurrence' smart constructor.
data ScheduledInstanceRecurrence = ScheduledInstanceRecurrence'
  { -- | The frequency (@Daily@, @Weekly@, or @Monthly@).
    frequency :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the occurrence is relative to the end of the specified
    -- week or month.
    occurrenceRelativeToEnd :: Prelude.Maybe Prelude.Bool,
    -- | The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@).
    occurrenceUnit :: Prelude.Maybe Prelude.Text,
    -- | The interval quantity. The interval unit depends on the value of
    -- @frequency@. For example, every 2 weeks or every 2 months.
    interval :: Prelude.Maybe Prelude.Int,
    -- | The days. For a monthly schedule, this is one or more days of the month
    -- (1-31). For a weekly schedule, this is one or more days of the week
    -- (1-7, where 1 is Sunday).
    occurrenceDaySet :: Prelude.Maybe [Prelude.Int]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledInstanceRecurrence' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frequency', 'scheduledInstanceRecurrence_frequency' - The frequency (@Daily@, @Weekly@, or @Monthly@).
--
-- 'occurrenceRelativeToEnd', 'scheduledInstanceRecurrence_occurrenceRelativeToEnd' - Indicates whether the occurrence is relative to the end of the specified
-- week or month.
--
-- 'occurrenceUnit', 'scheduledInstanceRecurrence_occurrenceUnit' - The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@).
--
-- 'interval', 'scheduledInstanceRecurrence_interval' - The interval quantity. The interval unit depends on the value of
-- @frequency@. For example, every 2 weeks or every 2 months.
--
-- 'occurrenceDaySet', 'scheduledInstanceRecurrence_occurrenceDaySet' - The days. For a monthly schedule, this is one or more days of the month
-- (1-31). For a weekly schedule, this is one or more days of the week
-- (1-7, where 1 is Sunday).
newScheduledInstanceRecurrence ::
  ScheduledInstanceRecurrence
newScheduledInstanceRecurrence =
  ScheduledInstanceRecurrence'
    { frequency =
        Prelude.Nothing,
      occurrenceRelativeToEnd = Prelude.Nothing,
      occurrenceUnit = Prelude.Nothing,
      interval = Prelude.Nothing,
      occurrenceDaySet = Prelude.Nothing
    }

-- | The frequency (@Daily@, @Weekly@, or @Monthly@).
scheduledInstanceRecurrence_frequency :: Lens.Lens' ScheduledInstanceRecurrence (Prelude.Maybe Prelude.Text)
scheduledInstanceRecurrence_frequency = Lens.lens (\ScheduledInstanceRecurrence' {frequency} -> frequency) (\s@ScheduledInstanceRecurrence' {} a -> s {frequency = a} :: ScheduledInstanceRecurrence)

-- | Indicates whether the occurrence is relative to the end of the specified
-- week or month.
scheduledInstanceRecurrence_occurrenceRelativeToEnd :: Lens.Lens' ScheduledInstanceRecurrence (Prelude.Maybe Prelude.Bool)
scheduledInstanceRecurrence_occurrenceRelativeToEnd = Lens.lens (\ScheduledInstanceRecurrence' {occurrenceRelativeToEnd} -> occurrenceRelativeToEnd) (\s@ScheduledInstanceRecurrence' {} a -> s {occurrenceRelativeToEnd = a} :: ScheduledInstanceRecurrence)

-- | The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@).
scheduledInstanceRecurrence_occurrenceUnit :: Lens.Lens' ScheduledInstanceRecurrence (Prelude.Maybe Prelude.Text)
scheduledInstanceRecurrence_occurrenceUnit = Lens.lens (\ScheduledInstanceRecurrence' {occurrenceUnit} -> occurrenceUnit) (\s@ScheduledInstanceRecurrence' {} a -> s {occurrenceUnit = a} :: ScheduledInstanceRecurrence)

-- | The interval quantity. The interval unit depends on the value of
-- @frequency@. For example, every 2 weeks or every 2 months.
scheduledInstanceRecurrence_interval :: Lens.Lens' ScheduledInstanceRecurrence (Prelude.Maybe Prelude.Int)
scheduledInstanceRecurrence_interval = Lens.lens (\ScheduledInstanceRecurrence' {interval} -> interval) (\s@ScheduledInstanceRecurrence' {} a -> s {interval = a} :: ScheduledInstanceRecurrence)

-- | The days. For a monthly schedule, this is one or more days of the month
-- (1-31). For a weekly schedule, this is one or more days of the week
-- (1-7, where 1 is Sunday).
scheduledInstanceRecurrence_occurrenceDaySet :: Lens.Lens' ScheduledInstanceRecurrence (Prelude.Maybe [Prelude.Int])
scheduledInstanceRecurrence_occurrenceDaySet = Lens.lens (\ScheduledInstanceRecurrence' {occurrenceDaySet} -> occurrenceDaySet) (\s@ScheduledInstanceRecurrence' {} a -> s {occurrenceDaySet = a} :: ScheduledInstanceRecurrence) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML ScheduledInstanceRecurrence where
  parseXML x =
    ScheduledInstanceRecurrence'
      Prelude.<$> (x Core..@? "frequency")
      Prelude.<*> (x Core..@? "occurrenceRelativeToEnd")
      Prelude.<*> (x Core..@? "occurrenceUnit")
      Prelude.<*> (x Core..@? "interval")
      Prelude.<*> ( x Core..@? "occurrenceDaySet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable ScheduledInstanceRecurrence where
  hashWithSalt salt' ScheduledInstanceRecurrence' {..} =
    salt' `Prelude.hashWithSalt` occurrenceDaySet
      `Prelude.hashWithSalt` interval
      `Prelude.hashWithSalt` occurrenceUnit
      `Prelude.hashWithSalt` occurrenceRelativeToEnd
      `Prelude.hashWithSalt` frequency

instance Prelude.NFData ScheduledInstanceRecurrence where
  rnf ScheduledInstanceRecurrence' {..} =
    Prelude.rnf frequency
      `Prelude.seq` Prelude.rnf occurrenceDaySet
      `Prelude.seq` Prelude.rnf interval
      `Prelude.seq` Prelude.rnf occurrenceUnit
      `Prelude.seq` Prelude.rnf occurrenceRelativeToEnd
