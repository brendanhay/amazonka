{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the recurring schedule for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstanceRecurrence' smart constructor.
data ScheduledInstanceRecurrence = ScheduledInstanceRecurrence'
  { -- | The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@).
    occurrenceUnit :: Prelude.Maybe Prelude.Text,
    -- | The interval quantity. The interval unit depends on the value of
    -- @frequency@. For example, every 2 weeks or every 2 months.
    interval :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the occurrence is relative to the end of the specified
    -- week or month.
    occurrenceRelativeToEnd :: Prelude.Maybe Prelude.Bool,
    -- | The frequency (@Daily@, @Weekly@, or @Monthly@).
    frequency :: Prelude.Maybe Prelude.Text,
    -- | The days. For a monthly schedule, this is one or more days of the month
    -- (1-31). For a weekly schedule, this is one or more days of the week
    -- (1-7, where 1 is Sunday).
    occurrenceDaySet :: Prelude.Maybe [Prelude.Int]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      interval = Prelude.Nothing,
      occurrenceRelativeToEnd = Prelude.Nothing,
      frequency = Prelude.Nothing,
      occurrenceDaySet = Prelude.Nothing
    }

-- | The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@).
scheduledInstanceRecurrence_occurrenceUnit :: Lens.Lens' ScheduledInstanceRecurrence (Prelude.Maybe Prelude.Text)
scheduledInstanceRecurrence_occurrenceUnit = Lens.lens (\ScheduledInstanceRecurrence' {occurrenceUnit} -> occurrenceUnit) (\s@ScheduledInstanceRecurrence' {} a -> s {occurrenceUnit = a} :: ScheduledInstanceRecurrence)

-- | The interval quantity. The interval unit depends on the value of
-- @frequency@. For example, every 2 weeks or every 2 months.
scheduledInstanceRecurrence_interval :: Lens.Lens' ScheduledInstanceRecurrence (Prelude.Maybe Prelude.Int)
scheduledInstanceRecurrence_interval = Lens.lens (\ScheduledInstanceRecurrence' {interval} -> interval) (\s@ScheduledInstanceRecurrence' {} a -> s {interval = a} :: ScheduledInstanceRecurrence)

-- | Indicates whether the occurrence is relative to the end of the specified
-- week or month.
scheduledInstanceRecurrence_occurrenceRelativeToEnd :: Lens.Lens' ScheduledInstanceRecurrence (Prelude.Maybe Prelude.Bool)
scheduledInstanceRecurrence_occurrenceRelativeToEnd = Lens.lens (\ScheduledInstanceRecurrence' {occurrenceRelativeToEnd} -> occurrenceRelativeToEnd) (\s@ScheduledInstanceRecurrence' {} a -> s {occurrenceRelativeToEnd = a} :: ScheduledInstanceRecurrence)

-- | The frequency (@Daily@, @Weekly@, or @Monthly@).
scheduledInstanceRecurrence_frequency :: Lens.Lens' ScheduledInstanceRecurrence (Prelude.Maybe Prelude.Text)
scheduledInstanceRecurrence_frequency = Lens.lens (\ScheduledInstanceRecurrence' {frequency} -> frequency) (\s@ScheduledInstanceRecurrence' {} a -> s {frequency = a} :: ScheduledInstanceRecurrence)

-- | The days. For a monthly schedule, this is one or more days of the month
-- (1-31). For a weekly schedule, this is one or more days of the week
-- (1-7, where 1 is Sunday).
scheduledInstanceRecurrence_occurrenceDaySet :: Lens.Lens' ScheduledInstanceRecurrence (Prelude.Maybe [Prelude.Int])
scheduledInstanceRecurrence_occurrenceDaySet = Lens.lens (\ScheduledInstanceRecurrence' {occurrenceDaySet} -> occurrenceDaySet) (\s@ScheduledInstanceRecurrence' {} a -> s {occurrenceDaySet = a} :: ScheduledInstanceRecurrence) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML ScheduledInstanceRecurrence where
  parseXML x =
    ScheduledInstanceRecurrence'
      Prelude.<$> (x Prelude..@? "occurrenceUnit")
      Prelude.<*> (x Prelude..@? "interval")
      Prelude.<*> (x Prelude..@? "occurrenceRelativeToEnd")
      Prelude.<*> (x Prelude..@? "frequency")
      Prelude.<*> ( x Prelude..@? "occurrenceDaySet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable ScheduledInstanceRecurrence

instance Prelude.NFData ScheduledInstanceRecurrence
