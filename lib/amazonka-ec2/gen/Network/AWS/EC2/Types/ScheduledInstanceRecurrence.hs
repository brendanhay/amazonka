{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstanceRecurrence
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstanceRecurrence
  ( ScheduledInstanceRecurrence (..),

    -- * Smart constructor
    mkScheduledInstanceRecurrence,

    -- * Lenses
    sirFrequency,
    sirOccurrenceRelativeToEnd,
    sirOccurrenceUnit,
    sirInterval,
    sirOccurrenceDaySet,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the recurring schedule for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstanceRecurrence' smart constructor.
data ScheduledInstanceRecurrence = ScheduledInstanceRecurrence'
  { -- | The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
    frequency :: Lude.Maybe Lude.Text,
    -- | Indicates whether the occurrence is relative to the end of the specified week or month.
    occurrenceRelativeToEnd :: Lude.Maybe Lude.Bool,
    -- | The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@ ).
    occurrenceUnit :: Lude.Maybe Lude.Text,
    -- | The interval quantity. The interval unit depends on the value of @frequency@ . For example, every 2 weeks or every 2 months.
    interval :: Lude.Maybe Lude.Int,
    -- | The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday).
    occurrenceDaySet :: Lude.Maybe [Lude.Int]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduledInstanceRecurrence' with the minimum fields required to make a request.
--
-- * 'frequency' - The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
-- * 'occurrenceRelativeToEnd' - Indicates whether the occurrence is relative to the end of the specified week or month.
-- * 'occurrenceUnit' - The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@ ).
-- * 'interval' - The interval quantity. The interval unit depends on the value of @frequency@ . For example, every 2 weeks or every 2 months.
-- * 'occurrenceDaySet' - The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday).
mkScheduledInstanceRecurrence ::
  ScheduledInstanceRecurrence
mkScheduledInstanceRecurrence =
  ScheduledInstanceRecurrence'
    { frequency = Lude.Nothing,
      occurrenceRelativeToEnd = Lude.Nothing,
      occurrenceUnit = Lude.Nothing,
      interval = Lude.Nothing,
      occurrenceDaySet = Lude.Nothing
    }

-- | The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirFrequency :: Lens.Lens' ScheduledInstanceRecurrence (Lude.Maybe Lude.Text)
sirFrequency = Lens.lens (frequency :: ScheduledInstanceRecurrence -> Lude.Maybe Lude.Text) (\s a -> s {frequency = a} :: ScheduledInstanceRecurrence)
{-# DEPRECATED sirFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | Indicates whether the occurrence is relative to the end of the specified week or month.
--
-- /Note:/ Consider using 'occurrenceRelativeToEnd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirOccurrenceRelativeToEnd :: Lens.Lens' ScheduledInstanceRecurrence (Lude.Maybe Lude.Bool)
sirOccurrenceRelativeToEnd = Lens.lens (occurrenceRelativeToEnd :: ScheduledInstanceRecurrence -> Lude.Maybe Lude.Bool) (\s a -> s {occurrenceRelativeToEnd = a} :: ScheduledInstanceRecurrence)
{-# DEPRECATED sirOccurrenceRelativeToEnd "Use generic-lens or generic-optics with 'occurrenceRelativeToEnd' instead." #-}

-- | The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@ ).
--
-- /Note:/ Consider using 'occurrenceUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirOccurrenceUnit :: Lens.Lens' ScheduledInstanceRecurrence (Lude.Maybe Lude.Text)
sirOccurrenceUnit = Lens.lens (occurrenceUnit :: ScheduledInstanceRecurrence -> Lude.Maybe Lude.Text) (\s a -> s {occurrenceUnit = a} :: ScheduledInstanceRecurrence)
{-# DEPRECATED sirOccurrenceUnit "Use generic-lens or generic-optics with 'occurrenceUnit' instead." #-}

-- | The interval quantity. The interval unit depends on the value of @frequency@ . For example, every 2 weeks or every 2 months.
--
-- /Note:/ Consider using 'interval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirInterval :: Lens.Lens' ScheduledInstanceRecurrence (Lude.Maybe Lude.Int)
sirInterval = Lens.lens (interval :: ScheduledInstanceRecurrence -> Lude.Maybe Lude.Int) (\s a -> s {interval = a} :: ScheduledInstanceRecurrence)
{-# DEPRECATED sirInterval "Use generic-lens or generic-optics with 'interval' instead." #-}

-- | The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday).
--
-- /Note:/ Consider using 'occurrenceDaySet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirOccurrenceDaySet :: Lens.Lens' ScheduledInstanceRecurrence (Lude.Maybe [Lude.Int])
sirOccurrenceDaySet = Lens.lens (occurrenceDaySet :: ScheduledInstanceRecurrence -> Lude.Maybe [Lude.Int]) (\s a -> s {occurrenceDaySet = a} :: ScheduledInstanceRecurrence)
{-# DEPRECATED sirOccurrenceDaySet "Use generic-lens or generic-optics with 'occurrenceDaySet' instead." #-}

instance Lude.FromXML ScheduledInstanceRecurrence where
  parseXML x =
    ScheduledInstanceRecurrence'
      Lude.<$> (x Lude..@? "frequency")
      Lude.<*> (x Lude..@? "occurrenceRelativeToEnd")
      Lude.<*> (x Lude..@? "occurrenceUnit")
      Lude.<*> (x Lude..@? "interval")
      Lude.<*> ( x Lude..@? "occurrenceDaySet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
