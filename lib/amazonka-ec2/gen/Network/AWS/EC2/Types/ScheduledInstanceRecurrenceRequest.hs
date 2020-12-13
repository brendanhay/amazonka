{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstanceRecurrenceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstanceRecurrenceRequest
  ( ScheduledInstanceRecurrenceRequest (..),

    -- * Smart constructor
    mkScheduledInstanceRecurrenceRequest,

    -- * Lenses
    sirrFrequency,
    sirrOccurrenceRelativeToEnd,
    sirrOccurrenceDays,
    sirrOccurrenceUnit,
    sirrInterval,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the recurring schedule for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstanceRecurrenceRequest' smart constructor.
data ScheduledInstanceRecurrenceRequest = ScheduledInstanceRecurrenceRequest'
  { -- | The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
    frequency :: Lude.Maybe Lude.Text,
    -- | Indicates whether the occurrence is relative to the end of the specified week or month. You can't specify this value with a daily schedule.
    occurrenceRelativeToEnd :: Lude.Maybe Lude.Bool,
    -- | The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday). You can't specify this value with a daily schedule. If the occurrence is relative to the end of the month, you can specify only a single day.
    occurrenceDays :: Lude.Maybe [Lude.Int],
    -- | The unit for @OccurrenceDays@ (@DayOfWeek@ or @DayOfMonth@ ). This value is required for a monthly schedule. You can't specify @DayOfWeek@ with a weekly schedule. You can't specify this value with a daily schedule.
    occurrenceUnit :: Lude.Maybe Lude.Text,
    -- | The interval quantity. The interval unit depends on the value of @Frequency@ . For example, every 2 weeks or every 2 months.
    interval :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduledInstanceRecurrenceRequest' with the minimum fields required to make a request.
--
-- * 'frequency' - The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
-- * 'occurrenceRelativeToEnd' - Indicates whether the occurrence is relative to the end of the specified week or month. You can't specify this value with a daily schedule.
-- * 'occurrenceDays' - The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday). You can't specify this value with a daily schedule. If the occurrence is relative to the end of the month, you can specify only a single day.
-- * 'occurrenceUnit' - The unit for @OccurrenceDays@ (@DayOfWeek@ or @DayOfMonth@ ). This value is required for a monthly schedule. You can't specify @DayOfWeek@ with a weekly schedule. You can't specify this value with a daily schedule.
-- * 'interval' - The interval quantity. The interval unit depends on the value of @Frequency@ . For example, every 2 weeks or every 2 months.
mkScheduledInstanceRecurrenceRequest ::
  ScheduledInstanceRecurrenceRequest
mkScheduledInstanceRecurrenceRequest =
  ScheduledInstanceRecurrenceRequest'
    { frequency = Lude.Nothing,
      occurrenceRelativeToEnd = Lude.Nothing,
      occurrenceDays = Lude.Nothing,
      occurrenceUnit = Lude.Nothing,
      interval = Lude.Nothing
    }

-- | The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrFrequency :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Lude.Maybe Lude.Text)
sirrFrequency = Lens.lens (frequency :: ScheduledInstanceRecurrenceRequest -> Lude.Maybe Lude.Text) (\s a -> s {frequency = a} :: ScheduledInstanceRecurrenceRequest)
{-# DEPRECATED sirrFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | Indicates whether the occurrence is relative to the end of the specified week or month. You can't specify this value with a daily schedule.
--
-- /Note:/ Consider using 'occurrenceRelativeToEnd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrOccurrenceRelativeToEnd :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Lude.Maybe Lude.Bool)
sirrOccurrenceRelativeToEnd = Lens.lens (occurrenceRelativeToEnd :: ScheduledInstanceRecurrenceRequest -> Lude.Maybe Lude.Bool) (\s a -> s {occurrenceRelativeToEnd = a} :: ScheduledInstanceRecurrenceRequest)
{-# DEPRECATED sirrOccurrenceRelativeToEnd "Use generic-lens or generic-optics with 'occurrenceRelativeToEnd' instead." #-}

-- | The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday). You can't specify this value with a daily schedule. If the occurrence is relative to the end of the month, you can specify only a single day.
--
-- /Note:/ Consider using 'occurrenceDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrOccurrenceDays :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Lude.Maybe [Lude.Int])
sirrOccurrenceDays = Lens.lens (occurrenceDays :: ScheduledInstanceRecurrenceRequest -> Lude.Maybe [Lude.Int]) (\s a -> s {occurrenceDays = a} :: ScheduledInstanceRecurrenceRequest)
{-# DEPRECATED sirrOccurrenceDays "Use generic-lens or generic-optics with 'occurrenceDays' instead." #-}

-- | The unit for @OccurrenceDays@ (@DayOfWeek@ or @DayOfMonth@ ). This value is required for a monthly schedule. You can't specify @DayOfWeek@ with a weekly schedule. You can't specify this value with a daily schedule.
--
-- /Note:/ Consider using 'occurrenceUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrOccurrenceUnit :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Lude.Maybe Lude.Text)
sirrOccurrenceUnit = Lens.lens (occurrenceUnit :: ScheduledInstanceRecurrenceRequest -> Lude.Maybe Lude.Text) (\s a -> s {occurrenceUnit = a} :: ScheduledInstanceRecurrenceRequest)
{-# DEPRECATED sirrOccurrenceUnit "Use generic-lens or generic-optics with 'occurrenceUnit' instead." #-}

-- | The interval quantity. The interval unit depends on the value of @Frequency@ . For example, every 2 weeks or every 2 months.
--
-- /Note:/ Consider using 'interval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrInterval :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Lude.Maybe Lude.Int)
sirrInterval = Lens.lens (interval :: ScheduledInstanceRecurrenceRequest -> Lude.Maybe Lude.Int) (\s a -> s {interval = a} :: ScheduledInstanceRecurrenceRequest)
{-# DEPRECATED sirrInterval "Use generic-lens or generic-optics with 'interval' instead." #-}

instance Lude.ToQuery ScheduledInstanceRecurrenceRequest where
  toQuery ScheduledInstanceRecurrenceRequest' {..} =
    Lude.mconcat
      [ "Frequency" Lude.=: frequency,
        "OccurrenceRelativeToEnd" Lude.=: occurrenceRelativeToEnd,
        Lude.toQuery
          (Lude.toQueryList "OccurrenceDay" Lude.<$> occurrenceDays),
        "OccurrenceUnit" Lude.=: occurrenceUnit,
        "Interval" Lude.=: interval
      ]
