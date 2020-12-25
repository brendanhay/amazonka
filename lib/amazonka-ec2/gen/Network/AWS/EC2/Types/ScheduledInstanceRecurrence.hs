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
    sirInterval,
    sirOccurrenceDaySet,
    sirOccurrenceRelativeToEnd,
    sirOccurrenceUnit,
  )
where

import qualified Network.AWS.EC2.Types.Frequency as Types
import qualified Network.AWS.EC2.Types.OccurrenceUnit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the recurring schedule for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstanceRecurrence' smart constructor.
data ScheduledInstanceRecurrence = ScheduledInstanceRecurrence'
  { -- | The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
    frequency :: Core.Maybe Types.Frequency,
    -- | The interval quantity. The interval unit depends on the value of @frequency@ . For example, every 2 weeks or every 2 months.
    interval :: Core.Maybe Core.Int,
    -- | The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday).
    occurrenceDaySet :: Core.Maybe [Core.Int],
    -- | Indicates whether the occurrence is relative to the end of the specified week or month.
    occurrenceRelativeToEnd :: Core.Maybe Core.Bool,
    -- | The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@ ).
    occurrenceUnit :: Core.Maybe Types.OccurrenceUnit
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduledInstanceRecurrence' value with any optional fields omitted.
mkScheduledInstanceRecurrence ::
  ScheduledInstanceRecurrence
mkScheduledInstanceRecurrence =
  ScheduledInstanceRecurrence'
    { frequency = Core.Nothing,
      interval = Core.Nothing,
      occurrenceDaySet = Core.Nothing,
      occurrenceRelativeToEnd = Core.Nothing,
      occurrenceUnit = Core.Nothing
    }

-- | The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirFrequency :: Lens.Lens' ScheduledInstanceRecurrence (Core.Maybe Types.Frequency)
sirFrequency = Lens.field @"frequency"
{-# DEPRECATED sirFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | The interval quantity. The interval unit depends on the value of @frequency@ . For example, every 2 weeks or every 2 months.
--
-- /Note:/ Consider using 'interval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirInterval :: Lens.Lens' ScheduledInstanceRecurrence (Core.Maybe Core.Int)
sirInterval = Lens.field @"interval"
{-# DEPRECATED sirInterval "Use generic-lens or generic-optics with 'interval' instead." #-}

-- | The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday).
--
-- /Note:/ Consider using 'occurrenceDaySet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirOccurrenceDaySet :: Lens.Lens' ScheduledInstanceRecurrence (Core.Maybe [Core.Int])
sirOccurrenceDaySet = Lens.field @"occurrenceDaySet"
{-# DEPRECATED sirOccurrenceDaySet "Use generic-lens or generic-optics with 'occurrenceDaySet' instead." #-}

-- | Indicates whether the occurrence is relative to the end of the specified week or month.
--
-- /Note:/ Consider using 'occurrenceRelativeToEnd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirOccurrenceRelativeToEnd :: Lens.Lens' ScheduledInstanceRecurrence (Core.Maybe Core.Bool)
sirOccurrenceRelativeToEnd = Lens.field @"occurrenceRelativeToEnd"
{-# DEPRECATED sirOccurrenceRelativeToEnd "Use generic-lens or generic-optics with 'occurrenceRelativeToEnd' instead." #-}

-- | The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@ ).
--
-- /Note:/ Consider using 'occurrenceUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirOccurrenceUnit :: Lens.Lens' ScheduledInstanceRecurrence (Core.Maybe Types.OccurrenceUnit)
sirOccurrenceUnit = Lens.field @"occurrenceUnit"
{-# DEPRECATED sirOccurrenceUnit "Use generic-lens or generic-optics with 'occurrenceUnit' instead." #-}

instance Core.FromXML ScheduledInstanceRecurrence where
  parseXML x =
    ScheduledInstanceRecurrence'
      Core.<$> (x Core..@? "frequency")
      Core.<*> (x Core..@? "interval")
      Core.<*> (x Core..@? "occurrenceDaySet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "occurrenceRelativeToEnd")
      Core.<*> (x Core..@? "occurrenceUnit")
