{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstanceRecurrence
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ScheduledInstanceRecurrence
  ( ScheduledInstanceRecurrence (..)
  -- * Smart constructor
  , mkScheduledInstanceRecurrence
  -- * Lenses
  , sirFrequency
  , sirInterval
  , sirOccurrenceDaySet
  , sirOccurrenceRelativeToEnd
  , sirOccurrenceUnit
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the recurring schedule for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstanceRecurrence' smart constructor.
data ScheduledInstanceRecurrence = ScheduledInstanceRecurrence'
  { frequency :: Core.Maybe Core.Text
    -- ^ The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
  , interval :: Core.Maybe Core.Int
    -- ^ The interval quantity. The interval unit depends on the value of @frequency@ . For example, every 2 weeks or every 2 months.
  , occurrenceDaySet :: Core.Maybe [Core.Int]
    -- ^ The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday).
  , occurrenceRelativeToEnd :: Core.Maybe Core.Bool
    -- ^ Indicates whether the occurrence is relative to the end of the specified week or month.
  , occurrenceUnit :: Core.Maybe Core.Text
    -- ^ The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduledInstanceRecurrence' value with any optional fields omitted.
mkScheduledInstanceRecurrence
    :: ScheduledInstanceRecurrence
mkScheduledInstanceRecurrence
  = ScheduledInstanceRecurrence'{frequency = Core.Nothing,
                                 interval = Core.Nothing, occurrenceDaySet = Core.Nothing,
                                 occurrenceRelativeToEnd = Core.Nothing,
                                 occurrenceUnit = Core.Nothing}

-- | The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirFrequency :: Lens.Lens' ScheduledInstanceRecurrence (Core.Maybe Core.Text)
sirFrequency = Lens.field @"frequency"
{-# INLINEABLE sirFrequency #-}
{-# DEPRECATED frequency "Use generic-lens or generic-optics with 'frequency' instead"  #-}

-- | The interval quantity. The interval unit depends on the value of @frequency@ . For example, every 2 weeks or every 2 months.
--
-- /Note:/ Consider using 'interval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirInterval :: Lens.Lens' ScheduledInstanceRecurrence (Core.Maybe Core.Int)
sirInterval = Lens.field @"interval"
{-# INLINEABLE sirInterval #-}
{-# DEPRECATED interval "Use generic-lens or generic-optics with 'interval' instead"  #-}

-- | The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday).
--
-- /Note:/ Consider using 'occurrenceDaySet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirOccurrenceDaySet :: Lens.Lens' ScheduledInstanceRecurrence (Core.Maybe [Core.Int])
sirOccurrenceDaySet = Lens.field @"occurrenceDaySet"
{-# INLINEABLE sirOccurrenceDaySet #-}
{-# DEPRECATED occurrenceDaySet "Use generic-lens or generic-optics with 'occurrenceDaySet' instead"  #-}

-- | Indicates whether the occurrence is relative to the end of the specified week or month.
--
-- /Note:/ Consider using 'occurrenceRelativeToEnd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirOccurrenceRelativeToEnd :: Lens.Lens' ScheduledInstanceRecurrence (Core.Maybe Core.Bool)
sirOccurrenceRelativeToEnd = Lens.field @"occurrenceRelativeToEnd"
{-# INLINEABLE sirOccurrenceRelativeToEnd #-}
{-# DEPRECATED occurrenceRelativeToEnd "Use generic-lens or generic-optics with 'occurrenceRelativeToEnd' instead"  #-}

-- | The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@ ).
--
-- /Note:/ Consider using 'occurrenceUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirOccurrenceUnit :: Lens.Lens' ScheduledInstanceRecurrence (Core.Maybe Core.Text)
sirOccurrenceUnit = Lens.field @"occurrenceUnit"
{-# INLINEABLE sirOccurrenceUnit #-}
{-# DEPRECATED occurrenceUnit "Use generic-lens or generic-optics with 'occurrenceUnit' instead"  #-}

instance Core.FromXML ScheduledInstanceRecurrence where
        parseXML x
          = ScheduledInstanceRecurrence' Core.<$>
              (x Core..@? "frequency") Core.<*> x Core..@? "interval" Core.<*>
                x Core..@? "occurrenceDaySet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "occurrenceRelativeToEnd"
                Core.<*> x Core..@? "occurrenceUnit"
