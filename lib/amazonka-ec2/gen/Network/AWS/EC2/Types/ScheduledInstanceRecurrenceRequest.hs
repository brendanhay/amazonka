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
    sirrInterval,
    sirrOccurrenceDays,
    sirrOccurrenceRelativeToEnd,
    sirrOccurrenceUnit,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the recurring schedule for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstanceRecurrenceRequest' smart constructor.
data ScheduledInstanceRecurrenceRequest = ScheduledInstanceRecurrenceRequest'
  { -- | The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
    frequency :: Core.Maybe Types.String,
    -- | The interval quantity. The interval unit depends on the value of @Frequency@ . For example, every 2 weeks or every 2 months.
    interval :: Core.Maybe Core.Int,
    -- | The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday). You can't specify this value with a daily schedule. If the occurrence is relative to the end of the month, you can specify only a single day.
    occurrenceDays :: Core.Maybe [Core.Int],
    -- | Indicates whether the occurrence is relative to the end of the specified week or month. You can't specify this value with a daily schedule.
    occurrenceRelativeToEnd :: Core.Maybe Core.Bool,
    -- | The unit for @OccurrenceDays@ (@DayOfWeek@ or @DayOfMonth@ ). This value is required for a monthly schedule. You can't specify @DayOfWeek@ with a weekly schedule. You can't specify this value with a daily schedule.
    occurrenceUnit :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduledInstanceRecurrenceRequest' value with any optional fields omitted.
mkScheduledInstanceRecurrenceRequest ::
  ScheduledInstanceRecurrenceRequest
mkScheduledInstanceRecurrenceRequest =
  ScheduledInstanceRecurrenceRequest'
    { frequency = Core.Nothing,
      interval = Core.Nothing,
      occurrenceDays = Core.Nothing,
      occurrenceRelativeToEnd = Core.Nothing,
      occurrenceUnit = Core.Nothing
    }

-- | The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrFrequency :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Core.Maybe Types.String)
sirrFrequency = Lens.field @"frequency"
{-# DEPRECATED sirrFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | The interval quantity. The interval unit depends on the value of @Frequency@ . For example, every 2 weeks or every 2 months.
--
-- /Note:/ Consider using 'interval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrInterval :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Core.Maybe Core.Int)
sirrInterval = Lens.field @"interval"
{-# DEPRECATED sirrInterval "Use generic-lens or generic-optics with 'interval' instead." #-}

-- | The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday). You can't specify this value with a daily schedule. If the occurrence is relative to the end of the month, you can specify only a single day.
--
-- /Note:/ Consider using 'occurrenceDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrOccurrenceDays :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Core.Maybe [Core.Int])
sirrOccurrenceDays = Lens.field @"occurrenceDays"
{-# DEPRECATED sirrOccurrenceDays "Use generic-lens or generic-optics with 'occurrenceDays' instead." #-}

-- | Indicates whether the occurrence is relative to the end of the specified week or month. You can't specify this value with a daily schedule.
--
-- /Note:/ Consider using 'occurrenceRelativeToEnd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrOccurrenceRelativeToEnd :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Core.Maybe Core.Bool)
sirrOccurrenceRelativeToEnd = Lens.field @"occurrenceRelativeToEnd"
{-# DEPRECATED sirrOccurrenceRelativeToEnd "Use generic-lens or generic-optics with 'occurrenceRelativeToEnd' instead." #-}

-- | The unit for @OccurrenceDays@ (@DayOfWeek@ or @DayOfMonth@ ). This value is required for a monthly schedule. You can't specify @DayOfWeek@ with a weekly schedule. You can't specify this value with a daily schedule.
--
-- /Note:/ Consider using 'occurrenceUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrOccurrenceUnit :: Lens.Lens' ScheduledInstanceRecurrenceRequest (Core.Maybe Types.String)
sirrOccurrenceUnit = Lens.field @"occurrenceUnit"
{-# DEPRECATED sirrOccurrenceUnit "Use generic-lens or generic-optics with 'occurrenceUnit' instead." #-}
