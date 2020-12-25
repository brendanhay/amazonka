{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule
  ( WeeklyAutoScalingSchedule (..),

    -- * Smart constructor
    mkWeeklyAutoScalingSchedule,

    -- * Lenses
    wassFriday,
    wassMonday,
    wassSaturday,
    wassSunday,
    wassThursday,
    wassTuesday,
    wassWednesday,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.Hour as Types
import qualified Network.AWS.OpsWorks.Types.Switch as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a time-based instance's auto scaling schedule. The schedule consists of a set of key-value pairs.
--
--
--     * The key is the time period (a UTC hour) and must be an integer from 0 - 23.
--
--
--     * The value indicates whether the instance should be online or offline for the specified period, and must be set to "on" or "off"
--
--
-- The default setting for all time periods is off, so you use the following parameters primarily to specify the online periods. You don't have to explicitly specify offline periods unless you want to change an online period to an offline period.
-- The following example specifies that the instance should be online for four hours, from UTC 1200 - 1600. It will be off for the remainder of the day.
-- @{ "12":"on", "13":"on", "14":"on", "15":"on" } @
--
-- /See:/ 'mkWeeklyAutoScalingSchedule' smart constructor.
data WeeklyAutoScalingSchedule = WeeklyAutoScalingSchedule'
  { -- | The schedule for Friday.
    friday :: Core.Maybe (Core.HashMap Types.Hour Types.Switch),
    -- | The schedule for Monday.
    monday :: Core.Maybe (Core.HashMap Types.Hour Types.Switch),
    -- | The schedule for Saturday.
    saturday :: Core.Maybe (Core.HashMap Types.Hour Types.Switch),
    -- | The schedule for Sunday.
    sunday :: Core.Maybe (Core.HashMap Types.Hour Types.Switch),
    -- | The schedule for Thursday.
    thursday :: Core.Maybe (Core.HashMap Types.Hour Types.Switch),
    -- | The schedule for Tuesday.
    tuesday :: Core.Maybe (Core.HashMap Types.Hour Types.Switch),
    -- | The schedule for Wednesday.
    wednesday :: Core.Maybe (Core.HashMap Types.Hour Types.Switch)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WeeklyAutoScalingSchedule' value with any optional fields omitted.
mkWeeklyAutoScalingSchedule ::
  WeeklyAutoScalingSchedule
mkWeeklyAutoScalingSchedule =
  WeeklyAutoScalingSchedule'
    { friday = Core.Nothing,
      monday = Core.Nothing,
      saturday = Core.Nothing,
      sunday = Core.Nothing,
      thursday = Core.Nothing,
      tuesday = Core.Nothing,
      wednesday = Core.Nothing
    }

-- | The schedule for Friday.
--
-- /Note:/ Consider using 'friday' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wassFriday :: Lens.Lens' WeeklyAutoScalingSchedule (Core.Maybe (Core.HashMap Types.Hour Types.Switch))
wassFriday = Lens.field @"friday"
{-# DEPRECATED wassFriday "Use generic-lens or generic-optics with 'friday' instead." #-}

-- | The schedule for Monday.
--
-- /Note:/ Consider using 'monday' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wassMonday :: Lens.Lens' WeeklyAutoScalingSchedule (Core.Maybe (Core.HashMap Types.Hour Types.Switch))
wassMonday = Lens.field @"monday"
{-# DEPRECATED wassMonday "Use generic-lens or generic-optics with 'monday' instead." #-}

-- | The schedule for Saturday.
--
-- /Note:/ Consider using 'saturday' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wassSaturday :: Lens.Lens' WeeklyAutoScalingSchedule (Core.Maybe (Core.HashMap Types.Hour Types.Switch))
wassSaturday = Lens.field @"saturday"
{-# DEPRECATED wassSaturday "Use generic-lens or generic-optics with 'saturday' instead." #-}

-- | The schedule for Sunday.
--
-- /Note:/ Consider using 'sunday' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wassSunday :: Lens.Lens' WeeklyAutoScalingSchedule (Core.Maybe (Core.HashMap Types.Hour Types.Switch))
wassSunday = Lens.field @"sunday"
{-# DEPRECATED wassSunday "Use generic-lens or generic-optics with 'sunday' instead." #-}

-- | The schedule for Thursday.
--
-- /Note:/ Consider using 'thursday' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wassThursday :: Lens.Lens' WeeklyAutoScalingSchedule (Core.Maybe (Core.HashMap Types.Hour Types.Switch))
wassThursday = Lens.field @"thursday"
{-# DEPRECATED wassThursday "Use generic-lens or generic-optics with 'thursday' instead." #-}

-- | The schedule for Tuesday.
--
-- /Note:/ Consider using 'tuesday' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wassTuesday :: Lens.Lens' WeeklyAutoScalingSchedule (Core.Maybe (Core.HashMap Types.Hour Types.Switch))
wassTuesday = Lens.field @"tuesday"
{-# DEPRECATED wassTuesday "Use generic-lens or generic-optics with 'tuesday' instead." #-}

-- | The schedule for Wednesday.
--
-- /Note:/ Consider using 'wednesday' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wassWednesday :: Lens.Lens' WeeklyAutoScalingSchedule (Core.Maybe (Core.HashMap Types.Hour Types.Switch))
wassWednesday = Lens.field @"wednesday"
{-# DEPRECATED wassWednesday "Use generic-lens or generic-optics with 'wednesday' instead." #-}

instance Core.FromJSON WeeklyAutoScalingSchedule where
  toJSON WeeklyAutoScalingSchedule {..} =
    Core.object
      ( Core.catMaybes
          [ ("Friday" Core..=) Core.<$> friday,
            ("Monday" Core..=) Core.<$> monday,
            ("Saturday" Core..=) Core.<$> saturday,
            ("Sunday" Core..=) Core.<$> sunday,
            ("Thursday" Core..=) Core.<$> thursday,
            ("Tuesday" Core..=) Core.<$> tuesday,
            ("Wednesday" Core..=) Core.<$> wednesday
          ]
      )

instance Core.FromJSON WeeklyAutoScalingSchedule where
  parseJSON =
    Core.withObject "WeeklyAutoScalingSchedule" Core.$
      \x ->
        WeeklyAutoScalingSchedule'
          Core.<$> (x Core..:? "Friday")
          Core.<*> (x Core..:? "Monday")
          Core.<*> (x Core..:? "Saturday")
          Core.<*> (x Core..:? "Sunday")
          Core.<*> (x Core..:? "Thursday")
          Core.<*> (x Core..:? "Tuesday")
          Core.<*> (x Core..:? "Wednesday")
