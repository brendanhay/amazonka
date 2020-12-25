{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Schedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Schedule
  ( Schedule (..),

    -- * Smart constructor
    mkSchedule,

    -- * Lenses
    sScheduleExpression,
    sState,
  )
where

import qualified Network.AWS.Glue.Types.ScheduleExpression as Types
import qualified Network.AWS.Glue.Types.ScheduleState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A scheduling object using a @cron@ statement to schedule an event.
--
-- /See:/ 'mkSchedule' smart constructor.
data Schedule = Schedule'
  { -- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
    scheduleExpression :: Core.Maybe Types.ScheduleExpression,
    -- | The state of the schedule.
    state :: Core.Maybe Types.ScheduleState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Schedule' value with any optional fields omitted.
mkSchedule ::
  Schedule
mkSchedule =
  Schedule'
    { scheduleExpression = Core.Nothing,
      state = Core.Nothing
    }

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sScheduleExpression :: Lens.Lens' Schedule (Core.Maybe Types.ScheduleExpression)
sScheduleExpression = Lens.field @"scheduleExpression"
{-# DEPRECATED sScheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead." #-}

-- | The state of the schedule.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sState :: Lens.Lens' Schedule (Core.Maybe Types.ScheduleState)
sState = Lens.field @"state"
{-# DEPRECATED sState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON Schedule where
  parseJSON =
    Core.withObject "Schedule" Core.$
      \x ->
        Schedule'
          Core.<$> (x Core..:? "ScheduleExpression") Core.<*> (x Core..:? "State")
