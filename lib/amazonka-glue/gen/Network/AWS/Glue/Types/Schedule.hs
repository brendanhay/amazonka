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
    sState,
    sScheduleExpression,
  )
where

import Network.AWS.Glue.Types.ScheduleState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A scheduling object using a @cron@ statement to schedule an event.
--
-- /See:/ 'mkSchedule' smart constructor.
data Schedule = Schedule'
  { state :: Lude.Maybe ScheduleState,
    scheduleExpression :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Schedule' with the minimum fields required to make a request.
--
-- * 'scheduleExpression' - A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
-- * 'state' - The state of the schedule.
mkSchedule ::
  Schedule
mkSchedule =
  Schedule'
    { state = Lude.Nothing,
      scheduleExpression = Lude.Nothing
    }

-- | The state of the schedule.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sState :: Lens.Lens' Schedule (Lude.Maybe ScheduleState)
sState = Lens.lens (state :: Schedule -> Lude.Maybe ScheduleState) (\s a -> s {state = a} :: Schedule)
{-# DEPRECATED sState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sScheduleExpression :: Lens.Lens' Schedule (Lude.Maybe Lude.Text)
sScheduleExpression = Lens.lens (scheduleExpression :: Schedule -> Lude.Maybe Lude.Text) (\s a -> s {scheduleExpression = a} :: Schedule)
{-# DEPRECATED sScheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead." #-}

instance Lude.FromJSON Schedule where
  parseJSON =
    Lude.withObject
      "Schedule"
      ( \x ->
          Schedule'
            Lude.<$> (x Lude..:? "State") Lude.<*> (x Lude..:? "ScheduleExpression")
      )
