{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ScheduledUpdateGroupActionRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.ScheduledUpdateGroupActionRequest
  ( ScheduledUpdateGroupActionRequest (..),

    -- * Smart constructor
    mkScheduledUpdateGroupActionRequest,

    -- * Lenses
    sugarStartTime,
    sugarMaxSize,
    sugarRecurrence,
    sugarDesiredCapacity,
    sugarMinSize,
    sugarEndTime,
    sugarScheduledActionName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes information used for one or more scheduled scaling action updates in a 'BatchPutScheduledUpdateGroupAction' operation.
--
-- When updating a scheduled scaling action, all optional parameters are left unchanged if not specified.
--
-- /See:/ 'mkScheduledUpdateGroupActionRequest' smart constructor.
data ScheduledUpdateGroupActionRequest = ScheduledUpdateGroupActionRequest'
  { startTime ::
      Lude.Maybe
        Lude.DateTime,
    maxSize ::
      Lude.Maybe Lude.Int,
    recurrence ::
      Lude.Maybe Lude.Text,
    desiredCapacity ::
      Lude.Maybe Lude.Int,
    minSize ::
      Lude.Maybe Lude.Int,
    endTime ::
      Lude.Maybe
        Lude.DateTime,
    scheduledActionName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduledUpdateGroupActionRequest' with the minimum fields required to make a request.
--
-- * 'desiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group after the scheduled action runs and the capacity it attempts to maintain.
-- * 'endTime' - The date and time for the recurring schedule to end. Amazon EC2 Auto Scaling does not perform the action after this time.
-- * 'maxSize' - The maximum size of the Auto Scaling group.
-- * 'minSize' - The minimum size of the Auto Scaling group.
-- * 'recurrence' - The recurring schedule for the action, in Unix cron syntax format. This format consists of five fields separated by white spaces: [Minute] [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be in quotes (for example, @"30 0 1 1,6,12 *"@ ). For more information about this format, see <http://crontab.org Crontab> .
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action starts and stops.
-- * 'scheduledActionName' - The name of the scaling action.
-- * 'startTime' - The date and time for the action to start, in YYYY-MM-DDThh:mm:ssZ format in UTC/GMT only and in quotes (for example, @"2019-06-01T00:00:00Z"@ ).
--
-- If you specify @Recurrence@ and @StartTime@ , Amazon EC2 Auto Scaling performs the action at this time, and then performs the action based on the specified recurrence.
-- If you try to schedule the action in the past, Amazon EC2 Auto Scaling returns an error message.
mkScheduledUpdateGroupActionRequest ::
  -- | 'scheduledActionName'
  Lude.Text ->
  ScheduledUpdateGroupActionRequest
mkScheduledUpdateGroupActionRequest pScheduledActionName_ =
  ScheduledUpdateGroupActionRequest'
    { startTime = Lude.Nothing,
      maxSize = Lude.Nothing,
      recurrence = Lude.Nothing,
      desiredCapacity = Lude.Nothing,
      minSize = Lude.Nothing,
      endTime = Lude.Nothing,
      scheduledActionName = pScheduledActionName_
    }

-- | The date and time for the action to start, in YYYY-MM-DDThh:mm:ssZ format in UTC/GMT only and in quotes (for example, @"2019-06-01T00:00:00Z"@ ).
--
-- If you specify @Recurrence@ and @StartTime@ , Amazon EC2 Auto Scaling performs the action at this time, and then performs the action based on the specified recurrence.
-- If you try to schedule the action in the past, Amazon EC2 Auto Scaling returns an error message.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugarStartTime :: Lens.Lens' ScheduledUpdateGroupActionRequest (Lude.Maybe Lude.DateTime)
sugarStartTime = Lens.lens (startTime :: ScheduledUpdateGroupActionRequest -> Lude.Maybe Lude.DateTime) (\s a -> s {startTime = a} :: ScheduledUpdateGroupActionRequest)
{-# DEPRECATED sugarStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The maximum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugarMaxSize :: Lens.Lens' ScheduledUpdateGroupActionRequest (Lude.Maybe Lude.Int)
sugarMaxSize = Lens.lens (maxSize :: ScheduledUpdateGroupActionRequest -> Lude.Maybe Lude.Int) (\s a -> s {maxSize = a} :: ScheduledUpdateGroupActionRequest)
{-# DEPRECATED sugarMaxSize "Use generic-lens or generic-optics with 'maxSize' instead." #-}

-- | The recurring schedule for the action, in Unix cron syntax format. This format consists of five fields separated by white spaces: [Minute] [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be in quotes (for example, @"30 0 1 1,6,12 *"@ ). For more information about this format, see <http://crontab.org Crontab> .
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action starts and stops.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugarRecurrence :: Lens.Lens' ScheduledUpdateGroupActionRequest (Lude.Maybe Lude.Text)
sugarRecurrence = Lens.lens (recurrence :: ScheduledUpdateGroupActionRequest -> Lude.Maybe Lude.Text) (\s a -> s {recurrence = a} :: ScheduledUpdateGroupActionRequest)
{-# DEPRECATED sugarRecurrence "Use generic-lens or generic-optics with 'recurrence' instead." #-}

-- | The desired capacity is the initial capacity of the Auto Scaling group after the scheduled action runs and the capacity it attempts to maintain.
--
-- /Note:/ Consider using 'desiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugarDesiredCapacity :: Lens.Lens' ScheduledUpdateGroupActionRequest (Lude.Maybe Lude.Int)
sugarDesiredCapacity = Lens.lens (desiredCapacity :: ScheduledUpdateGroupActionRequest -> Lude.Maybe Lude.Int) (\s a -> s {desiredCapacity = a} :: ScheduledUpdateGroupActionRequest)
{-# DEPRECATED sugarDesiredCapacity "Use generic-lens or generic-optics with 'desiredCapacity' instead." #-}

-- | The minimum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugarMinSize :: Lens.Lens' ScheduledUpdateGroupActionRequest (Lude.Maybe Lude.Int)
sugarMinSize = Lens.lens (minSize :: ScheduledUpdateGroupActionRequest -> Lude.Maybe Lude.Int) (\s a -> s {minSize = a} :: ScheduledUpdateGroupActionRequest)
{-# DEPRECATED sugarMinSize "Use generic-lens or generic-optics with 'minSize' instead." #-}

-- | The date and time for the recurring schedule to end. Amazon EC2 Auto Scaling does not perform the action after this time.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugarEndTime :: Lens.Lens' ScheduledUpdateGroupActionRequest (Lude.Maybe Lude.DateTime)
sugarEndTime = Lens.lens (endTime :: ScheduledUpdateGroupActionRequest -> Lude.Maybe Lude.DateTime) (\s a -> s {endTime = a} :: ScheduledUpdateGroupActionRequest)
{-# DEPRECATED sugarEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The name of the scaling action.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugarScheduledActionName :: Lens.Lens' ScheduledUpdateGroupActionRequest Lude.Text
sugarScheduledActionName = Lens.lens (scheduledActionName :: ScheduledUpdateGroupActionRequest -> Lude.Text) (\s a -> s {scheduledActionName = a} :: ScheduledUpdateGroupActionRequest)
{-# DEPRECATED sugarScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

instance Lude.ToQuery ScheduledUpdateGroupActionRequest where
  toQuery ScheduledUpdateGroupActionRequest' {..} =
    Lude.mconcat
      [ "StartTime" Lude.=: startTime,
        "MaxSize" Lude.=: maxSize,
        "Recurrence" Lude.=: recurrence,
        "DesiredCapacity" Lude.=: desiredCapacity,
        "MinSize" Lude.=: minSize,
        "EndTime" Lude.=: endTime,
        "ScheduledActionName" Lude.=: scheduledActionName
      ]
