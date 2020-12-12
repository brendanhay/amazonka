{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ScheduledUpdateGroupAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.ScheduledUpdateGroupAction
  ( ScheduledUpdateGroupAction (..),

    -- * Smart constructor
    mkScheduledUpdateGroupAction,

    -- * Lenses
    sugaScheduledActionARN,
    sugaStartTime,
    sugaTime,
    sugaScheduledActionName,
    sugaMaxSize,
    sugaRecurrence,
    sugaDesiredCapacity,
    sugaMinSize,
    sugaAutoScalingGroupName,
    sugaEndTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a scheduled scaling action.
--
-- /See:/ 'mkScheduledUpdateGroupAction' smart constructor.
data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction'
  { scheduledActionARN ::
      Lude.Maybe Lude.Text,
    startTime :: Lude.Maybe Lude.DateTime,
    time :: Lude.Maybe Lude.DateTime,
    scheduledActionName ::
      Lude.Maybe Lude.Text,
    maxSize :: Lude.Maybe Lude.Int,
    recurrence :: Lude.Maybe Lude.Text,
    desiredCapacity ::
      Lude.Maybe Lude.Int,
    minSize :: Lude.Maybe Lude.Int,
    autoScalingGroupName ::
      Lude.Maybe Lude.Text,
    endTime :: Lude.Maybe Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduledUpdateGroupAction' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'desiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group after the scheduled action runs and the capacity it attempts to maintain.
-- * 'endTime' - The date and time in UTC for the recurring schedule to end. For example, @"2019-06-01T00:00:00Z"@ .
-- * 'maxSize' - The maximum size of the Auto Scaling group.
-- * 'minSize' - The minimum size of the Auto Scaling group.
-- * 'recurrence' - The recurring schedule for the action, in Unix cron syntax format.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action starts and stops.
-- * 'scheduledActionARN' - The Amazon Resource Name (ARN) of the scheduled action.
-- * 'scheduledActionName' - The name of the scheduled action.
-- * 'startTime' - The date and time in UTC for this action to start. For example, @"2019-06-01T00:00:00Z"@ .
-- * 'time' - This parameter is no longer used.
mkScheduledUpdateGroupAction ::
  ScheduledUpdateGroupAction
mkScheduledUpdateGroupAction =
  ScheduledUpdateGroupAction'
    { scheduledActionARN = Lude.Nothing,
      startTime = Lude.Nothing,
      time = Lude.Nothing,
      scheduledActionName = Lude.Nothing,
      maxSize = Lude.Nothing,
      recurrence = Lude.Nothing,
      desiredCapacity = Lude.Nothing,
      minSize = Lude.Nothing,
      autoScalingGroupName = Lude.Nothing,
      endTime = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaScheduledActionARN :: Lens.Lens' ScheduledUpdateGroupAction (Lude.Maybe Lude.Text)
sugaScheduledActionARN = Lens.lens (scheduledActionARN :: ScheduledUpdateGroupAction -> Lude.Maybe Lude.Text) (\s a -> s {scheduledActionARN = a} :: ScheduledUpdateGroupAction)
{-# DEPRECATED sugaScheduledActionARN "Use generic-lens or generic-optics with 'scheduledActionARN' instead." #-}

-- | The date and time in UTC for this action to start. For example, @"2019-06-01T00:00:00Z"@ .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaStartTime :: Lens.Lens' ScheduledUpdateGroupAction (Lude.Maybe Lude.DateTime)
sugaStartTime = Lens.lens (startTime :: ScheduledUpdateGroupAction -> Lude.Maybe Lude.DateTime) (\s a -> s {startTime = a} :: ScheduledUpdateGroupAction)
{-# DEPRECATED sugaStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | This parameter is no longer used.
--
-- /Note:/ Consider using 'time' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaTime :: Lens.Lens' ScheduledUpdateGroupAction (Lude.Maybe Lude.DateTime)
sugaTime = Lens.lens (time :: ScheduledUpdateGroupAction -> Lude.Maybe Lude.DateTime) (\s a -> s {time = a} :: ScheduledUpdateGroupAction)
{-# DEPRECATED sugaTime "Use generic-lens or generic-optics with 'time' instead." #-}

-- | The name of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaScheduledActionName :: Lens.Lens' ScheduledUpdateGroupAction (Lude.Maybe Lude.Text)
sugaScheduledActionName = Lens.lens (scheduledActionName :: ScheduledUpdateGroupAction -> Lude.Maybe Lude.Text) (\s a -> s {scheduledActionName = a} :: ScheduledUpdateGroupAction)
{-# DEPRECATED sugaScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

-- | The maximum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaMaxSize :: Lens.Lens' ScheduledUpdateGroupAction (Lude.Maybe Lude.Int)
sugaMaxSize = Lens.lens (maxSize :: ScheduledUpdateGroupAction -> Lude.Maybe Lude.Int) (\s a -> s {maxSize = a} :: ScheduledUpdateGroupAction)
{-# DEPRECATED sugaMaxSize "Use generic-lens or generic-optics with 'maxSize' instead." #-}

-- | The recurring schedule for the action, in Unix cron syntax format.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action starts and stops.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaRecurrence :: Lens.Lens' ScheduledUpdateGroupAction (Lude.Maybe Lude.Text)
sugaRecurrence = Lens.lens (recurrence :: ScheduledUpdateGroupAction -> Lude.Maybe Lude.Text) (\s a -> s {recurrence = a} :: ScheduledUpdateGroupAction)
{-# DEPRECATED sugaRecurrence "Use generic-lens or generic-optics with 'recurrence' instead." #-}

-- | The desired capacity is the initial capacity of the Auto Scaling group after the scheduled action runs and the capacity it attempts to maintain.
--
-- /Note:/ Consider using 'desiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaDesiredCapacity :: Lens.Lens' ScheduledUpdateGroupAction (Lude.Maybe Lude.Int)
sugaDesiredCapacity = Lens.lens (desiredCapacity :: ScheduledUpdateGroupAction -> Lude.Maybe Lude.Int) (\s a -> s {desiredCapacity = a} :: ScheduledUpdateGroupAction)
{-# DEPRECATED sugaDesiredCapacity "Use generic-lens or generic-optics with 'desiredCapacity' instead." #-}

-- | The minimum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaMinSize :: Lens.Lens' ScheduledUpdateGroupAction (Lude.Maybe Lude.Int)
sugaMinSize = Lens.lens (minSize :: ScheduledUpdateGroupAction -> Lude.Maybe Lude.Int) (\s a -> s {minSize = a} :: ScheduledUpdateGroupAction)
{-# DEPRECATED sugaMinSize "Use generic-lens or generic-optics with 'minSize' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaAutoScalingGroupName :: Lens.Lens' ScheduledUpdateGroupAction (Lude.Maybe Lude.Text)
sugaAutoScalingGroupName = Lens.lens (autoScalingGroupName :: ScheduledUpdateGroupAction -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingGroupName = a} :: ScheduledUpdateGroupAction)
{-# DEPRECATED sugaAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The date and time in UTC for the recurring schedule to end. For example, @"2019-06-01T00:00:00Z"@ .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaEndTime :: Lens.Lens' ScheduledUpdateGroupAction (Lude.Maybe Lude.DateTime)
sugaEndTime = Lens.lens (endTime :: ScheduledUpdateGroupAction -> Lude.Maybe Lude.DateTime) (\s a -> s {endTime = a} :: ScheduledUpdateGroupAction)
{-# DEPRECATED sugaEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Lude.FromXML ScheduledUpdateGroupAction where
  parseXML x =
    ScheduledUpdateGroupAction'
      Lude.<$> (x Lude..@? "ScheduledActionARN")
      Lude.<*> (x Lude..@? "StartTime")
      Lude.<*> (x Lude..@? "Time")
      Lude.<*> (x Lude..@? "ScheduledActionName")
      Lude.<*> (x Lude..@? "MaxSize")
      Lude.<*> (x Lude..@? "Recurrence")
      Lude.<*> (x Lude..@? "DesiredCapacity")
      Lude.<*> (x Lude..@? "MinSize")
      Lude.<*> (x Lude..@? "AutoScalingGroupName")
      Lude.<*> (x Lude..@? "EndTime")
