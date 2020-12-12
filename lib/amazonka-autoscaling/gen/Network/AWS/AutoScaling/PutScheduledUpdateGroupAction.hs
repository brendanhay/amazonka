{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scheduled scaling action for an Auto Scaling group. If you leave a parameter unspecified when updating a scheduled scaling action, the corresponding value remains unchanged.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/schedule_time.html Scheduled scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
  ( -- * Creating a request
    PutScheduledUpdateGroupAction (..),
    mkPutScheduledUpdateGroupAction,

    -- ** Request lenses
    psugaStartTime,
    psugaTime,
    psugaMaxSize,
    psugaRecurrence,
    psugaDesiredCapacity,
    psugaMinSize,
    psugaEndTime,
    psugaAutoScalingGroupName,
    psugaScheduledActionName,

    -- * Destructuring the response
    PutScheduledUpdateGroupActionResponse (..),
    mkPutScheduledUpdateGroupActionResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutScheduledUpdateGroupAction' smart constructor.
data PutScheduledUpdateGroupAction = PutScheduledUpdateGroupAction'
  { startTime ::
      Lude.Maybe Lude.DateTime,
    time ::
      Lude.Maybe Lude.DateTime,
    maxSize :: Lude.Maybe Lude.Int,
    recurrence ::
      Lude.Maybe Lude.Text,
    desiredCapacity ::
      Lude.Maybe Lude.Int,
    minSize :: Lude.Maybe Lude.Int,
    endTime ::
      Lude.Maybe Lude.DateTime,
    autoScalingGroupName ::
      Lude.Text,
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

-- | Creates a value of 'PutScheduledUpdateGroupAction' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'desiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group after the scheduled action runs and the capacity it attempts to maintain. It can scale beyond this capacity if you add more scaling conditions.
-- * 'endTime' - The date and time for the recurring schedule to end. Amazon EC2 Auto Scaling does not perform the action after this time.
-- * 'maxSize' - The maximum size of the Auto Scaling group.
-- * 'minSize' - The minimum size of the Auto Scaling group.
-- * 'recurrence' - The recurring schedule for this action, in Unix cron syntax format. This format consists of five fields separated by white spaces: [Minute] [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be in quotes (for example, @"30 0 1 1,6,12 *"@ ). For more information about this format, see <http://crontab.org Crontab> .
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action starts and stops.
-- * 'scheduledActionName' - The name of this scaling action.
-- * 'startTime' - The date and time for this action to start, in YYYY-MM-DDThh:mm:ssZ format in UTC/GMT only and in quotes (for example, @"2019-06-01T00:00:00Z"@ ).
--
-- If you specify @Recurrence@ and @StartTime@ , Amazon EC2 Auto Scaling performs the action at this time, and then performs the action based on the specified recurrence.
-- If you try to schedule your action in the past, Amazon EC2 Auto Scaling returns an error message.
-- * 'time' - This parameter is no longer used.
mkPutScheduledUpdateGroupAction ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  -- | 'scheduledActionName'
  Lude.Text ->
  PutScheduledUpdateGroupAction
mkPutScheduledUpdateGroupAction
  pAutoScalingGroupName_
  pScheduledActionName_ =
    PutScheduledUpdateGroupAction'
      { startTime = Lude.Nothing,
        time = Lude.Nothing,
        maxSize = Lude.Nothing,
        recurrence = Lude.Nothing,
        desiredCapacity = Lude.Nothing,
        minSize = Lude.Nothing,
        endTime = Lude.Nothing,
        autoScalingGroupName = pAutoScalingGroupName_,
        scheduledActionName = pScheduledActionName_
      }

-- | The date and time for this action to start, in YYYY-MM-DDThh:mm:ssZ format in UTC/GMT only and in quotes (for example, @"2019-06-01T00:00:00Z"@ ).
--
-- If you specify @Recurrence@ and @StartTime@ , Amazon EC2 Auto Scaling performs the action at this time, and then performs the action based on the specified recurrence.
-- If you try to schedule your action in the past, Amazon EC2 Auto Scaling returns an error message.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaStartTime :: Lens.Lens' PutScheduledUpdateGroupAction (Lude.Maybe Lude.DateTime)
psugaStartTime = Lens.lens (startTime :: PutScheduledUpdateGroupAction -> Lude.Maybe Lude.DateTime) (\s a -> s {startTime = a} :: PutScheduledUpdateGroupAction)
{-# DEPRECATED psugaStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | This parameter is no longer used.
--
-- /Note:/ Consider using 'time' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaTime :: Lens.Lens' PutScheduledUpdateGroupAction (Lude.Maybe Lude.DateTime)
psugaTime = Lens.lens (time :: PutScheduledUpdateGroupAction -> Lude.Maybe Lude.DateTime) (\s a -> s {time = a} :: PutScheduledUpdateGroupAction)
{-# DEPRECATED psugaTime "Use generic-lens or generic-optics with 'time' instead." #-}

-- | The maximum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaMaxSize :: Lens.Lens' PutScheduledUpdateGroupAction (Lude.Maybe Lude.Int)
psugaMaxSize = Lens.lens (maxSize :: PutScheduledUpdateGroupAction -> Lude.Maybe Lude.Int) (\s a -> s {maxSize = a} :: PutScheduledUpdateGroupAction)
{-# DEPRECATED psugaMaxSize "Use generic-lens or generic-optics with 'maxSize' instead." #-}

-- | The recurring schedule for this action, in Unix cron syntax format. This format consists of five fields separated by white spaces: [Minute] [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be in quotes (for example, @"30 0 1 1,6,12 *"@ ). For more information about this format, see <http://crontab.org Crontab> .
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action starts and stops.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaRecurrence :: Lens.Lens' PutScheduledUpdateGroupAction (Lude.Maybe Lude.Text)
psugaRecurrence = Lens.lens (recurrence :: PutScheduledUpdateGroupAction -> Lude.Maybe Lude.Text) (\s a -> s {recurrence = a} :: PutScheduledUpdateGroupAction)
{-# DEPRECATED psugaRecurrence "Use generic-lens or generic-optics with 'recurrence' instead." #-}

-- | The desired capacity is the initial capacity of the Auto Scaling group after the scheduled action runs and the capacity it attempts to maintain. It can scale beyond this capacity if you add more scaling conditions.
--
-- /Note:/ Consider using 'desiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaDesiredCapacity :: Lens.Lens' PutScheduledUpdateGroupAction (Lude.Maybe Lude.Int)
psugaDesiredCapacity = Lens.lens (desiredCapacity :: PutScheduledUpdateGroupAction -> Lude.Maybe Lude.Int) (\s a -> s {desiredCapacity = a} :: PutScheduledUpdateGroupAction)
{-# DEPRECATED psugaDesiredCapacity "Use generic-lens or generic-optics with 'desiredCapacity' instead." #-}

-- | The minimum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaMinSize :: Lens.Lens' PutScheduledUpdateGroupAction (Lude.Maybe Lude.Int)
psugaMinSize = Lens.lens (minSize :: PutScheduledUpdateGroupAction -> Lude.Maybe Lude.Int) (\s a -> s {minSize = a} :: PutScheduledUpdateGroupAction)
{-# DEPRECATED psugaMinSize "Use generic-lens or generic-optics with 'minSize' instead." #-}

-- | The date and time for the recurring schedule to end. Amazon EC2 Auto Scaling does not perform the action after this time.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaEndTime :: Lens.Lens' PutScheduledUpdateGroupAction (Lude.Maybe Lude.DateTime)
psugaEndTime = Lens.lens (endTime :: PutScheduledUpdateGroupAction -> Lude.Maybe Lude.DateTime) (\s a -> s {endTime = a} :: PutScheduledUpdateGroupAction)
{-# DEPRECATED psugaEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaAutoScalingGroupName :: Lens.Lens' PutScheduledUpdateGroupAction Lude.Text
psugaAutoScalingGroupName = Lens.lens (autoScalingGroupName :: PutScheduledUpdateGroupAction -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: PutScheduledUpdateGroupAction)
{-# DEPRECATED psugaAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The name of this scaling action.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaScheduledActionName :: Lens.Lens' PutScheduledUpdateGroupAction Lude.Text
psugaScheduledActionName = Lens.lens (scheduledActionName :: PutScheduledUpdateGroupAction -> Lude.Text) (\s a -> s {scheduledActionName = a} :: PutScheduledUpdateGroupAction)
{-# DEPRECATED psugaScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

instance Lude.AWSRequest PutScheduledUpdateGroupAction where
  type
    Rs PutScheduledUpdateGroupAction =
      PutScheduledUpdateGroupActionResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull PutScheduledUpdateGroupActionResponse'

instance Lude.ToHeaders PutScheduledUpdateGroupAction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutScheduledUpdateGroupAction where
  toPath = Lude.const "/"

instance Lude.ToQuery PutScheduledUpdateGroupAction where
  toQuery PutScheduledUpdateGroupAction' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("PutScheduledUpdateGroupAction" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "StartTime" Lude.=: startTime,
        "Time" Lude.=: time,
        "MaxSize" Lude.=: maxSize,
        "Recurrence" Lude.=: recurrence,
        "DesiredCapacity" Lude.=: desiredCapacity,
        "MinSize" Lude.=: minSize,
        "EndTime" Lude.=: endTime,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "ScheduledActionName" Lude.=: scheduledActionName
      ]

-- | /See:/ 'mkPutScheduledUpdateGroupActionResponse' smart constructor.
data PutScheduledUpdateGroupActionResponse = PutScheduledUpdateGroupActionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutScheduledUpdateGroupActionResponse' with the minimum fields required to make a request.
mkPutScheduledUpdateGroupActionResponse ::
  PutScheduledUpdateGroupActionResponse
mkPutScheduledUpdateGroupActionResponse =
  PutScheduledUpdateGroupActionResponse'
