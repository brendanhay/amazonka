{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    psugaAutoScalingGroupName,
    psugaScheduledActionName,
    psugaDesiredCapacity,
    psugaEndTime,
    psugaMaxSize,
    psugaMinSize,
    psugaRecurrence,
    psugaStartTime,
    psugaTime,

    -- * Destructuring the response
    PutScheduledUpdateGroupActionResponse (..),
    mkPutScheduledUpdateGroupActionResponse,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutScheduledUpdateGroupAction' smart constructor.
data PutScheduledUpdateGroupAction = PutScheduledUpdateGroupAction'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.ResourceName,
    -- | The name of this scaling action.
    scheduledActionName :: Types.XmlStringMaxLen255,
    -- | The desired capacity is the initial capacity of the Auto Scaling group after the scheduled action runs and the capacity it attempts to maintain. It can scale beyond this capacity if you add more scaling conditions.
    desiredCapacity :: Core.Maybe Core.Int,
    -- | The date and time for the recurring schedule to end. Amazon EC2 Auto Scaling does not perform the action after this time.
    endTime :: Core.Maybe Core.UTCTime,
    -- | The maximum size of the Auto Scaling group.
    maxSize :: Core.Maybe Core.Int,
    -- | The minimum size of the Auto Scaling group.
    minSize :: Core.Maybe Core.Int,
    -- | The recurring schedule for this action, in Unix cron syntax format. This format consists of five fields separated by white spaces: [Minute] [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be in quotes (for example, @"30 0 1 1,6,12 *"@ ). For more information about this format, see <http://crontab.org Crontab> .
    --
    -- When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action starts and stops.
    recurrence :: Core.Maybe Types.XmlStringMaxLen255,
    -- | The date and time for this action to start, in YYYY-MM-DDThh:mm:ssZ format in UTC/GMT only and in quotes (for example, @"2019-06-01T00:00:00Z"@ ).
    --
    -- If you specify @Recurrence@ and @StartTime@ , Amazon EC2 Auto Scaling performs the action at this time, and then performs the action based on the specified recurrence.
    -- If you try to schedule your action in the past, Amazon EC2 Auto Scaling returns an error message.
    startTime :: Core.Maybe Core.UTCTime,
    -- | This parameter is no longer used.
    time :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PutScheduledUpdateGroupAction' value with any optional fields omitted.
mkPutScheduledUpdateGroupAction ::
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  -- | 'scheduledActionName'
  Types.XmlStringMaxLen255 ->
  PutScheduledUpdateGroupAction
mkPutScheduledUpdateGroupAction
  autoScalingGroupName
  scheduledActionName =
    PutScheduledUpdateGroupAction'
      { autoScalingGroupName,
        scheduledActionName,
        desiredCapacity = Core.Nothing,
        endTime = Core.Nothing,
        maxSize = Core.Nothing,
        minSize = Core.Nothing,
        recurrence = Core.Nothing,
        startTime = Core.Nothing,
        time = Core.Nothing
      }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaAutoScalingGroupName :: Lens.Lens' PutScheduledUpdateGroupAction Types.ResourceName
psugaAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED psugaAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The name of this scaling action.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaScheduledActionName :: Lens.Lens' PutScheduledUpdateGroupAction Types.XmlStringMaxLen255
psugaScheduledActionName = Lens.field @"scheduledActionName"
{-# DEPRECATED psugaScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

-- | The desired capacity is the initial capacity of the Auto Scaling group after the scheduled action runs and the capacity it attempts to maintain. It can scale beyond this capacity if you add more scaling conditions.
--
-- /Note:/ Consider using 'desiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaDesiredCapacity :: Lens.Lens' PutScheduledUpdateGroupAction (Core.Maybe Core.Int)
psugaDesiredCapacity = Lens.field @"desiredCapacity"
{-# DEPRECATED psugaDesiredCapacity "Use generic-lens or generic-optics with 'desiredCapacity' instead." #-}

-- | The date and time for the recurring schedule to end. Amazon EC2 Auto Scaling does not perform the action after this time.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaEndTime :: Lens.Lens' PutScheduledUpdateGroupAction (Core.Maybe Core.UTCTime)
psugaEndTime = Lens.field @"endTime"
{-# DEPRECATED psugaEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The maximum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaMaxSize :: Lens.Lens' PutScheduledUpdateGroupAction (Core.Maybe Core.Int)
psugaMaxSize = Lens.field @"maxSize"
{-# DEPRECATED psugaMaxSize "Use generic-lens or generic-optics with 'maxSize' instead." #-}

-- | The minimum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaMinSize :: Lens.Lens' PutScheduledUpdateGroupAction (Core.Maybe Core.Int)
psugaMinSize = Lens.field @"minSize"
{-# DEPRECATED psugaMinSize "Use generic-lens or generic-optics with 'minSize' instead." #-}

-- | The recurring schedule for this action, in Unix cron syntax format. This format consists of five fields separated by white spaces: [Minute] [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be in quotes (for example, @"30 0 1 1,6,12 *"@ ). For more information about this format, see <http://crontab.org Crontab> .
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action starts and stops.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaRecurrence :: Lens.Lens' PutScheduledUpdateGroupAction (Core.Maybe Types.XmlStringMaxLen255)
psugaRecurrence = Lens.field @"recurrence"
{-# DEPRECATED psugaRecurrence "Use generic-lens or generic-optics with 'recurrence' instead." #-}

-- | The date and time for this action to start, in YYYY-MM-DDThh:mm:ssZ format in UTC/GMT only and in quotes (for example, @"2019-06-01T00:00:00Z"@ ).
--
-- If you specify @Recurrence@ and @StartTime@ , Amazon EC2 Auto Scaling performs the action at this time, and then performs the action based on the specified recurrence.
-- If you try to schedule your action in the past, Amazon EC2 Auto Scaling returns an error message.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaStartTime :: Lens.Lens' PutScheduledUpdateGroupAction (Core.Maybe Core.UTCTime)
psugaStartTime = Lens.field @"startTime"
{-# DEPRECATED psugaStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | This parameter is no longer used.
--
-- /Note:/ Consider using 'time' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psugaTime :: Lens.Lens' PutScheduledUpdateGroupAction (Core.Maybe Core.UTCTime)
psugaTime = Lens.field @"time"
{-# DEPRECATED psugaTime "Use generic-lens or generic-optics with 'time' instead." #-}

instance Core.AWSRequest PutScheduledUpdateGroupAction where
  type
    Rs PutScheduledUpdateGroupAction =
      PutScheduledUpdateGroupActionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "PutScheduledUpdateGroupAction")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> (Core.toQueryValue "ScheduledActionName" scheduledActionName)
                Core.<> (Core.toQueryValue "DesiredCapacity" Core.<$> desiredCapacity)
                Core.<> (Core.toQueryValue "EndTime" Core.<$> endTime)
                Core.<> (Core.toQueryValue "MaxSize" Core.<$> maxSize)
                Core.<> (Core.toQueryValue "MinSize" Core.<$> minSize)
                Core.<> (Core.toQueryValue "Recurrence" Core.<$> recurrence)
                Core.<> (Core.toQueryValue "StartTime" Core.<$> startTime)
                Core.<> (Core.toQueryValue "Time" Core.<$> time)
            )
      }
  response =
    Response.receiveNull PutScheduledUpdateGroupActionResponse'

-- | /See:/ 'mkPutScheduledUpdateGroupActionResponse' smart constructor.
data PutScheduledUpdateGroupActionResponse = PutScheduledUpdateGroupActionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutScheduledUpdateGroupActionResponse' value with any optional fields omitted.
mkPutScheduledUpdateGroupActionResponse ::
  PutScheduledUpdateGroupActionResponse
mkPutScheduledUpdateGroupActionResponse =
  PutScheduledUpdateGroupActionResponse'
