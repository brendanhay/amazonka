{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ScheduledUpdateGroupActionRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.ScheduledUpdateGroupActionRequest
  ( ScheduledUpdateGroupActionRequest (..)
  -- * Smart constructor
  , mkScheduledUpdateGroupActionRequest
  -- * Lenses
  , sugarScheduledActionName
  , sugarDesiredCapacity
  , sugarEndTime
  , sugarMaxSize
  , sugarMinSize
  , sugarRecurrence
  , sugarStartTime
  ) where

import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes information used for one or more scheduled scaling action updates in a 'BatchPutScheduledUpdateGroupAction' operation.
--
-- When updating a scheduled scaling action, all optional parameters are left unchanged if not specified.
--
-- /See:/ 'mkScheduledUpdateGroupActionRequest' smart constructor.
data ScheduledUpdateGroupActionRequest = ScheduledUpdateGroupActionRequest'
  { scheduledActionName :: Types.XmlStringMaxLen255
    -- ^ The name of the scaling action.
  , desiredCapacity :: Core.Maybe Core.Int
    -- ^ The desired capacity is the initial capacity of the Auto Scaling group after the scheduled action runs and the capacity it attempts to maintain.
  , endTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time for the recurring schedule to end. Amazon EC2 Auto Scaling does not perform the action after this time.
  , maxSize :: Core.Maybe Core.Int
    -- ^ The maximum size of the Auto Scaling group.
  , minSize :: Core.Maybe Core.Int
    -- ^ The minimum size of the Auto Scaling group.
  , recurrence :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The recurring schedule for the action, in Unix cron syntax format. This format consists of five fields separated by white spaces: [Minute] [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be in quotes (for example, @"30 0 1 1,6,12 *"@ ). For more information about this format, see <http://crontab.org Crontab> .
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action starts and stops.
  , startTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time for the action to start, in YYYY-MM-DDThh:mm:ssZ format in UTC/GMT only and in quotes (for example, @"2019-06-01T00:00:00Z"@ ).
--
-- If you specify @Recurrence@ and @StartTime@ , Amazon EC2 Auto Scaling performs the action at this time, and then performs the action based on the specified recurrence.
-- If you try to schedule the action in the past, Amazon EC2 Auto Scaling returns an error message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ScheduledUpdateGroupActionRequest' value with any optional fields omitted.
mkScheduledUpdateGroupActionRequest
    :: Types.XmlStringMaxLen255 -- ^ 'scheduledActionName'
    -> ScheduledUpdateGroupActionRequest
mkScheduledUpdateGroupActionRequest scheduledActionName
  = ScheduledUpdateGroupActionRequest'{scheduledActionName,
                                       desiredCapacity = Core.Nothing, endTime = Core.Nothing,
                                       maxSize = Core.Nothing, minSize = Core.Nothing,
                                       recurrence = Core.Nothing, startTime = Core.Nothing}

-- | The name of the scaling action.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugarScheduledActionName :: Lens.Lens' ScheduledUpdateGroupActionRequest Types.XmlStringMaxLen255
sugarScheduledActionName = Lens.field @"scheduledActionName"
{-# INLINEABLE sugarScheduledActionName #-}
{-# DEPRECATED scheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead"  #-}

-- | The desired capacity is the initial capacity of the Auto Scaling group after the scheduled action runs and the capacity it attempts to maintain.
--
-- /Note:/ Consider using 'desiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugarDesiredCapacity :: Lens.Lens' ScheduledUpdateGroupActionRequest (Core.Maybe Core.Int)
sugarDesiredCapacity = Lens.field @"desiredCapacity"
{-# INLINEABLE sugarDesiredCapacity #-}
{-# DEPRECATED desiredCapacity "Use generic-lens or generic-optics with 'desiredCapacity' instead"  #-}

-- | The date and time for the recurring schedule to end. Amazon EC2 Auto Scaling does not perform the action after this time.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugarEndTime :: Lens.Lens' ScheduledUpdateGroupActionRequest (Core.Maybe Core.UTCTime)
sugarEndTime = Lens.field @"endTime"
{-# INLINEABLE sugarEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The maximum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugarMaxSize :: Lens.Lens' ScheduledUpdateGroupActionRequest (Core.Maybe Core.Int)
sugarMaxSize = Lens.field @"maxSize"
{-# INLINEABLE sugarMaxSize #-}
{-# DEPRECATED maxSize "Use generic-lens or generic-optics with 'maxSize' instead"  #-}

-- | The minimum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugarMinSize :: Lens.Lens' ScheduledUpdateGroupActionRequest (Core.Maybe Core.Int)
sugarMinSize = Lens.field @"minSize"
{-# INLINEABLE sugarMinSize #-}
{-# DEPRECATED minSize "Use generic-lens or generic-optics with 'minSize' instead"  #-}

-- | The recurring schedule for the action, in Unix cron syntax format. This format consists of five fields separated by white spaces: [Minute] [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be in quotes (for example, @"30 0 1 1,6,12 *"@ ). For more information about this format, see <http://crontab.org Crontab> .
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action starts and stops.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugarRecurrence :: Lens.Lens' ScheduledUpdateGroupActionRequest (Core.Maybe Types.XmlStringMaxLen255)
sugarRecurrence = Lens.field @"recurrence"
{-# INLINEABLE sugarRecurrence #-}
{-# DEPRECATED recurrence "Use generic-lens or generic-optics with 'recurrence' instead"  #-}

-- | The date and time for the action to start, in YYYY-MM-DDThh:mm:ssZ format in UTC/GMT only and in quotes (for example, @"2019-06-01T00:00:00Z"@ ).
--
-- If you specify @Recurrence@ and @StartTime@ , Amazon EC2 Auto Scaling performs the action at this time, and then performs the action based on the specified recurrence.
-- If you try to schedule the action in the past, Amazon EC2 Auto Scaling returns an error message.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugarStartTime :: Lens.Lens' ScheduledUpdateGroupActionRequest (Core.Maybe Core.UTCTime)
sugarStartTime = Lens.field @"startTime"
{-# INLINEABLE sugarStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

instance Core.ToQuery ScheduledUpdateGroupActionRequest where
        toQuery ScheduledUpdateGroupActionRequest{..}
          = Core.toQueryPair "ScheduledActionName" scheduledActionName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DesiredCapacity")
                desiredCapacity
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "EndTime") endTime
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "MaxSize") maxSize
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "MinSize") minSize
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Recurrence") recurrence
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StartTime") startTime
