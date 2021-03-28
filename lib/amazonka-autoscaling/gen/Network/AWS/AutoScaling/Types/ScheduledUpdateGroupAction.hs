{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ScheduledUpdateGroupAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.ScheduledUpdateGroupAction
  ( ScheduledUpdateGroupAction (..)
  -- * Smart constructor
  , mkScheduledUpdateGroupAction
  -- * Lenses
  , sugaAutoScalingGroupName
  , sugaDesiredCapacity
  , sugaEndTime
  , sugaMaxSize
  , sugaMinSize
  , sugaRecurrence
  , sugaScheduledActionARN
  , sugaScheduledActionName
  , sugaStartTime
  , sugaTime
  ) where

import qualified Network.AWS.AutoScaling.Types.ResourceName as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a scheduled scaling action.
--
-- /See:/ 'mkScheduledUpdateGroupAction' smart constructor.
data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction'
  { autoScalingGroupName :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The name of the Auto Scaling group.
  , desiredCapacity :: Core.Maybe Core.Int
    -- ^ The desired capacity is the initial capacity of the Auto Scaling group after the scheduled action runs and the capacity it attempts to maintain.
  , endTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time in UTC for the recurring schedule to end. For example, @"2019-06-01T00:00:00Z"@ . 
  , maxSize :: Core.Maybe Core.Int
    -- ^ The maximum size of the Auto Scaling group.
  , minSize :: Core.Maybe Core.Int
    -- ^ The minimum size of the Auto Scaling group.
  , recurrence :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The recurring schedule for the action, in Unix cron syntax format.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action starts and stops.
  , scheduledActionARN :: Core.Maybe Types.ResourceName
    -- ^ The Amazon Resource Name (ARN) of the scheduled action.
  , scheduledActionName :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The name of the scheduled action.
  , startTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time in UTC for this action to start. For example, @"2019-06-01T00:00:00Z"@ . 
  , time :: Core.Maybe Core.UTCTime
    -- ^ This parameter is no longer used.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ScheduledUpdateGroupAction' value with any optional fields omitted.
mkScheduledUpdateGroupAction
    :: ScheduledUpdateGroupAction
mkScheduledUpdateGroupAction
  = ScheduledUpdateGroupAction'{autoScalingGroupName = Core.Nothing,
                                desiredCapacity = Core.Nothing, endTime = Core.Nothing,
                                maxSize = Core.Nothing, minSize = Core.Nothing,
                                recurrence = Core.Nothing, scheduledActionARN = Core.Nothing,
                                scheduledActionName = Core.Nothing, startTime = Core.Nothing,
                                time = Core.Nothing}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaAutoScalingGroupName :: Lens.Lens' ScheduledUpdateGroupAction (Core.Maybe Types.XmlStringMaxLen255)
sugaAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE sugaAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The desired capacity is the initial capacity of the Auto Scaling group after the scheduled action runs and the capacity it attempts to maintain.
--
-- /Note:/ Consider using 'desiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaDesiredCapacity :: Lens.Lens' ScheduledUpdateGroupAction (Core.Maybe Core.Int)
sugaDesiredCapacity = Lens.field @"desiredCapacity"
{-# INLINEABLE sugaDesiredCapacity #-}
{-# DEPRECATED desiredCapacity "Use generic-lens or generic-optics with 'desiredCapacity' instead"  #-}

-- | The date and time in UTC for the recurring schedule to end. For example, @"2019-06-01T00:00:00Z"@ . 
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaEndTime :: Lens.Lens' ScheduledUpdateGroupAction (Core.Maybe Core.UTCTime)
sugaEndTime = Lens.field @"endTime"
{-# INLINEABLE sugaEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The maximum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaMaxSize :: Lens.Lens' ScheduledUpdateGroupAction (Core.Maybe Core.Int)
sugaMaxSize = Lens.field @"maxSize"
{-# INLINEABLE sugaMaxSize #-}
{-# DEPRECATED maxSize "Use generic-lens or generic-optics with 'maxSize' instead"  #-}

-- | The minimum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaMinSize :: Lens.Lens' ScheduledUpdateGroupAction (Core.Maybe Core.Int)
sugaMinSize = Lens.field @"minSize"
{-# INLINEABLE sugaMinSize #-}
{-# DEPRECATED minSize "Use generic-lens or generic-optics with 'minSize' instead"  #-}

-- | The recurring schedule for the action, in Unix cron syntax format.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action starts and stops.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaRecurrence :: Lens.Lens' ScheduledUpdateGroupAction (Core.Maybe Types.XmlStringMaxLen255)
sugaRecurrence = Lens.field @"recurrence"
{-# INLINEABLE sugaRecurrence #-}
{-# DEPRECATED recurrence "Use generic-lens or generic-optics with 'recurrence' instead"  #-}

-- | The Amazon Resource Name (ARN) of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaScheduledActionARN :: Lens.Lens' ScheduledUpdateGroupAction (Core.Maybe Types.ResourceName)
sugaScheduledActionARN = Lens.field @"scheduledActionARN"
{-# INLINEABLE sugaScheduledActionARN #-}
{-# DEPRECATED scheduledActionARN "Use generic-lens or generic-optics with 'scheduledActionARN' instead"  #-}

-- | The name of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaScheduledActionName :: Lens.Lens' ScheduledUpdateGroupAction (Core.Maybe Types.XmlStringMaxLen255)
sugaScheduledActionName = Lens.field @"scheduledActionName"
{-# INLINEABLE sugaScheduledActionName #-}
{-# DEPRECATED scheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead"  #-}

-- | The date and time in UTC for this action to start. For example, @"2019-06-01T00:00:00Z"@ . 
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaStartTime :: Lens.Lens' ScheduledUpdateGroupAction (Core.Maybe Core.UTCTime)
sugaStartTime = Lens.field @"startTime"
{-# INLINEABLE sugaStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | This parameter is no longer used.
--
-- /Note:/ Consider using 'time' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sugaTime :: Lens.Lens' ScheduledUpdateGroupAction (Core.Maybe Core.UTCTime)
sugaTime = Lens.field @"time"
{-# INLINEABLE sugaTime #-}
{-# DEPRECATED time "Use generic-lens or generic-optics with 'time' instead"  #-}

instance Core.FromXML ScheduledUpdateGroupAction where
        parseXML x
          = ScheduledUpdateGroupAction' Core.<$>
              (x Core..@? "AutoScalingGroupName") Core.<*>
                x Core..@? "DesiredCapacity"
                Core.<*> x Core..@? "EndTime"
                Core.<*> x Core..@? "MaxSize"
                Core.<*> x Core..@? "MinSize"
                Core.<*> x Core..@? "Recurrence"
                Core.<*> x Core..@? "ScheduledActionARN"
                Core.<*> x Core..@? "ScheduledActionName"
                Core.<*> x Core..@? "StartTime"
                Core.<*> x Core..@? "Time"
