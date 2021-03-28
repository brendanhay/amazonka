{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Activity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.Activity
  ( Activity (..)
  -- * Smart constructor
  , mkActivity
  -- * Lenses
  , aActivityId
  , aAutoScalingGroupName
  , aCause
  , aStartTime
  , aStatusCode
  , aDescription
  , aDetails
  , aEndTime
  , aProgress
  , aStatusMessage
  ) where

import qualified Network.AWS.AutoScaling.Types.ScalingActivityStatusCode as Types
import qualified Network.AWS.AutoScaling.Types.XmlString as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen1023 as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes scaling activity, which is a long-running process that represents a change to your Auto Scaling group, such as changing its size or replacing an instance.
--
-- /See:/ 'mkActivity' smart constructor.
data Activity = Activity'
  { activityId :: Types.XmlString
    -- ^ The ID of the activity.
  , autoScalingGroupName :: Types.XmlStringMaxLen255
    -- ^ The name of the Auto Scaling group.
  , cause :: Types.XmlStringMaxLen1023
    -- ^ The reason the activity began.
  , startTime :: Core.UTCTime
    -- ^ The start time of the activity.
  , statusCode :: Types.ScalingActivityStatusCode
    -- ^ The current status of the activity.
  , description :: Core.Maybe Types.XmlString
    -- ^ A friendly, more verbose description of the activity.
  , details :: Core.Maybe Types.XmlString
    -- ^ The details about the activity.
  , endTime :: Core.Maybe Core.UTCTime
    -- ^ The end time of the activity.
  , progress :: Core.Maybe Core.Int
    -- ^ A value between 0 and 100 that indicates the progress of the activity.
  , statusMessage :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ A friendly, more verbose description of the activity status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Activity' value with any optional fields omitted.
mkActivity
    :: Types.XmlString -- ^ 'activityId'
    -> Types.XmlStringMaxLen255 -- ^ 'autoScalingGroupName'
    -> Types.XmlStringMaxLen1023 -- ^ 'cause'
    -> Core.UTCTime -- ^ 'startTime'
    -> Types.ScalingActivityStatusCode -- ^ 'statusCode'
    -> Activity
mkActivity activityId autoScalingGroupName cause startTime
  statusCode
  = Activity'{activityId, autoScalingGroupName, cause, startTime,
              statusCode, description = Core.Nothing, details = Core.Nothing,
              endTime = Core.Nothing, progress = Core.Nothing,
              statusMessage = Core.Nothing}

-- | The ID of the activity.
--
-- /Note:/ Consider using 'activityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aActivityId :: Lens.Lens' Activity Types.XmlString
aActivityId = Lens.field @"activityId"
{-# INLINEABLE aActivityId #-}
{-# DEPRECATED activityId "Use generic-lens or generic-optics with 'activityId' instead"  #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAutoScalingGroupName :: Lens.Lens' Activity Types.XmlStringMaxLen255
aAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE aAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The reason the activity began.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCause :: Lens.Lens' Activity Types.XmlStringMaxLen1023
aCause = Lens.field @"cause"
{-# INLINEABLE aCause #-}
{-# DEPRECATED cause "Use generic-lens or generic-optics with 'cause' instead"  #-}

-- | The start time of the activity.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStartTime :: Lens.Lens' Activity Core.UTCTime
aStartTime = Lens.field @"startTime"
{-# INLINEABLE aStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The current status of the activity.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStatusCode :: Lens.Lens' Activity Types.ScalingActivityStatusCode
aStatusCode = Lens.field @"statusCode"
{-# INLINEABLE aStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

-- | A friendly, more verbose description of the activity.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDescription :: Lens.Lens' Activity (Core.Maybe Types.XmlString)
aDescription = Lens.field @"description"
{-# INLINEABLE aDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The details about the activity.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDetails :: Lens.Lens' Activity (Core.Maybe Types.XmlString)
aDetails = Lens.field @"details"
{-# INLINEABLE aDetails #-}
{-# DEPRECATED details "Use generic-lens or generic-optics with 'details' instead"  #-}

-- | The end time of the activity.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEndTime :: Lens.Lens' Activity (Core.Maybe Core.UTCTime)
aEndTime = Lens.field @"endTime"
{-# INLINEABLE aEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | A value between 0 and 100 that indicates the progress of the activity.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aProgress :: Lens.Lens' Activity (Core.Maybe Core.Int)
aProgress = Lens.field @"progress"
{-# INLINEABLE aProgress #-}
{-# DEPRECATED progress "Use generic-lens or generic-optics with 'progress' instead"  #-}

-- | A friendly, more verbose description of the activity status.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStatusMessage :: Lens.Lens' Activity (Core.Maybe Types.XmlStringMaxLen255)
aStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE aStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

instance Core.FromXML Activity where
        parseXML x
          = Activity' Core.<$>
              (x Core..@ "ActivityId") Core.<*> x Core..@ "AutoScalingGroupName"
                Core.<*> x Core..@ "Cause"
                Core.<*> x Core..@ "StartTime"
                Core.<*> x Core..@ "StatusCode"
                Core.<*> x Core..@? "Description"
                Core.<*> x Core..@? "Details"
                Core.<*> x Core..@? "EndTime"
                Core.<*> x Core..@? "Progress"
                Core.<*> x Core..@? "StatusMessage"
