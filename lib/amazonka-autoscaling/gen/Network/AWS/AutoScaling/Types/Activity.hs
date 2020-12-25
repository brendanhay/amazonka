{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Activity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.Activity
  ( Activity (..),

    -- * Smart constructor
    mkActivity,

    -- * Lenses
    aActivityId,
    aAutoScalingGroupName,
    aCause,
    aStartTime,
    aStatusCode,
    aDescription,
    aDetails,
    aEndTime,
    aProgress,
    aStatusMessage,
  )
where

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
  { -- | The ID of the activity.
    activityId :: Types.XmlString,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.XmlStringMaxLen255,
    -- | The reason the activity began.
    cause :: Types.XmlStringMaxLen1023,
    -- | The start time of the activity.
    startTime :: Core.UTCTime,
    -- | The current status of the activity.
    statusCode :: Types.ScalingActivityStatusCode,
    -- | A friendly, more verbose description of the activity.
    description :: Core.Maybe Types.XmlString,
    -- | The details about the activity.
    details :: Core.Maybe Types.XmlString,
    -- | The end time of the activity.
    endTime :: Core.Maybe Core.UTCTime,
    -- | A value between 0 and 100 that indicates the progress of the activity.
    progress :: Core.Maybe Core.Int,
    -- | A friendly, more verbose description of the activity status.
    statusMessage :: Core.Maybe Types.XmlStringMaxLen255
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Activity' value with any optional fields omitted.
mkActivity ::
  -- | 'activityId'
  Types.XmlString ->
  -- | 'autoScalingGroupName'
  Types.XmlStringMaxLen255 ->
  -- | 'cause'
  Types.XmlStringMaxLen1023 ->
  -- | 'startTime'
  Core.UTCTime ->
  -- | 'statusCode'
  Types.ScalingActivityStatusCode ->
  Activity
mkActivity
  activityId
  autoScalingGroupName
  cause
  startTime
  statusCode =
    Activity'
      { activityId,
        autoScalingGroupName,
        cause,
        startTime,
        statusCode,
        description = Core.Nothing,
        details = Core.Nothing,
        endTime = Core.Nothing,
        progress = Core.Nothing,
        statusMessage = Core.Nothing
      }

-- | The ID of the activity.
--
-- /Note:/ Consider using 'activityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aActivityId :: Lens.Lens' Activity Types.XmlString
aActivityId = Lens.field @"activityId"
{-# DEPRECATED aActivityId "Use generic-lens or generic-optics with 'activityId' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAutoScalingGroupName :: Lens.Lens' Activity Types.XmlStringMaxLen255
aAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED aAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The reason the activity began.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCause :: Lens.Lens' Activity Types.XmlStringMaxLen1023
aCause = Lens.field @"cause"
{-# DEPRECATED aCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The start time of the activity.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStartTime :: Lens.Lens' Activity Core.UTCTime
aStartTime = Lens.field @"startTime"
{-# DEPRECATED aStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The current status of the activity.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStatusCode :: Lens.Lens' Activity Types.ScalingActivityStatusCode
aStatusCode = Lens.field @"statusCode"
{-# DEPRECATED aStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | A friendly, more verbose description of the activity.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDescription :: Lens.Lens' Activity (Core.Maybe Types.XmlString)
aDescription = Lens.field @"description"
{-# DEPRECATED aDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The details about the activity.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDetails :: Lens.Lens' Activity (Core.Maybe Types.XmlString)
aDetails = Lens.field @"details"
{-# DEPRECATED aDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The end time of the activity.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEndTime :: Lens.Lens' Activity (Core.Maybe Core.UTCTime)
aEndTime = Lens.field @"endTime"
{-# DEPRECATED aEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | A value between 0 and 100 that indicates the progress of the activity.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aProgress :: Lens.Lens' Activity (Core.Maybe Core.Int)
aProgress = Lens.field @"progress"
{-# DEPRECATED aProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | A friendly, more verbose description of the activity status.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStatusMessage :: Lens.Lens' Activity (Core.Maybe Types.XmlStringMaxLen255)
aStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED aStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

instance Core.FromXML Activity where
  parseXML x =
    Activity'
      Core.<$> (x Core..@ "ActivityId")
      Core.<*> (x Core..@ "AutoScalingGroupName")
      Core.<*> (x Core..@ "Cause")
      Core.<*> (x Core..@ "StartTime")
      Core.<*> (x Core..@ "StatusCode")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "Details")
      Core.<*> (x Core..@? "EndTime")
      Core.<*> (x Core..@? "Progress")
      Core.<*> (x Core..@? "StatusMessage")
