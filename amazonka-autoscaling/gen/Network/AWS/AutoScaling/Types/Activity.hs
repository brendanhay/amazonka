{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Activity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.Activity where

import Network.AWS.AutoScaling.Types.ScalingActivityStatusCode
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes scaling activity, which is a long-running process that
-- represents a change to your Auto Scaling group, such as changing its
-- size or replacing an instance.
--
-- /See:/ 'newActivity' smart constructor.
data Activity = Activity'
  { -- | A friendly, more verbose description of the activity status.
    statusMessage :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the Auto Scaling group.
    autoScalingGroupARN :: Core.Maybe Core.Text,
    -- | The details about the activity.
    details :: Core.Maybe Core.Text,
    -- | The end time of the activity.
    endTime :: Core.Maybe Core.ISO8601,
    -- | The state of the Auto Scaling group, which is either @InService@ or
    -- @Deleted@.
    autoScalingGroupState :: Core.Maybe Core.Text,
    -- | A friendly, more verbose description of the activity.
    description :: Core.Maybe Core.Text,
    -- | A value between 0 and 100 that indicates the progress of the activity.
    progress :: Core.Maybe Core.Int,
    -- | The ID of the activity.
    activityId :: Core.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Text,
    -- | The reason the activity began.
    cause :: Core.Text,
    -- | The start time of the activity.
    startTime :: Core.ISO8601,
    -- | The current status of the activity.
    statusCode :: ScalingActivityStatusCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Activity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'activity_statusMessage' - A friendly, more verbose description of the activity status.
--
-- 'autoScalingGroupARN', 'activity_autoScalingGroupARN' - The Amazon Resource Name (ARN) of the Auto Scaling group.
--
-- 'details', 'activity_details' - The details about the activity.
--
-- 'endTime', 'activity_endTime' - The end time of the activity.
--
-- 'autoScalingGroupState', 'activity_autoScalingGroupState' - The state of the Auto Scaling group, which is either @InService@ or
-- @Deleted@.
--
-- 'description', 'activity_description' - A friendly, more verbose description of the activity.
--
-- 'progress', 'activity_progress' - A value between 0 and 100 that indicates the progress of the activity.
--
-- 'activityId', 'activity_activityId' - The ID of the activity.
--
-- 'autoScalingGroupName', 'activity_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'cause', 'activity_cause' - The reason the activity began.
--
-- 'startTime', 'activity_startTime' - The start time of the activity.
--
-- 'statusCode', 'activity_statusCode' - The current status of the activity.
newActivity ::
  -- | 'activityId'
  Core.Text ->
  -- | 'autoScalingGroupName'
  Core.Text ->
  -- | 'cause'
  Core.Text ->
  -- | 'startTime'
  Core.UTCTime ->
  -- | 'statusCode'
  ScalingActivityStatusCode ->
  Activity
newActivity
  pActivityId_
  pAutoScalingGroupName_
  pCause_
  pStartTime_
  pStatusCode_ =
    Activity'
      { statusMessage = Core.Nothing,
        autoScalingGroupARN = Core.Nothing,
        details = Core.Nothing,
        endTime = Core.Nothing,
        autoScalingGroupState = Core.Nothing,
        description = Core.Nothing,
        progress = Core.Nothing,
        activityId = pActivityId_,
        autoScalingGroupName = pAutoScalingGroupName_,
        cause = pCause_,
        startTime = Core._Time Lens.# pStartTime_,
        statusCode = pStatusCode_
      }

-- | A friendly, more verbose description of the activity status.
activity_statusMessage :: Lens.Lens' Activity (Core.Maybe Core.Text)
activity_statusMessage = Lens.lens (\Activity' {statusMessage} -> statusMessage) (\s@Activity' {} a -> s {statusMessage = a} :: Activity)

-- | The Amazon Resource Name (ARN) of the Auto Scaling group.
activity_autoScalingGroupARN :: Lens.Lens' Activity (Core.Maybe Core.Text)
activity_autoScalingGroupARN = Lens.lens (\Activity' {autoScalingGroupARN} -> autoScalingGroupARN) (\s@Activity' {} a -> s {autoScalingGroupARN = a} :: Activity)

-- | The details about the activity.
activity_details :: Lens.Lens' Activity (Core.Maybe Core.Text)
activity_details = Lens.lens (\Activity' {details} -> details) (\s@Activity' {} a -> s {details = a} :: Activity)

-- | The end time of the activity.
activity_endTime :: Lens.Lens' Activity (Core.Maybe Core.UTCTime)
activity_endTime = Lens.lens (\Activity' {endTime} -> endTime) (\s@Activity' {} a -> s {endTime = a} :: Activity) Core.. Lens.mapping Core._Time

-- | The state of the Auto Scaling group, which is either @InService@ or
-- @Deleted@.
activity_autoScalingGroupState :: Lens.Lens' Activity (Core.Maybe Core.Text)
activity_autoScalingGroupState = Lens.lens (\Activity' {autoScalingGroupState} -> autoScalingGroupState) (\s@Activity' {} a -> s {autoScalingGroupState = a} :: Activity)

-- | A friendly, more verbose description of the activity.
activity_description :: Lens.Lens' Activity (Core.Maybe Core.Text)
activity_description = Lens.lens (\Activity' {description} -> description) (\s@Activity' {} a -> s {description = a} :: Activity)

-- | A value between 0 and 100 that indicates the progress of the activity.
activity_progress :: Lens.Lens' Activity (Core.Maybe Core.Int)
activity_progress = Lens.lens (\Activity' {progress} -> progress) (\s@Activity' {} a -> s {progress = a} :: Activity)

-- | The ID of the activity.
activity_activityId :: Lens.Lens' Activity Core.Text
activity_activityId = Lens.lens (\Activity' {activityId} -> activityId) (\s@Activity' {} a -> s {activityId = a} :: Activity)

-- | The name of the Auto Scaling group.
activity_autoScalingGroupName :: Lens.Lens' Activity Core.Text
activity_autoScalingGroupName = Lens.lens (\Activity' {autoScalingGroupName} -> autoScalingGroupName) (\s@Activity' {} a -> s {autoScalingGroupName = a} :: Activity)

-- | The reason the activity began.
activity_cause :: Lens.Lens' Activity Core.Text
activity_cause = Lens.lens (\Activity' {cause} -> cause) (\s@Activity' {} a -> s {cause = a} :: Activity)

-- | The start time of the activity.
activity_startTime :: Lens.Lens' Activity Core.UTCTime
activity_startTime = Lens.lens (\Activity' {startTime} -> startTime) (\s@Activity' {} a -> s {startTime = a} :: Activity) Core.. Core._Time

-- | The current status of the activity.
activity_statusCode :: Lens.Lens' Activity ScalingActivityStatusCode
activity_statusCode = Lens.lens (\Activity' {statusCode} -> statusCode) (\s@Activity' {} a -> s {statusCode = a} :: Activity)

instance Core.FromXML Activity where
  parseXML x =
    Activity'
      Core.<$> (x Core..@? "StatusMessage")
      Core.<*> (x Core..@? "AutoScalingGroupARN")
      Core.<*> (x Core..@? "Details")
      Core.<*> (x Core..@? "EndTime")
      Core.<*> (x Core..@? "AutoScalingGroupState")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "Progress")
      Core.<*> (x Core..@ "ActivityId")
      Core.<*> (x Core..@ "AutoScalingGroupName")
      Core.<*> (x Core..@ "Cause")
      Core.<*> (x Core..@ "StartTime")
      Core.<*> (x Core..@ "StatusCode")

instance Core.Hashable Activity

instance Core.NFData Activity
