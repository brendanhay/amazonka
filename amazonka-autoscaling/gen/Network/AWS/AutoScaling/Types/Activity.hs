{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes scaling activity, which is a long-running process that
-- represents a change to your Auto Scaling group, such as changing its
-- size or replacing an instance.
--
-- /See:/ 'newActivity' smart constructor.
data Activity = Activity'
  { -- | A friendly, more verbose description of the activity status.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Auto Scaling group.
    autoScalingGroupARN :: Prelude.Maybe Prelude.Text,
    -- | The details about the activity.
    details :: Prelude.Maybe Prelude.Text,
    -- | The end time of the activity.
    endTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The state of the Auto Scaling group, which is either @InService@ or
    -- @Deleted@.
    autoScalingGroupState :: Prelude.Maybe Prelude.Text,
    -- | A friendly, more verbose description of the activity.
    description :: Prelude.Maybe Prelude.Text,
    -- | A value between 0 and 100 that indicates the progress of the activity.
    progress :: Prelude.Maybe Prelude.Int,
    -- | The ID of the activity.
    activityId :: Prelude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The reason the activity began.
    cause :: Prelude.Text,
    -- | The start time of the activity.
    startTime :: Prelude.ISO8601,
    -- | The current status of the activity.
    statusCode :: ScalingActivityStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  -- | 'cause'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
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
      { statusMessage = Prelude.Nothing,
        autoScalingGroupARN = Prelude.Nothing,
        details = Prelude.Nothing,
        endTime = Prelude.Nothing,
        autoScalingGroupState = Prelude.Nothing,
        description = Prelude.Nothing,
        progress = Prelude.Nothing,
        activityId = pActivityId_,
        autoScalingGroupName = pAutoScalingGroupName_,
        cause = pCause_,
        startTime = Prelude._Time Lens.# pStartTime_,
        statusCode = pStatusCode_
      }

-- | A friendly, more verbose description of the activity status.
activity_statusMessage :: Lens.Lens' Activity (Prelude.Maybe Prelude.Text)
activity_statusMessage = Lens.lens (\Activity' {statusMessage} -> statusMessage) (\s@Activity' {} a -> s {statusMessage = a} :: Activity)

-- | The Amazon Resource Name (ARN) of the Auto Scaling group.
activity_autoScalingGroupARN :: Lens.Lens' Activity (Prelude.Maybe Prelude.Text)
activity_autoScalingGroupARN = Lens.lens (\Activity' {autoScalingGroupARN} -> autoScalingGroupARN) (\s@Activity' {} a -> s {autoScalingGroupARN = a} :: Activity)

-- | The details about the activity.
activity_details :: Lens.Lens' Activity (Prelude.Maybe Prelude.Text)
activity_details = Lens.lens (\Activity' {details} -> details) (\s@Activity' {} a -> s {details = a} :: Activity)

-- | The end time of the activity.
activity_endTime :: Lens.Lens' Activity (Prelude.Maybe Prelude.UTCTime)
activity_endTime = Lens.lens (\Activity' {endTime} -> endTime) (\s@Activity' {} a -> s {endTime = a} :: Activity) Prelude.. Lens.mapping Prelude._Time

-- | The state of the Auto Scaling group, which is either @InService@ or
-- @Deleted@.
activity_autoScalingGroupState :: Lens.Lens' Activity (Prelude.Maybe Prelude.Text)
activity_autoScalingGroupState = Lens.lens (\Activity' {autoScalingGroupState} -> autoScalingGroupState) (\s@Activity' {} a -> s {autoScalingGroupState = a} :: Activity)

-- | A friendly, more verbose description of the activity.
activity_description :: Lens.Lens' Activity (Prelude.Maybe Prelude.Text)
activity_description = Lens.lens (\Activity' {description} -> description) (\s@Activity' {} a -> s {description = a} :: Activity)

-- | A value between 0 and 100 that indicates the progress of the activity.
activity_progress :: Lens.Lens' Activity (Prelude.Maybe Prelude.Int)
activity_progress = Lens.lens (\Activity' {progress} -> progress) (\s@Activity' {} a -> s {progress = a} :: Activity)

-- | The ID of the activity.
activity_activityId :: Lens.Lens' Activity Prelude.Text
activity_activityId = Lens.lens (\Activity' {activityId} -> activityId) (\s@Activity' {} a -> s {activityId = a} :: Activity)

-- | The name of the Auto Scaling group.
activity_autoScalingGroupName :: Lens.Lens' Activity Prelude.Text
activity_autoScalingGroupName = Lens.lens (\Activity' {autoScalingGroupName} -> autoScalingGroupName) (\s@Activity' {} a -> s {autoScalingGroupName = a} :: Activity)

-- | The reason the activity began.
activity_cause :: Lens.Lens' Activity Prelude.Text
activity_cause = Lens.lens (\Activity' {cause} -> cause) (\s@Activity' {} a -> s {cause = a} :: Activity)

-- | The start time of the activity.
activity_startTime :: Lens.Lens' Activity Prelude.UTCTime
activity_startTime = Lens.lens (\Activity' {startTime} -> startTime) (\s@Activity' {} a -> s {startTime = a} :: Activity) Prelude.. Prelude._Time

-- | The current status of the activity.
activity_statusCode :: Lens.Lens' Activity ScalingActivityStatusCode
activity_statusCode = Lens.lens (\Activity' {statusCode} -> statusCode) (\s@Activity' {} a -> s {statusCode = a} :: Activity)

instance Prelude.FromXML Activity where
  parseXML x =
    Activity'
      Prelude.<$> (x Prelude..@? "StatusMessage")
      Prelude.<*> (x Prelude..@? "AutoScalingGroupARN")
      Prelude.<*> (x Prelude..@? "Details")
      Prelude.<*> (x Prelude..@? "EndTime")
      Prelude.<*> (x Prelude..@? "AutoScalingGroupState")
      Prelude.<*> (x Prelude..@? "Description")
      Prelude.<*> (x Prelude..@? "Progress")
      Prelude.<*> (x Prelude..@ "ActivityId")
      Prelude.<*> (x Prelude..@ "AutoScalingGroupName")
      Prelude.<*> (x Prelude..@ "Cause")
      Prelude.<*> (x Prelude..@ "StartTime")
      Prelude.<*> (x Prelude..@ "StatusCode")

instance Prelude.Hashable Activity

instance Prelude.NFData Activity
