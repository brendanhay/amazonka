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
-- Module      : Amazonka.AutoScaling.Types.Activity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.Activity where

import Amazonka.AutoScaling.Types.ScalingActivityStatusCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes scaling activity, which is a long-running process that
-- represents a change to your Auto Scaling group, such as changing its
-- size or replacing an instance.
--
-- /See:/ 'newActivity' smart constructor.
data Activity = Activity'
  { -- | The Amazon Resource Name (ARN) of the Auto Scaling group.
    autoScalingGroupARN :: Prelude.Maybe Prelude.Text,
    -- | The state of the Auto Scaling group, which is either @InService@ or
    -- @Deleted@.
    autoScalingGroupState :: Prelude.Maybe Prelude.Text,
    -- | A friendly, more verbose description of the activity.
    description :: Prelude.Maybe Prelude.Text,
    -- | The details about the activity.
    details :: Prelude.Maybe Prelude.Text,
    -- | The end time of the activity.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | A value between 0 and 100 that indicates the progress of the activity.
    progress :: Prelude.Maybe Prelude.Int,
    -- | A friendly, more verbose description of the activity status.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the activity.
    activityId :: Prelude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The reason the activity began.
    cause :: Prelude.Text,
    -- | The start time of the activity.
    startTime :: Data.ISO8601,
    -- | The current status of the activity.
    statusCode :: ScalingActivityStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Activity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupARN', 'activity_autoScalingGroupARN' - The Amazon Resource Name (ARN) of the Auto Scaling group.
--
-- 'autoScalingGroupState', 'activity_autoScalingGroupState' - The state of the Auto Scaling group, which is either @InService@ or
-- @Deleted@.
--
-- 'description', 'activity_description' - A friendly, more verbose description of the activity.
--
-- 'details', 'activity_details' - The details about the activity.
--
-- 'endTime', 'activity_endTime' - The end time of the activity.
--
-- 'progress', 'activity_progress' - A value between 0 and 100 that indicates the progress of the activity.
--
-- 'statusMessage', 'activity_statusMessage' - A friendly, more verbose description of the activity status.
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
      { autoScalingGroupARN = Prelude.Nothing,
        autoScalingGroupState = Prelude.Nothing,
        description = Prelude.Nothing,
        details = Prelude.Nothing,
        endTime = Prelude.Nothing,
        progress = Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        activityId = pActivityId_,
        autoScalingGroupName = pAutoScalingGroupName_,
        cause = pCause_,
        startTime = Data._Time Lens.# pStartTime_,
        statusCode = pStatusCode_
      }

-- | The Amazon Resource Name (ARN) of the Auto Scaling group.
activity_autoScalingGroupARN :: Lens.Lens' Activity (Prelude.Maybe Prelude.Text)
activity_autoScalingGroupARN = Lens.lens (\Activity' {autoScalingGroupARN} -> autoScalingGroupARN) (\s@Activity' {} a -> s {autoScalingGroupARN = a} :: Activity)

-- | The state of the Auto Scaling group, which is either @InService@ or
-- @Deleted@.
activity_autoScalingGroupState :: Lens.Lens' Activity (Prelude.Maybe Prelude.Text)
activity_autoScalingGroupState = Lens.lens (\Activity' {autoScalingGroupState} -> autoScalingGroupState) (\s@Activity' {} a -> s {autoScalingGroupState = a} :: Activity)

-- | A friendly, more verbose description of the activity.
activity_description :: Lens.Lens' Activity (Prelude.Maybe Prelude.Text)
activity_description = Lens.lens (\Activity' {description} -> description) (\s@Activity' {} a -> s {description = a} :: Activity)

-- | The details about the activity.
activity_details :: Lens.Lens' Activity (Prelude.Maybe Prelude.Text)
activity_details = Lens.lens (\Activity' {details} -> details) (\s@Activity' {} a -> s {details = a} :: Activity)

-- | The end time of the activity.
activity_endTime :: Lens.Lens' Activity (Prelude.Maybe Prelude.UTCTime)
activity_endTime = Lens.lens (\Activity' {endTime} -> endTime) (\s@Activity' {} a -> s {endTime = a} :: Activity) Prelude.. Lens.mapping Data._Time

-- | A value between 0 and 100 that indicates the progress of the activity.
activity_progress :: Lens.Lens' Activity (Prelude.Maybe Prelude.Int)
activity_progress = Lens.lens (\Activity' {progress} -> progress) (\s@Activity' {} a -> s {progress = a} :: Activity)

-- | A friendly, more verbose description of the activity status.
activity_statusMessage :: Lens.Lens' Activity (Prelude.Maybe Prelude.Text)
activity_statusMessage = Lens.lens (\Activity' {statusMessage} -> statusMessage) (\s@Activity' {} a -> s {statusMessage = a} :: Activity)

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
activity_startTime = Lens.lens (\Activity' {startTime} -> startTime) (\s@Activity' {} a -> s {startTime = a} :: Activity) Prelude.. Data._Time

-- | The current status of the activity.
activity_statusCode :: Lens.Lens' Activity ScalingActivityStatusCode
activity_statusCode = Lens.lens (\Activity' {statusCode} -> statusCode) (\s@Activity' {} a -> s {statusCode = a} :: Activity)

instance Data.FromXML Activity where
  parseXML x =
    Activity'
      Prelude.<$> (x Data..@? "AutoScalingGroupARN")
      Prelude.<*> (x Data..@? "AutoScalingGroupState")
      Prelude.<*> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "Details")
      Prelude.<*> (x Data..@? "EndTime")
      Prelude.<*> (x Data..@? "Progress")
      Prelude.<*> (x Data..@? "StatusMessage")
      Prelude.<*> (x Data..@ "ActivityId")
      Prelude.<*> (x Data..@ "AutoScalingGroupName")
      Prelude.<*> (x Data..@ "Cause")
      Prelude.<*> (x Data..@ "StartTime")
      Prelude.<*> (x Data..@ "StatusCode")

instance Prelude.Hashable Activity where
  hashWithSalt _salt Activity' {..} =
    _salt
      `Prelude.hashWithSalt` autoScalingGroupARN
      `Prelude.hashWithSalt` autoScalingGroupState
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` progress
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` activityId
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` cause
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData Activity where
  rnf Activity' {..} =
    Prelude.rnf autoScalingGroupARN `Prelude.seq`
      Prelude.rnf autoScalingGroupState `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf details `Prelude.seq`
            Prelude.rnf endTime `Prelude.seq`
              Prelude.rnf progress `Prelude.seq`
                Prelude.rnf statusMessage `Prelude.seq`
                  Prelude.rnf activityId `Prelude.seq`
                    Prelude.rnf autoScalingGroupName `Prelude.seq`
                      Prelude.rnf cause `Prelude.seq`
                        Prelude.rnf startTime `Prelude.seq`
                          Prelude.rnf statusCode
