{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AutoScaling.PutScheduledUpdateGroupAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scheduled scaling action for an Auto Scaling group.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/schedule_time.html Scheduled scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- You can view the scheduled actions for an Auto Scaling group using the
-- DescribeScheduledActions API call. If you are no longer using a
-- scheduled action, you can delete it by calling the DeleteScheduledAction
-- API.
module Amazonka.AutoScaling.PutScheduledUpdateGroupAction
  ( -- * Creating a Request
    PutScheduledUpdateGroupAction (..),
    newPutScheduledUpdateGroupAction,

    -- * Request Lenses
    putScheduledUpdateGroupAction_startTime,
    putScheduledUpdateGroupAction_time,
    putScheduledUpdateGroupAction_maxSize,
    putScheduledUpdateGroupAction_recurrence,
    putScheduledUpdateGroupAction_desiredCapacity,
    putScheduledUpdateGroupAction_minSize,
    putScheduledUpdateGroupAction_endTime,
    putScheduledUpdateGroupAction_timeZone,
    putScheduledUpdateGroupAction_autoScalingGroupName,
    putScheduledUpdateGroupAction_scheduledActionName,

    -- * Destructuring the Response
    PutScheduledUpdateGroupActionResponse (..),
    newPutScheduledUpdateGroupActionResponse,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutScheduledUpdateGroupAction' smart constructor.
data PutScheduledUpdateGroupAction = PutScheduledUpdateGroupAction'
  { -- | The date and time for this action to start, in YYYY-MM-DDThh:mm:ssZ
    -- format in UTC\/GMT only and in quotes (for example,
    -- @\"2019-06-01T00:00:00Z\"@).
    --
    -- If you specify @Recurrence@ and @StartTime@, Amazon EC2 Auto Scaling
    -- performs the action at this time, and then performs the action based on
    -- the specified recurrence.
    --
    -- If you try to schedule your action in the past, Amazon EC2 Auto Scaling
    -- returns an error message.
    startTime :: Prelude.Maybe Core.ISO8601,
    -- | This parameter is no longer used.
    time :: Prelude.Maybe Core.ISO8601,
    -- | The maximum size of the Auto Scaling group.
    maxSize :: Prelude.Maybe Prelude.Int,
    -- | The recurring schedule for this action. This format consists of five
    -- fields separated by white spaces: [Minute] [Hour] [Day_of_Month]
    -- [Month_of_Year] [Day_of_Week]. The value must be in quotes (for example,
    -- @\"30 0 1 1,6,12 *\"@). For more information about this format, see
    -- <http://crontab.org Crontab>.
    --
    -- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
    -- form the boundaries of when the recurring action starts and stops.
    --
    -- Cron expressions use Universal Coordinated Time (UTC) by default.
    recurrence :: Prelude.Maybe Prelude.Text,
    -- | The desired capacity is the initial capacity of the Auto Scaling group
    -- after the scheduled action runs and the capacity it attempts to
    -- maintain. It can scale beyond this capacity if you add more scaling
    -- conditions.
    desiredCapacity :: Prelude.Maybe Prelude.Int,
    -- | The minimum size of the Auto Scaling group.
    minSize :: Prelude.Maybe Prelude.Int,
    -- | The date and time for the recurring schedule to end, in UTC.
    endTime :: Prelude.Maybe Core.ISO8601,
    -- | Specifies the time zone for a cron expression. If a time zone is not
    -- provided, UTC is used by default.
    --
    -- Valid values are the canonical names of the IANA time zones, derived
    -- from the IANA Time Zone Database (such as @Etc\/GMT+9@ or
    -- @Pacific\/Tahiti@). For more information, see
    -- <https://en.wikipedia.org/wiki/List_of_tz_database_time_zones>.
    timeZone :: Prelude.Maybe Prelude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The name of this scaling action.
    scheduledActionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutScheduledUpdateGroupAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'putScheduledUpdateGroupAction_startTime' - The date and time for this action to start, in YYYY-MM-DDThh:mm:ssZ
-- format in UTC\/GMT only and in quotes (for example,
-- @\"2019-06-01T00:00:00Z\"@).
--
-- If you specify @Recurrence@ and @StartTime@, Amazon EC2 Auto Scaling
-- performs the action at this time, and then performs the action based on
-- the specified recurrence.
--
-- If you try to schedule your action in the past, Amazon EC2 Auto Scaling
-- returns an error message.
--
-- 'time', 'putScheduledUpdateGroupAction_time' - This parameter is no longer used.
--
-- 'maxSize', 'putScheduledUpdateGroupAction_maxSize' - The maximum size of the Auto Scaling group.
--
-- 'recurrence', 'putScheduledUpdateGroupAction_recurrence' - The recurring schedule for this action. This format consists of five
-- fields separated by white spaces: [Minute] [Hour] [Day_of_Month]
-- [Month_of_Year] [Day_of_Week]. The value must be in quotes (for example,
-- @\"30 0 1 1,6,12 *\"@). For more information about this format, see
-- <http://crontab.org Crontab>.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
-- form the boundaries of when the recurring action starts and stops.
--
-- Cron expressions use Universal Coordinated Time (UTC) by default.
--
-- 'desiredCapacity', 'putScheduledUpdateGroupAction_desiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group
-- after the scheduled action runs and the capacity it attempts to
-- maintain. It can scale beyond this capacity if you add more scaling
-- conditions.
--
-- 'minSize', 'putScheduledUpdateGroupAction_minSize' - The minimum size of the Auto Scaling group.
--
-- 'endTime', 'putScheduledUpdateGroupAction_endTime' - The date and time for the recurring schedule to end, in UTC.
--
-- 'timeZone', 'putScheduledUpdateGroupAction_timeZone' - Specifies the time zone for a cron expression. If a time zone is not
-- provided, UTC is used by default.
--
-- Valid values are the canonical names of the IANA time zones, derived
-- from the IANA Time Zone Database (such as @Etc\/GMT+9@ or
-- @Pacific\/Tahiti@). For more information, see
-- <https://en.wikipedia.org/wiki/List_of_tz_database_time_zones>.
--
-- 'autoScalingGroupName', 'putScheduledUpdateGroupAction_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'scheduledActionName', 'putScheduledUpdateGroupAction_scheduledActionName' - The name of this scaling action.
newPutScheduledUpdateGroupAction ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  -- | 'scheduledActionName'
  Prelude.Text ->
  PutScheduledUpdateGroupAction
newPutScheduledUpdateGroupAction
  pAutoScalingGroupName_
  pScheduledActionName_ =
    PutScheduledUpdateGroupAction'
      { startTime =
          Prelude.Nothing,
        time = Prelude.Nothing,
        maxSize = Prelude.Nothing,
        recurrence = Prelude.Nothing,
        desiredCapacity = Prelude.Nothing,
        minSize = Prelude.Nothing,
        endTime = Prelude.Nothing,
        timeZone = Prelude.Nothing,
        autoScalingGroupName =
          pAutoScalingGroupName_,
        scheduledActionName = pScheduledActionName_
      }

-- | The date and time for this action to start, in YYYY-MM-DDThh:mm:ssZ
-- format in UTC\/GMT only and in quotes (for example,
-- @\"2019-06-01T00:00:00Z\"@).
--
-- If you specify @Recurrence@ and @StartTime@, Amazon EC2 Auto Scaling
-- performs the action at this time, and then performs the action based on
-- the specified recurrence.
--
-- If you try to schedule your action in the past, Amazon EC2 Auto Scaling
-- returns an error message.
putScheduledUpdateGroupAction_startTime :: Lens.Lens' PutScheduledUpdateGroupAction (Prelude.Maybe Prelude.UTCTime)
putScheduledUpdateGroupAction_startTime = Lens.lens (\PutScheduledUpdateGroupAction' {startTime} -> startTime) (\s@PutScheduledUpdateGroupAction' {} a -> s {startTime = a} :: PutScheduledUpdateGroupAction) Prelude.. Lens.mapping Core._Time

-- | This parameter is no longer used.
putScheduledUpdateGroupAction_time :: Lens.Lens' PutScheduledUpdateGroupAction (Prelude.Maybe Prelude.UTCTime)
putScheduledUpdateGroupAction_time = Lens.lens (\PutScheduledUpdateGroupAction' {time} -> time) (\s@PutScheduledUpdateGroupAction' {} a -> s {time = a} :: PutScheduledUpdateGroupAction) Prelude.. Lens.mapping Core._Time

-- | The maximum size of the Auto Scaling group.
putScheduledUpdateGroupAction_maxSize :: Lens.Lens' PutScheduledUpdateGroupAction (Prelude.Maybe Prelude.Int)
putScheduledUpdateGroupAction_maxSize = Lens.lens (\PutScheduledUpdateGroupAction' {maxSize} -> maxSize) (\s@PutScheduledUpdateGroupAction' {} a -> s {maxSize = a} :: PutScheduledUpdateGroupAction)

-- | The recurring schedule for this action. This format consists of five
-- fields separated by white spaces: [Minute] [Hour] [Day_of_Month]
-- [Month_of_Year] [Day_of_Week]. The value must be in quotes (for example,
-- @\"30 0 1 1,6,12 *\"@). For more information about this format, see
-- <http://crontab.org Crontab>.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
-- form the boundaries of when the recurring action starts and stops.
--
-- Cron expressions use Universal Coordinated Time (UTC) by default.
putScheduledUpdateGroupAction_recurrence :: Lens.Lens' PutScheduledUpdateGroupAction (Prelude.Maybe Prelude.Text)
putScheduledUpdateGroupAction_recurrence = Lens.lens (\PutScheduledUpdateGroupAction' {recurrence} -> recurrence) (\s@PutScheduledUpdateGroupAction' {} a -> s {recurrence = a} :: PutScheduledUpdateGroupAction)

-- | The desired capacity is the initial capacity of the Auto Scaling group
-- after the scheduled action runs and the capacity it attempts to
-- maintain. It can scale beyond this capacity if you add more scaling
-- conditions.
putScheduledUpdateGroupAction_desiredCapacity :: Lens.Lens' PutScheduledUpdateGroupAction (Prelude.Maybe Prelude.Int)
putScheduledUpdateGroupAction_desiredCapacity = Lens.lens (\PutScheduledUpdateGroupAction' {desiredCapacity} -> desiredCapacity) (\s@PutScheduledUpdateGroupAction' {} a -> s {desiredCapacity = a} :: PutScheduledUpdateGroupAction)

-- | The minimum size of the Auto Scaling group.
putScheduledUpdateGroupAction_minSize :: Lens.Lens' PutScheduledUpdateGroupAction (Prelude.Maybe Prelude.Int)
putScheduledUpdateGroupAction_minSize = Lens.lens (\PutScheduledUpdateGroupAction' {minSize} -> minSize) (\s@PutScheduledUpdateGroupAction' {} a -> s {minSize = a} :: PutScheduledUpdateGroupAction)

-- | The date and time for the recurring schedule to end, in UTC.
putScheduledUpdateGroupAction_endTime :: Lens.Lens' PutScheduledUpdateGroupAction (Prelude.Maybe Prelude.UTCTime)
putScheduledUpdateGroupAction_endTime = Lens.lens (\PutScheduledUpdateGroupAction' {endTime} -> endTime) (\s@PutScheduledUpdateGroupAction' {} a -> s {endTime = a} :: PutScheduledUpdateGroupAction) Prelude.. Lens.mapping Core._Time

-- | Specifies the time zone for a cron expression. If a time zone is not
-- provided, UTC is used by default.
--
-- Valid values are the canonical names of the IANA time zones, derived
-- from the IANA Time Zone Database (such as @Etc\/GMT+9@ or
-- @Pacific\/Tahiti@). For more information, see
-- <https://en.wikipedia.org/wiki/List_of_tz_database_time_zones>.
putScheduledUpdateGroupAction_timeZone :: Lens.Lens' PutScheduledUpdateGroupAction (Prelude.Maybe Prelude.Text)
putScheduledUpdateGroupAction_timeZone = Lens.lens (\PutScheduledUpdateGroupAction' {timeZone} -> timeZone) (\s@PutScheduledUpdateGroupAction' {} a -> s {timeZone = a} :: PutScheduledUpdateGroupAction)

-- | The name of the Auto Scaling group.
putScheduledUpdateGroupAction_autoScalingGroupName :: Lens.Lens' PutScheduledUpdateGroupAction Prelude.Text
putScheduledUpdateGroupAction_autoScalingGroupName = Lens.lens (\PutScheduledUpdateGroupAction' {autoScalingGroupName} -> autoScalingGroupName) (\s@PutScheduledUpdateGroupAction' {} a -> s {autoScalingGroupName = a} :: PutScheduledUpdateGroupAction)

-- | The name of this scaling action.
putScheduledUpdateGroupAction_scheduledActionName :: Lens.Lens' PutScheduledUpdateGroupAction Prelude.Text
putScheduledUpdateGroupAction_scheduledActionName = Lens.lens (\PutScheduledUpdateGroupAction' {scheduledActionName} -> scheduledActionName) (\s@PutScheduledUpdateGroupAction' {} a -> s {scheduledActionName = a} :: PutScheduledUpdateGroupAction)

instance
  Core.AWSRequest
    PutScheduledUpdateGroupAction
  where
  type
    AWSResponse PutScheduledUpdateGroupAction =
      PutScheduledUpdateGroupActionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      PutScheduledUpdateGroupActionResponse'

instance
  Prelude.Hashable
    PutScheduledUpdateGroupAction
  where
  hashWithSalt salt' PutScheduledUpdateGroupAction' {..} =
    salt' `Prelude.hashWithSalt` scheduledActionName
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` timeZone
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` minSize
      `Prelude.hashWithSalt` desiredCapacity
      `Prelude.hashWithSalt` recurrence
      `Prelude.hashWithSalt` maxSize
      `Prelude.hashWithSalt` time
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData PutScheduledUpdateGroupAction where
  rnf PutScheduledUpdateGroupAction' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf scheduledActionName
      `Prelude.seq` Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf timeZone
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf minSize
      `Prelude.seq` Prelude.rnf desiredCapacity
      `Prelude.seq` Prelude.rnf recurrence
      `Prelude.seq` Prelude.rnf maxSize
      `Prelude.seq` Prelude.rnf time

instance Core.ToHeaders PutScheduledUpdateGroupAction where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath PutScheduledUpdateGroupAction where
  toPath = Prelude.const "/"

instance Core.ToQuery PutScheduledUpdateGroupAction where
  toQuery PutScheduledUpdateGroupAction' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "PutScheduledUpdateGroupAction" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2011-01-01" :: Prelude.ByteString),
        "StartTime" Core.=: startTime,
        "Time" Core.=: time,
        "MaxSize" Core.=: maxSize,
        "Recurrence" Core.=: recurrence,
        "DesiredCapacity" Core.=: desiredCapacity,
        "MinSize" Core.=: minSize,
        "EndTime" Core.=: endTime,
        "TimeZone" Core.=: timeZone,
        "AutoScalingGroupName" Core.=: autoScalingGroupName,
        "ScheduledActionName" Core.=: scheduledActionName
      ]

-- | /See:/ 'newPutScheduledUpdateGroupActionResponse' smart constructor.
data PutScheduledUpdateGroupActionResponse = PutScheduledUpdateGroupActionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutScheduledUpdateGroupActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutScheduledUpdateGroupActionResponse ::
  PutScheduledUpdateGroupActionResponse
newPutScheduledUpdateGroupActionResponse =
  PutScheduledUpdateGroupActionResponse'

instance
  Prelude.NFData
    PutScheduledUpdateGroupActionResponse
  where
  rnf _ = ()
