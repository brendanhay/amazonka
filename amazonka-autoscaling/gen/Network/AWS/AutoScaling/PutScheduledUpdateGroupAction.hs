{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scheduled scaling action for an Auto Scaling group.
-- If you leave a parameter unspecified when updating a scheduled scaling
-- action, the corresponding value remains unchanged.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/schedule_time.html Scheduled scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
  ( -- * Creating a Request
    PutScheduledUpdateGroupAction (..),
    newPutScheduledUpdateGroupAction,

    -- * Request Lenses
    putScheduledUpdateGroupAction_minSize,
    putScheduledUpdateGroupAction_desiredCapacity,
    putScheduledUpdateGroupAction_startTime,
    putScheduledUpdateGroupAction_endTime,
    putScheduledUpdateGroupAction_recurrence,
    putScheduledUpdateGroupAction_maxSize,
    putScheduledUpdateGroupAction_time,
    putScheduledUpdateGroupAction_autoScalingGroupName,
    putScheduledUpdateGroupAction_scheduledActionName,

    -- * Destructuring the Response
    PutScheduledUpdateGroupActionResponse (..),
    newPutScheduledUpdateGroupActionResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutScheduledUpdateGroupAction' smart constructor.
data PutScheduledUpdateGroupAction = PutScheduledUpdateGroupAction'
  { -- | The minimum size of the Auto Scaling group.
    minSize :: Prelude.Maybe Prelude.Int,
    -- | The desired capacity is the initial capacity of the Auto Scaling group
    -- after the scheduled action runs and the capacity it attempts to
    -- maintain. It can scale beyond this capacity if you add more scaling
    -- conditions.
    desiredCapacity :: Prelude.Maybe Prelude.Int,
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
    startTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The date and time for the recurring schedule to end. Amazon EC2 Auto
    -- Scaling does not perform the action after this time.
    endTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The recurring schedule for this action, in Unix cron syntax format. This
    -- format consists of five fields separated by white spaces: [Minute]
    -- [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be
    -- in quotes (for example, @\"30 0 1 1,6,12 *\"@). For more information
    -- about this format, see <http://crontab.org Crontab>.
    --
    -- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
    -- form the boundaries of when the recurring action starts and stops.
    recurrence :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of the Auto Scaling group.
    maxSize :: Prelude.Maybe Prelude.Int,
    -- | This parameter is no longer used.
    time :: Prelude.Maybe Prelude.ISO8601,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The name of this scaling action.
    scheduledActionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutScheduledUpdateGroupAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minSize', 'putScheduledUpdateGroupAction_minSize' - The minimum size of the Auto Scaling group.
--
-- 'desiredCapacity', 'putScheduledUpdateGroupAction_desiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group
-- after the scheduled action runs and the capacity it attempts to
-- maintain. It can scale beyond this capacity if you add more scaling
-- conditions.
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
-- 'endTime', 'putScheduledUpdateGroupAction_endTime' - The date and time for the recurring schedule to end. Amazon EC2 Auto
-- Scaling does not perform the action after this time.
--
-- 'recurrence', 'putScheduledUpdateGroupAction_recurrence' - The recurring schedule for this action, in Unix cron syntax format. This
-- format consists of five fields separated by white spaces: [Minute]
-- [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be
-- in quotes (for example, @\"30 0 1 1,6,12 *\"@). For more information
-- about this format, see <http://crontab.org Crontab>.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
-- form the boundaries of when the recurring action starts and stops.
--
-- 'maxSize', 'putScheduledUpdateGroupAction_maxSize' - The maximum size of the Auto Scaling group.
--
-- 'time', 'putScheduledUpdateGroupAction_time' - This parameter is no longer used.
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
      { minSize =
          Prelude.Nothing,
        desiredCapacity = Prelude.Nothing,
        startTime = Prelude.Nothing,
        endTime = Prelude.Nothing,
        recurrence = Prelude.Nothing,
        maxSize = Prelude.Nothing,
        time = Prelude.Nothing,
        autoScalingGroupName =
          pAutoScalingGroupName_,
        scheduledActionName = pScheduledActionName_
      }

-- | The minimum size of the Auto Scaling group.
putScheduledUpdateGroupAction_minSize :: Lens.Lens' PutScheduledUpdateGroupAction (Prelude.Maybe Prelude.Int)
putScheduledUpdateGroupAction_minSize = Lens.lens (\PutScheduledUpdateGroupAction' {minSize} -> minSize) (\s@PutScheduledUpdateGroupAction' {} a -> s {minSize = a} :: PutScheduledUpdateGroupAction)

-- | The desired capacity is the initial capacity of the Auto Scaling group
-- after the scheduled action runs and the capacity it attempts to
-- maintain. It can scale beyond this capacity if you add more scaling
-- conditions.
putScheduledUpdateGroupAction_desiredCapacity :: Lens.Lens' PutScheduledUpdateGroupAction (Prelude.Maybe Prelude.Int)
putScheduledUpdateGroupAction_desiredCapacity = Lens.lens (\PutScheduledUpdateGroupAction' {desiredCapacity} -> desiredCapacity) (\s@PutScheduledUpdateGroupAction' {} a -> s {desiredCapacity = a} :: PutScheduledUpdateGroupAction)

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
putScheduledUpdateGroupAction_startTime = Lens.lens (\PutScheduledUpdateGroupAction' {startTime} -> startTime) (\s@PutScheduledUpdateGroupAction' {} a -> s {startTime = a} :: PutScheduledUpdateGroupAction) Prelude.. Lens.mapping Prelude._Time

-- | The date and time for the recurring schedule to end. Amazon EC2 Auto
-- Scaling does not perform the action after this time.
putScheduledUpdateGroupAction_endTime :: Lens.Lens' PutScheduledUpdateGroupAction (Prelude.Maybe Prelude.UTCTime)
putScheduledUpdateGroupAction_endTime = Lens.lens (\PutScheduledUpdateGroupAction' {endTime} -> endTime) (\s@PutScheduledUpdateGroupAction' {} a -> s {endTime = a} :: PutScheduledUpdateGroupAction) Prelude.. Lens.mapping Prelude._Time

-- | The recurring schedule for this action, in Unix cron syntax format. This
-- format consists of five fields separated by white spaces: [Minute]
-- [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be
-- in quotes (for example, @\"30 0 1 1,6,12 *\"@). For more information
-- about this format, see <http://crontab.org Crontab>.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
-- form the boundaries of when the recurring action starts and stops.
putScheduledUpdateGroupAction_recurrence :: Lens.Lens' PutScheduledUpdateGroupAction (Prelude.Maybe Prelude.Text)
putScheduledUpdateGroupAction_recurrence = Lens.lens (\PutScheduledUpdateGroupAction' {recurrence} -> recurrence) (\s@PutScheduledUpdateGroupAction' {} a -> s {recurrence = a} :: PutScheduledUpdateGroupAction)

-- | The maximum size of the Auto Scaling group.
putScheduledUpdateGroupAction_maxSize :: Lens.Lens' PutScheduledUpdateGroupAction (Prelude.Maybe Prelude.Int)
putScheduledUpdateGroupAction_maxSize = Lens.lens (\PutScheduledUpdateGroupAction' {maxSize} -> maxSize) (\s@PutScheduledUpdateGroupAction' {} a -> s {maxSize = a} :: PutScheduledUpdateGroupAction)

-- | This parameter is no longer used.
putScheduledUpdateGroupAction_time :: Lens.Lens' PutScheduledUpdateGroupAction (Prelude.Maybe Prelude.UTCTime)
putScheduledUpdateGroupAction_time = Lens.lens (\PutScheduledUpdateGroupAction' {time} -> time) (\s@PutScheduledUpdateGroupAction' {} a -> s {time = a} :: PutScheduledUpdateGroupAction) Prelude.. Lens.mapping Prelude._Time

-- | The name of the Auto Scaling group.
putScheduledUpdateGroupAction_autoScalingGroupName :: Lens.Lens' PutScheduledUpdateGroupAction Prelude.Text
putScheduledUpdateGroupAction_autoScalingGroupName = Lens.lens (\PutScheduledUpdateGroupAction' {autoScalingGroupName} -> autoScalingGroupName) (\s@PutScheduledUpdateGroupAction' {} a -> s {autoScalingGroupName = a} :: PutScheduledUpdateGroupAction)

-- | The name of this scaling action.
putScheduledUpdateGroupAction_scheduledActionName :: Lens.Lens' PutScheduledUpdateGroupAction Prelude.Text
putScheduledUpdateGroupAction_scheduledActionName = Lens.lens (\PutScheduledUpdateGroupAction' {scheduledActionName} -> scheduledActionName) (\s@PutScheduledUpdateGroupAction' {} a -> s {scheduledActionName = a} :: PutScheduledUpdateGroupAction)

instance
  Prelude.AWSRequest
    PutScheduledUpdateGroupAction
  where
  type
    Rs PutScheduledUpdateGroupAction =
      PutScheduledUpdateGroupActionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      PutScheduledUpdateGroupActionResponse'

instance
  Prelude.Hashable
    PutScheduledUpdateGroupAction

instance Prelude.NFData PutScheduledUpdateGroupAction

instance
  Prelude.ToHeaders
    PutScheduledUpdateGroupAction
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath PutScheduledUpdateGroupAction where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    PutScheduledUpdateGroupAction
  where
  toQuery PutScheduledUpdateGroupAction' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "PutScheduledUpdateGroupAction" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2011-01-01" :: Prelude.ByteString),
        "MinSize" Prelude.=: minSize,
        "DesiredCapacity" Prelude.=: desiredCapacity,
        "StartTime" Prelude.=: startTime,
        "EndTime" Prelude.=: endTime,
        "Recurrence" Prelude.=: recurrence,
        "MaxSize" Prelude.=: maxSize,
        "Time" Prelude.=: time,
        "AutoScalingGroupName"
          Prelude.=: autoScalingGroupName,
        "ScheduledActionName" Prelude.=: scheduledActionName
      ]

-- | /See:/ 'newPutScheduledUpdateGroupActionResponse' smart constructor.
data PutScheduledUpdateGroupActionResponse = PutScheduledUpdateGroupActionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
