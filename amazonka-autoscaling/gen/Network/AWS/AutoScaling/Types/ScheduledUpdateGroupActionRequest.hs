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
-- Module      : Network.AWS.AutoScaling.Types.ScheduledUpdateGroupActionRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.ScheduledUpdateGroupActionRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes information used for one or more scheduled scaling action
-- updates in a BatchPutScheduledUpdateGroupAction operation.
--
-- When updating a scheduled scaling action, all optional parameters are
-- left unchanged if not specified.
--
-- /See:/ 'newScheduledUpdateGroupActionRequest' smart constructor.
data ScheduledUpdateGroupActionRequest = ScheduledUpdateGroupActionRequest'
  { -- | The minimum size of the Auto Scaling group.
    minSize :: Prelude.Maybe Prelude.Int,
    -- | The desired capacity is the initial capacity of the Auto Scaling group
    -- after the scheduled action runs and the capacity it attempts to
    -- maintain.
    desiredCapacity :: Prelude.Maybe Prelude.Int,
    -- | The date and time for the action to start, in YYYY-MM-DDThh:mm:ssZ
    -- format in UTC\/GMT only and in quotes (for example,
    -- @\"2019-06-01T00:00:00Z\"@).
    --
    -- If you specify @Recurrence@ and @StartTime@, Amazon EC2 Auto Scaling
    -- performs the action at this time, and then performs the action based on
    -- the specified recurrence.
    --
    -- If you try to schedule the action in the past, Amazon EC2 Auto Scaling
    -- returns an error message.
    startTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The date and time for the recurring schedule to end. Amazon EC2 Auto
    -- Scaling does not perform the action after this time.
    endTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The recurring schedule for the action, in Unix cron syntax format. This
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
    -- | The name of the scaling action.
    scheduledActionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScheduledUpdateGroupActionRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minSize', 'scheduledUpdateGroupActionRequest_minSize' - The minimum size of the Auto Scaling group.
--
-- 'desiredCapacity', 'scheduledUpdateGroupActionRequest_desiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group
-- after the scheduled action runs and the capacity it attempts to
-- maintain.
--
-- 'startTime', 'scheduledUpdateGroupActionRequest_startTime' - The date and time for the action to start, in YYYY-MM-DDThh:mm:ssZ
-- format in UTC\/GMT only and in quotes (for example,
-- @\"2019-06-01T00:00:00Z\"@).
--
-- If you specify @Recurrence@ and @StartTime@, Amazon EC2 Auto Scaling
-- performs the action at this time, and then performs the action based on
-- the specified recurrence.
--
-- If you try to schedule the action in the past, Amazon EC2 Auto Scaling
-- returns an error message.
--
-- 'endTime', 'scheduledUpdateGroupActionRequest_endTime' - The date and time for the recurring schedule to end. Amazon EC2 Auto
-- Scaling does not perform the action after this time.
--
-- 'recurrence', 'scheduledUpdateGroupActionRequest_recurrence' - The recurring schedule for the action, in Unix cron syntax format. This
-- format consists of five fields separated by white spaces: [Minute]
-- [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be
-- in quotes (for example, @\"30 0 1 1,6,12 *\"@). For more information
-- about this format, see <http://crontab.org Crontab>.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
-- form the boundaries of when the recurring action starts and stops.
--
-- 'maxSize', 'scheduledUpdateGroupActionRequest_maxSize' - The maximum size of the Auto Scaling group.
--
-- 'scheduledActionName', 'scheduledUpdateGroupActionRequest_scheduledActionName' - The name of the scaling action.
newScheduledUpdateGroupActionRequest ::
  -- | 'scheduledActionName'
  Prelude.Text ->
  ScheduledUpdateGroupActionRequest
newScheduledUpdateGroupActionRequest
  pScheduledActionName_ =
    ScheduledUpdateGroupActionRequest'
      { minSize =
          Prelude.Nothing,
        desiredCapacity = Prelude.Nothing,
        startTime = Prelude.Nothing,
        endTime = Prelude.Nothing,
        recurrence = Prelude.Nothing,
        maxSize = Prelude.Nothing,
        scheduledActionName =
          pScheduledActionName_
      }

-- | The minimum size of the Auto Scaling group.
scheduledUpdateGroupActionRequest_minSize :: Lens.Lens' ScheduledUpdateGroupActionRequest (Prelude.Maybe Prelude.Int)
scheduledUpdateGroupActionRequest_minSize = Lens.lens (\ScheduledUpdateGroupActionRequest' {minSize} -> minSize) (\s@ScheduledUpdateGroupActionRequest' {} a -> s {minSize = a} :: ScheduledUpdateGroupActionRequest)

-- | The desired capacity is the initial capacity of the Auto Scaling group
-- after the scheduled action runs and the capacity it attempts to
-- maintain.
scheduledUpdateGroupActionRequest_desiredCapacity :: Lens.Lens' ScheduledUpdateGroupActionRequest (Prelude.Maybe Prelude.Int)
scheduledUpdateGroupActionRequest_desiredCapacity = Lens.lens (\ScheduledUpdateGroupActionRequest' {desiredCapacity} -> desiredCapacity) (\s@ScheduledUpdateGroupActionRequest' {} a -> s {desiredCapacity = a} :: ScheduledUpdateGroupActionRequest)

-- | The date and time for the action to start, in YYYY-MM-DDThh:mm:ssZ
-- format in UTC\/GMT only and in quotes (for example,
-- @\"2019-06-01T00:00:00Z\"@).
--
-- If you specify @Recurrence@ and @StartTime@, Amazon EC2 Auto Scaling
-- performs the action at this time, and then performs the action based on
-- the specified recurrence.
--
-- If you try to schedule the action in the past, Amazon EC2 Auto Scaling
-- returns an error message.
scheduledUpdateGroupActionRequest_startTime :: Lens.Lens' ScheduledUpdateGroupActionRequest (Prelude.Maybe Prelude.UTCTime)
scheduledUpdateGroupActionRequest_startTime = Lens.lens (\ScheduledUpdateGroupActionRequest' {startTime} -> startTime) (\s@ScheduledUpdateGroupActionRequest' {} a -> s {startTime = a} :: ScheduledUpdateGroupActionRequest) Prelude.. Lens.mapping Prelude._Time

-- | The date and time for the recurring schedule to end. Amazon EC2 Auto
-- Scaling does not perform the action after this time.
scheduledUpdateGroupActionRequest_endTime :: Lens.Lens' ScheduledUpdateGroupActionRequest (Prelude.Maybe Prelude.UTCTime)
scheduledUpdateGroupActionRequest_endTime = Lens.lens (\ScheduledUpdateGroupActionRequest' {endTime} -> endTime) (\s@ScheduledUpdateGroupActionRequest' {} a -> s {endTime = a} :: ScheduledUpdateGroupActionRequest) Prelude.. Lens.mapping Prelude._Time

-- | The recurring schedule for the action, in Unix cron syntax format. This
-- format consists of five fields separated by white spaces: [Minute]
-- [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be
-- in quotes (for example, @\"30 0 1 1,6,12 *\"@). For more information
-- about this format, see <http://crontab.org Crontab>.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
-- form the boundaries of when the recurring action starts and stops.
scheduledUpdateGroupActionRequest_recurrence :: Lens.Lens' ScheduledUpdateGroupActionRequest (Prelude.Maybe Prelude.Text)
scheduledUpdateGroupActionRequest_recurrence = Lens.lens (\ScheduledUpdateGroupActionRequest' {recurrence} -> recurrence) (\s@ScheduledUpdateGroupActionRequest' {} a -> s {recurrence = a} :: ScheduledUpdateGroupActionRequest)

-- | The maximum size of the Auto Scaling group.
scheduledUpdateGroupActionRequest_maxSize :: Lens.Lens' ScheduledUpdateGroupActionRequest (Prelude.Maybe Prelude.Int)
scheduledUpdateGroupActionRequest_maxSize = Lens.lens (\ScheduledUpdateGroupActionRequest' {maxSize} -> maxSize) (\s@ScheduledUpdateGroupActionRequest' {} a -> s {maxSize = a} :: ScheduledUpdateGroupActionRequest)

-- | The name of the scaling action.
scheduledUpdateGroupActionRequest_scheduledActionName :: Lens.Lens' ScheduledUpdateGroupActionRequest Prelude.Text
scheduledUpdateGroupActionRequest_scheduledActionName = Lens.lens (\ScheduledUpdateGroupActionRequest' {scheduledActionName} -> scheduledActionName) (\s@ScheduledUpdateGroupActionRequest' {} a -> s {scheduledActionName = a} :: ScheduledUpdateGroupActionRequest)

instance
  Prelude.Hashable
    ScheduledUpdateGroupActionRequest

instance
  Prelude.NFData
    ScheduledUpdateGroupActionRequest

instance
  Prelude.ToQuery
    ScheduledUpdateGroupActionRequest
  where
  toQuery ScheduledUpdateGroupActionRequest' {..} =
    Prelude.mconcat
      [ "MinSize" Prelude.=: minSize,
        "DesiredCapacity" Prelude.=: desiredCapacity,
        "StartTime" Prelude.=: startTime,
        "EndTime" Prelude.=: endTime,
        "Recurrence" Prelude.=: recurrence,
        "MaxSize" Prelude.=: maxSize,
        "ScheduledActionName" Prelude.=: scheduledActionName
      ]
