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
-- Module      : Amazonka.AutoScaling.Types.ScheduledUpdateGroupActionRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.ScheduledUpdateGroupActionRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes information used for one or more scheduled scaling action
-- updates in a BatchPutScheduledUpdateGroupAction operation.
--
-- /See:/ 'newScheduledUpdateGroupActionRequest' smart constructor.
data ScheduledUpdateGroupActionRequest = ScheduledUpdateGroupActionRequest'
  { -- | The desired capacity is the initial capacity of the Auto Scaling group
    -- after the scheduled action runs and the capacity it attempts to
    -- maintain.
    desiredCapacity :: Prelude.Maybe Prelude.Int,
    -- | The date and time for the recurring schedule to end, in UTC.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The maximum size of the Auto Scaling group.
    maxSize :: Prelude.Maybe Prelude.Int,
    -- | The minimum size of the Auto Scaling group.
    minSize :: Prelude.Maybe Prelude.Int,
    -- | The recurring schedule for the action, in Unix cron syntax format. This
    -- format consists of five fields separated by white spaces: [Minute]
    -- [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be
    -- in quotes (for example, @\"30 0 1 1,6,12 *\"@). For more information
    -- about this format, see <http://crontab.org Crontab>.
    --
    -- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
    -- form the boundaries of when the recurring action starts and stops.
    --
    -- Cron expressions use Universal Coordinated Time (UTC) by default.
    recurrence :: Prelude.Maybe Prelude.Text,
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
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | Specifies the time zone for a cron expression. If a time zone is not
    -- provided, UTC is used by default.
    --
    -- Valid values are the canonical names of the IANA time zones, derived
    -- from the IANA Time Zone Database (such as @Etc\/GMT+9@ or
    -- @Pacific\/Tahiti@). For more information, see
    -- <https://en.wikipedia.org/wiki/List_of_tz_database_time_zones>.
    timeZone :: Prelude.Maybe Prelude.Text,
    -- | The name of the scaling action.
    scheduledActionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledUpdateGroupActionRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredCapacity', 'scheduledUpdateGroupActionRequest_desiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group
-- after the scheduled action runs and the capacity it attempts to
-- maintain.
--
-- 'endTime', 'scheduledUpdateGroupActionRequest_endTime' - The date and time for the recurring schedule to end, in UTC.
--
-- 'maxSize', 'scheduledUpdateGroupActionRequest_maxSize' - The maximum size of the Auto Scaling group.
--
-- 'minSize', 'scheduledUpdateGroupActionRequest_minSize' - The minimum size of the Auto Scaling group.
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
-- Cron expressions use Universal Coordinated Time (UTC) by default.
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
-- 'timeZone', 'scheduledUpdateGroupActionRequest_timeZone' - Specifies the time zone for a cron expression. If a time zone is not
-- provided, UTC is used by default.
--
-- Valid values are the canonical names of the IANA time zones, derived
-- from the IANA Time Zone Database (such as @Etc\/GMT+9@ or
-- @Pacific\/Tahiti@). For more information, see
-- <https://en.wikipedia.org/wiki/List_of_tz_database_time_zones>.
--
-- 'scheduledActionName', 'scheduledUpdateGroupActionRequest_scheduledActionName' - The name of the scaling action.
newScheduledUpdateGroupActionRequest ::
  -- | 'scheduledActionName'
  Prelude.Text ->
  ScheduledUpdateGroupActionRequest
newScheduledUpdateGroupActionRequest
  pScheduledActionName_ =
    ScheduledUpdateGroupActionRequest'
      { desiredCapacity =
          Prelude.Nothing,
        endTime = Prelude.Nothing,
        maxSize = Prelude.Nothing,
        minSize = Prelude.Nothing,
        recurrence = Prelude.Nothing,
        startTime = Prelude.Nothing,
        timeZone = Prelude.Nothing,
        scheduledActionName =
          pScheduledActionName_
      }

-- | The desired capacity is the initial capacity of the Auto Scaling group
-- after the scheduled action runs and the capacity it attempts to
-- maintain.
scheduledUpdateGroupActionRequest_desiredCapacity :: Lens.Lens' ScheduledUpdateGroupActionRequest (Prelude.Maybe Prelude.Int)
scheduledUpdateGroupActionRequest_desiredCapacity = Lens.lens (\ScheduledUpdateGroupActionRequest' {desiredCapacity} -> desiredCapacity) (\s@ScheduledUpdateGroupActionRequest' {} a -> s {desiredCapacity = a} :: ScheduledUpdateGroupActionRequest)

-- | The date and time for the recurring schedule to end, in UTC.
scheduledUpdateGroupActionRequest_endTime :: Lens.Lens' ScheduledUpdateGroupActionRequest (Prelude.Maybe Prelude.UTCTime)
scheduledUpdateGroupActionRequest_endTime = Lens.lens (\ScheduledUpdateGroupActionRequest' {endTime} -> endTime) (\s@ScheduledUpdateGroupActionRequest' {} a -> s {endTime = a} :: ScheduledUpdateGroupActionRequest) Prelude.. Lens.mapping Data._Time

-- | The maximum size of the Auto Scaling group.
scheduledUpdateGroupActionRequest_maxSize :: Lens.Lens' ScheduledUpdateGroupActionRequest (Prelude.Maybe Prelude.Int)
scheduledUpdateGroupActionRequest_maxSize = Lens.lens (\ScheduledUpdateGroupActionRequest' {maxSize} -> maxSize) (\s@ScheduledUpdateGroupActionRequest' {} a -> s {maxSize = a} :: ScheduledUpdateGroupActionRequest)

-- | The minimum size of the Auto Scaling group.
scheduledUpdateGroupActionRequest_minSize :: Lens.Lens' ScheduledUpdateGroupActionRequest (Prelude.Maybe Prelude.Int)
scheduledUpdateGroupActionRequest_minSize = Lens.lens (\ScheduledUpdateGroupActionRequest' {minSize} -> minSize) (\s@ScheduledUpdateGroupActionRequest' {} a -> s {minSize = a} :: ScheduledUpdateGroupActionRequest)

-- | The recurring schedule for the action, in Unix cron syntax format. This
-- format consists of five fields separated by white spaces: [Minute]
-- [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be
-- in quotes (for example, @\"30 0 1 1,6,12 *\"@). For more information
-- about this format, see <http://crontab.org Crontab>.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
-- form the boundaries of when the recurring action starts and stops.
--
-- Cron expressions use Universal Coordinated Time (UTC) by default.
scheduledUpdateGroupActionRequest_recurrence :: Lens.Lens' ScheduledUpdateGroupActionRequest (Prelude.Maybe Prelude.Text)
scheduledUpdateGroupActionRequest_recurrence = Lens.lens (\ScheduledUpdateGroupActionRequest' {recurrence} -> recurrence) (\s@ScheduledUpdateGroupActionRequest' {} a -> s {recurrence = a} :: ScheduledUpdateGroupActionRequest)

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
scheduledUpdateGroupActionRequest_startTime = Lens.lens (\ScheduledUpdateGroupActionRequest' {startTime} -> startTime) (\s@ScheduledUpdateGroupActionRequest' {} a -> s {startTime = a} :: ScheduledUpdateGroupActionRequest) Prelude.. Lens.mapping Data._Time

-- | Specifies the time zone for a cron expression. If a time zone is not
-- provided, UTC is used by default.
--
-- Valid values are the canonical names of the IANA time zones, derived
-- from the IANA Time Zone Database (such as @Etc\/GMT+9@ or
-- @Pacific\/Tahiti@). For more information, see
-- <https://en.wikipedia.org/wiki/List_of_tz_database_time_zones>.
scheduledUpdateGroupActionRequest_timeZone :: Lens.Lens' ScheduledUpdateGroupActionRequest (Prelude.Maybe Prelude.Text)
scheduledUpdateGroupActionRequest_timeZone = Lens.lens (\ScheduledUpdateGroupActionRequest' {timeZone} -> timeZone) (\s@ScheduledUpdateGroupActionRequest' {} a -> s {timeZone = a} :: ScheduledUpdateGroupActionRequest)

-- | The name of the scaling action.
scheduledUpdateGroupActionRequest_scheduledActionName :: Lens.Lens' ScheduledUpdateGroupActionRequest Prelude.Text
scheduledUpdateGroupActionRequest_scheduledActionName = Lens.lens (\ScheduledUpdateGroupActionRequest' {scheduledActionName} -> scheduledActionName) (\s@ScheduledUpdateGroupActionRequest' {} a -> s {scheduledActionName = a} :: ScheduledUpdateGroupActionRequest)

instance
  Prelude.Hashable
    ScheduledUpdateGroupActionRequest
  where
  hashWithSalt
    _salt
    ScheduledUpdateGroupActionRequest' {..} =
      _salt
        `Prelude.hashWithSalt` desiredCapacity
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` maxSize
        `Prelude.hashWithSalt` minSize
        `Prelude.hashWithSalt` recurrence
        `Prelude.hashWithSalt` startTime
        `Prelude.hashWithSalt` timeZone
        `Prelude.hashWithSalt` scheduledActionName

instance
  Prelude.NFData
    ScheduledUpdateGroupActionRequest
  where
  rnf ScheduledUpdateGroupActionRequest' {..} =
    Prelude.rnf desiredCapacity `Prelude.seq`
      Prelude.rnf endTime `Prelude.seq`
        Prelude.rnf maxSize `Prelude.seq`
          Prelude.rnf minSize `Prelude.seq`
            Prelude.rnf recurrence `Prelude.seq`
              Prelude.rnf startTime `Prelude.seq`
                Prelude.rnf timeZone `Prelude.seq`
                  Prelude.rnf scheduledActionName

instance
  Data.ToQuery
    ScheduledUpdateGroupActionRequest
  where
  toQuery ScheduledUpdateGroupActionRequest' {..} =
    Prelude.mconcat
      [ "DesiredCapacity" Data.=: desiredCapacity,
        "EndTime" Data.=: endTime,
        "MaxSize" Data.=: maxSize,
        "MinSize" Data.=: minSize,
        "Recurrence" Data.=: recurrence,
        "StartTime" Data.=: startTime,
        "TimeZone" Data.=: timeZone,
        "ScheduledActionName" Data.=: scheduledActionName
      ]
