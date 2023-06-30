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
-- Module      : Amazonka.AutoScaling.Types.ScheduledUpdateGroupAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.ScheduledUpdateGroupAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a scheduled scaling action.
--
-- /See:/ 'newScheduledUpdateGroupAction' smart constructor.
data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The desired capacity is the initial capacity of the Auto Scaling group
    -- after the scheduled action runs and the capacity it attempts to
    -- maintain.
    desiredCapacity :: Prelude.Maybe Prelude.Int,
    -- | The date and time in UTC for the recurring schedule to end. For example,
    -- @\"2019-06-01T00:00:00Z\"@.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The maximum size of the Auto Scaling group.
    maxSize :: Prelude.Maybe Prelude.Int,
    -- | The minimum size of the Auto Scaling group.
    minSize :: Prelude.Maybe Prelude.Int,
    -- | The recurring schedule for the action, in Unix cron syntax format.
    --
    -- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
    -- form the boundaries of when the recurring action starts and stops.
    recurrence :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the scheduled action.
    scheduledActionARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the scheduled action.
    scheduledActionName :: Prelude.Maybe Prelude.Text,
    -- | The date and time in UTC for this action to start. For example,
    -- @\"2019-06-01T00:00:00Z\"@.
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | This property is no longer used.
    time :: Prelude.Maybe Data.ISO8601,
    -- | The time zone for the cron expression.
    timeZone :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledUpdateGroupAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupName', 'scheduledUpdateGroupAction_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'desiredCapacity', 'scheduledUpdateGroupAction_desiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group
-- after the scheduled action runs and the capacity it attempts to
-- maintain.
--
-- 'endTime', 'scheduledUpdateGroupAction_endTime' - The date and time in UTC for the recurring schedule to end. For example,
-- @\"2019-06-01T00:00:00Z\"@.
--
-- 'maxSize', 'scheduledUpdateGroupAction_maxSize' - The maximum size of the Auto Scaling group.
--
-- 'minSize', 'scheduledUpdateGroupAction_minSize' - The minimum size of the Auto Scaling group.
--
-- 'recurrence', 'scheduledUpdateGroupAction_recurrence' - The recurring schedule for the action, in Unix cron syntax format.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
-- form the boundaries of when the recurring action starts and stops.
--
-- 'scheduledActionARN', 'scheduledUpdateGroupAction_scheduledActionARN' - The Amazon Resource Name (ARN) of the scheduled action.
--
-- 'scheduledActionName', 'scheduledUpdateGroupAction_scheduledActionName' - The name of the scheduled action.
--
-- 'startTime', 'scheduledUpdateGroupAction_startTime' - The date and time in UTC for this action to start. For example,
-- @\"2019-06-01T00:00:00Z\"@.
--
-- 'time', 'scheduledUpdateGroupAction_time' - This property is no longer used.
--
-- 'timeZone', 'scheduledUpdateGroupAction_timeZone' - The time zone for the cron expression.
newScheduledUpdateGroupAction ::
  ScheduledUpdateGroupAction
newScheduledUpdateGroupAction =
  ScheduledUpdateGroupAction'
    { autoScalingGroupName =
        Prelude.Nothing,
      desiredCapacity = Prelude.Nothing,
      endTime = Prelude.Nothing,
      maxSize = Prelude.Nothing,
      minSize = Prelude.Nothing,
      recurrence = Prelude.Nothing,
      scheduledActionARN = Prelude.Nothing,
      scheduledActionName = Prelude.Nothing,
      startTime = Prelude.Nothing,
      time = Prelude.Nothing,
      timeZone = Prelude.Nothing
    }

-- | The name of the Auto Scaling group.
scheduledUpdateGroupAction_autoScalingGroupName :: Lens.Lens' ScheduledUpdateGroupAction (Prelude.Maybe Prelude.Text)
scheduledUpdateGroupAction_autoScalingGroupName = Lens.lens (\ScheduledUpdateGroupAction' {autoScalingGroupName} -> autoScalingGroupName) (\s@ScheduledUpdateGroupAction' {} a -> s {autoScalingGroupName = a} :: ScheduledUpdateGroupAction)

-- | The desired capacity is the initial capacity of the Auto Scaling group
-- after the scheduled action runs and the capacity it attempts to
-- maintain.
scheduledUpdateGroupAction_desiredCapacity :: Lens.Lens' ScheduledUpdateGroupAction (Prelude.Maybe Prelude.Int)
scheduledUpdateGroupAction_desiredCapacity = Lens.lens (\ScheduledUpdateGroupAction' {desiredCapacity} -> desiredCapacity) (\s@ScheduledUpdateGroupAction' {} a -> s {desiredCapacity = a} :: ScheduledUpdateGroupAction)

-- | The date and time in UTC for the recurring schedule to end. For example,
-- @\"2019-06-01T00:00:00Z\"@.
scheduledUpdateGroupAction_endTime :: Lens.Lens' ScheduledUpdateGroupAction (Prelude.Maybe Prelude.UTCTime)
scheduledUpdateGroupAction_endTime = Lens.lens (\ScheduledUpdateGroupAction' {endTime} -> endTime) (\s@ScheduledUpdateGroupAction' {} a -> s {endTime = a} :: ScheduledUpdateGroupAction) Prelude.. Lens.mapping Data._Time

-- | The maximum size of the Auto Scaling group.
scheduledUpdateGroupAction_maxSize :: Lens.Lens' ScheduledUpdateGroupAction (Prelude.Maybe Prelude.Int)
scheduledUpdateGroupAction_maxSize = Lens.lens (\ScheduledUpdateGroupAction' {maxSize} -> maxSize) (\s@ScheduledUpdateGroupAction' {} a -> s {maxSize = a} :: ScheduledUpdateGroupAction)

-- | The minimum size of the Auto Scaling group.
scheduledUpdateGroupAction_minSize :: Lens.Lens' ScheduledUpdateGroupAction (Prelude.Maybe Prelude.Int)
scheduledUpdateGroupAction_minSize = Lens.lens (\ScheduledUpdateGroupAction' {minSize} -> minSize) (\s@ScheduledUpdateGroupAction' {} a -> s {minSize = a} :: ScheduledUpdateGroupAction)

-- | The recurring schedule for the action, in Unix cron syntax format.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
-- form the boundaries of when the recurring action starts and stops.
scheduledUpdateGroupAction_recurrence :: Lens.Lens' ScheduledUpdateGroupAction (Prelude.Maybe Prelude.Text)
scheduledUpdateGroupAction_recurrence = Lens.lens (\ScheduledUpdateGroupAction' {recurrence} -> recurrence) (\s@ScheduledUpdateGroupAction' {} a -> s {recurrence = a} :: ScheduledUpdateGroupAction)

-- | The Amazon Resource Name (ARN) of the scheduled action.
scheduledUpdateGroupAction_scheduledActionARN :: Lens.Lens' ScheduledUpdateGroupAction (Prelude.Maybe Prelude.Text)
scheduledUpdateGroupAction_scheduledActionARN = Lens.lens (\ScheduledUpdateGroupAction' {scheduledActionARN} -> scheduledActionARN) (\s@ScheduledUpdateGroupAction' {} a -> s {scheduledActionARN = a} :: ScheduledUpdateGroupAction)

-- | The name of the scheduled action.
scheduledUpdateGroupAction_scheduledActionName :: Lens.Lens' ScheduledUpdateGroupAction (Prelude.Maybe Prelude.Text)
scheduledUpdateGroupAction_scheduledActionName = Lens.lens (\ScheduledUpdateGroupAction' {scheduledActionName} -> scheduledActionName) (\s@ScheduledUpdateGroupAction' {} a -> s {scheduledActionName = a} :: ScheduledUpdateGroupAction)

-- | The date and time in UTC for this action to start. For example,
-- @\"2019-06-01T00:00:00Z\"@.
scheduledUpdateGroupAction_startTime :: Lens.Lens' ScheduledUpdateGroupAction (Prelude.Maybe Prelude.UTCTime)
scheduledUpdateGroupAction_startTime = Lens.lens (\ScheduledUpdateGroupAction' {startTime} -> startTime) (\s@ScheduledUpdateGroupAction' {} a -> s {startTime = a} :: ScheduledUpdateGroupAction) Prelude.. Lens.mapping Data._Time

-- | This property is no longer used.
scheduledUpdateGroupAction_time :: Lens.Lens' ScheduledUpdateGroupAction (Prelude.Maybe Prelude.UTCTime)
scheduledUpdateGroupAction_time = Lens.lens (\ScheduledUpdateGroupAction' {time} -> time) (\s@ScheduledUpdateGroupAction' {} a -> s {time = a} :: ScheduledUpdateGroupAction) Prelude.. Lens.mapping Data._Time

-- | The time zone for the cron expression.
scheduledUpdateGroupAction_timeZone :: Lens.Lens' ScheduledUpdateGroupAction (Prelude.Maybe Prelude.Text)
scheduledUpdateGroupAction_timeZone = Lens.lens (\ScheduledUpdateGroupAction' {timeZone} -> timeZone) (\s@ScheduledUpdateGroupAction' {} a -> s {timeZone = a} :: ScheduledUpdateGroupAction)

instance Data.FromXML ScheduledUpdateGroupAction where
  parseXML x =
    ScheduledUpdateGroupAction'
      Prelude.<$> (x Data..@? "AutoScalingGroupName")
      Prelude.<*> (x Data..@? "DesiredCapacity")
      Prelude.<*> (x Data..@? "EndTime")
      Prelude.<*> (x Data..@? "MaxSize")
      Prelude.<*> (x Data..@? "MinSize")
      Prelude.<*> (x Data..@? "Recurrence")
      Prelude.<*> (x Data..@? "ScheduledActionARN")
      Prelude.<*> (x Data..@? "ScheduledActionName")
      Prelude.<*> (x Data..@? "StartTime")
      Prelude.<*> (x Data..@? "Time")
      Prelude.<*> (x Data..@? "TimeZone")

instance Prelude.Hashable ScheduledUpdateGroupAction where
  hashWithSalt _salt ScheduledUpdateGroupAction' {..} =
    _salt
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` desiredCapacity
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` maxSize
      `Prelude.hashWithSalt` minSize
      `Prelude.hashWithSalt` recurrence
      `Prelude.hashWithSalt` scheduledActionARN
      `Prelude.hashWithSalt` scheduledActionName
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` time
      `Prelude.hashWithSalt` timeZone

instance Prelude.NFData ScheduledUpdateGroupAction where
  rnf ScheduledUpdateGroupAction' {..} =
    Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf desiredCapacity
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf maxSize
      `Prelude.seq` Prelude.rnf minSize
      `Prelude.seq` Prelude.rnf recurrence
      `Prelude.seq` Prelude.rnf scheduledActionARN
      `Prelude.seq` Prelude.rnf scheduledActionName
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf time
      `Prelude.seq` Prelude.rnf timeZone
