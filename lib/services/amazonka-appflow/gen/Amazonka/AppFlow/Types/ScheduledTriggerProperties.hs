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
-- Module      : Amazonka.AppFlow.Types.ScheduledTriggerProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ScheduledTriggerProperties where

import Amazonka.AppFlow.Types.DataPullMode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration details of a schedule-triggered flow as
-- defined by the user. Currently, these settings only apply to the
-- @Scheduled@ trigger type.
--
-- /See:/ 'newScheduledTriggerProperties' smart constructor.
data ScheduledTriggerProperties = ScheduledTriggerProperties'
  { -- | Specifies whether a scheduled flow has an incremental data transfer or a
    -- complete data transfer for each flow run.
    dataPullMode :: Prelude.Maybe DataPullMode,
    -- | Specifies the date range for the records to import from the connector in
    -- the first flow run.
    firstExecutionFrom :: Prelude.Maybe Data.POSIX,
    -- | Defines how many times a scheduled flow fails consecutively before
    -- Amazon AppFlow deactivates it.
    flowErrorDeactivationThreshold :: Prelude.Maybe Prelude.Natural,
    -- | The time at which the scheduled flow ends. The time is formatted as a
    -- timestamp that follows the ISO 8601 standard, such as
    -- @2022-04-27T13:00:00-07:00@.
    scheduleEndTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies the optional offset that is added to the time interval for a
    -- schedule-triggered flow.
    scheduleOffset :: Prelude.Maybe Prelude.Natural,
    -- | The time at which the scheduled flow starts. The time is formatted as a
    -- timestamp that follows the ISO 8601 standard, such as
    -- @2022-04-26T13:00:00-07:00@.
    scheduleStartTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies the time zone used when referring to the dates and times of a
    -- scheduled flow, such as @America\/New_York@. This time zone is only a
    -- descriptive label. It doesn\'t affect how Amazon AppFlow interprets the
    -- timestamps that you specify to schedule the flow.
    --
    -- If you want to schedule a flow by using times in a particular time zone,
    -- indicate the time zone as a UTC offset in your timestamps. For example,
    -- the UTC offsets for the @America\/New_York@ timezone are @-04:00@ EDT
    -- and @-05:00 EST@.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The scheduling expression that determines the rate at which the schedule
    -- will run, for example @rate(5minutes)@.
    scheduleExpression :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledTriggerProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataPullMode', 'scheduledTriggerProperties_dataPullMode' - Specifies whether a scheduled flow has an incremental data transfer or a
-- complete data transfer for each flow run.
--
-- 'firstExecutionFrom', 'scheduledTriggerProperties_firstExecutionFrom' - Specifies the date range for the records to import from the connector in
-- the first flow run.
--
-- 'flowErrorDeactivationThreshold', 'scheduledTriggerProperties_flowErrorDeactivationThreshold' - Defines how many times a scheduled flow fails consecutively before
-- Amazon AppFlow deactivates it.
--
-- 'scheduleEndTime', 'scheduledTriggerProperties_scheduleEndTime' - The time at which the scheduled flow ends. The time is formatted as a
-- timestamp that follows the ISO 8601 standard, such as
-- @2022-04-27T13:00:00-07:00@.
--
-- 'scheduleOffset', 'scheduledTriggerProperties_scheduleOffset' - Specifies the optional offset that is added to the time interval for a
-- schedule-triggered flow.
--
-- 'scheduleStartTime', 'scheduledTriggerProperties_scheduleStartTime' - The time at which the scheduled flow starts. The time is formatted as a
-- timestamp that follows the ISO 8601 standard, such as
-- @2022-04-26T13:00:00-07:00@.
--
-- 'timezone', 'scheduledTriggerProperties_timezone' - Specifies the time zone used when referring to the dates and times of a
-- scheduled flow, such as @America\/New_York@. This time zone is only a
-- descriptive label. It doesn\'t affect how Amazon AppFlow interprets the
-- timestamps that you specify to schedule the flow.
--
-- If you want to schedule a flow by using times in a particular time zone,
-- indicate the time zone as a UTC offset in your timestamps. For example,
-- the UTC offsets for the @America\/New_York@ timezone are @-04:00@ EDT
-- and @-05:00 EST@.
--
-- 'scheduleExpression', 'scheduledTriggerProperties_scheduleExpression' - The scheduling expression that determines the rate at which the schedule
-- will run, for example @rate(5minutes)@.
newScheduledTriggerProperties ::
  -- | 'scheduleExpression'
  Prelude.Text ->
  ScheduledTriggerProperties
newScheduledTriggerProperties pScheduleExpression_ =
  ScheduledTriggerProperties'
    { dataPullMode =
        Prelude.Nothing,
      firstExecutionFrom = Prelude.Nothing,
      flowErrorDeactivationThreshold =
        Prelude.Nothing,
      scheduleEndTime = Prelude.Nothing,
      scheduleOffset = Prelude.Nothing,
      scheduleStartTime = Prelude.Nothing,
      timezone = Prelude.Nothing,
      scheduleExpression = pScheduleExpression_
    }

-- | Specifies whether a scheduled flow has an incremental data transfer or a
-- complete data transfer for each flow run.
scheduledTriggerProperties_dataPullMode :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe DataPullMode)
scheduledTriggerProperties_dataPullMode = Lens.lens (\ScheduledTriggerProperties' {dataPullMode} -> dataPullMode) (\s@ScheduledTriggerProperties' {} a -> s {dataPullMode = a} :: ScheduledTriggerProperties)

-- | Specifies the date range for the records to import from the connector in
-- the first flow run.
scheduledTriggerProperties_firstExecutionFrom :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe Prelude.UTCTime)
scheduledTriggerProperties_firstExecutionFrom = Lens.lens (\ScheduledTriggerProperties' {firstExecutionFrom} -> firstExecutionFrom) (\s@ScheduledTriggerProperties' {} a -> s {firstExecutionFrom = a} :: ScheduledTriggerProperties) Prelude.. Lens.mapping Data._Time

-- | Defines how many times a scheduled flow fails consecutively before
-- Amazon AppFlow deactivates it.
scheduledTriggerProperties_flowErrorDeactivationThreshold :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe Prelude.Natural)
scheduledTriggerProperties_flowErrorDeactivationThreshold = Lens.lens (\ScheduledTriggerProperties' {flowErrorDeactivationThreshold} -> flowErrorDeactivationThreshold) (\s@ScheduledTriggerProperties' {} a -> s {flowErrorDeactivationThreshold = a} :: ScheduledTriggerProperties)

-- | The time at which the scheduled flow ends. The time is formatted as a
-- timestamp that follows the ISO 8601 standard, such as
-- @2022-04-27T13:00:00-07:00@.
scheduledTriggerProperties_scheduleEndTime :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe Prelude.UTCTime)
scheduledTriggerProperties_scheduleEndTime = Lens.lens (\ScheduledTriggerProperties' {scheduleEndTime} -> scheduleEndTime) (\s@ScheduledTriggerProperties' {} a -> s {scheduleEndTime = a} :: ScheduledTriggerProperties) Prelude.. Lens.mapping Data._Time

-- | Specifies the optional offset that is added to the time interval for a
-- schedule-triggered flow.
scheduledTriggerProperties_scheduleOffset :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe Prelude.Natural)
scheduledTriggerProperties_scheduleOffset = Lens.lens (\ScheduledTriggerProperties' {scheduleOffset} -> scheduleOffset) (\s@ScheduledTriggerProperties' {} a -> s {scheduleOffset = a} :: ScheduledTriggerProperties)

-- | The time at which the scheduled flow starts. The time is formatted as a
-- timestamp that follows the ISO 8601 standard, such as
-- @2022-04-26T13:00:00-07:00@.
scheduledTriggerProperties_scheduleStartTime :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe Prelude.UTCTime)
scheduledTriggerProperties_scheduleStartTime = Lens.lens (\ScheduledTriggerProperties' {scheduleStartTime} -> scheduleStartTime) (\s@ScheduledTriggerProperties' {} a -> s {scheduleStartTime = a} :: ScheduledTriggerProperties) Prelude.. Lens.mapping Data._Time

-- | Specifies the time zone used when referring to the dates and times of a
-- scheduled flow, such as @America\/New_York@. This time zone is only a
-- descriptive label. It doesn\'t affect how Amazon AppFlow interprets the
-- timestamps that you specify to schedule the flow.
--
-- If you want to schedule a flow by using times in a particular time zone,
-- indicate the time zone as a UTC offset in your timestamps. For example,
-- the UTC offsets for the @America\/New_York@ timezone are @-04:00@ EDT
-- and @-05:00 EST@.
scheduledTriggerProperties_timezone :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe Prelude.Text)
scheduledTriggerProperties_timezone = Lens.lens (\ScheduledTriggerProperties' {timezone} -> timezone) (\s@ScheduledTriggerProperties' {} a -> s {timezone = a} :: ScheduledTriggerProperties)

-- | The scheduling expression that determines the rate at which the schedule
-- will run, for example @rate(5minutes)@.
scheduledTriggerProperties_scheduleExpression :: Lens.Lens' ScheduledTriggerProperties Prelude.Text
scheduledTriggerProperties_scheduleExpression = Lens.lens (\ScheduledTriggerProperties' {scheduleExpression} -> scheduleExpression) (\s@ScheduledTriggerProperties' {} a -> s {scheduleExpression = a} :: ScheduledTriggerProperties)

instance Data.FromJSON ScheduledTriggerProperties where
  parseJSON =
    Data.withObject
      "ScheduledTriggerProperties"
      ( \x ->
          ScheduledTriggerProperties'
            Prelude.<$> (x Data..:? "dataPullMode")
            Prelude.<*> (x Data..:? "firstExecutionFrom")
            Prelude.<*> (x Data..:? "flowErrorDeactivationThreshold")
            Prelude.<*> (x Data..:? "scheduleEndTime")
            Prelude.<*> (x Data..:? "scheduleOffset")
            Prelude.<*> (x Data..:? "scheduleStartTime")
            Prelude.<*> (x Data..:? "timezone")
            Prelude.<*> (x Data..: "scheduleExpression")
      )

instance Prelude.Hashable ScheduledTriggerProperties where
  hashWithSalt _salt ScheduledTriggerProperties' {..} =
    _salt
      `Prelude.hashWithSalt` dataPullMode
      `Prelude.hashWithSalt` firstExecutionFrom
      `Prelude.hashWithSalt` flowErrorDeactivationThreshold
      `Prelude.hashWithSalt` scheduleEndTime
      `Prelude.hashWithSalt` scheduleOffset
      `Prelude.hashWithSalt` scheduleStartTime
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` scheduleExpression

instance Prelude.NFData ScheduledTriggerProperties where
  rnf ScheduledTriggerProperties' {..} =
    Prelude.rnf dataPullMode
      `Prelude.seq` Prelude.rnf firstExecutionFrom
      `Prelude.seq` Prelude.rnf flowErrorDeactivationThreshold
      `Prelude.seq` Prelude.rnf scheduleEndTime
      `Prelude.seq` Prelude.rnf scheduleOffset
      `Prelude.seq` Prelude.rnf scheduleStartTime
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf scheduleExpression

instance Data.ToJSON ScheduledTriggerProperties where
  toJSON ScheduledTriggerProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dataPullMode" Data..=) Prelude.<$> dataPullMode,
            ("firstExecutionFrom" Data..=)
              Prelude.<$> firstExecutionFrom,
            ("flowErrorDeactivationThreshold" Data..=)
              Prelude.<$> flowErrorDeactivationThreshold,
            ("scheduleEndTime" Data..=)
              Prelude.<$> scheduleEndTime,
            ("scheduleOffset" Data..=)
              Prelude.<$> scheduleOffset,
            ("scheduleStartTime" Data..=)
              Prelude.<$> scheduleStartTime,
            ("timezone" Data..=) Prelude.<$> timezone,
            Prelude.Just
              ("scheduleExpression" Data..= scheduleExpression)
          ]
      )
