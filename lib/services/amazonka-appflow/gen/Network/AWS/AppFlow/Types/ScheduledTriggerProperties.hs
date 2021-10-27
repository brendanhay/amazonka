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
-- Module      : Network.AWS.AppFlow.Types.ScheduledTriggerProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppFlow.Types.ScheduledTriggerProperties where

import Network.AWS.AppFlow.Types.DataPullMode
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the configuration details of a schedule-triggered flow as
-- defined by the user. Currently, these settings only apply to the
-- @Scheduled@ trigger type.
--
-- /See:/ 'newScheduledTriggerProperties' smart constructor.
data ScheduledTriggerProperties = ScheduledTriggerProperties'
  { -- | Specifies the scheduled end time for a schedule-triggered flow.
    scheduleEndTime :: Prelude.Maybe Core.POSIX,
    -- | Specifies the optional offset that is added to the time interval for a
    -- schedule-triggered flow.
    scheduleOffset :: Prelude.Maybe Prelude.Natural,
    -- | Specifies whether a scheduled flow has an incremental data transfer or a
    -- complete data transfer for each flow run.
    dataPullMode :: Prelude.Maybe DataPullMode,
    -- | Specifies the scheduled start time for a schedule-triggered flow.
    scheduleStartTime :: Prelude.Maybe Core.POSIX,
    -- | Specifies the time zone used when referring to the date and time of a
    -- scheduled-triggered flow, such as @America\/New_York@.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | Specifies the date range for the records to import from the connector in
    -- the first flow run.
    firstExecutionFrom :: Prelude.Maybe Core.POSIX,
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
-- 'scheduleEndTime', 'scheduledTriggerProperties_scheduleEndTime' - Specifies the scheduled end time for a schedule-triggered flow.
--
-- 'scheduleOffset', 'scheduledTriggerProperties_scheduleOffset' - Specifies the optional offset that is added to the time interval for a
-- schedule-triggered flow.
--
-- 'dataPullMode', 'scheduledTriggerProperties_dataPullMode' - Specifies whether a scheduled flow has an incremental data transfer or a
-- complete data transfer for each flow run.
--
-- 'scheduleStartTime', 'scheduledTriggerProperties_scheduleStartTime' - Specifies the scheduled start time for a schedule-triggered flow.
--
-- 'timezone', 'scheduledTriggerProperties_timezone' - Specifies the time zone used when referring to the date and time of a
-- scheduled-triggered flow, such as @America\/New_York@.
--
-- 'firstExecutionFrom', 'scheduledTriggerProperties_firstExecutionFrom' - Specifies the date range for the records to import from the connector in
-- the first flow run.
--
-- 'scheduleExpression', 'scheduledTriggerProperties_scheduleExpression' - The scheduling expression that determines the rate at which the schedule
-- will run, for example @rate(5minutes)@.
newScheduledTriggerProperties ::
  -- | 'scheduleExpression'
  Prelude.Text ->
  ScheduledTriggerProperties
newScheduledTriggerProperties pScheduleExpression_ =
  ScheduledTriggerProperties'
    { scheduleEndTime =
        Prelude.Nothing,
      scheduleOffset = Prelude.Nothing,
      dataPullMode = Prelude.Nothing,
      scheduleStartTime = Prelude.Nothing,
      timezone = Prelude.Nothing,
      firstExecutionFrom = Prelude.Nothing,
      scheduleExpression = pScheduleExpression_
    }

-- | Specifies the scheduled end time for a schedule-triggered flow.
scheduledTriggerProperties_scheduleEndTime :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe Prelude.UTCTime)
scheduledTriggerProperties_scheduleEndTime = Lens.lens (\ScheduledTriggerProperties' {scheduleEndTime} -> scheduleEndTime) (\s@ScheduledTriggerProperties' {} a -> s {scheduleEndTime = a} :: ScheduledTriggerProperties) Prelude.. Lens.mapping Core._Time

-- | Specifies the optional offset that is added to the time interval for a
-- schedule-triggered flow.
scheduledTriggerProperties_scheduleOffset :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe Prelude.Natural)
scheduledTriggerProperties_scheduleOffset = Lens.lens (\ScheduledTriggerProperties' {scheduleOffset} -> scheduleOffset) (\s@ScheduledTriggerProperties' {} a -> s {scheduleOffset = a} :: ScheduledTriggerProperties)

-- | Specifies whether a scheduled flow has an incremental data transfer or a
-- complete data transfer for each flow run.
scheduledTriggerProperties_dataPullMode :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe DataPullMode)
scheduledTriggerProperties_dataPullMode = Lens.lens (\ScheduledTriggerProperties' {dataPullMode} -> dataPullMode) (\s@ScheduledTriggerProperties' {} a -> s {dataPullMode = a} :: ScheduledTriggerProperties)

-- | Specifies the scheduled start time for a schedule-triggered flow.
scheduledTriggerProperties_scheduleStartTime :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe Prelude.UTCTime)
scheduledTriggerProperties_scheduleStartTime = Lens.lens (\ScheduledTriggerProperties' {scheduleStartTime} -> scheduleStartTime) (\s@ScheduledTriggerProperties' {} a -> s {scheduleStartTime = a} :: ScheduledTriggerProperties) Prelude.. Lens.mapping Core._Time

-- | Specifies the time zone used when referring to the date and time of a
-- scheduled-triggered flow, such as @America\/New_York@.
scheduledTriggerProperties_timezone :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe Prelude.Text)
scheduledTriggerProperties_timezone = Lens.lens (\ScheduledTriggerProperties' {timezone} -> timezone) (\s@ScheduledTriggerProperties' {} a -> s {timezone = a} :: ScheduledTriggerProperties)

-- | Specifies the date range for the records to import from the connector in
-- the first flow run.
scheduledTriggerProperties_firstExecutionFrom :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe Prelude.UTCTime)
scheduledTriggerProperties_firstExecutionFrom = Lens.lens (\ScheduledTriggerProperties' {firstExecutionFrom} -> firstExecutionFrom) (\s@ScheduledTriggerProperties' {} a -> s {firstExecutionFrom = a} :: ScheduledTriggerProperties) Prelude.. Lens.mapping Core._Time

-- | The scheduling expression that determines the rate at which the schedule
-- will run, for example @rate(5minutes)@.
scheduledTriggerProperties_scheduleExpression :: Lens.Lens' ScheduledTriggerProperties Prelude.Text
scheduledTriggerProperties_scheduleExpression = Lens.lens (\ScheduledTriggerProperties' {scheduleExpression} -> scheduleExpression) (\s@ScheduledTriggerProperties' {} a -> s {scheduleExpression = a} :: ScheduledTriggerProperties)

instance Core.FromJSON ScheduledTriggerProperties where
  parseJSON =
    Core.withObject
      "ScheduledTriggerProperties"
      ( \x ->
          ScheduledTriggerProperties'
            Prelude.<$> (x Core..:? "scheduleEndTime")
            Prelude.<*> (x Core..:? "scheduleOffset")
            Prelude.<*> (x Core..:? "dataPullMode")
            Prelude.<*> (x Core..:? "scheduleStartTime")
            Prelude.<*> (x Core..:? "timezone")
            Prelude.<*> (x Core..:? "firstExecutionFrom")
            Prelude.<*> (x Core..: "scheduleExpression")
      )

instance Prelude.Hashable ScheduledTriggerProperties

instance Prelude.NFData ScheduledTriggerProperties

instance Core.ToJSON ScheduledTriggerProperties where
  toJSON ScheduledTriggerProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("scheduleEndTime" Core..=)
              Prelude.<$> scheduleEndTime,
            ("scheduleOffset" Core..=)
              Prelude.<$> scheduleOffset,
            ("dataPullMode" Core..=) Prelude.<$> dataPullMode,
            ("scheduleStartTime" Core..=)
              Prelude.<$> scheduleStartTime,
            ("timezone" Core..=) Prelude.<$> timezone,
            ("firstExecutionFrom" Core..=)
              Prelude.<$> firstExecutionFrom,
            Prelude.Just
              ("scheduleExpression" Core..= scheduleExpression)
          ]
      )
