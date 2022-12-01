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
-- Module      : Amazonka.CustomerProfiles.Types.ScheduledTriggerProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.ScheduledTriggerProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.DataPullMode
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration details of a scheduled-trigger flow that you
-- define. Currently, these settings only apply to the scheduled-trigger
-- type.
--
-- /See:/ 'newScheduledTriggerProperties' smart constructor.
data ScheduledTriggerProperties = ScheduledTriggerProperties'
  { -- | Specifies the scheduled end time for a scheduled-trigger flow.
    scheduleEndTime :: Prelude.Maybe Core.POSIX,
    -- | Specifies the scheduled start time for a scheduled-trigger flow.
    scheduleStartTime :: Prelude.Maybe Core.POSIX,
    -- | Specifies the time zone used when referring to the date and time of a
    -- scheduled-triggered flow, such as America\/New_York.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | Specifies the optional offset that is added to the time interval for a
    -- schedule-triggered flow.
    scheduleOffset :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the date range for the records to import from the connector in
    -- the first flow run.
    firstExecutionFrom :: Prelude.Maybe Core.POSIX,
    -- | Specifies whether a scheduled flow has an incremental data transfer or a
    -- complete data transfer for each flow run.
    dataPullMode :: Prelude.Maybe DataPullMode,
    -- | The scheduling expression that determines the rate at which the schedule
    -- will run, for example rate (5 minutes).
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
-- 'scheduleEndTime', 'scheduledTriggerProperties_scheduleEndTime' - Specifies the scheduled end time for a scheduled-trigger flow.
--
-- 'scheduleStartTime', 'scheduledTriggerProperties_scheduleStartTime' - Specifies the scheduled start time for a scheduled-trigger flow.
--
-- 'timezone', 'scheduledTriggerProperties_timezone' - Specifies the time zone used when referring to the date and time of a
-- scheduled-triggered flow, such as America\/New_York.
--
-- 'scheduleOffset', 'scheduledTriggerProperties_scheduleOffset' - Specifies the optional offset that is added to the time interval for a
-- schedule-triggered flow.
--
-- 'firstExecutionFrom', 'scheduledTriggerProperties_firstExecutionFrom' - Specifies the date range for the records to import from the connector in
-- the first flow run.
--
-- 'dataPullMode', 'scheduledTriggerProperties_dataPullMode' - Specifies whether a scheduled flow has an incremental data transfer or a
-- complete data transfer for each flow run.
--
-- 'scheduleExpression', 'scheduledTriggerProperties_scheduleExpression' - The scheduling expression that determines the rate at which the schedule
-- will run, for example rate (5 minutes).
newScheduledTriggerProperties ::
  -- | 'scheduleExpression'
  Prelude.Text ->
  ScheduledTriggerProperties
newScheduledTriggerProperties pScheduleExpression_ =
  ScheduledTriggerProperties'
    { scheduleEndTime =
        Prelude.Nothing,
      scheduleStartTime = Prelude.Nothing,
      timezone = Prelude.Nothing,
      scheduleOffset = Prelude.Nothing,
      firstExecutionFrom = Prelude.Nothing,
      dataPullMode = Prelude.Nothing,
      scheduleExpression = pScheduleExpression_
    }

-- | Specifies the scheduled end time for a scheduled-trigger flow.
scheduledTriggerProperties_scheduleEndTime :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe Prelude.UTCTime)
scheduledTriggerProperties_scheduleEndTime = Lens.lens (\ScheduledTriggerProperties' {scheduleEndTime} -> scheduleEndTime) (\s@ScheduledTriggerProperties' {} a -> s {scheduleEndTime = a} :: ScheduledTriggerProperties) Prelude.. Lens.mapping Core._Time

-- | Specifies the scheduled start time for a scheduled-trigger flow.
scheduledTriggerProperties_scheduleStartTime :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe Prelude.UTCTime)
scheduledTriggerProperties_scheduleStartTime = Lens.lens (\ScheduledTriggerProperties' {scheduleStartTime} -> scheduleStartTime) (\s@ScheduledTriggerProperties' {} a -> s {scheduleStartTime = a} :: ScheduledTriggerProperties) Prelude.. Lens.mapping Core._Time

-- | Specifies the time zone used when referring to the date and time of a
-- scheduled-triggered flow, such as America\/New_York.
scheduledTriggerProperties_timezone :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe Prelude.Text)
scheduledTriggerProperties_timezone = Lens.lens (\ScheduledTriggerProperties' {timezone} -> timezone) (\s@ScheduledTriggerProperties' {} a -> s {timezone = a} :: ScheduledTriggerProperties)

-- | Specifies the optional offset that is added to the time interval for a
-- schedule-triggered flow.
scheduledTriggerProperties_scheduleOffset :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe Prelude.Natural)
scheduledTriggerProperties_scheduleOffset = Lens.lens (\ScheduledTriggerProperties' {scheduleOffset} -> scheduleOffset) (\s@ScheduledTriggerProperties' {} a -> s {scheduleOffset = a} :: ScheduledTriggerProperties)

-- | Specifies the date range for the records to import from the connector in
-- the first flow run.
scheduledTriggerProperties_firstExecutionFrom :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe Prelude.UTCTime)
scheduledTriggerProperties_firstExecutionFrom = Lens.lens (\ScheduledTriggerProperties' {firstExecutionFrom} -> firstExecutionFrom) (\s@ScheduledTriggerProperties' {} a -> s {firstExecutionFrom = a} :: ScheduledTriggerProperties) Prelude.. Lens.mapping Core._Time

-- | Specifies whether a scheduled flow has an incremental data transfer or a
-- complete data transfer for each flow run.
scheduledTriggerProperties_dataPullMode :: Lens.Lens' ScheduledTriggerProperties (Prelude.Maybe DataPullMode)
scheduledTriggerProperties_dataPullMode = Lens.lens (\ScheduledTriggerProperties' {dataPullMode} -> dataPullMode) (\s@ScheduledTriggerProperties' {} a -> s {dataPullMode = a} :: ScheduledTriggerProperties)

-- | The scheduling expression that determines the rate at which the schedule
-- will run, for example rate (5 minutes).
scheduledTriggerProperties_scheduleExpression :: Lens.Lens' ScheduledTriggerProperties Prelude.Text
scheduledTriggerProperties_scheduleExpression = Lens.lens (\ScheduledTriggerProperties' {scheduleExpression} -> scheduleExpression) (\s@ScheduledTriggerProperties' {} a -> s {scheduleExpression = a} :: ScheduledTriggerProperties)

instance Prelude.Hashable ScheduledTriggerProperties where
  hashWithSalt _salt ScheduledTriggerProperties' {..} =
    _salt `Prelude.hashWithSalt` scheduleEndTime
      `Prelude.hashWithSalt` scheduleStartTime
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` scheduleOffset
      `Prelude.hashWithSalt` firstExecutionFrom
      `Prelude.hashWithSalt` dataPullMode
      `Prelude.hashWithSalt` scheduleExpression

instance Prelude.NFData ScheduledTriggerProperties where
  rnf ScheduledTriggerProperties' {..} =
    Prelude.rnf scheduleEndTime
      `Prelude.seq` Prelude.rnf scheduleStartTime
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf scheduleOffset
      `Prelude.seq` Prelude.rnf firstExecutionFrom
      `Prelude.seq` Prelude.rnf dataPullMode
      `Prelude.seq` Prelude.rnf scheduleExpression

instance Core.ToJSON ScheduledTriggerProperties where
  toJSON ScheduledTriggerProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ScheduleEndTime" Core..=)
              Prelude.<$> scheduleEndTime,
            ("ScheduleStartTime" Core..=)
              Prelude.<$> scheduleStartTime,
            ("Timezone" Core..=) Prelude.<$> timezone,
            ("ScheduleOffset" Core..=)
              Prelude.<$> scheduleOffset,
            ("FirstExecutionFrom" Core..=)
              Prelude.<$> firstExecutionFrom,
            ("DataPullMode" Core..=) Prelude.<$> dataPullMode,
            Prelude.Just
              ("ScheduleExpression" Core..= scheduleExpression)
          ]
      )
