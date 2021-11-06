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
-- Module      : Amazonka.IoTEventsData.Types.Alarm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.Alarm where

import qualified Amazonka.Core as Core
import Amazonka.IoTEventsData.Types.AlarmState
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an alarm.
--
-- /See:/ 'newAlarm' smart constructor.
data Alarm = Alarm'
  { -- | The value of the key used as a filter to select only the alarms
    -- associated with the
    -- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateAlarmModel.html#iotevents-CreateAlarmModel-request-key key>.
    keyValue :: Prelude.Maybe Prelude.Text,
    -- | The time the alarm was created, in the Unix epoch format.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Contains information about the current state of the alarm.
    alarmState :: Prelude.Maybe AlarmState,
    -- | The name of the alarm model.
    alarmModelName :: Prelude.Maybe Prelude.Text,
    -- | A non-negative integer that reflects the severity level of the alarm.
    severity :: Prelude.Maybe Prelude.Natural,
    -- | The time the alarm was last updated, in the Unix epoch format.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | The version of the alarm model.
    alarmModelVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Alarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyValue', 'alarm_keyValue' - The value of the key used as a filter to select only the alarms
-- associated with the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateAlarmModel.html#iotevents-CreateAlarmModel-request-key key>.
--
-- 'creationTime', 'alarm_creationTime' - The time the alarm was created, in the Unix epoch format.
--
-- 'alarmState', 'alarm_alarmState' - Contains information about the current state of the alarm.
--
-- 'alarmModelName', 'alarm_alarmModelName' - The name of the alarm model.
--
-- 'severity', 'alarm_severity' - A non-negative integer that reflects the severity level of the alarm.
--
-- 'lastUpdateTime', 'alarm_lastUpdateTime' - The time the alarm was last updated, in the Unix epoch format.
--
-- 'alarmModelVersion', 'alarm_alarmModelVersion' - The version of the alarm model.
newAlarm ::
  Alarm
newAlarm =
  Alarm'
    { keyValue = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      alarmState = Prelude.Nothing,
      alarmModelName = Prelude.Nothing,
      severity = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      alarmModelVersion = Prelude.Nothing
    }

-- | The value of the key used as a filter to select only the alarms
-- associated with the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateAlarmModel.html#iotevents-CreateAlarmModel-request-key key>.
alarm_keyValue :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Text)
alarm_keyValue = Lens.lens (\Alarm' {keyValue} -> keyValue) (\s@Alarm' {} a -> s {keyValue = a} :: Alarm)

-- | The time the alarm was created, in the Unix epoch format.
alarm_creationTime :: Lens.Lens' Alarm (Prelude.Maybe Prelude.UTCTime)
alarm_creationTime = Lens.lens (\Alarm' {creationTime} -> creationTime) (\s@Alarm' {} a -> s {creationTime = a} :: Alarm) Prelude.. Lens.mapping Core._Time

-- | Contains information about the current state of the alarm.
alarm_alarmState :: Lens.Lens' Alarm (Prelude.Maybe AlarmState)
alarm_alarmState = Lens.lens (\Alarm' {alarmState} -> alarmState) (\s@Alarm' {} a -> s {alarmState = a} :: Alarm)

-- | The name of the alarm model.
alarm_alarmModelName :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Text)
alarm_alarmModelName = Lens.lens (\Alarm' {alarmModelName} -> alarmModelName) (\s@Alarm' {} a -> s {alarmModelName = a} :: Alarm)

-- | A non-negative integer that reflects the severity level of the alarm.
alarm_severity :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Natural)
alarm_severity = Lens.lens (\Alarm' {severity} -> severity) (\s@Alarm' {} a -> s {severity = a} :: Alarm)

-- | The time the alarm was last updated, in the Unix epoch format.
alarm_lastUpdateTime :: Lens.Lens' Alarm (Prelude.Maybe Prelude.UTCTime)
alarm_lastUpdateTime = Lens.lens (\Alarm' {lastUpdateTime} -> lastUpdateTime) (\s@Alarm' {} a -> s {lastUpdateTime = a} :: Alarm) Prelude.. Lens.mapping Core._Time

-- | The version of the alarm model.
alarm_alarmModelVersion :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Text)
alarm_alarmModelVersion = Lens.lens (\Alarm' {alarmModelVersion} -> alarmModelVersion) (\s@Alarm' {} a -> s {alarmModelVersion = a} :: Alarm)

instance Core.FromJSON Alarm where
  parseJSON =
    Core.withObject
      "Alarm"
      ( \x ->
          Alarm'
            Prelude.<$> (x Core..:? "keyValue")
            Prelude.<*> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "alarmState")
            Prelude.<*> (x Core..:? "alarmModelName")
            Prelude.<*> (x Core..:? "severity")
            Prelude.<*> (x Core..:? "lastUpdateTime")
            Prelude.<*> (x Core..:? "alarmModelVersion")
      )

instance Prelude.Hashable Alarm

instance Prelude.NFData Alarm
