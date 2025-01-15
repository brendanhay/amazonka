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
-- Module      : Amazonka.IoTEventsData.Types.AlarmSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.AlarmSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEventsData.Types.AlarmStateName
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of an alarm.
--
-- /See:/ 'newAlarmSummary' smart constructor.
data AlarmSummary = AlarmSummary'
  { -- | The name of the alarm model.
    alarmModelName :: Prelude.Maybe Prelude.Text,
    -- | The version of the alarm model.
    alarmModelVersion :: Prelude.Maybe Prelude.Text,
    -- | The time the alarm was created, in the Unix epoch format.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The value of the key used as a filter to select only the alarms
    -- associated with the
    -- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateAlarmModel.html#iotevents-CreateAlarmModel-request-key key>.
    keyValue :: Prelude.Maybe Prelude.Text,
    -- | The time the alarm was last updated, in the Unix epoch format.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the alarm state. The state name can be one of the following
    -- values:
    --
    -- -   @DISABLED@ - When the alarm is in the @DISABLED@ state, it isn\'t
    --     ready to evaluate data. To enable the alarm, you must change the
    --     alarm to the @NORMAL@ state.
    --
    -- -   @NORMAL@ - When the alarm is in the @NORMAL@ state, it\'s ready to
    --     evaluate data.
    --
    -- -   @ACTIVE@ - If the alarm is in the @ACTIVE@ state, the alarm is
    --     invoked.
    --
    -- -   @ACKNOWLEDGED@ - When the alarm is in the @ACKNOWLEDGED@ state, the
    --     alarm was invoked and you acknowledged the alarm.
    --
    -- -   @SNOOZE_DISABLED@ - When the alarm is in the @SNOOZE_DISABLED@
    --     state, the alarm is disabled for a specified period of time. After
    --     the snooze time, the alarm automatically changes to the @NORMAL@
    --     state.
    --
    -- -   @LATCHED@ - When the alarm is in the @LATCHED@ state, the alarm was
    --     invoked. However, the data that the alarm is currently evaluating is
    --     within the specified range. To change the alarm to the @NORMAL@
    --     state, you must acknowledge the alarm.
    stateName :: Prelude.Maybe AlarmStateName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlarmSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmModelName', 'alarmSummary_alarmModelName' - The name of the alarm model.
--
-- 'alarmModelVersion', 'alarmSummary_alarmModelVersion' - The version of the alarm model.
--
-- 'creationTime', 'alarmSummary_creationTime' - The time the alarm was created, in the Unix epoch format.
--
-- 'keyValue', 'alarmSummary_keyValue' - The value of the key used as a filter to select only the alarms
-- associated with the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateAlarmModel.html#iotevents-CreateAlarmModel-request-key key>.
--
-- 'lastUpdateTime', 'alarmSummary_lastUpdateTime' - The time the alarm was last updated, in the Unix epoch format.
--
-- 'stateName', 'alarmSummary_stateName' - The name of the alarm state. The state name can be one of the following
-- values:
--
-- -   @DISABLED@ - When the alarm is in the @DISABLED@ state, it isn\'t
--     ready to evaluate data. To enable the alarm, you must change the
--     alarm to the @NORMAL@ state.
--
-- -   @NORMAL@ - When the alarm is in the @NORMAL@ state, it\'s ready to
--     evaluate data.
--
-- -   @ACTIVE@ - If the alarm is in the @ACTIVE@ state, the alarm is
--     invoked.
--
-- -   @ACKNOWLEDGED@ - When the alarm is in the @ACKNOWLEDGED@ state, the
--     alarm was invoked and you acknowledged the alarm.
--
-- -   @SNOOZE_DISABLED@ - When the alarm is in the @SNOOZE_DISABLED@
--     state, the alarm is disabled for a specified period of time. After
--     the snooze time, the alarm automatically changes to the @NORMAL@
--     state.
--
-- -   @LATCHED@ - When the alarm is in the @LATCHED@ state, the alarm was
--     invoked. However, the data that the alarm is currently evaluating is
--     within the specified range. To change the alarm to the @NORMAL@
--     state, you must acknowledge the alarm.
newAlarmSummary ::
  AlarmSummary
newAlarmSummary =
  AlarmSummary'
    { alarmModelName = Prelude.Nothing,
      alarmModelVersion = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      keyValue = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      stateName = Prelude.Nothing
    }

-- | The name of the alarm model.
alarmSummary_alarmModelName :: Lens.Lens' AlarmSummary (Prelude.Maybe Prelude.Text)
alarmSummary_alarmModelName = Lens.lens (\AlarmSummary' {alarmModelName} -> alarmModelName) (\s@AlarmSummary' {} a -> s {alarmModelName = a} :: AlarmSummary)

-- | The version of the alarm model.
alarmSummary_alarmModelVersion :: Lens.Lens' AlarmSummary (Prelude.Maybe Prelude.Text)
alarmSummary_alarmModelVersion = Lens.lens (\AlarmSummary' {alarmModelVersion} -> alarmModelVersion) (\s@AlarmSummary' {} a -> s {alarmModelVersion = a} :: AlarmSummary)

-- | The time the alarm was created, in the Unix epoch format.
alarmSummary_creationTime :: Lens.Lens' AlarmSummary (Prelude.Maybe Prelude.UTCTime)
alarmSummary_creationTime = Lens.lens (\AlarmSummary' {creationTime} -> creationTime) (\s@AlarmSummary' {} a -> s {creationTime = a} :: AlarmSummary) Prelude.. Lens.mapping Data._Time

-- | The value of the key used as a filter to select only the alarms
-- associated with the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateAlarmModel.html#iotevents-CreateAlarmModel-request-key key>.
alarmSummary_keyValue :: Lens.Lens' AlarmSummary (Prelude.Maybe Prelude.Text)
alarmSummary_keyValue = Lens.lens (\AlarmSummary' {keyValue} -> keyValue) (\s@AlarmSummary' {} a -> s {keyValue = a} :: AlarmSummary)

-- | The time the alarm was last updated, in the Unix epoch format.
alarmSummary_lastUpdateTime :: Lens.Lens' AlarmSummary (Prelude.Maybe Prelude.UTCTime)
alarmSummary_lastUpdateTime = Lens.lens (\AlarmSummary' {lastUpdateTime} -> lastUpdateTime) (\s@AlarmSummary' {} a -> s {lastUpdateTime = a} :: AlarmSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the alarm state. The state name can be one of the following
-- values:
--
-- -   @DISABLED@ - When the alarm is in the @DISABLED@ state, it isn\'t
--     ready to evaluate data. To enable the alarm, you must change the
--     alarm to the @NORMAL@ state.
--
-- -   @NORMAL@ - When the alarm is in the @NORMAL@ state, it\'s ready to
--     evaluate data.
--
-- -   @ACTIVE@ - If the alarm is in the @ACTIVE@ state, the alarm is
--     invoked.
--
-- -   @ACKNOWLEDGED@ - When the alarm is in the @ACKNOWLEDGED@ state, the
--     alarm was invoked and you acknowledged the alarm.
--
-- -   @SNOOZE_DISABLED@ - When the alarm is in the @SNOOZE_DISABLED@
--     state, the alarm is disabled for a specified period of time. After
--     the snooze time, the alarm automatically changes to the @NORMAL@
--     state.
--
-- -   @LATCHED@ - When the alarm is in the @LATCHED@ state, the alarm was
--     invoked. However, the data that the alarm is currently evaluating is
--     within the specified range. To change the alarm to the @NORMAL@
--     state, you must acknowledge the alarm.
alarmSummary_stateName :: Lens.Lens' AlarmSummary (Prelude.Maybe AlarmStateName)
alarmSummary_stateName = Lens.lens (\AlarmSummary' {stateName} -> stateName) (\s@AlarmSummary' {} a -> s {stateName = a} :: AlarmSummary)

instance Data.FromJSON AlarmSummary where
  parseJSON =
    Data.withObject
      "AlarmSummary"
      ( \x ->
          AlarmSummary'
            Prelude.<$> (x Data..:? "alarmModelName")
            Prelude.<*> (x Data..:? "alarmModelVersion")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "keyValue")
            Prelude.<*> (x Data..:? "lastUpdateTime")
            Prelude.<*> (x Data..:? "stateName")
      )

instance Prelude.Hashable AlarmSummary where
  hashWithSalt _salt AlarmSummary' {..} =
    _salt
      `Prelude.hashWithSalt` alarmModelName
      `Prelude.hashWithSalt` alarmModelVersion
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` keyValue
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` stateName

instance Prelude.NFData AlarmSummary where
  rnf AlarmSummary' {..} =
    Prelude.rnf alarmModelName `Prelude.seq`
      Prelude.rnf alarmModelVersion `Prelude.seq`
        Prelude.rnf creationTime `Prelude.seq`
          Prelude.rnf keyValue `Prelude.seq`
            Prelude.rnf lastUpdateTime `Prelude.seq`
              Prelude.rnf stateName
