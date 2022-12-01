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
-- Module      : Amazonka.IoTEventsData.Types.AlarmState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.AlarmState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEventsData.Types.AlarmStateName
import Amazonka.IoTEventsData.Types.CustomerAction
import Amazonka.IoTEventsData.Types.RuleEvaluation
import Amazonka.IoTEventsData.Types.SystemEvent
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the current state of the alarm.
--
-- /See:/ 'newAlarmState' smart constructor.
data AlarmState = AlarmState'
  { -- | Information needed to evaluate data.
    ruleEvaluation :: Prelude.Maybe RuleEvaluation,
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
    stateName :: Prelude.Maybe AlarmStateName,
    -- | Contains information about alarm state changes.
    systemEvent :: Prelude.Maybe SystemEvent,
    -- | Contains information about the action that you can take to respond to
    -- the alarm.
    customerAction :: Prelude.Maybe CustomerAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlarmState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleEvaluation', 'alarmState_ruleEvaluation' - Information needed to evaluate data.
--
-- 'stateName', 'alarmState_stateName' - The name of the alarm state. The state name can be one of the following
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
--
-- 'systemEvent', 'alarmState_systemEvent' - Contains information about alarm state changes.
--
-- 'customerAction', 'alarmState_customerAction' - Contains information about the action that you can take to respond to
-- the alarm.
newAlarmState ::
  AlarmState
newAlarmState =
  AlarmState'
    { ruleEvaluation = Prelude.Nothing,
      stateName = Prelude.Nothing,
      systemEvent = Prelude.Nothing,
      customerAction = Prelude.Nothing
    }

-- | Information needed to evaluate data.
alarmState_ruleEvaluation :: Lens.Lens' AlarmState (Prelude.Maybe RuleEvaluation)
alarmState_ruleEvaluation = Lens.lens (\AlarmState' {ruleEvaluation} -> ruleEvaluation) (\s@AlarmState' {} a -> s {ruleEvaluation = a} :: AlarmState)

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
alarmState_stateName :: Lens.Lens' AlarmState (Prelude.Maybe AlarmStateName)
alarmState_stateName = Lens.lens (\AlarmState' {stateName} -> stateName) (\s@AlarmState' {} a -> s {stateName = a} :: AlarmState)

-- | Contains information about alarm state changes.
alarmState_systemEvent :: Lens.Lens' AlarmState (Prelude.Maybe SystemEvent)
alarmState_systemEvent = Lens.lens (\AlarmState' {systemEvent} -> systemEvent) (\s@AlarmState' {} a -> s {systemEvent = a} :: AlarmState)

-- | Contains information about the action that you can take to respond to
-- the alarm.
alarmState_customerAction :: Lens.Lens' AlarmState (Prelude.Maybe CustomerAction)
alarmState_customerAction = Lens.lens (\AlarmState' {customerAction} -> customerAction) (\s@AlarmState' {} a -> s {customerAction = a} :: AlarmState)

instance Core.FromJSON AlarmState where
  parseJSON =
    Core.withObject
      "AlarmState"
      ( \x ->
          AlarmState'
            Prelude.<$> (x Core..:? "ruleEvaluation")
            Prelude.<*> (x Core..:? "stateName")
            Prelude.<*> (x Core..:? "systemEvent")
            Prelude.<*> (x Core..:? "customerAction")
      )

instance Prelude.Hashable AlarmState where
  hashWithSalt _salt AlarmState' {..} =
    _salt `Prelude.hashWithSalt` ruleEvaluation
      `Prelude.hashWithSalt` stateName
      `Prelude.hashWithSalt` systemEvent
      `Prelude.hashWithSalt` customerAction

instance Prelude.NFData AlarmState where
  rnf AlarmState' {..} =
    Prelude.rnf ruleEvaluation
      `Prelude.seq` Prelude.rnf stateName
      `Prelude.seq` Prelude.rnf systemEvent
      `Prelude.seq` Prelude.rnf customerAction
