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
-- Module      : Amazonka.CloudWatch.Types.CompositeAlarm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.CompositeAlarm where

import Amazonka.CloudWatch.Types.ActionsSuppressedBy
import Amazonka.CloudWatch.Types.StateValue
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details about a composite alarm.
--
-- /See:/ 'newCompositeAlarm' smart constructor.
data CompositeAlarm = CompositeAlarm'
  { -- | Indicates whether actions should be executed during any changes to the
    -- alarm state.
    actionsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | When the value is @ALARM@, it means that the actions are suppressed
    -- because the suppressor alarm is in @ALARM@ When the value is
    -- @WaitPeriod@, it means that the actions are suppressed because the
    -- composite alarm is waiting for the suppressor alarm to go into into the
    -- @ALARM@ state. The maximum waiting time is as specified in
    -- @ActionsSuppressorWaitPeriod@. After this time, the composite alarm
    -- performs its actions. When the value is @ExtensionPeriod@, it means that
    -- the actions are suppressed because the composite alarm is waiting after
    -- the suppressor alarm went out of the @ALARM@ state. The maximum waiting
    -- time is as specified in @ActionsSuppressorExtensionPeriod@. After this
    -- time, the composite alarm performs its actions.
    actionsSuppressedBy :: Prelude.Maybe ActionsSuppressedBy,
    -- | Captures the reason for action suppression.
    actionsSuppressedReason :: Prelude.Maybe Prelude.Text,
    -- | Actions will be suppressed if the suppressor alarm is in the @ALARM@
    -- state. @ActionsSuppressor@ can be an AlarmName or an Amazon Resource
    -- Name (ARN) from an existing alarm.
    actionsSuppressor :: Prelude.Maybe Prelude.Text,
    -- | The maximum time in seconds that the composite alarm waits after
    -- suppressor alarm goes out of the @ALARM@ state. After this time, the
    -- composite alarm performs its actions.
    --
    -- @ExtensionPeriod@ is required only when @ActionsSuppressor@ is
    -- specified.
    actionsSuppressorExtensionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The maximum time in seconds that the composite alarm waits for the
    -- suppressor alarm to go into the @ALARM@ state. After this time, the
    -- composite alarm performs its actions.
    --
    -- @WaitPeriod@ is required only when @ActionsSuppressor@ is specified.
    actionsSuppressorWaitPeriod :: Prelude.Maybe Prelude.Int,
    -- | The actions to execute when this alarm transitions to the ALARM state
    -- from any other state. Each action is specified as an Amazon Resource
    -- Name (ARN).
    alarmActions :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the alarm.
    alarmArn :: Prelude.Maybe Prelude.Text,
    -- | The time stamp of the last update to the alarm configuration.
    alarmConfigurationUpdatedTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The description of the alarm.
    alarmDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the alarm.
    alarmName :: Prelude.Maybe Prelude.Text,
    -- | The rule that this alarm uses to evaluate its alarm state.
    alarmRule :: Prelude.Maybe Prelude.Text,
    -- | The actions to execute when this alarm transitions to the
    -- INSUFFICIENT_DATA state from any other state. Each action is specified
    -- as an Amazon Resource Name (ARN).
    insufficientDataActions :: Prelude.Maybe [Prelude.Text],
    -- | The actions to execute when this alarm transitions to the OK state from
    -- any other state. Each action is specified as an Amazon Resource Name
    -- (ARN).
    oKActions :: Prelude.Maybe [Prelude.Text],
    -- | An explanation for the alarm state, in text format.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | An explanation for the alarm state, in JSON format.
    stateReasonData :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the last change to the alarm\'s @StateValue@.
    stateTransitionedTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | Tracks the timestamp of any state update, even if @StateValue@ doesn\'t
    -- change.
    stateUpdatedTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The state value for the alarm.
    stateValue :: Prelude.Maybe StateValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompositeAlarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionsEnabled', 'compositeAlarm_actionsEnabled' - Indicates whether actions should be executed during any changes to the
-- alarm state.
--
-- 'actionsSuppressedBy', 'compositeAlarm_actionsSuppressedBy' - When the value is @ALARM@, it means that the actions are suppressed
-- because the suppressor alarm is in @ALARM@ When the value is
-- @WaitPeriod@, it means that the actions are suppressed because the
-- composite alarm is waiting for the suppressor alarm to go into into the
-- @ALARM@ state. The maximum waiting time is as specified in
-- @ActionsSuppressorWaitPeriod@. After this time, the composite alarm
-- performs its actions. When the value is @ExtensionPeriod@, it means that
-- the actions are suppressed because the composite alarm is waiting after
-- the suppressor alarm went out of the @ALARM@ state. The maximum waiting
-- time is as specified in @ActionsSuppressorExtensionPeriod@. After this
-- time, the composite alarm performs its actions.
--
-- 'actionsSuppressedReason', 'compositeAlarm_actionsSuppressedReason' - Captures the reason for action suppression.
--
-- 'actionsSuppressor', 'compositeAlarm_actionsSuppressor' - Actions will be suppressed if the suppressor alarm is in the @ALARM@
-- state. @ActionsSuppressor@ can be an AlarmName or an Amazon Resource
-- Name (ARN) from an existing alarm.
--
-- 'actionsSuppressorExtensionPeriod', 'compositeAlarm_actionsSuppressorExtensionPeriod' - The maximum time in seconds that the composite alarm waits after
-- suppressor alarm goes out of the @ALARM@ state. After this time, the
-- composite alarm performs its actions.
--
-- @ExtensionPeriod@ is required only when @ActionsSuppressor@ is
-- specified.
--
-- 'actionsSuppressorWaitPeriod', 'compositeAlarm_actionsSuppressorWaitPeriod' - The maximum time in seconds that the composite alarm waits for the
-- suppressor alarm to go into the @ALARM@ state. After this time, the
-- composite alarm performs its actions.
--
-- @WaitPeriod@ is required only when @ActionsSuppressor@ is specified.
--
-- 'alarmActions', 'compositeAlarm_alarmActions' - The actions to execute when this alarm transitions to the ALARM state
-- from any other state. Each action is specified as an Amazon Resource
-- Name (ARN).
--
-- 'alarmArn', 'compositeAlarm_alarmArn' - The Amazon Resource Name (ARN) of the alarm.
--
-- 'alarmConfigurationUpdatedTimestamp', 'compositeAlarm_alarmConfigurationUpdatedTimestamp' - The time stamp of the last update to the alarm configuration.
--
-- 'alarmDescription', 'compositeAlarm_alarmDescription' - The description of the alarm.
--
-- 'alarmName', 'compositeAlarm_alarmName' - The name of the alarm.
--
-- 'alarmRule', 'compositeAlarm_alarmRule' - The rule that this alarm uses to evaluate its alarm state.
--
-- 'insufficientDataActions', 'compositeAlarm_insufficientDataActions' - The actions to execute when this alarm transitions to the
-- INSUFFICIENT_DATA state from any other state. Each action is specified
-- as an Amazon Resource Name (ARN).
--
-- 'oKActions', 'compositeAlarm_oKActions' - The actions to execute when this alarm transitions to the OK state from
-- any other state. Each action is specified as an Amazon Resource Name
-- (ARN).
--
-- 'stateReason', 'compositeAlarm_stateReason' - An explanation for the alarm state, in text format.
--
-- 'stateReasonData', 'compositeAlarm_stateReasonData' - An explanation for the alarm state, in JSON format.
--
-- 'stateTransitionedTimestamp', 'compositeAlarm_stateTransitionedTimestamp' - The timestamp of the last change to the alarm\'s @StateValue@.
--
-- 'stateUpdatedTimestamp', 'compositeAlarm_stateUpdatedTimestamp' - Tracks the timestamp of any state update, even if @StateValue@ doesn\'t
-- change.
--
-- 'stateValue', 'compositeAlarm_stateValue' - The state value for the alarm.
newCompositeAlarm ::
  CompositeAlarm
newCompositeAlarm =
  CompositeAlarm'
    { actionsEnabled = Prelude.Nothing,
      actionsSuppressedBy = Prelude.Nothing,
      actionsSuppressedReason = Prelude.Nothing,
      actionsSuppressor = Prelude.Nothing,
      actionsSuppressorExtensionPeriod = Prelude.Nothing,
      actionsSuppressorWaitPeriod = Prelude.Nothing,
      alarmActions = Prelude.Nothing,
      alarmArn = Prelude.Nothing,
      alarmConfigurationUpdatedTimestamp = Prelude.Nothing,
      alarmDescription = Prelude.Nothing,
      alarmName = Prelude.Nothing,
      alarmRule = Prelude.Nothing,
      insufficientDataActions = Prelude.Nothing,
      oKActions = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      stateReasonData = Prelude.Nothing,
      stateTransitionedTimestamp = Prelude.Nothing,
      stateUpdatedTimestamp = Prelude.Nothing,
      stateValue = Prelude.Nothing
    }

-- | Indicates whether actions should be executed during any changes to the
-- alarm state.
compositeAlarm_actionsEnabled :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Bool)
compositeAlarm_actionsEnabled = Lens.lens (\CompositeAlarm' {actionsEnabled} -> actionsEnabled) (\s@CompositeAlarm' {} a -> s {actionsEnabled = a} :: CompositeAlarm)

-- | When the value is @ALARM@, it means that the actions are suppressed
-- because the suppressor alarm is in @ALARM@ When the value is
-- @WaitPeriod@, it means that the actions are suppressed because the
-- composite alarm is waiting for the suppressor alarm to go into into the
-- @ALARM@ state. The maximum waiting time is as specified in
-- @ActionsSuppressorWaitPeriod@. After this time, the composite alarm
-- performs its actions. When the value is @ExtensionPeriod@, it means that
-- the actions are suppressed because the composite alarm is waiting after
-- the suppressor alarm went out of the @ALARM@ state. The maximum waiting
-- time is as specified in @ActionsSuppressorExtensionPeriod@. After this
-- time, the composite alarm performs its actions.
compositeAlarm_actionsSuppressedBy :: Lens.Lens' CompositeAlarm (Prelude.Maybe ActionsSuppressedBy)
compositeAlarm_actionsSuppressedBy = Lens.lens (\CompositeAlarm' {actionsSuppressedBy} -> actionsSuppressedBy) (\s@CompositeAlarm' {} a -> s {actionsSuppressedBy = a} :: CompositeAlarm)

-- | Captures the reason for action suppression.
compositeAlarm_actionsSuppressedReason :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Text)
compositeAlarm_actionsSuppressedReason = Lens.lens (\CompositeAlarm' {actionsSuppressedReason} -> actionsSuppressedReason) (\s@CompositeAlarm' {} a -> s {actionsSuppressedReason = a} :: CompositeAlarm)

-- | Actions will be suppressed if the suppressor alarm is in the @ALARM@
-- state. @ActionsSuppressor@ can be an AlarmName or an Amazon Resource
-- Name (ARN) from an existing alarm.
compositeAlarm_actionsSuppressor :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Text)
compositeAlarm_actionsSuppressor = Lens.lens (\CompositeAlarm' {actionsSuppressor} -> actionsSuppressor) (\s@CompositeAlarm' {} a -> s {actionsSuppressor = a} :: CompositeAlarm)

-- | The maximum time in seconds that the composite alarm waits after
-- suppressor alarm goes out of the @ALARM@ state. After this time, the
-- composite alarm performs its actions.
--
-- @ExtensionPeriod@ is required only when @ActionsSuppressor@ is
-- specified.
compositeAlarm_actionsSuppressorExtensionPeriod :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Int)
compositeAlarm_actionsSuppressorExtensionPeriod = Lens.lens (\CompositeAlarm' {actionsSuppressorExtensionPeriod} -> actionsSuppressorExtensionPeriod) (\s@CompositeAlarm' {} a -> s {actionsSuppressorExtensionPeriod = a} :: CompositeAlarm)

-- | The maximum time in seconds that the composite alarm waits for the
-- suppressor alarm to go into the @ALARM@ state. After this time, the
-- composite alarm performs its actions.
--
-- @WaitPeriod@ is required only when @ActionsSuppressor@ is specified.
compositeAlarm_actionsSuppressorWaitPeriod :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Int)
compositeAlarm_actionsSuppressorWaitPeriod = Lens.lens (\CompositeAlarm' {actionsSuppressorWaitPeriod} -> actionsSuppressorWaitPeriod) (\s@CompositeAlarm' {} a -> s {actionsSuppressorWaitPeriod = a} :: CompositeAlarm)

-- | The actions to execute when this alarm transitions to the ALARM state
-- from any other state. Each action is specified as an Amazon Resource
-- Name (ARN).
compositeAlarm_alarmActions :: Lens.Lens' CompositeAlarm (Prelude.Maybe [Prelude.Text])
compositeAlarm_alarmActions = Lens.lens (\CompositeAlarm' {alarmActions} -> alarmActions) (\s@CompositeAlarm' {} a -> s {alarmActions = a} :: CompositeAlarm) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the alarm.
compositeAlarm_alarmArn :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Text)
compositeAlarm_alarmArn = Lens.lens (\CompositeAlarm' {alarmArn} -> alarmArn) (\s@CompositeAlarm' {} a -> s {alarmArn = a} :: CompositeAlarm)

-- | The time stamp of the last update to the alarm configuration.
compositeAlarm_alarmConfigurationUpdatedTimestamp :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.UTCTime)
compositeAlarm_alarmConfigurationUpdatedTimestamp = Lens.lens (\CompositeAlarm' {alarmConfigurationUpdatedTimestamp} -> alarmConfigurationUpdatedTimestamp) (\s@CompositeAlarm' {} a -> s {alarmConfigurationUpdatedTimestamp = a} :: CompositeAlarm) Prelude.. Lens.mapping Data._Time

-- | The description of the alarm.
compositeAlarm_alarmDescription :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Text)
compositeAlarm_alarmDescription = Lens.lens (\CompositeAlarm' {alarmDescription} -> alarmDescription) (\s@CompositeAlarm' {} a -> s {alarmDescription = a} :: CompositeAlarm)

-- | The name of the alarm.
compositeAlarm_alarmName :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Text)
compositeAlarm_alarmName = Lens.lens (\CompositeAlarm' {alarmName} -> alarmName) (\s@CompositeAlarm' {} a -> s {alarmName = a} :: CompositeAlarm)

-- | The rule that this alarm uses to evaluate its alarm state.
compositeAlarm_alarmRule :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Text)
compositeAlarm_alarmRule = Lens.lens (\CompositeAlarm' {alarmRule} -> alarmRule) (\s@CompositeAlarm' {} a -> s {alarmRule = a} :: CompositeAlarm)

-- | The actions to execute when this alarm transitions to the
-- INSUFFICIENT_DATA state from any other state. Each action is specified
-- as an Amazon Resource Name (ARN).
compositeAlarm_insufficientDataActions :: Lens.Lens' CompositeAlarm (Prelude.Maybe [Prelude.Text])
compositeAlarm_insufficientDataActions = Lens.lens (\CompositeAlarm' {insufficientDataActions} -> insufficientDataActions) (\s@CompositeAlarm' {} a -> s {insufficientDataActions = a} :: CompositeAlarm) Prelude.. Lens.mapping Lens.coerced

-- | The actions to execute when this alarm transitions to the OK state from
-- any other state. Each action is specified as an Amazon Resource Name
-- (ARN).
compositeAlarm_oKActions :: Lens.Lens' CompositeAlarm (Prelude.Maybe [Prelude.Text])
compositeAlarm_oKActions = Lens.lens (\CompositeAlarm' {oKActions} -> oKActions) (\s@CompositeAlarm' {} a -> s {oKActions = a} :: CompositeAlarm) Prelude.. Lens.mapping Lens.coerced

-- | An explanation for the alarm state, in text format.
compositeAlarm_stateReason :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Text)
compositeAlarm_stateReason = Lens.lens (\CompositeAlarm' {stateReason} -> stateReason) (\s@CompositeAlarm' {} a -> s {stateReason = a} :: CompositeAlarm)

-- | An explanation for the alarm state, in JSON format.
compositeAlarm_stateReasonData :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Text)
compositeAlarm_stateReasonData = Lens.lens (\CompositeAlarm' {stateReasonData} -> stateReasonData) (\s@CompositeAlarm' {} a -> s {stateReasonData = a} :: CompositeAlarm)

-- | The timestamp of the last change to the alarm\'s @StateValue@.
compositeAlarm_stateTransitionedTimestamp :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.UTCTime)
compositeAlarm_stateTransitionedTimestamp = Lens.lens (\CompositeAlarm' {stateTransitionedTimestamp} -> stateTransitionedTimestamp) (\s@CompositeAlarm' {} a -> s {stateTransitionedTimestamp = a} :: CompositeAlarm) Prelude.. Lens.mapping Data._Time

-- | Tracks the timestamp of any state update, even if @StateValue@ doesn\'t
-- change.
compositeAlarm_stateUpdatedTimestamp :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.UTCTime)
compositeAlarm_stateUpdatedTimestamp = Lens.lens (\CompositeAlarm' {stateUpdatedTimestamp} -> stateUpdatedTimestamp) (\s@CompositeAlarm' {} a -> s {stateUpdatedTimestamp = a} :: CompositeAlarm) Prelude.. Lens.mapping Data._Time

-- | The state value for the alarm.
compositeAlarm_stateValue :: Lens.Lens' CompositeAlarm (Prelude.Maybe StateValue)
compositeAlarm_stateValue = Lens.lens (\CompositeAlarm' {stateValue} -> stateValue) (\s@CompositeAlarm' {} a -> s {stateValue = a} :: CompositeAlarm)

instance Data.FromXML CompositeAlarm where
  parseXML x =
    CompositeAlarm'
      Prelude.<$> (x Data..@? "ActionsEnabled")
      Prelude.<*> (x Data..@? "ActionsSuppressedBy")
      Prelude.<*> (x Data..@? "ActionsSuppressedReason")
      Prelude.<*> (x Data..@? "ActionsSuppressor")
      Prelude.<*> (x Data..@? "ActionsSuppressorExtensionPeriod")
      Prelude.<*> (x Data..@? "ActionsSuppressorWaitPeriod")
      Prelude.<*> ( x Data..@? "AlarmActions" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "AlarmArn")
      Prelude.<*> (x Data..@? "AlarmConfigurationUpdatedTimestamp")
      Prelude.<*> (x Data..@? "AlarmDescription")
      Prelude.<*> (x Data..@? "AlarmName")
      Prelude.<*> (x Data..@? "AlarmRule")
      Prelude.<*> ( x
                      Data..@? "InsufficientDataActions"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x Data..@? "OKActions" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "StateReason")
      Prelude.<*> (x Data..@? "StateReasonData")
      Prelude.<*> (x Data..@? "StateTransitionedTimestamp")
      Prelude.<*> (x Data..@? "StateUpdatedTimestamp")
      Prelude.<*> (x Data..@? "StateValue")

instance Prelude.Hashable CompositeAlarm where
  hashWithSalt _salt CompositeAlarm' {..} =
    _salt
      `Prelude.hashWithSalt` actionsEnabled
      `Prelude.hashWithSalt` actionsSuppressedBy
      `Prelude.hashWithSalt` actionsSuppressedReason
      `Prelude.hashWithSalt` actionsSuppressor
      `Prelude.hashWithSalt` actionsSuppressorExtensionPeriod
      `Prelude.hashWithSalt` actionsSuppressorWaitPeriod
      `Prelude.hashWithSalt` alarmActions
      `Prelude.hashWithSalt` alarmArn
      `Prelude.hashWithSalt` alarmConfigurationUpdatedTimestamp
      `Prelude.hashWithSalt` alarmDescription
      `Prelude.hashWithSalt` alarmName
      `Prelude.hashWithSalt` alarmRule
      `Prelude.hashWithSalt` insufficientDataActions
      `Prelude.hashWithSalt` oKActions
      `Prelude.hashWithSalt` stateReason
      `Prelude.hashWithSalt` stateReasonData
      `Prelude.hashWithSalt` stateTransitionedTimestamp
      `Prelude.hashWithSalt` stateUpdatedTimestamp
      `Prelude.hashWithSalt` stateValue

instance Prelude.NFData CompositeAlarm where
  rnf CompositeAlarm' {..} =
    Prelude.rnf actionsEnabled
      `Prelude.seq` Prelude.rnf actionsSuppressedBy
      `Prelude.seq` Prelude.rnf actionsSuppressedReason
      `Prelude.seq` Prelude.rnf actionsSuppressor
      `Prelude.seq` Prelude.rnf actionsSuppressorExtensionPeriod
      `Prelude.seq` Prelude.rnf actionsSuppressorWaitPeriod
      `Prelude.seq` Prelude.rnf alarmActions
      `Prelude.seq` Prelude.rnf alarmArn
      `Prelude.seq` Prelude.rnf alarmConfigurationUpdatedTimestamp
      `Prelude.seq` Prelude.rnf alarmDescription
      `Prelude.seq` Prelude.rnf alarmName
      `Prelude.seq` Prelude.rnf alarmRule
      `Prelude.seq` Prelude.rnf insufficientDataActions
      `Prelude.seq` Prelude.rnf oKActions
      `Prelude.seq` Prelude.rnf stateReason
      `Prelude.seq` Prelude.rnf stateReasonData
      `Prelude.seq` Prelude.rnf
        stateTransitionedTimestamp
      `Prelude.seq` Prelude.rnf stateUpdatedTimestamp
      `Prelude.seq` Prelude.rnf stateValue
