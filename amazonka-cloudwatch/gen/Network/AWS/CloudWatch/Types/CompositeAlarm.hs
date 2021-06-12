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
-- Module      : Network.AWS.CloudWatch.Types.CompositeAlarm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.CompositeAlarm where

import Network.AWS.CloudWatch.Types.StateValue
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The details about a composite alarm.
--
-- /See:/ 'newCompositeAlarm' smart constructor.
data CompositeAlarm = CompositeAlarm'
  { -- | The Amazon Resource Name (ARN) of the alarm.
    alarmArn :: Core.Maybe Core.Text,
    -- | The actions to execute when this alarm transitions to the ALARM state
    -- from any other state. Each action is specified as an Amazon Resource
    -- Name (ARN).
    alarmActions :: Core.Maybe [Core.Text],
    -- | An explanation for the alarm state, in text format.
    stateReason :: Core.Maybe Core.Text,
    -- | An explanation for the alarm state, in JSON format.
    stateReasonData :: Core.Maybe Core.Text,
    -- | The actions to execute when this alarm transitions to the
    -- INSUFFICIENT_DATA state from any other state. Each action is specified
    -- as an Amazon Resource Name (ARN).
    insufficientDataActions :: Core.Maybe [Core.Text],
    -- | The rule that this alarm uses to evaluate its alarm state.
    alarmRule :: Core.Maybe Core.Text,
    -- | The time stamp of the last update to the alarm state.
    stateUpdatedTimestamp :: Core.Maybe Core.ISO8601,
    -- | The state value for the alarm.
    stateValue :: Core.Maybe StateValue,
    -- | The name of the alarm.
    alarmName :: Core.Maybe Core.Text,
    -- | The actions to execute when this alarm transitions to the OK state from
    -- any other state. Each action is specified as an Amazon Resource Name
    -- (ARN).
    oKActions :: Core.Maybe [Core.Text],
    -- | Indicates whether actions should be executed during any changes to the
    -- alarm state.
    actionsEnabled :: Core.Maybe Core.Bool,
    -- | The time stamp of the last update to the alarm configuration.
    alarmConfigurationUpdatedTimestamp :: Core.Maybe Core.ISO8601,
    -- | The description of the alarm.
    alarmDescription :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CompositeAlarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmArn', 'compositeAlarm_alarmArn' - The Amazon Resource Name (ARN) of the alarm.
--
-- 'alarmActions', 'compositeAlarm_alarmActions' - The actions to execute when this alarm transitions to the ALARM state
-- from any other state. Each action is specified as an Amazon Resource
-- Name (ARN).
--
-- 'stateReason', 'compositeAlarm_stateReason' - An explanation for the alarm state, in text format.
--
-- 'stateReasonData', 'compositeAlarm_stateReasonData' - An explanation for the alarm state, in JSON format.
--
-- 'insufficientDataActions', 'compositeAlarm_insufficientDataActions' - The actions to execute when this alarm transitions to the
-- INSUFFICIENT_DATA state from any other state. Each action is specified
-- as an Amazon Resource Name (ARN).
--
-- 'alarmRule', 'compositeAlarm_alarmRule' - The rule that this alarm uses to evaluate its alarm state.
--
-- 'stateUpdatedTimestamp', 'compositeAlarm_stateUpdatedTimestamp' - The time stamp of the last update to the alarm state.
--
-- 'stateValue', 'compositeAlarm_stateValue' - The state value for the alarm.
--
-- 'alarmName', 'compositeAlarm_alarmName' - The name of the alarm.
--
-- 'oKActions', 'compositeAlarm_oKActions' - The actions to execute when this alarm transitions to the OK state from
-- any other state. Each action is specified as an Amazon Resource Name
-- (ARN).
--
-- 'actionsEnabled', 'compositeAlarm_actionsEnabled' - Indicates whether actions should be executed during any changes to the
-- alarm state.
--
-- 'alarmConfigurationUpdatedTimestamp', 'compositeAlarm_alarmConfigurationUpdatedTimestamp' - The time stamp of the last update to the alarm configuration.
--
-- 'alarmDescription', 'compositeAlarm_alarmDescription' - The description of the alarm.
newCompositeAlarm ::
  CompositeAlarm
newCompositeAlarm =
  CompositeAlarm'
    { alarmArn = Core.Nothing,
      alarmActions = Core.Nothing,
      stateReason = Core.Nothing,
      stateReasonData = Core.Nothing,
      insufficientDataActions = Core.Nothing,
      alarmRule = Core.Nothing,
      stateUpdatedTimestamp = Core.Nothing,
      stateValue = Core.Nothing,
      alarmName = Core.Nothing,
      oKActions = Core.Nothing,
      actionsEnabled = Core.Nothing,
      alarmConfigurationUpdatedTimestamp = Core.Nothing,
      alarmDescription = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the alarm.
compositeAlarm_alarmArn :: Lens.Lens' CompositeAlarm (Core.Maybe Core.Text)
compositeAlarm_alarmArn = Lens.lens (\CompositeAlarm' {alarmArn} -> alarmArn) (\s@CompositeAlarm' {} a -> s {alarmArn = a} :: CompositeAlarm)

-- | The actions to execute when this alarm transitions to the ALARM state
-- from any other state. Each action is specified as an Amazon Resource
-- Name (ARN).
compositeAlarm_alarmActions :: Lens.Lens' CompositeAlarm (Core.Maybe [Core.Text])
compositeAlarm_alarmActions = Lens.lens (\CompositeAlarm' {alarmActions} -> alarmActions) (\s@CompositeAlarm' {} a -> s {alarmActions = a} :: CompositeAlarm) Core.. Lens.mapping Lens._Coerce

-- | An explanation for the alarm state, in text format.
compositeAlarm_stateReason :: Lens.Lens' CompositeAlarm (Core.Maybe Core.Text)
compositeAlarm_stateReason = Lens.lens (\CompositeAlarm' {stateReason} -> stateReason) (\s@CompositeAlarm' {} a -> s {stateReason = a} :: CompositeAlarm)

-- | An explanation for the alarm state, in JSON format.
compositeAlarm_stateReasonData :: Lens.Lens' CompositeAlarm (Core.Maybe Core.Text)
compositeAlarm_stateReasonData = Lens.lens (\CompositeAlarm' {stateReasonData} -> stateReasonData) (\s@CompositeAlarm' {} a -> s {stateReasonData = a} :: CompositeAlarm)

-- | The actions to execute when this alarm transitions to the
-- INSUFFICIENT_DATA state from any other state. Each action is specified
-- as an Amazon Resource Name (ARN).
compositeAlarm_insufficientDataActions :: Lens.Lens' CompositeAlarm (Core.Maybe [Core.Text])
compositeAlarm_insufficientDataActions = Lens.lens (\CompositeAlarm' {insufficientDataActions} -> insufficientDataActions) (\s@CompositeAlarm' {} a -> s {insufficientDataActions = a} :: CompositeAlarm) Core.. Lens.mapping Lens._Coerce

-- | The rule that this alarm uses to evaluate its alarm state.
compositeAlarm_alarmRule :: Lens.Lens' CompositeAlarm (Core.Maybe Core.Text)
compositeAlarm_alarmRule = Lens.lens (\CompositeAlarm' {alarmRule} -> alarmRule) (\s@CompositeAlarm' {} a -> s {alarmRule = a} :: CompositeAlarm)

-- | The time stamp of the last update to the alarm state.
compositeAlarm_stateUpdatedTimestamp :: Lens.Lens' CompositeAlarm (Core.Maybe Core.UTCTime)
compositeAlarm_stateUpdatedTimestamp = Lens.lens (\CompositeAlarm' {stateUpdatedTimestamp} -> stateUpdatedTimestamp) (\s@CompositeAlarm' {} a -> s {stateUpdatedTimestamp = a} :: CompositeAlarm) Core.. Lens.mapping Core._Time

-- | The state value for the alarm.
compositeAlarm_stateValue :: Lens.Lens' CompositeAlarm (Core.Maybe StateValue)
compositeAlarm_stateValue = Lens.lens (\CompositeAlarm' {stateValue} -> stateValue) (\s@CompositeAlarm' {} a -> s {stateValue = a} :: CompositeAlarm)

-- | The name of the alarm.
compositeAlarm_alarmName :: Lens.Lens' CompositeAlarm (Core.Maybe Core.Text)
compositeAlarm_alarmName = Lens.lens (\CompositeAlarm' {alarmName} -> alarmName) (\s@CompositeAlarm' {} a -> s {alarmName = a} :: CompositeAlarm)

-- | The actions to execute when this alarm transitions to the OK state from
-- any other state. Each action is specified as an Amazon Resource Name
-- (ARN).
compositeAlarm_oKActions :: Lens.Lens' CompositeAlarm (Core.Maybe [Core.Text])
compositeAlarm_oKActions = Lens.lens (\CompositeAlarm' {oKActions} -> oKActions) (\s@CompositeAlarm' {} a -> s {oKActions = a} :: CompositeAlarm) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether actions should be executed during any changes to the
-- alarm state.
compositeAlarm_actionsEnabled :: Lens.Lens' CompositeAlarm (Core.Maybe Core.Bool)
compositeAlarm_actionsEnabled = Lens.lens (\CompositeAlarm' {actionsEnabled} -> actionsEnabled) (\s@CompositeAlarm' {} a -> s {actionsEnabled = a} :: CompositeAlarm)

-- | The time stamp of the last update to the alarm configuration.
compositeAlarm_alarmConfigurationUpdatedTimestamp :: Lens.Lens' CompositeAlarm (Core.Maybe Core.UTCTime)
compositeAlarm_alarmConfigurationUpdatedTimestamp = Lens.lens (\CompositeAlarm' {alarmConfigurationUpdatedTimestamp} -> alarmConfigurationUpdatedTimestamp) (\s@CompositeAlarm' {} a -> s {alarmConfigurationUpdatedTimestamp = a} :: CompositeAlarm) Core.. Lens.mapping Core._Time

-- | The description of the alarm.
compositeAlarm_alarmDescription :: Lens.Lens' CompositeAlarm (Core.Maybe Core.Text)
compositeAlarm_alarmDescription = Lens.lens (\CompositeAlarm' {alarmDescription} -> alarmDescription) (\s@CompositeAlarm' {} a -> s {alarmDescription = a} :: CompositeAlarm)

instance Core.FromXML CompositeAlarm where
  parseXML x =
    CompositeAlarm'
      Core.<$> (x Core..@? "AlarmArn")
      Core.<*> ( x Core..@? "AlarmActions" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "StateReason")
      Core.<*> (x Core..@? "StateReasonData")
      Core.<*> ( x Core..@? "InsufficientDataActions"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "AlarmRule")
      Core.<*> (x Core..@? "StateUpdatedTimestamp")
      Core.<*> (x Core..@? "StateValue")
      Core.<*> (x Core..@? "AlarmName")
      Core.<*> ( x Core..@? "OKActions" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "ActionsEnabled")
      Core.<*> (x Core..@? "AlarmConfigurationUpdatedTimestamp")
      Core.<*> (x Core..@? "AlarmDescription")

instance Core.Hashable CompositeAlarm

instance Core.NFData CompositeAlarm
