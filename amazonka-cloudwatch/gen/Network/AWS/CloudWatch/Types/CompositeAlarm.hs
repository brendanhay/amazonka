{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details about a composite alarm.
--
-- /See:/ 'newCompositeAlarm' smart constructor.
data CompositeAlarm = CompositeAlarm'
  { -- | The Amazon Resource Name (ARN) of the alarm.
    alarmArn :: Prelude.Maybe Prelude.Text,
    -- | The actions to execute when this alarm transitions to the ALARM state
    -- from any other state. Each action is specified as an Amazon Resource
    -- Name (ARN).
    alarmActions :: Prelude.Maybe [Prelude.Text],
    -- | An explanation for the alarm state, in text format.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | An explanation for the alarm state, in JSON format.
    stateReasonData :: Prelude.Maybe Prelude.Text,
    -- | The actions to execute when this alarm transitions to the
    -- INSUFFICIENT_DATA state from any other state. Each action is specified
    -- as an Amazon Resource Name (ARN).
    insufficientDataActions :: Prelude.Maybe [Prelude.Text],
    -- | The rule that this alarm uses to evaluate its alarm state.
    alarmRule :: Prelude.Maybe Prelude.Text,
    -- | The time stamp of the last update to the alarm state.
    stateUpdatedTimestamp :: Prelude.Maybe Prelude.ISO8601,
    -- | The state value for the alarm.
    stateValue :: Prelude.Maybe StateValue,
    -- | The name of the alarm.
    alarmName :: Prelude.Maybe Prelude.Text,
    -- | The actions to execute when this alarm transitions to the OK state from
    -- any other state. Each action is specified as an Amazon Resource Name
    -- (ARN).
    oKActions :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether actions should be executed during any changes to the
    -- alarm state.
    actionsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The time stamp of the last update to the alarm configuration.
    alarmConfigurationUpdatedTimestamp :: Prelude.Maybe Prelude.ISO8601,
    -- | The description of the alarm.
    alarmDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { alarmArn = Prelude.Nothing,
      alarmActions = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      stateReasonData = Prelude.Nothing,
      insufficientDataActions = Prelude.Nothing,
      alarmRule = Prelude.Nothing,
      stateUpdatedTimestamp = Prelude.Nothing,
      stateValue = Prelude.Nothing,
      alarmName = Prelude.Nothing,
      oKActions = Prelude.Nothing,
      actionsEnabled = Prelude.Nothing,
      alarmConfigurationUpdatedTimestamp = Prelude.Nothing,
      alarmDescription = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the alarm.
compositeAlarm_alarmArn :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Text)
compositeAlarm_alarmArn = Lens.lens (\CompositeAlarm' {alarmArn} -> alarmArn) (\s@CompositeAlarm' {} a -> s {alarmArn = a} :: CompositeAlarm)

-- | The actions to execute when this alarm transitions to the ALARM state
-- from any other state. Each action is specified as an Amazon Resource
-- Name (ARN).
compositeAlarm_alarmActions :: Lens.Lens' CompositeAlarm (Prelude.Maybe [Prelude.Text])
compositeAlarm_alarmActions = Lens.lens (\CompositeAlarm' {alarmActions} -> alarmActions) (\s@CompositeAlarm' {} a -> s {alarmActions = a} :: CompositeAlarm) Prelude.. Lens.mapping Prelude._Coerce

-- | An explanation for the alarm state, in text format.
compositeAlarm_stateReason :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Text)
compositeAlarm_stateReason = Lens.lens (\CompositeAlarm' {stateReason} -> stateReason) (\s@CompositeAlarm' {} a -> s {stateReason = a} :: CompositeAlarm)

-- | An explanation for the alarm state, in JSON format.
compositeAlarm_stateReasonData :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Text)
compositeAlarm_stateReasonData = Lens.lens (\CompositeAlarm' {stateReasonData} -> stateReasonData) (\s@CompositeAlarm' {} a -> s {stateReasonData = a} :: CompositeAlarm)

-- | The actions to execute when this alarm transitions to the
-- INSUFFICIENT_DATA state from any other state. Each action is specified
-- as an Amazon Resource Name (ARN).
compositeAlarm_insufficientDataActions :: Lens.Lens' CompositeAlarm (Prelude.Maybe [Prelude.Text])
compositeAlarm_insufficientDataActions = Lens.lens (\CompositeAlarm' {insufficientDataActions} -> insufficientDataActions) (\s@CompositeAlarm' {} a -> s {insufficientDataActions = a} :: CompositeAlarm) Prelude.. Lens.mapping Prelude._Coerce

-- | The rule that this alarm uses to evaluate its alarm state.
compositeAlarm_alarmRule :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Text)
compositeAlarm_alarmRule = Lens.lens (\CompositeAlarm' {alarmRule} -> alarmRule) (\s@CompositeAlarm' {} a -> s {alarmRule = a} :: CompositeAlarm)

-- | The time stamp of the last update to the alarm state.
compositeAlarm_stateUpdatedTimestamp :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.UTCTime)
compositeAlarm_stateUpdatedTimestamp = Lens.lens (\CompositeAlarm' {stateUpdatedTimestamp} -> stateUpdatedTimestamp) (\s@CompositeAlarm' {} a -> s {stateUpdatedTimestamp = a} :: CompositeAlarm) Prelude.. Lens.mapping Prelude._Time

-- | The state value for the alarm.
compositeAlarm_stateValue :: Lens.Lens' CompositeAlarm (Prelude.Maybe StateValue)
compositeAlarm_stateValue = Lens.lens (\CompositeAlarm' {stateValue} -> stateValue) (\s@CompositeAlarm' {} a -> s {stateValue = a} :: CompositeAlarm)

-- | The name of the alarm.
compositeAlarm_alarmName :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Text)
compositeAlarm_alarmName = Lens.lens (\CompositeAlarm' {alarmName} -> alarmName) (\s@CompositeAlarm' {} a -> s {alarmName = a} :: CompositeAlarm)

-- | The actions to execute when this alarm transitions to the OK state from
-- any other state. Each action is specified as an Amazon Resource Name
-- (ARN).
compositeAlarm_oKActions :: Lens.Lens' CompositeAlarm (Prelude.Maybe [Prelude.Text])
compositeAlarm_oKActions = Lens.lens (\CompositeAlarm' {oKActions} -> oKActions) (\s@CompositeAlarm' {} a -> s {oKActions = a} :: CompositeAlarm) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether actions should be executed during any changes to the
-- alarm state.
compositeAlarm_actionsEnabled :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Bool)
compositeAlarm_actionsEnabled = Lens.lens (\CompositeAlarm' {actionsEnabled} -> actionsEnabled) (\s@CompositeAlarm' {} a -> s {actionsEnabled = a} :: CompositeAlarm)

-- | The time stamp of the last update to the alarm configuration.
compositeAlarm_alarmConfigurationUpdatedTimestamp :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.UTCTime)
compositeAlarm_alarmConfigurationUpdatedTimestamp = Lens.lens (\CompositeAlarm' {alarmConfigurationUpdatedTimestamp} -> alarmConfigurationUpdatedTimestamp) (\s@CompositeAlarm' {} a -> s {alarmConfigurationUpdatedTimestamp = a} :: CompositeAlarm) Prelude.. Lens.mapping Prelude._Time

-- | The description of the alarm.
compositeAlarm_alarmDescription :: Lens.Lens' CompositeAlarm (Prelude.Maybe Prelude.Text)
compositeAlarm_alarmDescription = Lens.lens (\CompositeAlarm' {alarmDescription} -> alarmDescription) (\s@CompositeAlarm' {} a -> s {alarmDescription = a} :: CompositeAlarm)

instance Prelude.FromXML CompositeAlarm where
  parseXML x =
    CompositeAlarm'
      Prelude.<$> (x Prelude..@? "AlarmArn")
      Prelude.<*> ( x Prelude..@? "AlarmActions"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "StateReason")
      Prelude.<*> (x Prelude..@? "StateReasonData")
      Prelude.<*> ( x Prelude..@? "InsufficientDataActions"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "AlarmRule")
      Prelude.<*> (x Prelude..@? "StateUpdatedTimestamp")
      Prelude.<*> (x Prelude..@? "StateValue")
      Prelude.<*> (x Prelude..@? "AlarmName")
      Prelude.<*> ( x Prelude..@? "OKActions" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "ActionsEnabled")
      Prelude.<*> (x Prelude..@? "AlarmConfigurationUpdatedTimestamp")
      Prelude.<*> (x Prelude..@? "AlarmDescription")

instance Prelude.Hashable CompositeAlarm

instance Prelude.NFData CompositeAlarm
