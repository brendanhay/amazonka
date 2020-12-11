-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.CompositeAlarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.CompositeAlarm
  ( CompositeAlarm (..),

    -- * Smart constructor
    mkCompositeAlarm,

    -- * Lenses
    caAlarmName,
    caStateUpdatedTimestamp,
    caAlarmDescription,
    caAlarmRule,
    caOKActions,
    caStateValue,
    caAlarmConfigurationUpdatedTimestamp,
    caActionsEnabled,
    caInsufficientDataActions,
    caStateReason,
    caStateReasonData,
    caAlarmARN,
    caAlarmActions,
  )
where

import Network.AWS.CloudWatch.Types.StateValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details about a composite alarm.
--
-- /See:/ 'mkCompositeAlarm' smart constructor.
data CompositeAlarm = CompositeAlarm'
  { alarmName ::
      Lude.Maybe Lude.Text,
    stateUpdatedTimestamp :: Lude.Maybe Lude.ISO8601,
    alarmDescription :: Lude.Maybe Lude.Text,
    alarmRule :: Lude.Maybe Lude.Text,
    okActions :: Lude.Maybe [Lude.Text],
    stateValue :: Lude.Maybe StateValue,
    alarmConfigurationUpdatedTimestamp :: Lude.Maybe Lude.ISO8601,
    actionsEnabled :: Lude.Maybe Lude.Bool,
    insufficientDataActions :: Lude.Maybe [Lude.Text],
    stateReason :: Lude.Maybe Lude.Text,
    stateReasonData :: Lude.Maybe Lude.Text,
    alarmARN :: Lude.Maybe Lude.Text,
    alarmActions :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompositeAlarm' with the minimum fields required to make a request.
--
-- * 'actionsEnabled' - Indicates whether actions should be executed during any changes to the alarm state.
-- * 'alarmARN' - The Amazon Resource Name (ARN) of the alarm.
-- * 'alarmActions' - The actions to execute when this alarm transitions to the ALARM state from any other state. Each action is specified as an Amazon Resource Name (ARN).
-- * 'alarmConfigurationUpdatedTimestamp' - The time stamp of the last update to the alarm configuration.
-- * 'alarmDescription' - The description of the alarm.
-- * 'alarmName' - The name of the alarm.
-- * 'alarmRule' - The rule that this alarm uses to evaluate its alarm state.
-- * 'insufficientDataActions' - The actions to execute when this alarm transitions to the INSUFFICIENT_DATA state from any other state. Each action is specified as an Amazon Resource Name (ARN).
-- * 'okActions' - The actions to execute when this alarm transitions to the OK state from any other state. Each action is specified as an Amazon Resource Name (ARN).
-- * 'stateReason' - An explanation for the alarm state, in text format.
-- * 'stateReasonData' - An explanation for the alarm state, in JSON format.
-- * 'stateUpdatedTimestamp' - The time stamp of the last update to the alarm state.
-- * 'stateValue' - The state value for the alarm.
mkCompositeAlarm ::
  CompositeAlarm
mkCompositeAlarm =
  CompositeAlarm'
    { alarmName = Lude.Nothing,
      stateUpdatedTimestamp = Lude.Nothing,
      alarmDescription = Lude.Nothing,
      alarmRule = Lude.Nothing,
      okActions = Lude.Nothing,
      stateValue = Lude.Nothing,
      alarmConfigurationUpdatedTimestamp = Lude.Nothing,
      actionsEnabled = Lude.Nothing,
      insufficientDataActions = Lude.Nothing,
      stateReason = Lude.Nothing,
      stateReasonData = Lude.Nothing,
      alarmARN = Lude.Nothing,
      alarmActions = Lude.Nothing
    }

-- | The name of the alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlarmName :: Lens.Lens' CompositeAlarm (Lude.Maybe Lude.Text)
caAlarmName = Lens.lens (alarmName :: CompositeAlarm -> Lude.Maybe Lude.Text) (\s a -> s {alarmName = a} :: CompositeAlarm)
{-# DEPRECATED caAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

-- | The time stamp of the last update to the alarm state.
--
-- /Note:/ Consider using 'stateUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caStateUpdatedTimestamp :: Lens.Lens' CompositeAlarm (Lude.Maybe Lude.ISO8601)
caStateUpdatedTimestamp = Lens.lens (stateUpdatedTimestamp :: CompositeAlarm -> Lude.Maybe Lude.ISO8601) (\s a -> s {stateUpdatedTimestamp = a} :: CompositeAlarm)
{-# DEPRECATED caStateUpdatedTimestamp "Use generic-lens or generic-optics with 'stateUpdatedTimestamp' instead." #-}

-- | The description of the alarm.
--
-- /Note:/ Consider using 'alarmDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlarmDescription :: Lens.Lens' CompositeAlarm (Lude.Maybe Lude.Text)
caAlarmDescription = Lens.lens (alarmDescription :: CompositeAlarm -> Lude.Maybe Lude.Text) (\s a -> s {alarmDescription = a} :: CompositeAlarm)
{-# DEPRECATED caAlarmDescription "Use generic-lens or generic-optics with 'alarmDescription' instead." #-}

-- | The rule that this alarm uses to evaluate its alarm state.
--
-- /Note:/ Consider using 'alarmRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlarmRule :: Lens.Lens' CompositeAlarm (Lude.Maybe Lude.Text)
caAlarmRule = Lens.lens (alarmRule :: CompositeAlarm -> Lude.Maybe Lude.Text) (\s a -> s {alarmRule = a} :: CompositeAlarm)
{-# DEPRECATED caAlarmRule "Use generic-lens or generic-optics with 'alarmRule' instead." #-}

-- | The actions to execute when this alarm transitions to the OK state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'okActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caOKActions :: Lens.Lens' CompositeAlarm (Lude.Maybe [Lude.Text])
caOKActions = Lens.lens (okActions :: CompositeAlarm -> Lude.Maybe [Lude.Text]) (\s a -> s {okActions = a} :: CompositeAlarm)
{-# DEPRECATED caOKActions "Use generic-lens or generic-optics with 'okActions' instead." #-}

-- | The state value for the alarm.
--
-- /Note:/ Consider using 'stateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caStateValue :: Lens.Lens' CompositeAlarm (Lude.Maybe StateValue)
caStateValue = Lens.lens (stateValue :: CompositeAlarm -> Lude.Maybe StateValue) (\s a -> s {stateValue = a} :: CompositeAlarm)
{-# DEPRECATED caStateValue "Use generic-lens or generic-optics with 'stateValue' instead." #-}

-- | The time stamp of the last update to the alarm configuration.
--
-- /Note:/ Consider using 'alarmConfigurationUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlarmConfigurationUpdatedTimestamp :: Lens.Lens' CompositeAlarm (Lude.Maybe Lude.ISO8601)
caAlarmConfigurationUpdatedTimestamp = Lens.lens (alarmConfigurationUpdatedTimestamp :: CompositeAlarm -> Lude.Maybe Lude.ISO8601) (\s a -> s {alarmConfigurationUpdatedTimestamp = a} :: CompositeAlarm)
{-# DEPRECATED caAlarmConfigurationUpdatedTimestamp "Use generic-lens or generic-optics with 'alarmConfigurationUpdatedTimestamp' instead." #-}

-- | Indicates whether actions should be executed during any changes to the alarm state.
--
-- /Note:/ Consider using 'actionsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caActionsEnabled :: Lens.Lens' CompositeAlarm (Lude.Maybe Lude.Bool)
caActionsEnabled = Lens.lens (actionsEnabled :: CompositeAlarm -> Lude.Maybe Lude.Bool) (\s a -> s {actionsEnabled = a} :: CompositeAlarm)
{-# DEPRECATED caActionsEnabled "Use generic-lens or generic-optics with 'actionsEnabled' instead." #-}

-- | The actions to execute when this alarm transitions to the INSUFFICIENT_DATA state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'insufficientDataActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caInsufficientDataActions :: Lens.Lens' CompositeAlarm (Lude.Maybe [Lude.Text])
caInsufficientDataActions = Lens.lens (insufficientDataActions :: CompositeAlarm -> Lude.Maybe [Lude.Text]) (\s a -> s {insufficientDataActions = a} :: CompositeAlarm)
{-# DEPRECATED caInsufficientDataActions "Use generic-lens or generic-optics with 'insufficientDataActions' instead." #-}

-- | An explanation for the alarm state, in text format.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caStateReason :: Lens.Lens' CompositeAlarm (Lude.Maybe Lude.Text)
caStateReason = Lens.lens (stateReason :: CompositeAlarm -> Lude.Maybe Lude.Text) (\s a -> s {stateReason = a} :: CompositeAlarm)
{-# DEPRECATED caStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | An explanation for the alarm state, in JSON format.
--
-- /Note:/ Consider using 'stateReasonData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caStateReasonData :: Lens.Lens' CompositeAlarm (Lude.Maybe Lude.Text)
caStateReasonData = Lens.lens (stateReasonData :: CompositeAlarm -> Lude.Maybe Lude.Text) (\s a -> s {stateReasonData = a} :: CompositeAlarm)
{-# DEPRECATED caStateReasonData "Use generic-lens or generic-optics with 'stateReasonData' instead." #-}

-- | The Amazon Resource Name (ARN) of the alarm.
--
-- /Note:/ Consider using 'alarmARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlarmARN :: Lens.Lens' CompositeAlarm (Lude.Maybe Lude.Text)
caAlarmARN = Lens.lens (alarmARN :: CompositeAlarm -> Lude.Maybe Lude.Text) (\s a -> s {alarmARN = a} :: CompositeAlarm)
{-# DEPRECATED caAlarmARN "Use generic-lens or generic-optics with 'alarmARN' instead." #-}

-- | The actions to execute when this alarm transitions to the ALARM state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'alarmActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlarmActions :: Lens.Lens' CompositeAlarm (Lude.Maybe [Lude.Text])
caAlarmActions = Lens.lens (alarmActions :: CompositeAlarm -> Lude.Maybe [Lude.Text]) (\s a -> s {alarmActions = a} :: CompositeAlarm)
{-# DEPRECATED caAlarmActions "Use generic-lens or generic-optics with 'alarmActions' instead." #-}

instance Lude.FromXML CompositeAlarm where
  parseXML x =
    CompositeAlarm'
      Lude.<$> (x Lude..@? "AlarmName")
      Lude.<*> (x Lude..@? "StateUpdatedTimestamp")
      Lude.<*> (x Lude..@? "AlarmDescription")
      Lude.<*> (x Lude..@? "AlarmRule")
      Lude.<*> ( x Lude..@? "OKActions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "StateValue")
      Lude.<*> (x Lude..@? "AlarmConfigurationUpdatedTimestamp")
      Lude.<*> (x Lude..@? "ActionsEnabled")
      Lude.<*> ( x Lude..@? "InsufficientDataActions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "StateReason")
      Lude.<*> (x Lude..@? "StateReasonData")
      Lude.<*> (x Lude..@? "AlarmArn")
      Lude.<*> ( x Lude..@? "AlarmActions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
