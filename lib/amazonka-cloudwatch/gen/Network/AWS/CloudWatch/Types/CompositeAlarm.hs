{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    caActionsEnabled,
    caAlarmActions,
    caAlarmArn,
    caAlarmConfigurationUpdatedTimestamp,
    caAlarmDescription,
    caAlarmName,
    caAlarmRule,
    caInsufficientDataActions,
    caOKActions,
    caStateReason,
    caStateReasonData,
    caStateUpdatedTimestamp,
    caStateValue,
  )
where

import qualified Network.AWS.CloudWatch.Types.AlarmArn as Types
import qualified Network.AWS.CloudWatch.Types.AlarmDescription as Types
import qualified Network.AWS.CloudWatch.Types.AlarmName as Types
import qualified Network.AWS.CloudWatch.Types.AlarmRule as Types
import qualified Network.AWS.CloudWatch.Types.ResourceName as Types
import qualified Network.AWS.CloudWatch.Types.StateReason as Types
import qualified Network.AWS.CloudWatch.Types.StateReasonData as Types
import qualified Network.AWS.CloudWatch.Types.StateValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details about a composite alarm.
--
-- /See:/ 'mkCompositeAlarm' smart constructor.
data CompositeAlarm = CompositeAlarm'
  { -- | Indicates whether actions should be executed during any changes to the alarm state.
    actionsEnabled :: Core.Maybe Core.Bool,
    -- | The actions to execute when this alarm transitions to the ALARM state from any other state. Each action is specified as an Amazon Resource Name (ARN).
    alarmActions :: Core.Maybe [Types.ResourceName],
    -- | The Amazon Resource Name (ARN) of the alarm.
    alarmArn :: Core.Maybe Types.AlarmArn,
    -- | The time stamp of the last update to the alarm configuration.
    alarmConfigurationUpdatedTimestamp :: Core.Maybe Core.UTCTime,
    -- | The description of the alarm.
    alarmDescription :: Core.Maybe Types.AlarmDescription,
    -- | The name of the alarm.
    alarmName :: Core.Maybe Types.AlarmName,
    -- | The rule that this alarm uses to evaluate its alarm state.
    alarmRule :: Core.Maybe Types.AlarmRule,
    -- | The actions to execute when this alarm transitions to the INSUFFICIENT_DATA state from any other state. Each action is specified as an Amazon Resource Name (ARN).
    insufficientDataActions :: Core.Maybe [Types.ResourceName],
    -- | The actions to execute when this alarm transitions to the OK state from any other state. Each action is specified as an Amazon Resource Name (ARN).
    oKActions :: Core.Maybe [Types.ResourceName],
    -- | An explanation for the alarm state, in text format.
    stateReason :: Core.Maybe Types.StateReason,
    -- | An explanation for the alarm state, in JSON format.
    stateReasonData :: Core.Maybe Types.StateReasonData,
    -- | The time stamp of the last update to the alarm state.
    stateUpdatedTimestamp :: Core.Maybe Core.UTCTime,
    -- | The state value for the alarm.
    stateValue :: Core.Maybe Types.StateValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CompositeAlarm' value with any optional fields omitted.
mkCompositeAlarm ::
  CompositeAlarm
mkCompositeAlarm =
  CompositeAlarm'
    { actionsEnabled = Core.Nothing,
      alarmActions = Core.Nothing,
      alarmArn = Core.Nothing,
      alarmConfigurationUpdatedTimestamp = Core.Nothing,
      alarmDescription = Core.Nothing,
      alarmName = Core.Nothing,
      alarmRule = Core.Nothing,
      insufficientDataActions = Core.Nothing,
      oKActions = Core.Nothing,
      stateReason = Core.Nothing,
      stateReasonData = Core.Nothing,
      stateUpdatedTimestamp = Core.Nothing,
      stateValue = Core.Nothing
    }

-- | Indicates whether actions should be executed during any changes to the alarm state.
--
-- /Note:/ Consider using 'actionsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caActionsEnabled :: Lens.Lens' CompositeAlarm (Core.Maybe Core.Bool)
caActionsEnabled = Lens.field @"actionsEnabled"
{-# DEPRECATED caActionsEnabled "Use generic-lens or generic-optics with 'actionsEnabled' instead." #-}

-- | The actions to execute when this alarm transitions to the ALARM state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'alarmActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlarmActions :: Lens.Lens' CompositeAlarm (Core.Maybe [Types.ResourceName])
caAlarmActions = Lens.field @"alarmActions"
{-# DEPRECATED caAlarmActions "Use generic-lens or generic-optics with 'alarmActions' instead." #-}

-- | The Amazon Resource Name (ARN) of the alarm.
--
-- /Note:/ Consider using 'alarmArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlarmArn :: Lens.Lens' CompositeAlarm (Core.Maybe Types.AlarmArn)
caAlarmArn = Lens.field @"alarmArn"
{-# DEPRECATED caAlarmArn "Use generic-lens or generic-optics with 'alarmArn' instead." #-}

-- | The time stamp of the last update to the alarm configuration.
--
-- /Note:/ Consider using 'alarmConfigurationUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlarmConfigurationUpdatedTimestamp :: Lens.Lens' CompositeAlarm (Core.Maybe Core.UTCTime)
caAlarmConfigurationUpdatedTimestamp = Lens.field @"alarmConfigurationUpdatedTimestamp"
{-# DEPRECATED caAlarmConfigurationUpdatedTimestamp "Use generic-lens or generic-optics with 'alarmConfigurationUpdatedTimestamp' instead." #-}

-- | The description of the alarm.
--
-- /Note:/ Consider using 'alarmDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlarmDescription :: Lens.Lens' CompositeAlarm (Core.Maybe Types.AlarmDescription)
caAlarmDescription = Lens.field @"alarmDescription"
{-# DEPRECATED caAlarmDescription "Use generic-lens or generic-optics with 'alarmDescription' instead." #-}

-- | The name of the alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlarmName :: Lens.Lens' CompositeAlarm (Core.Maybe Types.AlarmName)
caAlarmName = Lens.field @"alarmName"
{-# DEPRECATED caAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

-- | The rule that this alarm uses to evaluate its alarm state.
--
-- /Note:/ Consider using 'alarmRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlarmRule :: Lens.Lens' CompositeAlarm (Core.Maybe Types.AlarmRule)
caAlarmRule = Lens.field @"alarmRule"
{-# DEPRECATED caAlarmRule "Use generic-lens or generic-optics with 'alarmRule' instead." #-}

-- | The actions to execute when this alarm transitions to the INSUFFICIENT_DATA state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'insufficientDataActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caInsufficientDataActions :: Lens.Lens' CompositeAlarm (Core.Maybe [Types.ResourceName])
caInsufficientDataActions = Lens.field @"insufficientDataActions"
{-# DEPRECATED caInsufficientDataActions "Use generic-lens or generic-optics with 'insufficientDataActions' instead." #-}

-- | The actions to execute when this alarm transitions to the OK state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'oKActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caOKActions :: Lens.Lens' CompositeAlarm (Core.Maybe [Types.ResourceName])
caOKActions = Lens.field @"oKActions"
{-# DEPRECATED caOKActions "Use generic-lens or generic-optics with 'oKActions' instead." #-}

-- | An explanation for the alarm state, in text format.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caStateReason :: Lens.Lens' CompositeAlarm (Core.Maybe Types.StateReason)
caStateReason = Lens.field @"stateReason"
{-# DEPRECATED caStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | An explanation for the alarm state, in JSON format.
--
-- /Note:/ Consider using 'stateReasonData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caStateReasonData :: Lens.Lens' CompositeAlarm (Core.Maybe Types.StateReasonData)
caStateReasonData = Lens.field @"stateReasonData"
{-# DEPRECATED caStateReasonData "Use generic-lens or generic-optics with 'stateReasonData' instead." #-}

-- | The time stamp of the last update to the alarm state.
--
-- /Note:/ Consider using 'stateUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caStateUpdatedTimestamp :: Lens.Lens' CompositeAlarm (Core.Maybe Core.UTCTime)
caStateUpdatedTimestamp = Lens.field @"stateUpdatedTimestamp"
{-# DEPRECATED caStateUpdatedTimestamp "Use generic-lens or generic-optics with 'stateUpdatedTimestamp' instead." #-}

-- | The state value for the alarm.
--
-- /Note:/ Consider using 'stateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caStateValue :: Lens.Lens' CompositeAlarm (Core.Maybe Types.StateValue)
caStateValue = Lens.field @"stateValue"
{-# DEPRECATED caStateValue "Use generic-lens or generic-optics with 'stateValue' instead." #-}

instance Core.FromXML CompositeAlarm where
  parseXML x =
    CompositeAlarm'
      Core.<$> (x Core..@? "ActionsEnabled")
      Core.<*> (x Core..@? "AlarmActions" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "AlarmArn")
      Core.<*> (x Core..@? "AlarmConfigurationUpdatedTimestamp")
      Core.<*> (x Core..@? "AlarmDescription")
      Core.<*> (x Core..@? "AlarmName")
      Core.<*> (x Core..@? "AlarmRule")
      Core.<*> ( x Core..@? "InsufficientDataActions"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "OKActions" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "StateReason")
      Core.<*> (x Core..@? "StateReasonData")
      Core.<*> (x Core..@? "StateUpdatedTimestamp")
      Core.<*> (x Core..@? "StateValue")
