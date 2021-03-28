{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CloudwatchAlarmAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.CloudwatchAlarmAction
  ( CloudwatchAlarmAction (..)
  -- * Smart constructor
  , mkCloudwatchAlarmAction
  -- * Lenses
  , caaRoleArn
  , caaAlarmName
  , caaStateReason
  , caaStateValue
  ) where

import qualified Network.AWS.IoT.Types.AlarmName as Types
import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.StateReason as Types
import qualified Network.AWS.IoT.Types.StateValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an action that updates a CloudWatch alarm.
--
-- /See:/ 'mkCloudwatchAlarmAction' smart constructor.
data CloudwatchAlarmAction = CloudwatchAlarmAction'
  { roleArn :: Types.AwsArn
    -- ^ The IAM role that allows access to the CloudWatch alarm.
  , alarmName :: Types.AlarmName
    -- ^ The CloudWatch alarm name.
  , stateReason :: Types.StateReason
    -- ^ The reason for the alarm change.
  , stateValue :: Types.StateValue
    -- ^ The value of the alarm state. Acceptable values are: OK, ALARM, INSUFFICIENT_DATA.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudwatchAlarmAction' value with any optional fields omitted.
mkCloudwatchAlarmAction
    :: Types.AwsArn -- ^ 'roleArn'
    -> Types.AlarmName -- ^ 'alarmName'
    -> Types.StateReason -- ^ 'stateReason'
    -> Types.StateValue -- ^ 'stateValue'
    -> CloudwatchAlarmAction
mkCloudwatchAlarmAction roleArn alarmName stateReason stateValue
  = CloudwatchAlarmAction'{roleArn, alarmName, stateReason,
                           stateValue}

-- | The IAM role that allows access to the CloudWatch alarm.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caaRoleArn :: Lens.Lens' CloudwatchAlarmAction Types.AwsArn
caaRoleArn = Lens.field @"roleArn"
{-# INLINEABLE caaRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The CloudWatch alarm name.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caaAlarmName :: Lens.Lens' CloudwatchAlarmAction Types.AlarmName
caaAlarmName = Lens.field @"alarmName"
{-# INLINEABLE caaAlarmName #-}
{-# DEPRECATED alarmName "Use generic-lens or generic-optics with 'alarmName' instead"  #-}

-- | The reason for the alarm change.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caaStateReason :: Lens.Lens' CloudwatchAlarmAction Types.StateReason
caaStateReason = Lens.field @"stateReason"
{-# INLINEABLE caaStateReason #-}
{-# DEPRECATED stateReason "Use generic-lens or generic-optics with 'stateReason' instead"  #-}

-- | The value of the alarm state. Acceptable values are: OK, ALARM, INSUFFICIENT_DATA.
--
-- /Note:/ Consider using 'stateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caaStateValue :: Lens.Lens' CloudwatchAlarmAction Types.StateValue
caaStateValue = Lens.field @"stateValue"
{-# INLINEABLE caaStateValue #-}
{-# DEPRECATED stateValue "Use generic-lens or generic-optics with 'stateValue' instead"  #-}

instance Core.FromJSON CloudwatchAlarmAction where
        toJSON CloudwatchAlarmAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("roleArn" Core..= roleArn),
                  Core.Just ("alarmName" Core..= alarmName),
                  Core.Just ("stateReason" Core..= stateReason),
                  Core.Just ("stateValue" Core..= stateValue)])

instance Core.FromJSON CloudwatchAlarmAction where
        parseJSON
          = Core.withObject "CloudwatchAlarmAction" Core.$
              \ x ->
                CloudwatchAlarmAction' Core.<$>
                  (x Core..: "roleArn") Core.<*> x Core..: "alarmName" Core.<*>
                    x Core..: "stateReason"
                    Core.<*> x Core..: "stateValue"
