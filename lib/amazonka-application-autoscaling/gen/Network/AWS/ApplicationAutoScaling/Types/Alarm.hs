{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.Alarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.Alarm
  ( Alarm (..)
  -- * Smart constructor
  , mkAlarm
  -- * Lenses
  , aAlarmName
  , aAlarmARN
  ) where

import qualified Network.AWS.ApplicationAutoScaling.Types.ResourceId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a CloudWatch alarm associated with a scaling policy.
--
-- /See:/ 'mkAlarm' smart constructor.
data Alarm = Alarm'
  { alarmName :: Types.ResourceId
    -- ^ The name of the alarm.
  , alarmARN :: Types.ResourceId
    -- ^ The Amazon Resource Name (ARN) of the alarm.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Alarm' value with any optional fields omitted.
mkAlarm
    :: Types.ResourceId -- ^ 'alarmName'
    -> Types.ResourceId -- ^ 'alarmARN'
    -> Alarm
mkAlarm alarmName alarmARN = Alarm'{alarmName, alarmARN}

-- | The name of the alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlarmName :: Lens.Lens' Alarm Types.ResourceId
aAlarmName = Lens.field @"alarmName"
{-# INLINEABLE aAlarmName #-}
{-# DEPRECATED alarmName "Use generic-lens or generic-optics with 'alarmName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the alarm.
--
-- /Note:/ Consider using 'alarmARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlarmARN :: Lens.Lens' Alarm Types.ResourceId
aAlarmARN = Lens.field @"alarmARN"
{-# INLINEABLE aAlarmARN #-}
{-# DEPRECATED alarmARN "Use generic-lens or generic-optics with 'alarmARN' instead"  #-}

instance Core.FromJSON Alarm where
        parseJSON
          = Core.withObject "Alarm" Core.$
              \ x ->
                Alarm' Core.<$>
                  (x Core..: "AlarmName") Core.<*> x Core..: "AlarmARN"
