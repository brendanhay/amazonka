{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Alarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.Alarm
  ( Alarm (..),

    -- * Smart constructor
    mkAlarm,

    -- * Lenses
    aAlarmARN,
    aAlarmName,
  )
where

import qualified Network.AWS.AutoScaling.Types.ResourceName as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an alarm.
--
-- /See:/ 'mkAlarm' smart constructor.
data Alarm = Alarm'
  { -- | The Amazon Resource Name (ARN) of the alarm.
    alarmARN :: Core.Maybe Types.ResourceName,
    -- | The name of the alarm.
    alarmName :: Core.Maybe Types.XmlStringMaxLen255
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Alarm' value with any optional fields omitted.
mkAlarm ::
  Alarm
mkAlarm = Alarm' {alarmARN = Core.Nothing, alarmName = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the alarm.
--
-- /Note:/ Consider using 'alarmARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlarmARN :: Lens.Lens' Alarm (Core.Maybe Types.ResourceName)
aAlarmARN = Lens.field @"alarmARN"
{-# DEPRECATED aAlarmARN "Use generic-lens or generic-optics with 'alarmARN' instead." #-}

-- | The name of the alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlarmName :: Lens.Lens' Alarm (Core.Maybe Types.XmlStringMaxLen255)
aAlarmName = Lens.field @"alarmName"
{-# DEPRECATED aAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

instance Core.FromXML Alarm where
  parseXML x =
    Alarm'
      Core.<$> (x Core..@? "AlarmARN") Core.<*> (x Core..@? "AlarmName")
