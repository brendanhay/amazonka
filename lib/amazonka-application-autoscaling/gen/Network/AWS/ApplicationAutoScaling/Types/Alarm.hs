-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.Alarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.Alarm
  ( Alarm (..),

    -- * Smart constructor
    mkAlarm,

    -- * Lenses
    aAlarmName,
    aAlarmARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a CloudWatch alarm associated with a scaling policy.
--
-- /See:/ 'mkAlarm' smart constructor.
data Alarm = Alarm' {alarmName :: Lude.Text, alarmARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Alarm' with the minimum fields required to make a request.
--
-- * 'alarmARN' - The Amazon Resource Name (ARN) of the alarm.
-- * 'alarmName' - The name of the alarm.
mkAlarm ::
  -- | 'alarmName'
  Lude.Text ->
  -- | 'alarmARN'
  Lude.Text ->
  Alarm
mkAlarm pAlarmName_ pAlarmARN_ =
  Alarm' {alarmName = pAlarmName_, alarmARN = pAlarmARN_}

-- | The name of the alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlarmName :: Lens.Lens' Alarm Lude.Text
aAlarmName = Lens.lens (alarmName :: Alarm -> Lude.Text) (\s a -> s {alarmName = a} :: Alarm)
{-# DEPRECATED aAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

-- | The Amazon Resource Name (ARN) of the alarm.
--
-- /Note:/ Consider using 'alarmARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlarmARN :: Lens.Lens' Alarm Lude.Text
aAlarmARN = Lens.lens (alarmARN :: Alarm -> Lude.Text) (\s a -> s {alarmARN = a} :: Alarm)
{-# DEPRECATED aAlarmARN "Use generic-lens or generic-optics with 'alarmARN' instead." #-}

instance Lude.FromJSON Alarm where
  parseJSON =
    Lude.withObject
      "Alarm"
      ( \x ->
          Alarm'
            Lude.<$> (x Lude..: "AlarmName") Lude.<*> (x Lude..: "AlarmARN")
      )
