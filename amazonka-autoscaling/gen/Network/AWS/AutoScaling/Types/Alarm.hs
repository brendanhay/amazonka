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
-- Module      : Network.AWS.AutoScaling.Types.Alarm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.Alarm where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an alarm.
--
-- /See:/ 'newAlarm' smart constructor.
data Alarm = Alarm'
  { -- | The Amazon Resource Name (ARN) of the alarm.
    alarmARN :: Core.Maybe Core.Text,
    -- | The name of the alarm.
    alarmName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Alarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmARN', 'alarm_alarmARN' - The Amazon Resource Name (ARN) of the alarm.
--
-- 'alarmName', 'alarm_alarmName' - The name of the alarm.
newAlarm ::
  Alarm
newAlarm =
  Alarm'
    { alarmARN = Core.Nothing,
      alarmName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the alarm.
alarm_alarmARN :: Lens.Lens' Alarm (Core.Maybe Core.Text)
alarm_alarmARN = Lens.lens (\Alarm' {alarmARN} -> alarmARN) (\s@Alarm' {} a -> s {alarmARN = a} :: Alarm)

-- | The name of the alarm.
alarm_alarmName :: Lens.Lens' Alarm (Core.Maybe Core.Text)
alarm_alarmName = Lens.lens (\Alarm' {alarmName} -> alarmName) (\s@Alarm' {} a -> s {alarmName = a} :: Alarm)

instance Core.FromXML Alarm where
  parseXML x =
    Alarm'
      Core.<$> (x Core..@? "AlarmARN")
      Core.<*> (x Core..@? "AlarmName")

instance Core.Hashable Alarm

instance Core.NFData Alarm
