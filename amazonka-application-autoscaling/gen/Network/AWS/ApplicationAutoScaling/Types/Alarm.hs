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
-- Module      : Network.AWS.ApplicationAutoScaling.Types.Alarm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.Alarm where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a CloudWatch alarm associated with a scaling policy.
--
-- /See:/ 'newAlarm' smart constructor.
data Alarm = Alarm'
  { -- | The name of the alarm.
    alarmName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the alarm.
    alarmARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Alarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmName', 'alarm_alarmName' - The name of the alarm.
--
-- 'alarmARN', 'alarm_alarmARN' - The Amazon Resource Name (ARN) of the alarm.
newAlarm ::
  -- | 'alarmName'
  Prelude.Text ->
  -- | 'alarmARN'
  Prelude.Text ->
  Alarm
newAlarm pAlarmName_ pAlarmARN_ =
  Alarm'
    { alarmName = pAlarmName_,
      alarmARN = pAlarmARN_
    }

-- | The name of the alarm.
alarm_alarmName :: Lens.Lens' Alarm Prelude.Text
alarm_alarmName = Lens.lens (\Alarm' {alarmName} -> alarmName) (\s@Alarm' {} a -> s {alarmName = a} :: Alarm)

-- | The Amazon Resource Name (ARN) of the alarm.
alarm_alarmARN :: Lens.Lens' Alarm Prelude.Text
alarm_alarmARN = Lens.lens (\Alarm' {alarmARN} -> alarmARN) (\s@Alarm' {} a -> s {alarmARN = a} :: Alarm)

instance Prelude.FromJSON Alarm where
  parseJSON =
    Prelude.withObject
      "Alarm"
      ( \x ->
          Alarm'
            Prelude.<$> (x Prelude..: "AlarmName")
            Prelude.<*> (x Prelude..: "AlarmARN")
      )

instance Prelude.Hashable Alarm

instance Prelude.NFData Alarm
