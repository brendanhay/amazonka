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
-- Module      : Network.AWS.SageMaker.Types.Alarm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Alarm where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This API is not supported.
--
-- /See:/ 'newAlarm' smart constructor.
data Alarm = Alarm'
  { alarmName :: Core.Maybe Core.Text
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
-- 'alarmName', 'alarm_alarmName' -
newAlarm ::
  Alarm
newAlarm = Alarm' {alarmName = Core.Nothing}

-- |
alarm_alarmName :: Lens.Lens' Alarm (Core.Maybe Core.Text)
alarm_alarmName = Lens.lens (\Alarm' {alarmName} -> alarmName) (\s@Alarm' {} a -> s {alarmName = a} :: Alarm)

instance Core.FromJSON Alarm where
  parseJSON =
    Core.withObject
      "Alarm"
      (\x -> Alarm' Core.<$> (x Core..:? "AlarmName"))

instance Core.Hashable Alarm

instance Core.NFData Alarm

instance Core.ToJSON Alarm where
  toJSON Alarm' {..} =
    Core.object
      ( Core.catMaybes
          [("AlarmName" Core..=) Core.<$> alarmName]
      )
