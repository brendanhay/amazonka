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
-- Module      : Network.AWS.SageMaker.Types.Alarm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Alarm where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This API is not supported.
--
-- /See:/ 'newAlarm' smart constructor.
data Alarm = Alarm'
  { alarmName :: Prelude.Maybe Prelude.Text
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
-- 'alarmName', 'alarm_alarmName' -
newAlarm ::
  Alarm
newAlarm = Alarm' {alarmName = Prelude.Nothing}

-- |
alarm_alarmName :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Text)
alarm_alarmName = Lens.lens (\Alarm' {alarmName} -> alarmName) (\s@Alarm' {} a -> s {alarmName = a} :: Alarm)

instance Prelude.FromJSON Alarm where
  parseJSON =
    Prelude.withObject
      "Alarm"
      ( \x ->
          Alarm' Prelude.<$> (x Prelude..:? "AlarmName")
      )

instance Prelude.Hashable Alarm

instance Prelude.NFData Alarm

instance Prelude.ToJSON Alarm where
  toJSON Alarm' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("AlarmName" Prelude..=) Prelude.<$> alarmName]
      )
