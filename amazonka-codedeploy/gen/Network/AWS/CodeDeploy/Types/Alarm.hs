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
-- Module      : Network.AWS.CodeDeploy.Types.Alarm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.Alarm where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an alarm.
--
-- /See:/ 'newAlarm' smart constructor.
data Alarm = Alarm'
  { -- | The name of the alarm. Maximum length is 255 characters. Each alarm name
    -- can be used only once in a list of alarms.
    name :: Prelude.Maybe Prelude.Text
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
-- 'name', 'alarm_name' - The name of the alarm. Maximum length is 255 characters. Each alarm name
-- can be used only once in a list of alarms.
newAlarm ::
  Alarm
newAlarm = Alarm' {name = Prelude.Nothing}

-- | The name of the alarm. Maximum length is 255 characters. Each alarm name
-- can be used only once in a list of alarms.
alarm_name :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Text)
alarm_name = Lens.lens (\Alarm' {name} -> name) (\s@Alarm' {} a -> s {name = a} :: Alarm)

instance Prelude.FromJSON Alarm where
  parseJSON =
    Prelude.withObject
      "Alarm"
      (\x -> Alarm' Prelude.<$> (x Prelude..:? "name"))

instance Prelude.Hashable Alarm

instance Prelude.NFData Alarm

instance Prelude.ToJSON Alarm where
  toJSON Alarm' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("name" Prelude..=) Prelude.<$> name]
      )
