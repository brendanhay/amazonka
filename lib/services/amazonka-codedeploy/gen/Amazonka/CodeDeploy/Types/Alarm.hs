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
-- Module      : Amazonka.CodeDeploy.Types.Alarm
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.Alarm where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an alarm.
--
-- /See:/ 'newAlarm' smart constructor.
data Alarm = Alarm'
  { -- | The name of the alarm. Maximum length is 255 characters. Each alarm name
    -- can be used only once in a list of alarms.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON Alarm where
  parseJSON =
    Data.withObject
      "Alarm"
      (\x -> Alarm' Prelude.<$> (x Data..:? "name"))

instance Prelude.Hashable Alarm where
  hashWithSalt _salt Alarm' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData Alarm where
  rnf Alarm' {..} = Prelude.rnf name

instance Data.ToJSON Alarm where
  toJSON Alarm' {..} =
    Data.object
      ( Prelude.catMaybes
          [("name" Data..=) Prelude.<$> name]
      )
