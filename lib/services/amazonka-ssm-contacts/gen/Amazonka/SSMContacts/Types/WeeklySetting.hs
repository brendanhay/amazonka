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
-- Module      : Amazonka.SSMContacts.Types.WeeklySetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.WeeklySetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMContacts.Types.DayOfWeek
import Amazonka.SSMContacts.Types.HandOffTime

-- | Information about rotations that recur weekly.
--
-- /See:/ 'newWeeklySetting' smart constructor.
data WeeklySetting = WeeklySetting'
  { -- | The day of the week when weekly recurring on-call shift rotations
    -- begins.
    dayOfWeek :: DayOfWeek,
    -- | The time of day when a weekly recurring on-call shift rotation begins.
    handOffTime :: HandOffTime
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WeeklySetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dayOfWeek', 'weeklySetting_dayOfWeek' - The day of the week when weekly recurring on-call shift rotations
-- begins.
--
-- 'handOffTime', 'weeklySetting_handOffTime' - The time of day when a weekly recurring on-call shift rotation begins.
newWeeklySetting ::
  -- | 'dayOfWeek'
  DayOfWeek ->
  -- | 'handOffTime'
  HandOffTime ->
  WeeklySetting
newWeeklySetting pDayOfWeek_ pHandOffTime_ =
  WeeklySetting'
    { dayOfWeek = pDayOfWeek_,
      handOffTime = pHandOffTime_
    }

-- | The day of the week when weekly recurring on-call shift rotations
-- begins.
weeklySetting_dayOfWeek :: Lens.Lens' WeeklySetting DayOfWeek
weeklySetting_dayOfWeek = Lens.lens (\WeeklySetting' {dayOfWeek} -> dayOfWeek) (\s@WeeklySetting' {} a -> s {dayOfWeek = a} :: WeeklySetting)

-- | The time of day when a weekly recurring on-call shift rotation begins.
weeklySetting_handOffTime :: Lens.Lens' WeeklySetting HandOffTime
weeklySetting_handOffTime = Lens.lens (\WeeklySetting' {handOffTime} -> handOffTime) (\s@WeeklySetting' {} a -> s {handOffTime = a} :: WeeklySetting)

instance Data.FromJSON WeeklySetting where
  parseJSON =
    Data.withObject
      "WeeklySetting"
      ( \x ->
          WeeklySetting'
            Prelude.<$> (x Data..: "DayOfWeek")
            Prelude.<*> (x Data..: "HandOffTime")
      )

instance Prelude.Hashable WeeklySetting where
  hashWithSalt _salt WeeklySetting' {..} =
    _salt
      `Prelude.hashWithSalt` dayOfWeek
      `Prelude.hashWithSalt` handOffTime

instance Prelude.NFData WeeklySetting where
  rnf WeeklySetting' {..} =
    Prelude.rnf dayOfWeek
      `Prelude.seq` Prelude.rnf handOffTime

instance Data.ToJSON WeeklySetting where
  toJSON WeeklySetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DayOfWeek" Data..= dayOfWeek),
            Prelude.Just ("HandOffTime" Data..= handOffTime)
          ]
      )
