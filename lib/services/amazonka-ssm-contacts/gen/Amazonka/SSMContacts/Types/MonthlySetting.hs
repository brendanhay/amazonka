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
-- Module      : Amazonka.SSMContacts.Types.MonthlySetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.MonthlySetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMContacts.Types.HandOffTime

-- | Information about on-call rotations that recur monthly.
--
-- /See:/ 'newMonthlySetting' smart constructor.
data MonthlySetting = MonthlySetting'
  { -- | The day of the month when monthly recurring on-call rotations begin.
    dayOfMonth :: Prelude.Natural,
    -- | The time of day when a monthly recurring on-call shift rotation begins.
    handOffTime :: HandOffTime
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonthlySetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dayOfMonth', 'monthlySetting_dayOfMonth' - The day of the month when monthly recurring on-call rotations begin.
--
-- 'handOffTime', 'monthlySetting_handOffTime' - The time of day when a monthly recurring on-call shift rotation begins.
newMonthlySetting ::
  -- | 'dayOfMonth'
  Prelude.Natural ->
  -- | 'handOffTime'
  HandOffTime ->
  MonthlySetting
newMonthlySetting pDayOfMonth_ pHandOffTime_ =
  MonthlySetting'
    { dayOfMonth = pDayOfMonth_,
      handOffTime = pHandOffTime_
    }

-- | The day of the month when monthly recurring on-call rotations begin.
monthlySetting_dayOfMonth :: Lens.Lens' MonthlySetting Prelude.Natural
monthlySetting_dayOfMonth = Lens.lens (\MonthlySetting' {dayOfMonth} -> dayOfMonth) (\s@MonthlySetting' {} a -> s {dayOfMonth = a} :: MonthlySetting)

-- | The time of day when a monthly recurring on-call shift rotation begins.
monthlySetting_handOffTime :: Lens.Lens' MonthlySetting HandOffTime
monthlySetting_handOffTime = Lens.lens (\MonthlySetting' {handOffTime} -> handOffTime) (\s@MonthlySetting' {} a -> s {handOffTime = a} :: MonthlySetting)

instance Data.FromJSON MonthlySetting where
  parseJSON =
    Data.withObject
      "MonthlySetting"
      ( \x ->
          MonthlySetting'
            Prelude.<$> (x Data..: "DayOfMonth")
            Prelude.<*> (x Data..: "HandOffTime")
      )

instance Prelude.Hashable MonthlySetting where
  hashWithSalt _salt MonthlySetting' {..} =
    _salt
      `Prelude.hashWithSalt` dayOfMonth
      `Prelude.hashWithSalt` handOffTime

instance Prelude.NFData MonthlySetting where
  rnf MonthlySetting' {..} =
    Prelude.rnf dayOfMonth
      `Prelude.seq` Prelude.rnf handOffTime

instance Data.ToJSON MonthlySetting where
  toJSON MonthlySetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DayOfMonth" Data..= dayOfMonth),
            Prelude.Just ("HandOffTime" Data..= handOffTime)
          ]
      )
