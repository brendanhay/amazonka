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
-- Module      : Amazonka.SSMContacts.Types.HandOffTime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.HandOffTime where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about when an on-call rotation shift begins or ends.
--
-- /See:/ 'newHandOffTime' smart constructor.
data HandOffTime = HandOffTime'
  { -- | The hour when an on-call rotation shift begins or ends.
    hourOfDay :: Prelude.Natural,
    -- | The minute when an on-call rotation shift begins or ends.
    minuteOfHour :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HandOffTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hourOfDay', 'handOffTime_hourOfDay' - The hour when an on-call rotation shift begins or ends.
--
-- 'minuteOfHour', 'handOffTime_minuteOfHour' - The minute when an on-call rotation shift begins or ends.
newHandOffTime ::
  -- | 'hourOfDay'
  Prelude.Natural ->
  -- | 'minuteOfHour'
  Prelude.Natural ->
  HandOffTime
newHandOffTime pHourOfDay_ pMinuteOfHour_ =
  HandOffTime'
    { hourOfDay = pHourOfDay_,
      minuteOfHour = pMinuteOfHour_
    }

-- | The hour when an on-call rotation shift begins or ends.
handOffTime_hourOfDay :: Lens.Lens' HandOffTime Prelude.Natural
handOffTime_hourOfDay = Lens.lens (\HandOffTime' {hourOfDay} -> hourOfDay) (\s@HandOffTime' {} a -> s {hourOfDay = a} :: HandOffTime)

-- | The minute when an on-call rotation shift begins or ends.
handOffTime_minuteOfHour :: Lens.Lens' HandOffTime Prelude.Natural
handOffTime_minuteOfHour = Lens.lens (\HandOffTime' {minuteOfHour} -> minuteOfHour) (\s@HandOffTime' {} a -> s {minuteOfHour = a} :: HandOffTime)

instance Data.FromJSON HandOffTime where
  parseJSON =
    Data.withObject
      "HandOffTime"
      ( \x ->
          HandOffTime'
            Prelude.<$> (x Data..: "HourOfDay")
            Prelude.<*> (x Data..: "MinuteOfHour")
      )

instance Prelude.Hashable HandOffTime where
  hashWithSalt _salt HandOffTime' {..} =
    _salt
      `Prelude.hashWithSalt` hourOfDay
      `Prelude.hashWithSalt` minuteOfHour

instance Prelude.NFData HandOffTime where
  rnf HandOffTime' {..} =
    Prelude.rnf hourOfDay
      `Prelude.seq` Prelude.rnf minuteOfHour

instance Data.ToJSON HandOffTime where
  toJSON HandOffTime' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("HourOfDay" Data..= hourOfDay),
            Prelude.Just ("MinuteOfHour" Data..= minuteOfHour)
          ]
      )
