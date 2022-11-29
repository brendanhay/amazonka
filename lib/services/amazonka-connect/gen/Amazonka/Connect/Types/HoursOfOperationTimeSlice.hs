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
-- Module      : Amazonka.Connect.Types.HoursOfOperationTimeSlice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HoursOfOperationTimeSlice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The start time or end time for an hours of operation.
--
-- /See:/ 'newHoursOfOperationTimeSlice' smart constructor.
data HoursOfOperationTimeSlice = HoursOfOperationTimeSlice'
  { -- | The hours.
    hours :: Prelude.Natural,
    -- | The minutes.
    minutes :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HoursOfOperationTimeSlice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hours', 'hoursOfOperationTimeSlice_hours' - The hours.
--
-- 'minutes', 'hoursOfOperationTimeSlice_minutes' - The minutes.
newHoursOfOperationTimeSlice ::
  -- | 'hours'
  Prelude.Natural ->
  -- | 'minutes'
  Prelude.Natural ->
  HoursOfOperationTimeSlice
newHoursOfOperationTimeSlice pHours_ pMinutes_ =
  HoursOfOperationTimeSlice'
    { hours = pHours_,
      minutes = pMinutes_
    }

-- | The hours.
hoursOfOperationTimeSlice_hours :: Lens.Lens' HoursOfOperationTimeSlice Prelude.Natural
hoursOfOperationTimeSlice_hours = Lens.lens (\HoursOfOperationTimeSlice' {hours} -> hours) (\s@HoursOfOperationTimeSlice' {} a -> s {hours = a} :: HoursOfOperationTimeSlice)

-- | The minutes.
hoursOfOperationTimeSlice_minutes :: Lens.Lens' HoursOfOperationTimeSlice Prelude.Natural
hoursOfOperationTimeSlice_minutes = Lens.lens (\HoursOfOperationTimeSlice' {minutes} -> minutes) (\s@HoursOfOperationTimeSlice' {} a -> s {minutes = a} :: HoursOfOperationTimeSlice)

instance Core.FromJSON HoursOfOperationTimeSlice where
  parseJSON =
    Core.withObject
      "HoursOfOperationTimeSlice"
      ( \x ->
          HoursOfOperationTimeSlice'
            Prelude.<$> (x Core..: "Hours")
            Prelude.<*> (x Core..: "Minutes")
      )

instance Prelude.Hashable HoursOfOperationTimeSlice where
  hashWithSalt _salt HoursOfOperationTimeSlice' {..} =
    _salt `Prelude.hashWithSalt` hours
      `Prelude.hashWithSalt` minutes

instance Prelude.NFData HoursOfOperationTimeSlice where
  rnf HoursOfOperationTimeSlice' {..} =
    Prelude.rnf hours `Prelude.seq` Prelude.rnf minutes

instance Core.ToJSON HoursOfOperationTimeSlice where
  toJSON HoursOfOperationTimeSlice' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Hours" Core..= hours),
            Prelude.Just ("Minutes" Core..= minutes)
          ]
      )
