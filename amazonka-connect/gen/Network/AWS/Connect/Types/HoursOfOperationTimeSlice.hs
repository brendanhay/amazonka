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
-- Module      : Network.AWS.Connect.Types.HoursOfOperationTimeSlice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HoursOfOperationTimeSlice where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The start time or end time for an hours of operation.
--
-- /See:/ 'newHoursOfOperationTimeSlice' smart constructor.
data HoursOfOperationTimeSlice = HoursOfOperationTimeSlice'
  { -- | The hours.
    hours :: Prelude.Maybe Prelude.Natural,
    -- | The minutes.
    minutes :: Prelude.Maybe Prelude.Natural
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
  HoursOfOperationTimeSlice
newHoursOfOperationTimeSlice =
  HoursOfOperationTimeSlice'
    { hours = Prelude.Nothing,
      minutes = Prelude.Nothing
    }

-- | The hours.
hoursOfOperationTimeSlice_hours :: Lens.Lens' HoursOfOperationTimeSlice (Prelude.Maybe Prelude.Natural)
hoursOfOperationTimeSlice_hours = Lens.lens (\HoursOfOperationTimeSlice' {hours} -> hours) (\s@HoursOfOperationTimeSlice' {} a -> s {hours = a} :: HoursOfOperationTimeSlice)

-- | The minutes.
hoursOfOperationTimeSlice_minutes :: Lens.Lens' HoursOfOperationTimeSlice (Prelude.Maybe Prelude.Natural)
hoursOfOperationTimeSlice_minutes = Lens.lens (\HoursOfOperationTimeSlice' {minutes} -> minutes) (\s@HoursOfOperationTimeSlice' {} a -> s {minutes = a} :: HoursOfOperationTimeSlice)

instance Core.FromJSON HoursOfOperationTimeSlice where
  parseJSON =
    Core.withObject
      "HoursOfOperationTimeSlice"
      ( \x ->
          HoursOfOperationTimeSlice'
            Prelude.<$> (x Core..:? "Hours")
            Prelude.<*> (x Core..:? "Minutes")
      )

instance Prelude.Hashable HoursOfOperationTimeSlice

instance Prelude.NFData HoursOfOperationTimeSlice
