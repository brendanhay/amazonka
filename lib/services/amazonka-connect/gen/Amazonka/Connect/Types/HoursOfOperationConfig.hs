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
-- Module      : Amazonka.Connect.Types.HoursOfOperationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HoursOfOperationConfig where

import Amazonka.Connect.Types.HoursOfOperationDays
import Amazonka.Connect.Types.HoursOfOperationTimeSlice
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the hours of operation.
--
-- /See:/ 'newHoursOfOperationConfig' smart constructor.
data HoursOfOperationConfig = HoursOfOperationConfig'
  { -- | The day that the hours of operation applies to.
    day :: HoursOfOperationDays,
    -- | The start time that your contact center opens.
    startTime :: HoursOfOperationTimeSlice,
    -- | The end time that your contact center closes.
    endTime :: HoursOfOperationTimeSlice
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HoursOfOperationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'day', 'hoursOfOperationConfig_day' - The day that the hours of operation applies to.
--
-- 'startTime', 'hoursOfOperationConfig_startTime' - The start time that your contact center opens.
--
-- 'endTime', 'hoursOfOperationConfig_endTime' - The end time that your contact center closes.
newHoursOfOperationConfig ::
  -- | 'day'
  HoursOfOperationDays ->
  -- | 'startTime'
  HoursOfOperationTimeSlice ->
  -- | 'endTime'
  HoursOfOperationTimeSlice ->
  HoursOfOperationConfig
newHoursOfOperationConfig pDay_ pStartTime_ pEndTime_ =
  HoursOfOperationConfig'
    { day = pDay_,
      startTime = pStartTime_,
      endTime = pEndTime_
    }

-- | The day that the hours of operation applies to.
hoursOfOperationConfig_day :: Lens.Lens' HoursOfOperationConfig HoursOfOperationDays
hoursOfOperationConfig_day = Lens.lens (\HoursOfOperationConfig' {day} -> day) (\s@HoursOfOperationConfig' {} a -> s {day = a} :: HoursOfOperationConfig)

-- | The start time that your contact center opens.
hoursOfOperationConfig_startTime :: Lens.Lens' HoursOfOperationConfig HoursOfOperationTimeSlice
hoursOfOperationConfig_startTime = Lens.lens (\HoursOfOperationConfig' {startTime} -> startTime) (\s@HoursOfOperationConfig' {} a -> s {startTime = a} :: HoursOfOperationConfig)

-- | The end time that your contact center closes.
hoursOfOperationConfig_endTime :: Lens.Lens' HoursOfOperationConfig HoursOfOperationTimeSlice
hoursOfOperationConfig_endTime = Lens.lens (\HoursOfOperationConfig' {endTime} -> endTime) (\s@HoursOfOperationConfig' {} a -> s {endTime = a} :: HoursOfOperationConfig)

instance Data.FromJSON HoursOfOperationConfig where
  parseJSON =
    Data.withObject
      "HoursOfOperationConfig"
      ( \x ->
          HoursOfOperationConfig'
            Prelude.<$> (x Data..: "Day")
            Prelude.<*> (x Data..: "StartTime")
            Prelude.<*> (x Data..: "EndTime")
      )

instance Prelude.Hashable HoursOfOperationConfig where
  hashWithSalt _salt HoursOfOperationConfig' {..} =
    _salt `Prelude.hashWithSalt` day
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData HoursOfOperationConfig where
  rnf HoursOfOperationConfig' {..} =
    Prelude.rnf day
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Data.ToJSON HoursOfOperationConfig where
  toJSON HoursOfOperationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Day" Data..= day),
            Prelude.Just ("StartTime" Data..= startTime),
            Prelude.Just ("EndTime" Data..= endTime)
          ]
      )
