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
-- Module      : Network.AWS.Connect.Types.HoursOfOperationConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HoursOfOperationConfig where

import Network.AWS.Connect.Types.HoursOfOperationDays
import Network.AWS.Connect.Types.HoursOfOperationTimeSlice
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the hours of operation.
--
-- /See:/ 'newHoursOfOperationConfig' smart constructor.
data HoursOfOperationConfig = HoursOfOperationConfig'
  { -- | The day that the hours of operation applies to.
    day :: Core.Maybe HoursOfOperationDays,
    -- | The start time that your contact center is open.
    startTime :: Core.Maybe HoursOfOperationTimeSlice,
    -- | The end time that your contact center is closes.
    endTime :: Core.Maybe HoursOfOperationTimeSlice
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'startTime', 'hoursOfOperationConfig_startTime' - The start time that your contact center is open.
--
-- 'endTime', 'hoursOfOperationConfig_endTime' - The end time that your contact center is closes.
newHoursOfOperationConfig ::
  HoursOfOperationConfig
newHoursOfOperationConfig =
  HoursOfOperationConfig'
    { day = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing
    }

-- | The day that the hours of operation applies to.
hoursOfOperationConfig_day :: Lens.Lens' HoursOfOperationConfig (Core.Maybe HoursOfOperationDays)
hoursOfOperationConfig_day = Lens.lens (\HoursOfOperationConfig' {day} -> day) (\s@HoursOfOperationConfig' {} a -> s {day = a} :: HoursOfOperationConfig)

-- | The start time that your contact center is open.
hoursOfOperationConfig_startTime :: Lens.Lens' HoursOfOperationConfig (Core.Maybe HoursOfOperationTimeSlice)
hoursOfOperationConfig_startTime = Lens.lens (\HoursOfOperationConfig' {startTime} -> startTime) (\s@HoursOfOperationConfig' {} a -> s {startTime = a} :: HoursOfOperationConfig)

-- | The end time that your contact center is closes.
hoursOfOperationConfig_endTime :: Lens.Lens' HoursOfOperationConfig (Core.Maybe HoursOfOperationTimeSlice)
hoursOfOperationConfig_endTime = Lens.lens (\HoursOfOperationConfig' {endTime} -> endTime) (\s@HoursOfOperationConfig' {} a -> s {endTime = a} :: HoursOfOperationConfig)

instance Core.FromJSON HoursOfOperationConfig where
  parseJSON =
    Core.withObject
      "HoursOfOperationConfig"
      ( \x ->
          HoursOfOperationConfig'
            Core.<$> (x Core..:? "Day")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "EndTime")
      )

instance Core.Hashable HoursOfOperationConfig

instance Core.NFData HoursOfOperationConfig
