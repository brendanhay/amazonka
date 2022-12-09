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
-- Module      : Amazonka.QuickSight.Types.WhatIfRangeScenario
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WhatIfRangeScenario where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the forecast to meet the target for a particular date range.
--
-- /See:/ 'newWhatIfRangeScenario' smart constructor.
data WhatIfRangeScenario = WhatIfRangeScenario'
  { -- | The start date in the date range that you need the forecast results for.
    startDate :: Data.POSIX,
    -- | The end date in the date range that you need the forecast results for.
    endDate :: Data.POSIX,
    -- | The target value that you want to meet for the provided date range.
    value :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WhatIfRangeScenario' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startDate', 'whatIfRangeScenario_startDate' - The start date in the date range that you need the forecast results for.
--
-- 'endDate', 'whatIfRangeScenario_endDate' - The end date in the date range that you need the forecast results for.
--
-- 'value', 'whatIfRangeScenario_value' - The target value that you want to meet for the provided date range.
newWhatIfRangeScenario ::
  -- | 'startDate'
  Prelude.UTCTime ->
  -- | 'endDate'
  Prelude.UTCTime ->
  -- | 'value'
  Prelude.Double ->
  WhatIfRangeScenario
newWhatIfRangeScenario pStartDate_ pEndDate_ pValue_ =
  WhatIfRangeScenario'
    { startDate =
        Data._Time Lens.# pStartDate_,
      endDate = Data._Time Lens.# pEndDate_,
      value = pValue_
    }

-- | The start date in the date range that you need the forecast results for.
whatIfRangeScenario_startDate :: Lens.Lens' WhatIfRangeScenario Prelude.UTCTime
whatIfRangeScenario_startDate = Lens.lens (\WhatIfRangeScenario' {startDate} -> startDate) (\s@WhatIfRangeScenario' {} a -> s {startDate = a} :: WhatIfRangeScenario) Prelude.. Data._Time

-- | The end date in the date range that you need the forecast results for.
whatIfRangeScenario_endDate :: Lens.Lens' WhatIfRangeScenario Prelude.UTCTime
whatIfRangeScenario_endDate = Lens.lens (\WhatIfRangeScenario' {endDate} -> endDate) (\s@WhatIfRangeScenario' {} a -> s {endDate = a} :: WhatIfRangeScenario) Prelude.. Data._Time

-- | The target value that you want to meet for the provided date range.
whatIfRangeScenario_value :: Lens.Lens' WhatIfRangeScenario Prelude.Double
whatIfRangeScenario_value = Lens.lens (\WhatIfRangeScenario' {value} -> value) (\s@WhatIfRangeScenario' {} a -> s {value = a} :: WhatIfRangeScenario)

instance Data.FromJSON WhatIfRangeScenario where
  parseJSON =
    Data.withObject
      "WhatIfRangeScenario"
      ( \x ->
          WhatIfRangeScenario'
            Prelude.<$> (x Data..: "StartDate")
            Prelude.<*> (x Data..: "EndDate")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable WhatIfRangeScenario where
  hashWithSalt _salt WhatIfRangeScenario' {..} =
    _salt `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` value

instance Prelude.NFData WhatIfRangeScenario where
  rnf WhatIfRangeScenario' {..} =
    Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON WhatIfRangeScenario where
  toJSON WhatIfRangeScenario' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("StartDate" Data..= startDate),
            Prelude.Just ("EndDate" Data..= endDate),
            Prelude.Just ("Value" Data..= value)
          ]
      )
