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
-- Module      : Amazonka.SecurityHub.Types.DateRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.DateRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.DateRangeUnit

-- | A date range for the date filter.
--
-- /See:/ 'newDateRange' smart constructor.
data DateRange = DateRange'
  { -- | A date range value for the date filter.
    value :: Prelude.Maybe Prelude.Int,
    -- | A date range unit for the date filter.
    unit :: Prelude.Maybe DateRangeUnit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'dateRange_value' - A date range value for the date filter.
--
-- 'unit', 'dateRange_unit' - A date range unit for the date filter.
newDateRange ::
  DateRange
newDateRange =
  DateRange'
    { value = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | A date range value for the date filter.
dateRange_value :: Lens.Lens' DateRange (Prelude.Maybe Prelude.Int)
dateRange_value = Lens.lens (\DateRange' {value} -> value) (\s@DateRange' {} a -> s {value = a} :: DateRange)

-- | A date range unit for the date filter.
dateRange_unit :: Lens.Lens' DateRange (Prelude.Maybe DateRangeUnit)
dateRange_unit = Lens.lens (\DateRange' {unit} -> unit) (\s@DateRange' {} a -> s {unit = a} :: DateRange)

instance Core.FromJSON DateRange where
  parseJSON =
    Core.withObject
      "DateRange"
      ( \x ->
          DateRange'
            Prelude.<$> (x Core..:? "Value") Prelude.<*> (x Core..:? "Unit")
      )

instance Prelude.Hashable DateRange

instance Prelude.NFData DateRange

instance Core.ToJSON DateRange where
  toJSON DateRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Value" Core..=) Prelude.<$> value,
            ("Unit" Core..=) Prelude.<$> unit
          ]
      )
