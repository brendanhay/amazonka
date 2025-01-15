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
-- Module      : Amazonka.QuickSight.Types.DrillDownFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DrillDownFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CategoryDrillDownFilter
import Amazonka.QuickSight.Types.NumericEqualityDrillDownFilter
import Amazonka.QuickSight.Types.TimeRangeDrillDownFilter

-- | The drill down filter for the column hierarchies.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newDrillDownFilter' smart constructor.
data DrillDownFilter = DrillDownFilter'
  { -- | The category type drill down filter. This filter is used for string type
    -- columns.
    categoryFilter :: Prelude.Maybe CategoryDrillDownFilter,
    -- | The numeric equality type drill down filter. This filter is used for
    -- number type columns.
    numericEqualityFilter :: Prelude.Maybe NumericEqualityDrillDownFilter,
    -- | The time range drill down filter. This filter is used for date time
    -- columns.
    timeRangeFilter :: Prelude.Maybe TimeRangeDrillDownFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DrillDownFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryFilter', 'drillDownFilter_categoryFilter' - The category type drill down filter. This filter is used for string type
-- columns.
--
-- 'numericEqualityFilter', 'drillDownFilter_numericEqualityFilter' - The numeric equality type drill down filter. This filter is used for
-- number type columns.
--
-- 'timeRangeFilter', 'drillDownFilter_timeRangeFilter' - The time range drill down filter. This filter is used for date time
-- columns.
newDrillDownFilter ::
  DrillDownFilter
newDrillDownFilter =
  DrillDownFilter'
    { categoryFilter = Prelude.Nothing,
      numericEqualityFilter = Prelude.Nothing,
      timeRangeFilter = Prelude.Nothing
    }

-- | The category type drill down filter. This filter is used for string type
-- columns.
drillDownFilter_categoryFilter :: Lens.Lens' DrillDownFilter (Prelude.Maybe CategoryDrillDownFilter)
drillDownFilter_categoryFilter = Lens.lens (\DrillDownFilter' {categoryFilter} -> categoryFilter) (\s@DrillDownFilter' {} a -> s {categoryFilter = a} :: DrillDownFilter)

-- | The numeric equality type drill down filter. This filter is used for
-- number type columns.
drillDownFilter_numericEqualityFilter :: Lens.Lens' DrillDownFilter (Prelude.Maybe NumericEqualityDrillDownFilter)
drillDownFilter_numericEqualityFilter = Lens.lens (\DrillDownFilter' {numericEqualityFilter} -> numericEqualityFilter) (\s@DrillDownFilter' {} a -> s {numericEqualityFilter = a} :: DrillDownFilter)

-- | The time range drill down filter. This filter is used for date time
-- columns.
drillDownFilter_timeRangeFilter :: Lens.Lens' DrillDownFilter (Prelude.Maybe TimeRangeDrillDownFilter)
drillDownFilter_timeRangeFilter = Lens.lens (\DrillDownFilter' {timeRangeFilter} -> timeRangeFilter) (\s@DrillDownFilter' {} a -> s {timeRangeFilter = a} :: DrillDownFilter)

instance Data.FromJSON DrillDownFilter where
  parseJSON =
    Data.withObject
      "DrillDownFilter"
      ( \x ->
          DrillDownFilter'
            Prelude.<$> (x Data..:? "CategoryFilter")
            Prelude.<*> (x Data..:? "NumericEqualityFilter")
            Prelude.<*> (x Data..:? "TimeRangeFilter")
      )

instance Prelude.Hashable DrillDownFilter where
  hashWithSalt _salt DrillDownFilter' {..} =
    _salt
      `Prelude.hashWithSalt` categoryFilter
      `Prelude.hashWithSalt` numericEqualityFilter
      `Prelude.hashWithSalt` timeRangeFilter

instance Prelude.NFData DrillDownFilter where
  rnf DrillDownFilter' {..} =
    Prelude.rnf categoryFilter `Prelude.seq`
      Prelude.rnf numericEqualityFilter `Prelude.seq`
        Prelude.rnf timeRangeFilter

instance Data.ToJSON DrillDownFilter where
  toJSON DrillDownFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryFilter" Data..=)
              Prelude.<$> categoryFilter,
            ("NumericEqualityFilter" Data..=)
              Prelude.<$> numericEqualityFilter,
            ("TimeRangeFilter" Data..=)
              Prelude.<$> timeRangeFilter
          ]
      )
