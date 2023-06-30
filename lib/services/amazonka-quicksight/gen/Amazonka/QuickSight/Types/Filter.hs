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
-- Module      : Amazonka.QuickSight.Types.Filter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CategoryFilter
import Amazonka.QuickSight.Types.NumericEqualityFilter
import Amazonka.QuickSight.Types.NumericRangeFilter
import Amazonka.QuickSight.Types.RelativeDatesFilter
import Amazonka.QuickSight.Types.TimeEqualityFilter
import Amazonka.QuickSight.Types.TimeRangeFilter
import Amazonka.QuickSight.Types.TopBottomFilter

-- | With a @Filter@, you can remove portions of data from a particular
-- visual or view.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | A @CategoryFilter@ filters text values.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/add-a-text-filter-data-prep.html Adding text filters>
    -- in the /Amazon QuickSight User Guide/.
    categoryFilter :: Prelude.Maybe CategoryFilter,
    -- | A @NumericEqualityFilter@ filters numeric values that equal or do not
    -- equal a given numeric value.
    numericEqualityFilter :: Prelude.Maybe NumericEqualityFilter,
    -- | A @NumericRangeFilter@ filters numeric values that are either inside or
    -- outside a given numeric range.
    numericRangeFilter :: Prelude.Maybe NumericRangeFilter,
    -- | A @RelativeDatesFilter@ filters date values that are relative to a given
    -- date.
    relativeDatesFilter :: Prelude.Maybe RelativeDatesFilter,
    -- | A @TimeEqualityFilter@ filters date-time values that equal or do not
    -- equal a given date\/time value.
    timeEqualityFilter :: Prelude.Maybe TimeEqualityFilter,
    -- | A @TimeRangeFilter@ filters date-time values that are either inside or
    -- outside a given date\/time range.
    timeRangeFilter :: Prelude.Maybe TimeRangeFilter,
    -- | A @TopBottomFilter@ filters data to the top or bottom values for a given
    -- column.
    topBottomFilter :: Prelude.Maybe TopBottomFilter
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryFilter', 'filter_categoryFilter' - A @CategoryFilter@ filters text values.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/add-a-text-filter-data-prep.html Adding text filters>
-- in the /Amazon QuickSight User Guide/.
--
-- 'numericEqualityFilter', 'filter_numericEqualityFilter' - A @NumericEqualityFilter@ filters numeric values that equal or do not
-- equal a given numeric value.
--
-- 'numericRangeFilter', 'filter_numericRangeFilter' - A @NumericRangeFilter@ filters numeric values that are either inside or
-- outside a given numeric range.
--
-- 'relativeDatesFilter', 'filter_relativeDatesFilter' - A @RelativeDatesFilter@ filters date values that are relative to a given
-- date.
--
-- 'timeEqualityFilter', 'filter_timeEqualityFilter' - A @TimeEqualityFilter@ filters date-time values that equal or do not
-- equal a given date\/time value.
--
-- 'timeRangeFilter', 'filter_timeRangeFilter' - A @TimeRangeFilter@ filters date-time values that are either inside or
-- outside a given date\/time range.
--
-- 'topBottomFilter', 'filter_topBottomFilter' - A @TopBottomFilter@ filters data to the top or bottom values for a given
-- column.
newFilter ::
  Filter
newFilter =
  Filter'
    { categoryFilter = Prelude.Nothing,
      numericEqualityFilter = Prelude.Nothing,
      numericRangeFilter = Prelude.Nothing,
      relativeDatesFilter = Prelude.Nothing,
      timeEqualityFilter = Prelude.Nothing,
      timeRangeFilter = Prelude.Nothing,
      topBottomFilter = Prelude.Nothing
    }

-- | A @CategoryFilter@ filters text values.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/add-a-text-filter-data-prep.html Adding text filters>
-- in the /Amazon QuickSight User Guide/.
filter_categoryFilter :: Lens.Lens' Filter (Prelude.Maybe CategoryFilter)
filter_categoryFilter = Lens.lens (\Filter' {categoryFilter} -> categoryFilter) (\s@Filter' {} a -> s {categoryFilter = a} :: Filter)

-- | A @NumericEqualityFilter@ filters numeric values that equal or do not
-- equal a given numeric value.
filter_numericEqualityFilter :: Lens.Lens' Filter (Prelude.Maybe NumericEqualityFilter)
filter_numericEqualityFilter = Lens.lens (\Filter' {numericEqualityFilter} -> numericEqualityFilter) (\s@Filter' {} a -> s {numericEqualityFilter = a} :: Filter)

-- | A @NumericRangeFilter@ filters numeric values that are either inside or
-- outside a given numeric range.
filter_numericRangeFilter :: Lens.Lens' Filter (Prelude.Maybe NumericRangeFilter)
filter_numericRangeFilter = Lens.lens (\Filter' {numericRangeFilter} -> numericRangeFilter) (\s@Filter' {} a -> s {numericRangeFilter = a} :: Filter)

-- | A @RelativeDatesFilter@ filters date values that are relative to a given
-- date.
filter_relativeDatesFilter :: Lens.Lens' Filter (Prelude.Maybe RelativeDatesFilter)
filter_relativeDatesFilter = Lens.lens (\Filter' {relativeDatesFilter} -> relativeDatesFilter) (\s@Filter' {} a -> s {relativeDatesFilter = a} :: Filter)

-- | A @TimeEqualityFilter@ filters date-time values that equal or do not
-- equal a given date\/time value.
filter_timeEqualityFilter :: Lens.Lens' Filter (Prelude.Maybe TimeEqualityFilter)
filter_timeEqualityFilter = Lens.lens (\Filter' {timeEqualityFilter} -> timeEqualityFilter) (\s@Filter' {} a -> s {timeEqualityFilter = a} :: Filter)

-- | A @TimeRangeFilter@ filters date-time values that are either inside or
-- outside a given date\/time range.
filter_timeRangeFilter :: Lens.Lens' Filter (Prelude.Maybe TimeRangeFilter)
filter_timeRangeFilter = Lens.lens (\Filter' {timeRangeFilter} -> timeRangeFilter) (\s@Filter' {} a -> s {timeRangeFilter = a} :: Filter)

-- | A @TopBottomFilter@ filters data to the top or bottom values for a given
-- column.
filter_topBottomFilter :: Lens.Lens' Filter (Prelude.Maybe TopBottomFilter)
filter_topBottomFilter = Lens.lens (\Filter' {topBottomFilter} -> topBottomFilter) (\s@Filter' {} a -> s {topBottomFilter = a} :: Filter)

instance Data.FromJSON Filter where
  parseJSON =
    Data.withObject
      "Filter"
      ( \x ->
          Filter'
            Prelude.<$> (x Data..:? "CategoryFilter")
            Prelude.<*> (x Data..:? "NumericEqualityFilter")
            Prelude.<*> (x Data..:? "NumericRangeFilter")
            Prelude.<*> (x Data..:? "RelativeDatesFilter")
            Prelude.<*> (x Data..:? "TimeEqualityFilter")
            Prelude.<*> (x Data..:? "TimeRangeFilter")
            Prelude.<*> (x Data..:? "TopBottomFilter")
      )

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt
      `Prelude.hashWithSalt` categoryFilter
      `Prelude.hashWithSalt` numericEqualityFilter
      `Prelude.hashWithSalt` numericRangeFilter
      `Prelude.hashWithSalt` relativeDatesFilter
      `Prelude.hashWithSalt` timeEqualityFilter
      `Prelude.hashWithSalt` timeRangeFilter
      `Prelude.hashWithSalt` topBottomFilter

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf categoryFilter
      `Prelude.seq` Prelude.rnf numericEqualityFilter
      `Prelude.seq` Prelude.rnf numericRangeFilter
      `Prelude.seq` Prelude.rnf relativeDatesFilter
      `Prelude.seq` Prelude.rnf timeEqualityFilter
      `Prelude.seq` Prelude.rnf timeRangeFilter
      `Prelude.seq` Prelude.rnf topBottomFilter

instance Data.ToJSON Filter where
  toJSON Filter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryFilter" Data..=)
              Prelude.<$> categoryFilter,
            ("NumericEqualityFilter" Data..=)
              Prelude.<$> numericEqualityFilter,
            ("NumericRangeFilter" Data..=)
              Prelude.<$> numericRangeFilter,
            ("RelativeDatesFilter" Data..=)
              Prelude.<$> relativeDatesFilter,
            ("TimeEqualityFilter" Data..=)
              Prelude.<$> timeEqualityFilter,
            ("TimeRangeFilter" Data..=)
              Prelude.<$> timeRangeFilter,
            ("TopBottomFilter" Data..=)
              Prelude.<$> topBottomFilter
          ]
      )
