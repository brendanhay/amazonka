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
-- Module      : Amazonka.QuickSight.Types.PivotTableSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableSortBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnSort
import Amazonka.QuickSight.Types.DataPathSort
import Amazonka.QuickSight.Types.FieldSort

-- | The sort by field for the field sort options.
--
-- /See:/ 'newPivotTableSortBy' smart constructor.
data PivotTableSortBy = PivotTableSortBy'
  { -- | The column sort (field id, direction) for the pivot table sort by
    -- options.
    column :: Prelude.Maybe ColumnSort,
    -- | The data path sort (data path value, direction) for the pivot table sort
    -- by options.
    dataPath :: Prelude.Maybe DataPathSort,
    -- | The field sort (field id, direction) for the pivot table sort by
    -- options.
    field :: Prelude.Maybe FieldSort
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableSortBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'column', 'pivotTableSortBy_column' - The column sort (field id, direction) for the pivot table sort by
-- options.
--
-- 'dataPath', 'pivotTableSortBy_dataPath' - The data path sort (data path value, direction) for the pivot table sort
-- by options.
--
-- 'field', 'pivotTableSortBy_field' - The field sort (field id, direction) for the pivot table sort by
-- options.
newPivotTableSortBy ::
  PivotTableSortBy
newPivotTableSortBy =
  PivotTableSortBy'
    { column = Prelude.Nothing,
      dataPath = Prelude.Nothing,
      field = Prelude.Nothing
    }

-- | The column sort (field id, direction) for the pivot table sort by
-- options.
pivotTableSortBy_column :: Lens.Lens' PivotTableSortBy (Prelude.Maybe ColumnSort)
pivotTableSortBy_column = Lens.lens (\PivotTableSortBy' {column} -> column) (\s@PivotTableSortBy' {} a -> s {column = a} :: PivotTableSortBy)

-- | The data path sort (data path value, direction) for the pivot table sort
-- by options.
pivotTableSortBy_dataPath :: Lens.Lens' PivotTableSortBy (Prelude.Maybe DataPathSort)
pivotTableSortBy_dataPath = Lens.lens (\PivotTableSortBy' {dataPath} -> dataPath) (\s@PivotTableSortBy' {} a -> s {dataPath = a} :: PivotTableSortBy)

-- | The field sort (field id, direction) for the pivot table sort by
-- options.
pivotTableSortBy_field :: Lens.Lens' PivotTableSortBy (Prelude.Maybe FieldSort)
pivotTableSortBy_field = Lens.lens (\PivotTableSortBy' {field} -> field) (\s@PivotTableSortBy' {} a -> s {field = a} :: PivotTableSortBy)

instance Data.FromJSON PivotTableSortBy where
  parseJSON =
    Data.withObject
      "PivotTableSortBy"
      ( \x ->
          PivotTableSortBy'
            Prelude.<$> (x Data..:? "Column")
            Prelude.<*> (x Data..:? "DataPath")
            Prelude.<*> (x Data..:? "Field")
      )

instance Prelude.Hashable PivotTableSortBy where
  hashWithSalt _salt PivotTableSortBy' {..} =
    _salt
      `Prelude.hashWithSalt` column
      `Prelude.hashWithSalt` dataPath
      `Prelude.hashWithSalt` field

instance Prelude.NFData PivotTableSortBy where
  rnf PivotTableSortBy' {..} =
    Prelude.rnf column
      `Prelude.seq` Prelude.rnf dataPath
      `Prelude.seq` Prelude.rnf field

instance Data.ToJSON PivotTableSortBy where
  toJSON PivotTableSortBy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Column" Data..=) Prelude.<$> column,
            ("DataPath" Data..=) Prelude.<$> dataPath,
            ("Field" Data..=) Prelude.<$> field
          ]
      )
