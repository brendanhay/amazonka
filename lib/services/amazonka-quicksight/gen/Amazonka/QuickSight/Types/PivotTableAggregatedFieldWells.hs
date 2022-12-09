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
-- Module      : Amazonka.QuickSight.Types.PivotTableAggregatedFieldWells
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The aggregated field well for the pivot table.
--
-- /See:/ 'newPivotTableAggregatedFieldWells' smart constructor.
data PivotTableAggregatedFieldWells = PivotTableAggregatedFieldWells'
  { -- | The columns field well for a pivot table. Values are grouped by columns
    -- fields.
    columns :: Prelude.Maybe [DimensionField],
    -- | The rows field well for a pivot table. Values are grouped by rows
    -- fields.
    rows :: Prelude.Maybe [DimensionField],
    -- | The values field well for a pivot table. Values are aggregated based on
    -- rows and columns fields.
    values :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columns', 'pivotTableAggregatedFieldWells_columns' - The columns field well for a pivot table. Values are grouped by columns
-- fields.
--
-- 'rows', 'pivotTableAggregatedFieldWells_rows' - The rows field well for a pivot table. Values are grouped by rows
-- fields.
--
-- 'values', 'pivotTableAggregatedFieldWells_values' - The values field well for a pivot table. Values are aggregated based on
-- rows and columns fields.
newPivotTableAggregatedFieldWells ::
  PivotTableAggregatedFieldWells
newPivotTableAggregatedFieldWells =
  PivotTableAggregatedFieldWells'
    { columns =
        Prelude.Nothing,
      rows = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The columns field well for a pivot table. Values are grouped by columns
-- fields.
pivotTableAggregatedFieldWells_columns :: Lens.Lens' PivotTableAggregatedFieldWells (Prelude.Maybe [DimensionField])
pivotTableAggregatedFieldWells_columns = Lens.lens (\PivotTableAggregatedFieldWells' {columns} -> columns) (\s@PivotTableAggregatedFieldWells' {} a -> s {columns = a} :: PivotTableAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The rows field well for a pivot table. Values are grouped by rows
-- fields.
pivotTableAggregatedFieldWells_rows :: Lens.Lens' PivotTableAggregatedFieldWells (Prelude.Maybe [DimensionField])
pivotTableAggregatedFieldWells_rows = Lens.lens (\PivotTableAggregatedFieldWells' {rows} -> rows) (\s@PivotTableAggregatedFieldWells' {} a -> s {rows = a} :: PivotTableAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The values field well for a pivot table. Values are aggregated based on
-- rows and columns fields.
pivotTableAggregatedFieldWells_values :: Lens.Lens' PivotTableAggregatedFieldWells (Prelude.Maybe [MeasureField])
pivotTableAggregatedFieldWells_values = Lens.lens (\PivotTableAggregatedFieldWells' {values} -> values) (\s@PivotTableAggregatedFieldWells' {} a -> s {values = a} :: PivotTableAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PivotTableAggregatedFieldWells where
  parseJSON =
    Data.withObject
      "PivotTableAggregatedFieldWells"
      ( \x ->
          PivotTableAggregatedFieldWells'
            Prelude.<$> (x Data..:? "Columns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Rows" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    PivotTableAggregatedFieldWells
  where
  hashWithSalt
    _salt
    PivotTableAggregatedFieldWells' {..} =
      _salt `Prelude.hashWithSalt` columns
        `Prelude.hashWithSalt` rows
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    PivotTableAggregatedFieldWells
  where
  rnf PivotTableAggregatedFieldWells' {..} =
    Prelude.rnf columns
      `Prelude.seq` Prelude.rnf rows
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON PivotTableAggregatedFieldWells where
  toJSON PivotTableAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Columns" Data..=) Prelude.<$> columns,
            ("Rows" Data..=) Prelude.<$> rows,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
