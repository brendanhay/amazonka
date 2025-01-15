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
-- Module      : Amazonka.QuickSight.Types.HeatMapAggregatedFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.HeatMapAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The aggregated field wells of a heat map.
--
-- /See:/ 'newHeatMapAggregatedFieldWells' smart constructor.
data HeatMapAggregatedFieldWells = HeatMapAggregatedFieldWells'
  { -- | The columns field well of a heat map.
    columns :: Prelude.Maybe [DimensionField],
    -- | The rows field well of a heat map.
    rows :: Prelude.Maybe [DimensionField],
    -- | The values field well of a heat map.
    values :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HeatMapAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columns', 'heatMapAggregatedFieldWells_columns' - The columns field well of a heat map.
--
-- 'rows', 'heatMapAggregatedFieldWells_rows' - The rows field well of a heat map.
--
-- 'values', 'heatMapAggregatedFieldWells_values' - The values field well of a heat map.
newHeatMapAggregatedFieldWells ::
  HeatMapAggregatedFieldWells
newHeatMapAggregatedFieldWells =
  HeatMapAggregatedFieldWells'
    { columns =
        Prelude.Nothing,
      rows = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The columns field well of a heat map.
heatMapAggregatedFieldWells_columns :: Lens.Lens' HeatMapAggregatedFieldWells (Prelude.Maybe [DimensionField])
heatMapAggregatedFieldWells_columns = Lens.lens (\HeatMapAggregatedFieldWells' {columns} -> columns) (\s@HeatMapAggregatedFieldWells' {} a -> s {columns = a} :: HeatMapAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The rows field well of a heat map.
heatMapAggregatedFieldWells_rows :: Lens.Lens' HeatMapAggregatedFieldWells (Prelude.Maybe [DimensionField])
heatMapAggregatedFieldWells_rows = Lens.lens (\HeatMapAggregatedFieldWells' {rows} -> rows) (\s@HeatMapAggregatedFieldWells' {} a -> s {rows = a} :: HeatMapAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The values field well of a heat map.
heatMapAggregatedFieldWells_values :: Lens.Lens' HeatMapAggregatedFieldWells (Prelude.Maybe [MeasureField])
heatMapAggregatedFieldWells_values = Lens.lens (\HeatMapAggregatedFieldWells' {values} -> values) (\s@HeatMapAggregatedFieldWells' {} a -> s {values = a} :: HeatMapAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON HeatMapAggregatedFieldWells where
  parseJSON =
    Data.withObject
      "HeatMapAggregatedFieldWells"
      ( \x ->
          HeatMapAggregatedFieldWells'
            Prelude.<$> (x Data..:? "Columns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Rows" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable HeatMapAggregatedFieldWells where
  hashWithSalt _salt HeatMapAggregatedFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` columns
      `Prelude.hashWithSalt` rows
      `Prelude.hashWithSalt` values

instance Prelude.NFData HeatMapAggregatedFieldWells where
  rnf HeatMapAggregatedFieldWells' {..} =
    Prelude.rnf columns `Prelude.seq`
      Prelude.rnf rows `Prelude.seq`
        Prelude.rnf values

instance Data.ToJSON HeatMapAggregatedFieldWells where
  toJSON HeatMapAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Columns" Data..=) Prelude.<$> columns,
            ("Rows" Data..=) Prelude.<$> rows,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
