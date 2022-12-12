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
-- Module      : Amazonka.QuickSight.Types.LineChartAggregatedFieldWells
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LineChartAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The field well configuration of a line chart.
--
-- /See:/ 'newLineChartAggregatedFieldWells' smart constructor.
data LineChartAggregatedFieldWells = LineChartAggregatedFieldWells'
  { -- | The category field wells of a line chart. Values are grouped by category
    -- fields.
    category :: Prelude.Maybe [DimensionField],
    -- | The color field wells of a line chart. Values are grouped by category
    -- fields.
    colors :: Prelude.Maybe [DimensionField],
    -- | The small multiples field well of a line chart.
    smallMultiples :: Prelude.Maybe [DimensionField],
    -- | The value field wells of a line chart. Values are aggregated based on
    -- categories.
    values :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LineChartAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'lineChartAggregatedFieldWells_category' - The category field wells of a line chart. Values are grouped by category
-- fields.
--
-- 'colors', 'lineChartAggregatedFieldWells_colors' - The color field wells of a line chart. Values are grouped by category
-- fields.
--
-- 'smallMultiples', 'lineChartAggregatedFieldWells_smallMultiples' - The small multiples field well of a line chart.
--
-- 'values', 'lineChartAggregatedFieldWells_values' - The value field wells of a line chart. Values are aggregated based on
-- categories.
newLineChartAggregatedFieldWells ::
  LineChartAggregatedFieldWells
newLineChartAggregatedFieldWells =
  LineChartAggregatedFieldWells'
    { category =
        Prelude.Nothing,
      colors = Prelude.Nothing,
      smallMultiples = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The category field wells of a line chart. Values are grouped by category
-- fields.
lineChartAggregatedFieldWells_category :: Lens.Lens' LineChartAggregatedFieldWells (Prelude.Maybe [DimensionField])
lineChartAggregatedFieldWells_category = Lens.lens (\LineChartAggregatedFieldWells' {category} -> category) (\s@LineChartAggregatedFieldWells' {} a -> s {category = a} :: LineChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The color field wells of a line chart. Values are grouped by category
-- fields.
lineChartAggregatedFieldWells_colors :: Lens.Lens' LineChartAggregatedFieldWells (Prelude.Maybe [DimensionField])
lineChartAggregatedFieldWells_colors = Lens.lens (\LineChartAggregatedFieldWells' {colors} -> colors) (\s@LineChartAggregatedFieldWells' {} a -> s {colors = a} :: LineChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The small multiples field well of a line chart.
lineChartAggregatedFieldWells_smallMultiples :: Lens.Lens' LineChartAggregatedFieldWells (Prelude.Maybe [DimensionField])
lineChartAggregatedFieldWells_smallMultiples = Lens.lens (\LineChartAggregatedFieldWells' {smallMultiples} -> smallMultiples) (\s@LineChartAggregatedFieldWells' {} a -> s {smallMultiples = a} :: LineChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The value field wells of a line chart. Values are aggregated based on
-- categories.
lineChartAggregatedFieldWells_values :: Lens.Lens' LineChartAggregatedFieldWells (Prelude.Maybe [MeasureField])
lineChartAggregatedFieldWells_values = Lens.lens (\LineChartAggregatedFieldWells' {values} -> values) (\s@LineChartAggregatedFieldWells' {} a -> s {values = a} :: LineChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LineChartAggregatedFieldWells where
  parseJSON =
    Data.withObject
      "LineChartAggregatedFieldWells"
      ( \x ->
          LineChartAggregatedFieldWells'
            Prelude.<$> (x Data..:? "Category" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Colors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SmallMultiples" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    LineChartAggregatedFieldWells
  where
  hashWithSalt _salt LineChartAggregatedFieldWells' {..} =
    _salt `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` colors
      `Prelude.hashWithSalt` smallMultiples
      `Prelude.hashWithSalt` values

instance Prelude.NFData LineChartAggregatedFieldWells where
  rnf LineChartAggregatedFieldWells' {..} =
    Prelude.rnf category
      `Prelude.seq` Prelude.rnf colors
      `Prelude.seq` Prelude.rnf smallMultiples
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON LineChartAggregatedFieldWells where
  toJSON LineChartAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Category" Data..=) Prelude.<$> category,
            ("Colors" Data..=) Prelude.<$> colors,
            ("SmallMultiples" Data..=)
              Prelude.<$> smallMultiples,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
