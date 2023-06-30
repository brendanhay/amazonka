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
-- Module      : Amazonka.QuickSight.Types.BarChartAggregatedFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BarChartAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The aggregated field wells of a bar chart.
--
-- /See:/ 'newBarChartAggregatedFieldWells' smart constructor.
data BarChartAggregatedFieldWells = BarChartAggregatedFieldWells'
  { -- | The category (y-axis) field well of a bar chart.
    category :: Prelude.Maybe [DimensionField],
    -- | The color (group\/color) field well of a bar chart.
    colors :: Prelude.Maybe [DimensionField],
    -- | The small multiples field well of a bar chart.
    smallMultiples :: Prelude.Maybe [DimensionField],
    -- | The value field wells of a bar chart. Values are aggregated by category.
    values :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BarChartAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'barChartAggregatedFieldWells_category' - The category (y-axis) field well of a bar chart.
--
-- 'colors', 'barChartAggregatedFieldWells_colors' - The color (group\/color) field well of a bar chart.
--
-- 'smallMultiples', 'barChartAggregatedFieldWells_smallMultiples' - The small multiples field well of a bar chart.
--
-- 'values', 'barChartAggregatedFieldWells_values' - The value field wells of a bar chart. Values are aggregated by category.
newBarChartAggregatedFieldWells ::
  BarChartAggregatedFieldWells
newBarChartAggregatedFieldWells =
  BarChartAggregatedFieldWells'
    { category =
        Prelude.Nothing,
      colors = Prelude.Nothing,
      smallMultiples = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The category (y-axis) field well of a bar chart.
barChartAggregatedFieldWells_category :: Lens.Lens' BarChartAggregatedFieldWells (Prelude.Maybe [DimensionField])
barChartAggregatedFieldWells_category = Lens.lens (\BarChartAggregatedFieldWells' {category} -> category) (\s@BarChartAggregatedFieldWells' {} a -> s {category = a} :: BarChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The color (group\/color) field well of a bar chart.
barChartAggregatedFieldWells_colors :: Lens.Lens' BarChartAggregatedFieldWells (Prelude.Maybe [DimensionField])
barChartAggregatedFieldWells_colors = Lens.lens (\BarChartAggregatedFieldWells' {colors} -> colors) (\s@BarChartAggregatedFieldWells' {} a -> s {colors = a} :: BarChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The small multiples field well of a bar chart.
barChartAggregatedFieldWells_smallMultiples :: Lens.Lens' BarChartAggregatedFieldWells (Prelude.Maybe [DimensionField])
barChartAggregatedFieldWells_smallMultiples = Lens.lens (\BarChartAggregatedFieldWells' {smallMultiples} -> smallMultiples) (\s@BarChartAggregatedFieldWells' {} a -> s {smallMultiples = a} :: BarChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The value field wells of a bar chart. Values are aggregated by category.
barChartAggregatedFieldWells_values :: Lens.Lens' BarChartAggregatedFieldWells (Prelude.Maybe [MeasureField])
barChartAggregatedFieldWells_values = Lens.lens (\BarChartAggregatedFieldWells' {values} -> values) (\s@BarChartAggregatedFieldWells' {} a -> s {values = a} :: BarChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON BarChartAggregatedFieldWells where
  parseJSON =
    Data.withObject
      "BarChartAggregatedFieldWells"
      ( \x ->
          BarChartAggregatedFieldWells'
            Prelude.<$> (x Data..:? "Category" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Colors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SmallMultiples" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    BarChartAggregatedFieldWells
  where
  hashWithSalt _salt BarChartAggregatedFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` colors
      `Prelude.hashWithSalt` smallMultiples
      `Prelude.hashWithSalt` values

instance Prelude.NFData BarChartAggregatedFieldWells where
  rnf BarChartAggregatedFieldWells' {..} =
    Prelude.rnf category
      `Prelude.seq` Prelude.rnf colors
      `Prelude.seq` Prelude.rnf smallMultiples
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON BarChartAggregatedFieldWells where
  toJSON BarChartAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Category" Data..=) Prelude.<$> category,
            ("Colors" Data..=) Prelude.<$> colors,
            ("SmallMultiples" Data..=)
              Prelude.<$> smallMultiples,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
