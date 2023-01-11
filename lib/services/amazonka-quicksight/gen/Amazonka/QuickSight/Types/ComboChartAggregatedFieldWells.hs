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
-- Module      : Amazonka.QuickSight.Types.ComboChartAggregatedFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ComboChartAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The aggregated field wells of a combo chart.
--
-- /See:/ 'newComboChartAggregatedFieldWells' smart constructor.
data ComboChartAggregatedFieldWells = ComboChartAggregatedFieldWells'
  { -- | The aggregated @BarValues@ field well of a combo chart.
    barValues :: Prelude.Maybe [MeasureField],
    -- | The aggregated category field wells of a combo chart.
    category :: Prelude.Maybe [DimensionField],
    -- | The aggregated colors field well of a combo chart.
    colors :: Prelude.Maybe [DimensionField],
    -- | The aggregated @LineValues@ field well of a combo chart.
    lineValues :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComboChartAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'barValues', 'comboChartAggregatedFieldWells_barValues' - The aggregated @BarValues@ field well of a combo chart.
--
-- 'category', 'comboChartAggregatedFieldWells_category' - The aggregated category field wells of a combo chart.
--
-- 'colors', 'comboChartAggregatedFieldWells_colors' - The aggregated colors field well of a combo chart.
--
-- 'lineValues', 'comboChartAggregatedFieldWells_lineValues' - The aggregated @LineValues@ field well of a combo chart.
newComboChartAggregatedFieldWells ::
  ComboChartAggregatedFieldWells
newComboChartAggregatedFieldWells =
  ComboChartAggregatedFieldWells'
    { barValues =
        Prelude.Nothing,
      category = Prelude.Nothing,
      colors = Prelude.Nothing,
      lineValues = Prelude.Nothing
    }

-- | The aggregated @BarValues@ field well of a combo chart.
comboChartAggregatedFieldWells_barValues :: Lens.Lens' ComboChartAggregatedFieldWells (Prelude.Maybe [MeasureField])
comboChartAggregatedFieldWells_barValues = Lens.lens (\ComboChartAggregatedFieldWells' {barValues} -> barValues) (\s@ComboChartAggregatedFieldWells' {} a -> s {barValues = a} :: ComboChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The aggregated category field wells of a combo chart.
comboChartAggregatedFieldWells_category :: Lens.Lens' ComboChartAggregatedFieldWells (Prelude.Maybe [DimensionField])
comboChartAggregatedFieldWells_category = Lens.lens (\ComboChartAggregatedFieldWells' {category} -> category) (\s@ComboChartAggregatedFieldWells' {} a -> s {category = a} :: ComboChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The aggregated colors field well of a combo chart.
comboChartAggregatedFieldWells_colors :: Lens.Lens' ComboChartAggregatedFieldWells (Prelude.Maybe [DimensionField])
comboChartAggregatedFieldWells_colors = Lens.lens (\ComboChartAggregatedFieldWells' {colors} -> colors) (\s@ComboChartAggregatedFieldWells' {} a -> s {colors = a} :: ComboChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The aggregated @LineValues@ field well of a combo chart.
comboChartAggregatedFieldWells_lineValues :: Lens.Lens' ComboChartAggregatedFieldWells (Prelude.Maybe [MeasureField])
comboChartAggregatedFieldWells_lineValues = Lens.lens (\ComboChartAggregatedFieldWells' {lineValues} -> lineValues) (\s@ComboChartAggregatedFieldWells' {} a -> s {lineValues = a} :: ComboChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ComboChartAggregatedFieldWells where
  parseJSON =
    Data.withObject
      "ComboChartAggregatedFieldWells"
      ( \x ->
          ComboChartAggregatedFieldWells'
            Prelude.<$> (x Data..:? "BarValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Category" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Colors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LineValues" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    ComboChartAggregatedFieldWells
  where
  hashWithSalt
    _salt
    ComboChartAggregatedFieldWells' {..} =
      _salt `Prelude.hashWithSalt` barValues
        `Prelude.hashWithSalt` category
        `Prelude.hashWithSalt` colors
        `Prelude.hashWithSalt` lineValues

instance
  Prelude.NFData
    ComboChartAggregatedFieldWells
  where
  rnf ComboChartAggregatedFieldWells' {..} =
    Prelude.rnf barValues
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf colors
      `Prelude.seq` Prelude.rnf lineValues

instance Data.ToJSON ComboChartAggregatedFieldWells where
  toJSON ComboChartAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BarValues" Data..=) Prelude.<$> barValues,
            ("Category" Data..=) Prelude.<$> category,
            ("Colors" Data..=) Prelude.<$> colors,
            ("LineValues" Data..=) Prelude.<$> lineValues
          ]
      )
