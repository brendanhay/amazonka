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
-- Module      : Amazonka.QuickSight.Types.ScatterPlotCategoricallyAggregatedFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ScatterPlotCategoricallyAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The aggregated field well of a scatter plot.
--
-- /See:/ 'newScatterPlotCategoricallyAggregatedFieldWells' smart constructor.
data ScatterPlotCategoricallyAggregatedFieldWells = ScatterPlotCategoricallyAggregatedFieldWells'
  { -- | The category field well of a scatter plot.
    category :: Prelude.Maybe [DimensionField],
    -- | The size field well of a scatter plot.
    size :: Prelude.Maybe [MeasureField],
    -- | The x-axis field well of a scatter plot.
    --
    -- The x-axis is aggregated by category.
    xAxis :: Prelude.Maybe [MeasureField],
    -- | The y-axis field well of a scatter plot.
    --
    -- The y-axis is aggregated by category.
    yAxis :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScatterPlotCategoricallyAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'scatterPlotCategoricallyAggregatedFieldWells_category' - The category field well of a scatter plot.
--
-- 'size', 'scatterPlotCategoricallyAggregatedFieldWells_size' - The size field well of a scatter plot.
--
-- 'xAxis', 'scatterPlotCategoricallyAggregatedFieldWells_xAxis' - The x-axis field well of a scatter plot.
--
-- The x-axis is aggregated by category.
--
-- 'yAxis', 'scatterPlotCategoricallyAggregatedFieldWells_yAxis' - The y-axis field well of a scatter plot.
--
-- The y-axis is aggregated by category.
newScatterPlotCategoricallyAggregatedFieldWells ::
  ScatterPlotCategoricallyAggregatedFieldWells
newScatterPlotCategoricallyAggregatedFieldWells =
  ScatterPlotCategoricallyAggregatedFieldWells'
    { category =
        Prelude.Nothing,
      size = Prelude.Nothing,
      xAxis = Prelude.Nothing,
      yAxis = Prelude.Nothing
    }

-- | The category field well of a scatter plot.
scatterPlotCategoricallyAggregatedFieldWells_category :: Lens.Lens' ScatterPlotCategoricallyAggregatedFieldWells (Prelude.Maybe [DimensionField])
scatterPlotCategoricallyAggregatedFieldWells_category = Lens.lens (\ScatterPlotCategoricallyAggregatedFieldWells' {category} -> category) (\s@ScatterPlotCategoricallyAggregatedFieldWells' {} a -> s {category = a} :: ScatterPlotCategoricallyAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The size field well of a scatter plot.
scatterPlotCategoricallyAggregatedFieldWells_size :: Lens.Lens' ScatterPlotCategoricallyAggregatedFieldWells (Prelude.Maybe [MeasureField])
scatterPlotCategoricallyAggregatedFieldWells_size = Lens.lens (\ScatterPlotCategoricallyAggregatedFieldWells' {size} -> size) (\s@ScatterPlotCategoricallyAggregatedFieldWells' {} a -> s {size = a} :: ScatterPlotCategoricallyAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The x-axis field well of a scatter plot.
--
-- The x-axis is aggregated by category.
scatterPlotCategoricallyAggregatedFieldWells_xAxis :: Lens.Lens' ScatterPlotCategoricallyAggregatedFieldWells (Prelude.Maybe [MeasureField])
scatterPlotCategoricallyAggregatedFieldWells_xAxis = Lens.lens (\ScatterPlotCategoricallyAggregatedFieldWells' {xAxis} -> xAxis) (\s@ScatterPlotCategoricallyAggregatedFieldWells' {} a -> s {xAxis = a} :: ScatterPlotCategoricallyAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The y-axis field well of a scatter plot.
--
-- The y-axis is aggregated by category.
scatterPlotCategoricallyAggregatedFieldWells_yAxis :: Lens.Lens' ScatterPlotCategoricallyAggregatedFieldWells (Prelude.Maybe [MeasureField])
scatterPlotCategoricallyAggregatedFieldWells_yAxis = Lens.lens (\ScatterPlotCategoricallyAggregatedFieldWells' {yAxis} -> yAxis) (\s@ScatterPlotCategoricallyAggregatedFieldWells' {} a -> s {yAxis = a} :: ScatterPlotCategoricallyAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    ScatterPlotCategoricallyAggregatedFieldWells
  where
  parseJSON =
    Data.withObject
      "ScatterPlotCategoricallyAggregatedFieldWells"
      ( \x ->
          ScatterPlotCategoricallyAggregatedFieldWells'
            Prelude.<$> (x Data..:? "Category" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Size" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "XAxis" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "YAxis" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    ScatterPlotCategoricallyAggregatedFieldWells
  where
  hashWithSalt
    _salt
    ScatterPlotCategoricallyAggregatedFieldWells' {..} =
      _salt
        `Prelude.hashWithSalt` category
        `Prelude.hashWithSalt` size
        `Prelude.hashWithSalt` xAxis
        `Prelude.hashWithSalt` yAxis

instance
  Prelude.NFData
    ScatterPlotCategoricallyAggregatedFieldWells
  where
  rnf ScatterPlotCategoricallyAggregatedFieldWells' {..} =
    Prelude.rnf category
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf xAxis
      `Prelude.seq` Prelude.rnf yAxis

instance
  Data.ToJSON
    ScatterPlotCategoricallyAggregatedFieldWells
  where
  toJSON
    ScatterPlotCategoricallyAggregatedFieldWells' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Category" Data..=) Prelude.<$> category,
              ("Size" Data..=) Prelude.<$> size,
              ("XAxis" Data..=) Prelude.<$> xAxis,
              ("YAxis" Data..=) Prelude.<$> yAxis
            ]
        )
