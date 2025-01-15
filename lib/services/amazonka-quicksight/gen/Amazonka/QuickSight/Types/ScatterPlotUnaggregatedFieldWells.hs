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
-- Module      : Amazonka.QuickSight.Types.ScatterPlotUnaggregatedFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ScatterPlotUnaggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The unaggregated field wells of a scatter plot.
--
-- /See:/ 'newScatterPlotUnaggregatedFieldWells' smart constructor.
data ScatterPlotUnaggregatedFieldWells = ScatterPlotUnaggregatedFieldWells'
  { -- | The size field well of a scatter plot.
    size :: Prelude.Maybe [MeasureField],
    -- | The x-axis field well of a scatter plot.
    --
    -- The x-axis is a dimension field and cannot be aggregated.
    xAxis :: Prelude.Maybe [DimensionField],
    -- | The y-axis field well of a scatter plot.
    --
    -- The y-axis is a dimension field and cannot be aggregated.
    yAxis :: Prelude.Maybe [DimensionField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScatterPlotUnaggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'size', 'scatterPlotUnaggregatedFieldWells_size' - The size field well of a scatter plot.
--
-- 'xAxis', 'scatterPlotUnaggregatedFieldWells_xAxis' - The x-axis field well of a scatter plot.
--
-- The x-axis is a dimension field and cannot be aggregated.
--
-- 'yAxis', 'scatterPlotUnaggregatedFieldWells_yAxis' - The y-axis field well of a scatter plot.
--
-- The y-axis is a dimension field and cannot be aggregated.
newScatterPlotUnaggregatedFieldWells ::
  ScatterPlotUnaggregatedFieldWells
newScatterPlotUnaggregatedFieldWells =
  ScatterPlotUnaggregatedFieldWells'
    { size =
        Prelude.Nothing,
      xAxis = Prelude.Nothing,
      yAxis = Prelude.Nothing
    }

-- | The size field well of a scatter plot.
scatterPlotUnaggregatedFieldWells_size :: Lens.Lens' ScatterPlotUnaggregatedFieldWells (Prelude.Maybe [MeasureField])
scatterPlotUnaggregatedFieldWells_size = Lens.lens (\ScatterPlotUnaggregatedFieldWells' {size} -> size) (\s@ScatterPlotUnaggregatedFieldWells' {} a -> s {size = a} :: ScatterPlotUnaggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The x-axis field well of a scatter plot.
--
-- The x-axis is a dimension field and cannot be aggregated.
scatterPlotUnaggregatedFieldWells_xAxis :: Lens.Lens' ScatterPlotUnaggregatedFieldWells (Prelude.Maybe [DimensionField])
scatterPlotUnaggregatedFieldWells_xAxis = Lens.lens (\ScatterPlotUnaggregatedFieldWells' {xAxis} -> xAxis) (\s@ScatterPlotUnaggregatedFieldWells' {} a -> s {xAxis = a} :: ScatterPlotUnaggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The y-axis field well of a scatter plot.
--
-- The y-axis is a dimension field and cannot be aggregated.
scatterPlotUnaggregatedFieldWells_yAxis :: Lens.Lens' ScatterPlotUnaggregatedFieldWells (Prelude.Maybe [DimensionField])
scatterPlotUnaggregatedFieldWells_yAxis = Lens.lens (\ScatterPlotUnaggregatedFieldWells' {yAxis} -> yAxis) (\s@ScatterPlotUnaggregatedFieldWells' {} a -> s {yAxis = a} :: ScatterPlotUnaggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    ScatterPlotUnaggregatedFieldWells
  where
  parseJSON =
    Data.withObject
      "ScatterPlotUnaggregatedFieldWells"
      ( \x ->
          ScatterPlotUnaggregatedFieldWells'
            Prelude.<$> (x Data..:? "Size" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "XAxis" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "YAxis" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    ScatterPlotUnaggregatedFieldWells
  where
  hashWithSalt
    _salt
    ScatterPlotUnaggregatedFieldWells' {..} =
      _salt
        `Prelude.hashWithSalt` size
        `Prelude.hashWithSalt` xAxis
        `Prelude.hashWithSalt` yAxis

instance
  Prelude.NFData
    ScatterPlotUnaggregatedFieldWells
  where
  rnf ScatterPlotUnaggregatedFieldWells' {..} =
    Prelude.rnf size `Prelude.seq`
      Prelude.rnf xAxis `Prelude.seq`
        Prelude.rnf yAxis

instance
  Data.ToJSON
    ScatterPlotUnaggregatedFieldWells
  where
  toJSON ScatterPlotUnaggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Size" Data..=) Prelude.<$> size,
            ("XAxis" Data..=) Prelude.<$> xAxis,
            ("YAxis" Data..=) Prelude.<$> yAxis
          ]
      )
