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
-- Module      : Amazonka.QuickSight.Types.WaterfallChartAggregatedFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WaterfallChartAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The field well configuration of a waterfall visual.
--
-- /See:/ 'newWaterfallChartAggregatedFieldWells' smart constructor.
data WaterfallChartAggregatedFieldWells = WaterfallChartAggregatedFieldWells'
  { -- | The breakdown field wells of a waterfall visual.
    breakdowns :: Prelude.Maybe [DimensionField],
    -- | The category field wells of a waterfall visual.
    categories :: Prelude.Maybe [DimensionField],
    -- | The value field wells of a waterfall visual.
    values :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WaterfallChartAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'breakdowns', 'waterfallChartAggregatedFieldWells_breakdowns' - The breakdown field wells of a waterfall visual.
--
-- 'categories', 'waterfallChartAggregatedFieldWells_categories' - The category field wells of a waterfall visual.
--
-- 'values', 'waterfallChartAggregatedFieldWells_values' - The value field wells of a waterfall visual.
newWaterfallChartAggregatedFieldWells ::
  WaterfallChartAggregatedFieldWells
newWaterfallChartAggregatedFieldWells =
  WaterfallChartAggregatedFieldWells'
    { breakdowns =
        Prelude.Nothing,
      categories = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The breakdown field wells of a waterfall visual.
waterfallChartAggregatedFieldWells_breakdowns :: Lens.Lens' WaterfallChartAggregatedFieldWells (Prelude.Maybe [DimensionField])
waterfallChartAggregatedFieldWells_breakdowns = Lens.lens (\WaterfallChartAggregatedFieldWells' {breakdowns} -> breakdowns) (\s@WaterfallChartAggregatedFieldWells' {} a -> s {breakdowns = a} :: WaterfallChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The category field wells of a waterfall visual.
waterfallChartAggregatedFieldWells_categories :: Lens.Lens' WaterfallChartAggregatedFieldWells (Prelude.Maybe [DimensionField])
waterfallChartAggregatedFieldWells_categories = Lens.lens (\WaterfallChartAggregatedFieldWells' {categories} -> categories) (\s@WaterfallChartAggregatedFieldWells' {} a -> s {categories = a} :: WaterfallChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The value field wells of a waterfall visual.
waterfallChartAggregatedFieldWells_values :: Lens.Lens' WaterfallChartAggregatedFieldWells (Prelude.Maybe [MeasureField])
waterfallChartAggregatedFieldWells_values = Lens.lens (\WaterfallChartAggregatedFieldWells' {values} -> values) (\s@WaterfallChartAggregatedFieldWells' {} a -> s {values = a} :: WaterfallChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    WaterfallChartAggregatedFieldWells
  where
  parseJSON =
    Data.withObject
      "WaterfallChartAggregatedFieldWells"
      ( \x ->
          WaterfallChartAggregatedFieldWells'
            Prelude.<$> (x Data..:? "Breakdowns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Categories" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    WaterfallChartAggregatedFieldWells
  where
  hashWithSalt
    _salt
    WaterfallChartAggregatedFieldWells' {..} =
      _salt
        `Prelude.hashWithSalt` breakdowns
        `Prelude.hashWithSalt` categories
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    WaterfallChartAggregatedFieldWells
  where
  rnf WaterfallChartAggregatedFieldWells' {..} =
    Prelude.rnf breakdowns
      `Prelude.seq` Prelude.rnf categories
      `Prelude.seq` Prelude.rnf values

instance
  Data.ToJSON
    WaterfallChartAggregatedFieldWells
  where
  toJSON WaterfallChartAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Breakdowns" Data..=) Prelude.<$> breakdowns,
            ("Categories" Data..=) Prelude.<$> categories,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
