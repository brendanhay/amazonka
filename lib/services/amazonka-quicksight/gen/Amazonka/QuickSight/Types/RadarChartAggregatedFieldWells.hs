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
-- Module      : Amazonka.QuickSight.Types.RadarChartAggregatedFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RadarChartAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The aggregated field well configuration of a @RadarChartVisual@.
--
-- /See:/ 'newRadarChartAggregatedFieldWells' smart constructor.
data RadarChartAggregatedFieldWells = RadarChartAggregatedFieldWells'
  { -- | The aggregated field well categories of a radar chart.
    category :: Prelude.Maybe [DimensionField],
    -- | The color that are assigned to the aggregated field wells of a radar
    -- chart.
    color :: Prelude.Maybe [DimensionField],
    -- | The values that are assigned to the aggregated field wells of a radar
    -- chart.
    values :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RadarChartAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'radarChartAggregatedFieldWells_category' - The aggregated field well categories of a radar chart.
--
-- 'color', 'radarChartAggregatedFieldWells_color' - The color that are assigned to the aggregated field wells of a radar
-- chart.
--
-- 'values', 'radarChartAggregatedFieldWells_values' - The values that are assigned to the aggregated field wells of a radar
-- chart.
newRadarChartAggregatedFieldWells ::
  RadarChartAggregatedFieldWells
newRadarChartAggregatedFieldWells =
  RadarChartAggregatedFieldWells'
    { category =
        Prelude.Nothing,
      color = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The aggregated field well categories of a radar chart.
radarChartAggregatedFieldWells_category :: Lens.Lens' RadarChartAggregatedFieldWells (Prelude.Maybe [DimensionField])
radarChartAggregatedFieldWells_category = Lens.lens (\RadarChartAggregatedFieldWells' {category} -> category) (\s@RadarChartAggregatedFieldWells' {} a -> s {category = a} :: RadarChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The color that are assigned to the aggregated field wells of a radar
-- chart.
radarChartAggregatedFieldWells_color :: Lens.Lens' RadarChartAggregatedFieldWells (Prelude.Maybe [DimensionField])
radarChartAggregatedFieldWells_color = Lens.lens (\RadarChartAggregatedFieldWells' {color} -> color) (\s@RadarChartAggregatedFieldWells' {} a -> s {color = a} :: RadarChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The values that are assigned to the aggregated field wells of a radar
-- chart.
radarChartAggregatedFieldWells_values :: Lens.Lens' RadarChartAggregatedFieldWells (Prelude.Maybe [MeasureField])
radarChartAggregatedFieldWells_values = Lens.lens (\RadarChartAggregatedFieldWells' {values} -> values) (\s@RadarChartAggregatedFieldWells' {} a -> s {values = a} :: RadarChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RadarChartAggregatedFieldWells where
  parseJSON =
    Data.withObject
      "RadarChartAggregatedFieldWells"
      ( \x ->
          RadarChartAggregatedFieldWells'
            Prelude.<$> (x Data..:? "Category" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Color" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    RadarChartAggregatedFieldWells
  where
  hashWithSalt
    _salt
    RadarChartAggregatedFieldWells' {..} =
      _salt
        `Prelude.hashWithSalt` category
        `Prelude.hashWithSalt` color
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    RadarChartAggregatedFieldWells
  where
  rnf RadarChartAggregatedFieldWells' {..} =
    Prelude.rnf category
      `Prelude.seq` Prelude.rnf color
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON RadarChartAggregatedFieldWells where
  toJSON RadarChartAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Category" Data..=) Prelude.<$> category,
            ("Color" Data..=) Prelude.<$> color,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
