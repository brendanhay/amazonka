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
-- Module      : Amazonka.QuickSight.Types.PieChartAggregatedFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PieChartAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The field well configuration of a pie chart.
--
-- /See:/ 'newPieChartAggregatedFieldWells' smart constructor.
data PieChartAggregatedFieldWells = PieChartAggregatedFieldWells'
  { -- | The category (group\/color) field wells of a pie chart.
    category :: Prelude.Maybe [DimensionField],
    -- | The small multiples field well of a pie chart.
    smallMultiples :: Prelude.Maybe [DimensionField],
    -- | The value field wells of a pie chart. Values are aggregated based on
    -- categories.
    values :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PieChartAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'pieChartAggregatedFieldWells_category' - The category (group\/color) field wells of a pie chart.
--
-- 'smallMultiples', 'pieChartAggregatedFieldWells_smallMultiples' - The small multiples field well of a pie chart.
--
-- 'values', 'pieChartAggregatedFieldWells_values' - The value field wells of a pie chart. Values are aggregated based on
-- categories.
newPieChartAggregatedFieldWells ::
  PieChartAggregatedFieldWells
newPieChartAggregatedFieldWells =
  PieChartAggregatedFieldWells'
    { category =
        Prelude.Nothing,
      smallMultiples = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The category (group\/color) field wells of a pie chart.
pieChartAggregatedFieldWells_category :: Lens.Lens' PieChartAggregatedFieldWells (Prelude.Maybe [DimensionField])
pieChartAggregatedFieldWells_category = Lens.lens (\PieChartAggregatedFieldWells' {category} -> category) (\s@PieChartAggregatedFieldWells' {} a -> s {category = a} :: PieChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The small multiples field well of a pie chart.
pieChartAggregatedFieldWells_smallMultiples :: Lens.Lens' PieChartAggregatedFieldWells (Prelude.Maybe [DimensionField])
pieChartAggregatedFieldWells_smallMultiples = Lens.lens (\PieChartAggregatedFieldWells' {smallMultiples} -> smallMultiples) (\s@PieChartAggregatedFieldWells' {} a -> s {smallMultiples = a} :: PieChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The value field wells of a pie chart. Values are aggregated based on
-- categories.
pieChartAggregatedFieldWells_values :: Lens.Lens' PieChartAggregatedFieldWells (Prelude.Maybe [MeasureField])
pieChartAggregatedFieldWells_values = Lens.lens (\PieChartAggregatedFieldWells' {values} -> values) (\s@PieChartAggregatedFieldWells' {} a -> s {values = a} :: PieChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PieChartAggregatedFieldWells where
  parseJSON =
    Data.withObject
      "PieChartAggregatedFieldWells"
      ( \x ->
          PieChartAggregatedFieldWells'
            Prelude.<$> (x Data..:? "Category" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SmallMultiples" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    PieChartAggregatedFieldWells
  where
  hashWithSalt _salt PieChartAggregatedFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` smallMultiples
      `Prelude.hashWithSalt` values

instance Prelude.NFData PieChartAggregatedFieldWells where
  rnf PieChartAggregatedFieldWells' {..} =
    Prelude.rnf category
      `Prelude.seq` Prelude.rnf smallMultiples
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON PieChartAggregatedFieldWells where
  toJSON PieChartAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Category" Data..=) Prelude.<$> category,
            ("SmallMultiples" Data..=)
              Prelude.<$> smallMultiples,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
