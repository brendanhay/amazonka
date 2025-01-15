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
-- Module      : Amazonka.QuickSight.Types.FunnelChartAggregatedFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FunnelChartAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The field well configuration of a @FunnelChartVisual@.
--
-- /See:/ 'newFunnelChartAggregatedFieldWells' smart constructor.
data FunnelChartAggregatedFieldWells = FunnelChartAggregatedFieldWells'
  { -- | The category field wells of a funnel chart. Values are grouped by
    -- category fields.
    category :: Prelude.Maybe [DimensionField],
    -- | The value field wells of a funnel chart. Values are aggregated based on
    -- categories.
    values :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunnelChartAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'funnelChartAggregatedFieldWells_category' - The category field wells of a funnel chart. Values are grouped by
-- category fields.
--
-- 'values', 'funnelChartAggregatedFieldWells_values' - The value field wells of a funnel chart. Values are aggregated based on
-- categories.
newFunnelChartAggregatedFieldWells ::
  FunnelChartAggregatedFieldWells
newFunnelChartAggregatedFieldWells =
  FunnelChartAggregatedFieldWells'
    { category =
        Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The category field wells of a funnel chart. Values are grouped by
-- category fields.
funnelChartAggregatedFieldWells_category :: Lens.Lens' FunnelChartAggregatedFieldWells (Prelude.Maybe [DimensionField])
funnelChartAggregatedFieldWells_category = Lens.lens (\FunnelChartAggregatedFieldWells' {category} -> category) (\s@FunnelChartAggregatedFieldWells' {} a -> s {category = a} :: FunnelChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The value field wells of a funnel chart. Values are aggregated based on
-- categories.
funnelChartAggregatedFieldWells_values :: Lens.Lens' FunnelChartAggregatedFieldWells (Prelude.Maybe [MeasureField])
funnelChartAggregatedFieldWells_values = Lens.lens (\FunnelChartAggregatedFieldWells' {values} -> values) (\s@FunnelChartAggregatedFieldWells' {} a -> s {values = a} :: FunnelChartAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    FunnelChartAggregatedFieldWells
  where
  parseJSON =
    Data.withObject
      "FunnelChartAggregatedFieldWells"
      ( \x ->
          FunnelChartAggregatedFieldWells'
            Prelude.<$> (x Data..:? "Category" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    FunnelChartAggregatedFieldWells
  where
  hashWithSalt
    _salt
    FunnelChartAggregatedFieldWells' {..} =
      _salt
        `Prelude.hashWithSalt` category
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    FunnelChartAggregatedFieldWells
  where
  rnf FunnelChartAggregatedFieldWells' {..} =
    Prelude.rnf category `Prelude.seq`
      Prelude.rnf values

instance Data.ToJSON FunnelChartAggregatedFieldWells where
  toJSON FunnelChartAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Category" Data..=) Prelude.<$> category,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
