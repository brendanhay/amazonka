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
-- Module      : Amazonka.QuickSight.Types.FilledMapAggregatedFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilledMapAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The aggregated field well of the filled map.
--
-- /See:/ 'newFilledMapAggregatedFieldWells' smart constructor.
data FilledMapAggregatedFieldWells = FilledMapAggregatedFieldWells'
  { -- | The aggregated location field well of the filled map. Values are grouped
    -- by location fields.
    geospatial :: Prelude.Maybe [DimensionField],
    -- | The aggregated color field well of a filled map. Values are aggregated
    -- based on location fields.
    values :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilledMapAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geospatial', 'filledMapAggregatedFieldWells_geospatial' - The aggregated location field well of the filled map. Values are grouped
-- by location fields.
--
-- 'values', 'filledMapAggregatedFieldWells_values' - The aggregated color field well of a filled map. Values are aggregated
-- based on location fields.
newFilledMapAggregatedFieldWells ::
  FilledMapAggregatedFieldWells
newFilledMapAggregatedFieldWells =
  FilledMapAggregatedFieldWells'
    { geospatial =
        Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The aggregated location field well of the filled map. Values are grouped
-- by location fields.
filledMapAggregatedFieldWells_geospatial :: Lens.Lens' FilledMapAggregatedFieldWells (Prelude.Maybe [DimensionField])
filledMapAggregatedFieldWells_geospatial = Lens.lens (\FilledMapAggregatedFieldWells' {geospatial} -> geospatial) (\s@FilledMapAggregatedFieldWells' {} a -> s {geospatial = a} :: FilledMapAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The aggregated color field well of a filled map. Values are aggregated
-- based on location fields.
filledMapAggregatedFieldWells_values :: Lens.Lens' FilledMapAggregatedFieldWells (Prelude.Maybe [MeasureField])
filledMapAggregatedFieldWells_values = Lens.lens (\FilledMapAggregatedFieldWells' {values} -> values) (\s@FilledMapAggregatedFieldWells' {} a -> s {values = a} :: FilledMapAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FilledMapAggregatedFieldWells where
  parseJSON =
    Data.withObject
      "FilledMapAggregatedFieldWells"
      ( \x ->
          FilledMapAggregatedFieldWells'
            Prelude.<$> (x Data..:? "Geospatial" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    FilledMapAggregatedFieldWells
  where
  hashWithSalt _salt FilledMapAggregatedFieldWells' {..} =
    _salt `Prelude.hashWithSalt` geospatial
      `Prelude.hashWithSalt` values

instance Prelude.NFData FilledMapAggregatedFieldWells where
  rnf FilledMapAggregatedFieldWells' {..} =
    Prelude.rnf geospatial
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON FilledMapAggregatedFieldWells where
  toJSON FilledMapAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Geospatial" Data..=) Prelude.<$> geospatial,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
