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
-- Module      : Amazonka.QuickSight.Types.GeospatialMapAggregatedFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GeospatialMapAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The aggregated field wells for a geospatial map.
--
-- /See:/ 'newGeospatialMapAggregatedFieldWells' smart constructor.
data GeospatialMapAggregatedFieldWells = GeospatialMapAggregatedFieldWells'
  { -- | The color field wells of a geospatial map.
    colors :: Prelude.Maybe [DimensionField],
    -- | The geospatial field wells of a geospatial map. Values are grouped by
    -- geospatial fields.
    geospatial :: Prelude.Maybe [DimensionField],
    -- | The size field wells of a geospatial map. Values are aggregated based on
    -- geospatial fields.
    values :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeospatialMapAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'colors', 'geospatialMapAggregatedFieldWells_colors' - The color field wells of a geospatial map.
--
-- 'geospatial', 'geospatialMapAggregatedFieldWells_geospatial' - The geospatial field wells of a geospatial map. Values are grouped by
-- geospatial fields.
--
-- 'values', 'geospatialMapAggregatedFieldWells_values' - The size field wells of a geospatial map. Values are aggregated based on
-- geospatial fields.
newGeospatialMapAggregatedFieldWells ::
  GeospatialMapAggregatedFieldWells
newGeospatialMapAggregatedFieldWells =
  GeospatialMapAggregatedFieldWells'
    { colors =
        Prelude.Nothing,
      geospatial = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The color field wells of a geospatial map.
geospatialMapAggregatedFieldWells_colors :: Lens.Lens' GeospatialMapAggregatedFieldWells (Prelude.Maybe [DimensionField])
geospatialMapAggregatedFieldWells_colors = Lens.lens (\GeospatialMapAggregatedFieldWells' {colors} -> colors) (\s@GeospatialMapAggregatedFieldWells' {} a -> s {colors = a} :: GeospatialMapAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The geospatial field wells of a geospatial map. Values are grouped by
-- geospatial fields.
geospatialMapAggregatedFieldWells_geospatial :: Lens.Lens' GeospatialMapAggregatedFieldWells (Prelude.Maybe [DimensionField])
geospatialMapAggregatedFieldWells_geospatial = Lens.lens (\GeospatialMapAggregatedFieldWells' {geospatial} -> geospatial) (\s@GeospatialMapAggregatedFieldWells' {} a -> s {geospatial = a} :: GeospatialMapAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The size field wells of a geospatial map. Values are aggregated based on
-- geospatial fields.
geospatialMapAggregatedFieldWells_values :: Lens.Lens' GeospatialMapAggregatedFieldWells (Prelude.Maybe [MeasureField])
geospatialMapAggregatedFieldWells_values = Lens.lens (\GeospatialMapAggregatedFieldWells' {values} -> values) (\s@GeospatialMapAggregatedFieldWells' {} a -> s {values = a} :: GeospatialMapAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    GeospatialMapAggregatedFieldWells
  where
  parseJSON =
    Data.withObject
      "GeospatialMapAggregatedFieldWells"
      ( \x ->
          GeospatialMapAggregatedFieldWells'
            Prelude.<$> (x Data..:? "Colors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Geospatial" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    GeospatialMapAggregatedFieldWells
  where
  hashWithSalt
    _salt
    GeospatialMapAggregatedFieldWells' {..} =
      _salt
        `Prelude.hashWithSalt` colors
        `Prelude.hashWithSalt` geospatial
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    GeospatialMapAggregatedFieldWells
  where
  rnf GeospatialMapAggregatedFieldWells' {..} =
    Prelude.rnf colors
      `Prelude.seq` Prelude.rnf geospatial
      `Prelude.seq` Prelude.rnf values

instance
  Data.ToJSON
    GeospatialMapAggregatedFieldWells
  where
  toJSON GeospatialMapAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Colors" Data..=) Prelude.<$> colors,
            ("Geospatial" Data..=) Prelude.<$> geospatial,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
