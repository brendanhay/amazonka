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
-- Module      : Amazonka.QuickSight.Types.GeospatialHeatmapColorScale
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GeospatialHeatmapColorScale where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GeospatialHeatmapDataColor

-- | The color scale specification for the heatmap point style.
--
-- /See:/ 'newGeospatialHeatmapColorScale' smart constructor.
data GeospatialHeatmapColorScale = GeospatialHeatmapColorScale'
  { -- | The list of colors to be used in heatmap point style.
    colors :: Prelude.Maybe (Prelude.NonEmpty GeospatialHeatmapDataColor)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeospatialHeatmapColorScale' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'colors', 'geospatialHeatmapColorScale_colors' - The list of colors to be used in heatmap point style.
newGeospatialHeatmapColorScale ::
  GeospatialHeatmapColorScale
newGeospatialHeatmapColorScale =
  GeospatialHeatmapColorScale'
    { colors =
        Prelude.Nothing
    }

-- | The list of colors to be used in heatmap point style.
geospatialHeatmapColorScale_colors :: Lens.Lens' GeospatialHeatmapColorScale (Prelude.Maybe (Prelude.NonEmpty GeospatialHeatmapDataColor))
geospatialHeatmapColorScale_colors = Lens.lens (\GeospatialHeatmapColorScale' {colors} -> colors) (\s@GeospatialHeatmapColorScale' {} a -> s {colors = a} :: GeospatialHeatmapColorScale) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON GeospatialHeatmapColorScale where
  parseJSON =
    Data.withObject
      "GeospatialHeatmapColorScale"
      ( \x ->
          GeospatialHeatmapColorScale'
            Prelude.<$> (x Data..:? "Colors")
      )

instance Prelude.Hashable GeospatialHeatmapColorScale where
  hashWithSalt _salt GeospatialHeatmapColorScale' {..} =
    _salt `Prelude.hashWithSalt` colors

instance Prelude.NFData GeospatialHeatmapColorScale where
  rnf GeospatialHeatmapColorScale' {..} =
    Prelude.rnf colors

instance Data.ToJSON GeospatialHeatmapColorScale where
  toJSON GeospatialHeatmapColorScale' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Colors" Data..=) Prelude.<$> colors]
      )
