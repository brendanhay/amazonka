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
-- Module      : Amazonka.QuickSight.Types.GeospatialHeatmapConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GeospatialHeatmapConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GeospatialHeatmapColorScale

-- | The heatmap configuration of the geospatial point style.
--
-- /See:/ 'newGeospatialHeatmapConfiguration' smart constructor.
data GeospatialHeatmapConfiguration = GeospatialHeatmapConfiguration'
  { -- | The color scale specification for the heatmap point style.
    heatmapColor :: Prelude.Maybe GeospatialHeatmapColorScale
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeospatialHeatmapConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'heatmapColor', 'geospatialHeatmapConfiguration_heatmapColor' - The color scale specification for the heatmap point style.
newGeospatialHeatmapConfiguration ::
  GeospatialHeatmapConfiguration
newGeospatialHeatmapConfiguration =
  GeospatialHeatmapConfiguration'
    { heatmapColor =
        Prelude.Nothing
    }

-- | The color scale specification for the heatmap point style.
geospatialHeatmapConfiguration_heatmapColor :: Lens.Lens' GeospatialHeatmapConfiguration (Prelude.Maybe GeospatialHeatmapColorScale)
geospatialHeatmapConfiguration_heatmapColor = Lens.lens (\GeospatialHeatmapConfiguration' {heatmapColor} -> heatmapColor) (\s@GeospatialHeatmapConfiguration' {} a -> s {heatmapColor = a} :: GeospatialHeatmapConfiguration)

instance Data.FromJSON GeospatialHeatmapConfiguration where
  parseJSON =
    Data.withObject
      "GeospatialHeatmapConfiguration"
      ( \x ->
          GeospatialHeatmapConfiguration'
            Prelude.<$> (x Data..:? "HeatmapColor")
      )

instance
  Prelude.Hashable
    GeospatialHeatmapConfiguration
  where
  hashWithSalt
    _salt
    GeospatialHeatmapConfiguration' {..} =
      _salt `Prelude.hashWithSalt` heatmapColor

instance
  Prelude.NFData
    GeospatialHeatmapConfiguration
  where
  rnf GeospatialHeatmapConfiguration' {..} =
    Prelude.rnf heatmapColor

instance Data.ToJSON GeospatialHeatmapConfiguration where
  toJSON GeospatialHeatmapConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("HeatmapColor" Data..=) Prelude.<$> heatmapColor]
      )
