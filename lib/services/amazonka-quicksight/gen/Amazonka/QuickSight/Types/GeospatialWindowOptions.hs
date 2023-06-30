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
-- Module      : Amazonka.QuickSight.Types.GeospatialWindowOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GeospatialWindowOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GeospatialCoordinateBounds
import Amazonka.QuickSight.Types.MapZoomMode

-- | The window options of the geospatial map visual.
--
-- /See:/ 'newGeospatialWindowOptions' smart constructor.
data GeospatialWindowOptions = GeospatialWindowOptions'
  { -- | The bounds options (north, south, west, east) of the geospatial window
    -- options.
    bounds :: Prelude.Maybe GeospatialCoordinateBounds,
    -- | The map zoom modes (manual, auto) of the geospatial window options.
    mapZoomMode :: Prelude.Maybe MapZoomMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeospatialWindowOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bounds', 'geospatialWindowOptions_bounds' - The bounds options (north, south, west, east) of the geospatial window
-- options.
--
-- 'mapZoomMode', 'geospatialWindowOptions_mapZoomMode' - The map zoom modes (manual, auto) of the geospatial window options.
newGeospatialWindowOptions ::
  GeospatialWindowOptions
newGeospatialWindowOptions =
  GeospatialWindowOptions'
    { bounds = Prelude.Nothing,
      mapZoomMode = Prelude.Nothing
    }

-- | The bounds options (north, south, west, east) of the geospatial window
-- options.
geospatialWindowOptions_bounds :: Lens.Lens' GeospatialWindowOptions (Prelude.Maybe GeospatialCoordinateBounds)
geospatialWindowOptions_bounds = Lens.lens (\GeospatialWindowOptions' {bounds} -> bounds) (\s@GeospatialWindowOptions' {} a -> s {bounds = a} :: GeospatialWindowOptions)

-- | The map zoom modes (manual, auto) of the geospatial window options.
geospatialWindowOptions_mapZoomMode :: Lens.Lens' GeospatialWindowOptions (Prelude.Maybe MapZoomMode)
geospatialWindowOptions_mapZoomMode = Lens.lens (\GeospatialWindowOptions' {mapZoomMode} -> mapZoomMode) (\s@GeospatialWindowOptions' {} a -> s {mapZoomMode = a} :: GeospatialWindowOptions)

instance Data.FromJSON GeospatialWindowOptions where
  parseJSON =
    Data.withObject
      "GeospatialWindowOptions"
      ( \x ->
          GeospatialWindowOptions'
            Prelude.<$> (x Data..:? "Bounds")
            Prelude.<*> (x Data..:? "MapZoomMode")
      )

instance Prelude.Hashable GeospatialWindowOptions where
  hashWithSalt _salt GeospatialWindowOptions' {..} =
    _salt
      `Prelude.hashWithSalt` bounds
      `Prelude.hashWithSalt` mapZoomMode

instance Prelude.NFData GeospatialWindowOptions where
  rnf GeospatialWindowOptions' {..} =
    Prelude.rnf bounds
      `Prelude.seq` Prelude.rnf mapZoomMode

instance Data.ToJSON GeospatialWindowOptions where
  toJSON GeospatialWindowOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Bounds" Data..=) Prelude.<$> bounds,
            ("MapZoomMode" Data..=) Prelude.<$> mapZoomMode
          ]
      )
