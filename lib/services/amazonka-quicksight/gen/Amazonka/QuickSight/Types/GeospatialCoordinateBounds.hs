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
-- Module      : Amazonka.QuickSight.Types.GeospatialCoordinateBounds
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GeospatialCoordinateBounds where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The bound options (north, south, west, east) of the geospatial window
-- options.
--
-- /See:/ 'newGeospatialCoordinateBounds' smart constructor.
data GeospatialCoordinateBounds = GeospatialCoordinateBounds'
  { -- | The latitude of the north bound of the geospatial coordinate bounds.
    north :: Prelude.Double,
    -- | The latitude of the south bound of the geospatial coordinate bounds.
    south :: Prelude.Double,
    -- | The longitude of the west bound of the geospatial coordinate bounds.
    west :: Prelude.Double,
    -- | The longitude of the east bound of the geospatial coordinate bounds.
    east :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeospatialCoordinateBounds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'north', 'geospatialCoordinateBounds_north' - The latitude of the north bound of the geospatial coordinate bounds.
--
-- 'south', 'geospatialCoordinateBounds_south' - The latitude of the south bound of the geospatial coordinate bounds.
--
-- 'west', 'geospatialCoordinateBounds_west' - The longitude of the west bound of the geospatial coordinate bounds.
--
-- 'east', 'geospatialCoordinateBounds_east' - The longitude of the east bound of the geospatial coordinate bounds.
newGeospatialCoordinateBounds ::
  -- | 'north'
  Prelude.Double ->
  -- | 'south'
  Prelude.Double ->
  -- | 'west'
  Prelude.Double ->
  -- | 'east'
  Prelude.Double ->
  GeospatialCoordinateBounds
newGeospatialCoordinateBounds
  pNorth_
  pSouth_
  pWest_
  pEast_ =
    GeospatialCoordinateBounds'
      { north = pNorth_,
        south = pSouth_,
        west = pWest_,
        east = pEast_
      }

-- | The latitude of the north bound of the geospatial coordinate bounds.
geospatialCoordinateBounds_north :: Lens.Lens' GeospatialCoordinateBounds Prelude.Double
geospatialCoordinateBounds_north = Lens.lens (\GeospatialCoordinateBounds' {north} -> north) (\s@GeospatialCoordinateBounds' {} a -> s {north = a} :: GeospatialCoordinateBounds)

-- | The latitude of the south bound of the geospatial coordinate bounds.
geospatialCoordinateBounds_south :: Lens.Lens' GeospatialCoordinateBounds Prelude.Double
geospatialCoordinateBounds_south = Lens.lens (\GeospatialCoordinateBounds' {south} -> south) (\s@GeospatialCoordinateBounds' {} a -> s {south = a} :: GeospatialCoordinateBounds)

-- | The longitude of the west bound of the geospatial coordinate bounds.
geospatialCoordinateBounds_west :: Lens.Lens' GeospatialCoordinateBounds Prelude.Double
geospatialCoordinateBounds_west = Lens.lens (\GeospatialCoordinateBounds' {west} -> west) (\s@GeospatialCoordinateBounds' {} a -> s {west = a} :: GeospatialCoordinateBounds)

-- | The longitude of the east bound of the geospatial coordinate bounds.
geospatialCoordinateBounds_east :: Lens.Lens' GeospatialCoordinateBounds Prelude.Double
geospatialCoordinateBounds_east = Lens.lens (\GeospatialCoordinateBounds' {east} -> east) (\s@GeospatialCoordinateBounds' {} a -> s {east = a} :: GeospatialCoordinateBounds)

instance Data.FromJSON GeospatialCoordinateBounds where
  parseJSON =
    Data.withObject
      "GeospatialCoordinateBounds"
      ( \x ->
          GeospatialCoordinateBounds'
            Prelude.<$> (x Data..: "North")
            Prelude.<*> (x Data..: "South")
            Prelude.<*> (x Data..: "West")
            Prelude.<*> (x Data..: "East")
      )

instance Prelude.Hashable GeospatialCoordinateBounds where
  hashWithSalt _salt GeospatialCoordinateBounds' {..} =
    _salt `Prelude.hashWithSalt` north
      `Prelude.hashWithSalt` south
      `Prelude.hashWithSalt` west
      `Prelude.hashWithSalt` east

instance Prelude.NFData GeospatialCoordinateBounds where
  rnf GeospatialCoordinateBounds' {..} =
    Prelude.rnf north
      `Prelude.seq` Prelude.rnf south
      `Prelude.seq` Prelude.rnf west
      `Prelude.seq` Prelude.rnf east

instance Data.ToJSON GeospatialCoordinateBounds where
  toJSON GeospatialCoordinateBounds' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("North" Data..= north),
            Prelude.Just ("South" Data..= south),
            Prelude.Just ("West" Data..= west),
            Prelude.Just ("East" Data..= east)
          ]
      )
