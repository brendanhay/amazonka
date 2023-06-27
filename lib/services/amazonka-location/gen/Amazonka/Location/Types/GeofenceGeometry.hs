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
-- Module      : Amazonka.Location.Types.GeofenceGeometry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.GeofenceGeometry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.Circle
import qualified Amazonka.Prelude as Prelude

-- | Contains the geofence geometry details.
--
-- A geofence geometry is made up of either a polygon or a circle. Can be
-- either a polygon or a circle. Including both will return a validation
-- error.
--
-- Amazon Location doesn\'t currently support polygons with holes,
-- multipolygons, polygons that are wound clockwise, or that cross the
-- antimeridian.
--
-- /See:/ 'newGeofenceGeometry' smart constructor.
data GeofenceGeometry = GeofenceGeometry'
  { -- | A circle on the earth, as defined by a center point and a radius.
    circle :: Prelude.Maybe (Data.Sensitive Circle),
    -- | A polygon is a list of linear rings which are each made up of a list of
    -- vertices.
    --
    -- Each vertex is a 2-dimensional point of the form:
    -- @[longitude, latitude]@. This is represented as an array of doubles of
    -- length 2 (so @[double, double]@).
    --
    -- An array of 4 or more vertices, where the first and last vertex are the
    -- same (to form a closed boundary), is called a linear ring. The linear
    -- ring vertices must be listed in counter-clockwise order around the
    -- ring’s interior. The linear ring is represented as an array of vertices,
    -- or an array of arrays of doubles (@[[double, double], ...]@).
    --
    -- A geofence consists of a single linear ring. To allow for future
    -- expansion, the Polygon parameter takes an array of linear rings, which
    -- is represented as an array of arrays of arrays of doubles
    -- (@[[[double, double], ...], ...]@).
    --
    -- A linear ring for use in geofences can consist of between 4 and 1,000
    -- vertices.
    polygon :: Prelude.Maybe (Prelude.NonEmpty (Prelude.NonEmpty (Data.Sensitive (Prelude.NonEmpty Prelude.Double))))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeofenceGeometry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'circle', 'geofenceGeometry_circle' - A circle on the earth, as defined by a center point and a radius.
--
-- 'polygon', 'geofenceGeometry_polygon' - A polygon is a list of linear rings which are each made up of a list of
-- vertices.
--
-- Each vertex is a 2-dimensional point of the form:
-- @[longitude, latitude]@. This is represented as an array of doubles of
-- length 2 (so @[double, double]@).
--
-- An array of 4 or more vertices, where the first and last vertex are the
-- same (to form a closed boundary), is called a linear ring. The linear
-- ring vertices must be listed in counter-clockwise order around the
-- ring’s interior. The linear ring is represented as an array of vertices,
-- or an array of arrays of doubles (@[[double, double], ...]@).
--
-- A geofence consists of a single linear ring. To allow for future
-- expansion, the Polygon parameter takes an array of linear rings, which
-- is represented as an array of arrays of arrays of doubles
-- (@[[[double, double], ...], ...]@).
--
-- A linear ring for use in geofences can consist of between 4 and 1,000
-- vertices.
newGeofenceGeometry ::
  GeofenceGeometry
newGeofenceGeometry =
  GeofenceGeometry'
    { circle = Prelude.Nothing,
      polygon = Prelude.Nothing
    }

-- | A circle on the earth, as defined by a center point and a radius.
geofenceGeometry_circle :: Lens.Lens' GeofenceGeometry (Prelude.Maybe Circle)
geofenceGeometry_circle = Lens.lens (\GeofenceGeometry' {circle} -> circle) (\s@GeofenceGeometry' {} a -> s {circle = a} :: GeofenceGeometry) Prelude.. Lens.mapping Data._Sensitive

-- | A polygon is a list of linear rings which are each made up of a list of
-- vertices.
--
-- Each vertex is a 2-dimensional point of the form:
-- @[longitude, latitude]@. This is represented as an array of doubles of
-- length 2 (so @[double, double]@).
--
-- An array of 4 or more vertices, where the first and last vertex are the
-- same (to form a closed boundary), is called a linear ring. The linear
-- ring vertices must be listed in counter-clockwise order around the
-- ring’s interior. The linear ring is represented as an array of vertices,
-- or an array of arrays of doubles (@[[double, double], ...]@).
--
-- A geofence consists of a single linear ring. To allow for future
-- expansion, the Polygon parameter takes an array of linear rings, which
-- is represented as an array of arrays of arrays of doubles
-- (@[[[double, double], ...], ...]@).
--
-- A linear ring for use in geofences can consist of between 4 and 1,000
-- vertices.
geofenceGeometry_polygon :: Lens.Lens' GeofenceGeometry (Prelude.Maybe (Prelude.NonEmpty (Prelude.NonEmpty (Prelude.NonEmpty Prelude.Double))))
geofenceGeometry_polygon = Lens.lens (\GeofenceGeometry' {polygon} -> polygon) (\s@GeofenceGeometry' {} a -> s {polygon = a} :: GeofenceGeometry) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON GeofenceGeometry where
  parseJSON =
    Data.withObject
      "GeofenceGeometry"
      ( \x ->
          GeofenceGeometry'
            Prelude.<$> (x Data..:? "Circle")
            Prelude.<*> (x Data..:? "Polygon")
      )

instance Prelude.Hashable GeofenceGeometry where
  hashWithSalt _salt GeofenceGeometry' {..} =
    _salt
      `Prelude.hashWithSalt` circle
      `Prelude.hashWithSalt` polygon

instance Prelude.NFData GeofenceGeometry where
  rnf GeofenceGeometry' {..} =
    Prelude.rnf circle
      `Prelude.seq` Prelude.rnf polygon

instance Data.ToJSON GeofenceGeometry where
  toJSON GeofenceGeometry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Circle" Data..=) Prelude.<$> circle,
            ("Polygon" Data..=) Prelude.<$> polygon
          ]
      )
