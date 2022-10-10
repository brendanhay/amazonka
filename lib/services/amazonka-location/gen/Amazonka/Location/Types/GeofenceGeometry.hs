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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.GeofenceGeometry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
  { -- | An array of 1 or more linear rings. A linear ring is an array of 4 or
    -- more vertices, where the first and last vertex are the same to form a
    -- closed boundary. Each vertex is a 2-dimensional point of the form:
    -- @[longitude, latitude]@.
    --
    -- The first linear ring is an outer ring, describing the polygon\'s
    -- boundary. Subsequent linear rings may be inner or outer rings to
    -- describe holes and islands. Outer rings must list their vertices in
    -- counter-clockwise order around the ring\'s center, where the left side
    -- is the polygon\'s exterior. Inner rings must list their vertices in
    -- clockwise order, where the left side is the polygon\'s interior.
    --
    -- A geofence polygon can consist of between 4 and 1,000 vertices.
    polygon :: Prelude.Maybe (Prelude.NonEmpty (Prelude.NonEmpty (Core.Sensitive (Prelude.NonEmpty Prelude.Double)))),
    -- | A circle on the earth, as defined by a center point and a radius.
    circle :: Prelude.Maybe (Core.Sensitive Circle)
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
-- 'polygon', 'geofenceGeometry_polygon' - An array of 1 or more linear rings. A linear ring is an array of 4 or
-- more vertices, where the first and last vertex are the same to form a
-- closed boundary. Each vertex is a 2-dimensional point of the form:
-- @[longitude, latitude]@.
--
-- The first linear ring is an outer ring, describing the polygon\'s
-- boundary. Subsequent linear rings may be inner or outer rings to
-- describe holes and islands. Outer rings must list their vertices in
-- counter-clockwise order around the ring\'s center, where the left side
-- is the polygon\'s exterior. Inner rings must list their vertices in
-- clockwise order, where the left side is the polygon\'s interior.
--
-- A geofence polygon can consist of between 4 and 1,000 vertices.
--
-- 'circle', 'geofenceGeometry_circle' - A circle on the earth, as defined by a center point and a radius.
newGeofenceGeometry ::
  GeofenceGeometry
newGeofenceGeometry =
  GeofenceGeometry'
    { polygon = Prelude.Nothing,
      circle = Prelude.Nothing
    }

-- | An array of 1 or more linear rings. A linear ring is an array of 4 or
-- more vertices, where the first and last vertex are the same to form a
-- closed boundary. Each vertex is a 2-dimensional point of the form:
-- @[longitude, latitude]@.
--
-- The first linear ring is an outer ring, describing the polygon\'s
-- boundary. Subsequent linear rings may be inner or outer rings to
-- describe holes and islands. Outer rings must list their vertices in
-- counter-clockwise order around the ring\'s center, where the left side
-- is the polygon\'s exterior. Inner rings must list their vertices in
-- clockwise order, where the left side is the polygon\'s interior.
--
-- A geofence polygon can consist of between 4 and 1,000 vertices.
geofenceGeometry_polygon :: Lens.Lens' GeofenceGeometry (Prelude.Maybe (Prelude.NonEmpty (Prelude.NonEmpty (Prelude.NonEmpty Prelude.Double))))
geofenceGeometry_polygon = Lens.lens (\GeofenceGeometry' {polygon} -> polygon) (\s@GeofenceGeometry' {} a -> s {polygon = a} :: GeofenceGeometry) Prelude.. Lens.mapping Lens.coerced

-- | A circle on the earth, as defined by a center point and a radius.
geofenceGeometry_circle :: Lens.Lens' GeofenceGeometry (Prelude.Maybe Circle)
geofenceGeometry_circle = Lens.lens (\GeofenceGeometry' {circle} -> circle) (\s@GeofenceGeometry' {} a -> s {circle = a} :: GeofenceGeometry) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON GeofenceGeometry where
  parseJSON =
    Core.withObject
      "GeofenceGeometry"
      ( \x ->
          GeofenceGeometry'
            Prelude.<$> (x Core..:? "Polygon")
            Prelude.<*> (x Core..:? "Circle")
      )

instance Prelude.Hashable GeofenceGeometry where
  hashWithSalt _salt GeofenceGeometry' {..} =
    _salt `Prelude.hashWithSalt` polygon
      `Prelude.hashWithSalt` circle

instance Prelude.NFData GeofenceGeometry where
  rnf GeofenceGeometry' {..} =
    Prelude.rnf polygon
      `Prelude.seq` Prelude.rnf circle

instance Core.ToJSON GeofenceGeometry where
  toJSON GeofenceGeometry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Polygon" Core..=) Prelude.<$> polygon,
            ("Circle" Core..=) Prelude.<$> circle
          ]
      )
