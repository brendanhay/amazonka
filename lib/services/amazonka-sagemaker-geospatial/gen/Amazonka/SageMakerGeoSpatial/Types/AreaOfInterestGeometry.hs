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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.AreaOfInterestGeometry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.AreaOfInterestGeometry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.MultiPolygonGeometryInput
import Amazonka.SageMakerGeoSpatial.Types.PolygonGeometryInput

-- | A GeoJSON object representing the geographic extent in the coordinate
-- space.
--
-- /See:/ 'newAreaOfInterestGeometry' smart constructor.
data AreaOfInterestGeometry = AreaOfInterestGeometry'
  { -- | The structure representing the MultiPolygon Geometry.
    multiPolygonGeometry :: Prelude.Maybe MultiPolygonGeometryInput,
    -- | The structure representing Polygon Geometry.
    polygonGeometry :: Prelude.Maybe PolygonGeometryInput
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AreaOfInterestGeometry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiPolygonGeometry', 'areaOfInterestGeometry_multiPolygonGeometry' - The structure representing the MultiPolygon Geometry.
--
-- 'polygonGeometry', 'areaOfInterestGeometry_polygonGeometry' - The structure representing Polygon Geometry.
newAreaOfInterestGeometry ::
  AreaOfInterestGeometry
newAreaOfInterestGeometry =
  AreaOfInterestGeometry'
    { multiPolygonGeometry =
        Prelude.Nothing,
      polygonGeometry = Prelude.Nothing
    }

-- | The structure representing the MultiPolygon Geometry.
areaOfInterestGeometry_multiPolygonGeometry :: Lens.Lens' AreaOfInterestGeometry (Prelude.Maybe MultiPolygonGeometryInput)
areaOfInterestGeometry_multiPolygonGeometry = Lens.lens (\AreaOfInterestGeometry' {multiPolygonGeometry} -> multiPolygonGeometry) (\s@AreaOfInterestGeometry' {} a -> s {multiPolygonGeometry = a} :: AreaOfInterestGeometry)

-- | The structure representing Polygon Geometry.
areaOfInterestGeometry_polygonGeometry :: Lens.Lens' AreaOfInterestGeometry (Prelude.Maybe PolygonGeometryInput)
areaOfInterestGeometry_polygonGeometry = Lens.lens (\AreaOfInterestGeometry' {polygonGeometry} -> polygonGeometry) (\s@AreaOfInterestGeometry' {} a -> s {polygonGeometry = a} :: AreaOfInterestGeometry)

instance Data.FromJSON AreaOfInterestGeometry where
  parseJSON =
    Data.withObject
      "AreaOfInterestGeometry"
      ( \x ->
          AreaOfInterestGeometry'
            Prelude.<$> (x Data..:? "MultiPolygonGeometry")
            Prelude.<*> (x Data..:? "PolygonGeometry")
      )

instance Prelude.Hashable AreaOfInterestGeometry where
  hashWithSalt _salt AreaOfInterestGeometry' {..} =
    _salt
      `Prelude.hashWithSalt` multiPolygonGeometry
      `Prelude.hashWithSalt` polygonGeometry

instance Prelude.NFData AreaOfInterestGeometry where
  rnf AreaOfInterestGeometry' {..} =
    Prelude.rnf multiPolygonGeometry
      `Prelude.seq` Prelude.rnf polygonGeometry

instance Data.ToJSON AreaOfInterestGeometry where
  toJSON AreaOfInterestGeometry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MultiPolygonGeometry" Data..=)
              Prelude.<$> multiPolygonGeometry,
            ("PolygonGeometry" Data..=)
              Prelude.<$> polygonGeometry
          ]
      )
