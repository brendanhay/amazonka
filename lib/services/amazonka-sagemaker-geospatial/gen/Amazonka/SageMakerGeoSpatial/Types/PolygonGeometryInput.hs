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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.PolygonGeometryInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.PolygonGeometryInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newPolygonGeometryInput' smart constructor.
data PolygonGeometryInput = PolygonGeometryInput'
  { coordinates :: Prelude.NonEmpty (Prelude.NonEmpty (Data.Sensitive (Prelude.NonEmpty Prelude.Double)))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolygonGeometryInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coordinates', 'polygonGeometryInput_coordinates' -
newPolygonGeometryInput ::
  -- | 'coordinates'
  Prelude.NonEmpty (Prelude.NonEmpty (Prelude.NonEmpty Prelude.Double)) ->
  PolygonGeometryInput
newPolygonGeometryInput pCoordinates_ =
  PolygonGeometryInput'
    { coordinates =
        Lens.coerced Lens.# pCoordinates_
    }

polygonGeometryInput_coordinates :: Lens.Lens' PolygonGeometryInput (Prelude.NonEmpty (Prelude.NonEmpty (Prelude.NonEmpty Prelude.Double)))
polygonGeometryInput_coordinates = Lens.lens (\PolygonGeometryInput' {coordinates} -> coordinates) (\s@PolygonGeometryInput' {} a -> s {coordinates = a} :: PolygonGeometryInput) Prelude.. Lens.coerced

instance Data.FromJSON PolygonGeometryInput where
  parseJSON =
    Data.withObject
      "PolygonGeometryInput"
      ( \x ->
          PolygonGeometryInput'
            Prelude.<$> (x Data..: "Coordinates")
      )

instance Prelude.Hashable PolygonGeometryInput where
  hashWithSalt _salt PolygonGeometryInput' {..} =
    _salt `Prelude.hashWithSalt` coordinates

instance Prelude.NFData PolygonGeometryInput where
  rnf PolygonGeometryInput' {..} =
    Prelude.rnf coordinates

instance Data.ToJSON PolygonGeometryInput where
  toJSON PolygonGeometryInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Coordinates" Data..= coordinates)]
      )
