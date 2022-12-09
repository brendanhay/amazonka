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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.Geometry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.Geometry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newGeometry' smart constructor.
data Geometry = Geometry'
  { coordinates :: Prelude.NonEmpty (Prelude.NonEmpty (Data.Sensitive (Prelude.NonEmpty Prelude.Double))),
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Geometry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coordinates', 'geometry_coordinates' -
--
-- 'type'', 'geometry_type' -
newGeometry ::
  -- | 'coordinates'
  Prelude.NonEmpty (Prelude.NonEmpty (Prelude.NonEmpty Prelude.Double)) ->
  -- | 'type''
  Prelude.Text ->
  Geometry
newGeometry pCoordinates_ pType_ =
  Geometry'
    { coordinates =
        Lens.coerced Lens.# pCoordinates_,
      type' = pType_
    }

-- |
geometry_coordinates :: Lens.Lens' Geometry (Prelude.NonEmpty (Prelude.NonEmpty (Prelude.NonEmpty Prelude.Double)))
geometry_coordinates = Lens.lens (\Geometry' {coordinates} -> coordinates) (\s@Geometry' {} a -> s {coordinates = a} :: Geometry) Prelude.. Lens.coerced

-- |
geometry_type :: Lens.Lens' Geometry Prelude.Text
geometry_type = Lens.lens (\Geometry' {type'} -> type') (\s@Geometry' {} a -> s {type' = a} :: Geometry)

instance Data.FromJSON Geometry where
  parseJSON =
    Data.withObject
      "Geometry"
      ( \x ->
          Geometry'
            Prelude.<$> (x Data..: "Coordinates")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable Geometry where
  hashWithSalt _salt Geometry' {..} =
    _salt `Prelude.hashWithSalt` coordinates
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Geometry where
  rnf Geometry' {..} =
    Prelude.rnf coordinates
      `Prelude.seq` Prelude.rnf type'
