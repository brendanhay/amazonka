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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.MultiPolygonGeometryInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.MultiPolygonGeometryInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newMultiPolygonGeometryInput' smart constructor.
data MultiPolygonGeometryInput = MultiPolygonGeometryInput'
  { -- | The coordinates of the multipolygon geometry.
    coordinates :: [Prelude.NonEmpty (Prelude.NonEmpty (Data.Sensitive (Prelude.NonEmpty Prelude.Double)))]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiPolygonGeometryInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coordinates', 'multiPolygonGeometryInput_coordinates' - The coordinates of the multipolygon geometry.
newMultiPolygonGeometryInput ::
  MultiPolygonGeometryInput
newMultiPolygonGeometryInput =
  MultiPolygonGeometryInput'
    { coordinates =
        Prelude.mempty
    }

-- | The coordinates of the multipolygon geometry.
multiPolygonGeometryInput_coordinates :: Lens.Lens' MultiPolygonGeometryInput [Prelude.NonEmpty (Prelude.NonEmpty (Prelude.NonEmpty Prelude.Double))]
multiPolygonGeometryInput_coordinates = Lens.lens (\MultiPolygonGeometryInput' {coordinates} -> coordinates) (\s@MultiPolygonGeometryInput' {} a -> s {coordinates = a} :: MultiPolygonGeometryInput) Prelude.. Lens.coerced

instance Data.FromJSON MultiPolygonGeometryInput where
  parseJSON =
    Data.withObject
      "MultiPolygonGeometryInput"
      ( \x ->
          MultiPolygonGeometryInput'
            Prelude.<$> (x Data..:? "Coordinates" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable MultiPolygonGeometryInput where
  hashWithSalt _salt MultiPolygonGeometryInput' {..} =
    _salt `Prelude.hashWithSalt` coordinates

instance Prelude.NFData MultiPolygonGeometryInput where
  rnf MultiPolygonGeometryInput' {..} =
    Prelude.rnf coordinates

instance Data.ToJSON MultiPolygonGeometryInput where
  toJSON MultiPolygonGeometryInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Coordinates" Data..= coordinates)]
      )
