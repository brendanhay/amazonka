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
-- Module      : Amazonka.Location.Types.PlaceGeometry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.PlaceGeometry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Places uses a point geometry to specify a location or a Place.
--
-- /See:/ 'newPlaceGeometry' smart constructor.
data PlaceGeometry = PlaceGeometry'
  { -- | A single point geometry specifies a location for a Place using
    -- <https://gisgeography.com/wgs84-world-geodetic-system/ WGS 84>
    -- coordinates:
    --
    -- -   /x/ — Specifies the x coordinate or longitude.
    --
    -- -   /y/ — Specifies the y coordinate or latitude.
    point :: Prelude.Maybe (Data.Sensitive (Prelude.NonEmpty Prelude.Double))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlaceGeometry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'point', 'placeGeometry_point' - A single point geometry specifies a location for a Place using
-- <https://gisgeography.com/wgs84-world-geodetic-system/ WGS 84>
-- coordinates:
--
-- -   /x/ — Specifies the x coordinate or longitude.
--
-- -   /y/ — Specifies the y coordinate or latitude.
newPlaceGeometry ::
  PlaceGeometry
newPlaceGeometry =
  PlaceGeometry' {point = Prelude.Nothing}

-- | A single point geometry specifies a location for a Place using
-- <https://gisgeography.com/wgs84-world-geodetic-system/ WGS 84>
-- coordinates:
--
-- -   /x/ — Specifies the x coordinate or longitude.
--
-- -   /y/ — Specifies the y coordinate or latitude.
placeGeometry_point :: Lens.Lens' PlaceGeometry (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
placeGeometry_point = Lens.lens (\PlaceGeometry' {point} -> point) (\s@PlaceGeometry' {} a -> s {point = a} :: PlaceGeometry) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

instance Data.FromJSON PlaceGeometry where
  parseJSON =
    Data.withObject
      "PlaceGeometry"
      ( \x ->
          PlaceGeometry' Prelude.<$> (x Data..:? "Point")
      )

instance Prelude.Hashable PlaceGeometry where
  hashWithSalt _salt PlaceGeometry' {..} =
    _salt `Prelude.hashWithSalt` point

instance Prelude.NFData PlaceGeometry where
  rnf PlaceGeometry' {..} = Prelude.rnf point
