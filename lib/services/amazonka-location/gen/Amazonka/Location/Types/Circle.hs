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
-- Module      : Amazonka.Location.Types.Circle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.Circle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A circle on the earth, as defined by a center point and a radius.
--
-- /See:/ 'newCircle' smart constructor.
data Circle = Circle'
  { -- | A single point geometry, specifying the center of the circle, using
    -- <https://gisgeography.com/wgs84-world-geodetic-system/ WGS 84>
    -- coordinates, in the form @[longitude, latitude]@.
    center :: Data.Sensitive (Prelude.NonEmpty Prelude.Double),
    -- | The radius of the circle in meters. Must be greater than zero and no
    -- larger than 100,000 (100 kilometers).
    radius :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Circle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'center', 'circle_center' - A single point geometry, specifying the center of the circle, using
-- <https://gisgeography.com/wgs84-world-geodetic-system/ WGS 84>
-- coordinates, in the form @[longitude, latitude]@.
--
-- 'radius', 'circle_radius' - The radius of the circle in meters. Must be greater than zero and no
-- larger than 100,000 (100 kilometers).
newCircle ::
  -- | 'center'
  Prelude.NonEmpty Prelude.Double ->
  -- | 'radius'
  Prelude.Double ->
  Circle
newCircle pCenter_ pRadius_ =
  Circle'
    { center =
        Data._Sensitive
          Prelude.. Lens.coerced
          Lens.# pCenter_,
      radius = pRadius_
    }

-- | A single point geometry, specifying the center of the circle, using
-- <https://gisgeography.com/wgs84-world-geodetic-system/ WGS 84>
-- coordinates, in the form @[longitude, latitude]@.
circle_center :: Lens.Lens' Circle (Prelude.NonEmpty Prelude.Double)
circle_center = Lens.lens (\Circle' {center} -> center) (\s@Circle' {} a -> s {center = a} :: Circle) Prelude.. Data._Sensitive Prelude.. Lens.coerced

-- | The radius of the circle in meters. Must be greater than zero and no
-- larger than 100,000 (100 kilometers).
circle_radius :: Lens.Lens' Circle Prelude.Double
circle_radius = Lens.lens (\Circle' {radius} -> radius) (\s@Circle' {} a -> s {radius = a} :: Circle)

instance Data.FromJSON Circle where
  parseJSON =
    Data.withObject
      "Circle"
      ( \x ->
          Circle'
            Prelude.<$> (x Data..: "Center")
            Prelude.<*> (x Data..: "Radius")
      )

instance Prelude.Hashable Circle where
  hashWithSalt _salt Circle' {..} =
    _salt
      `Prelude.hashWithSalt` center
      `Prelude.hashWithSalt` radius

instance Prelude.NFData Circle where
  rnf Circle' {..} =
    Prelude.rnf center `Prelude.seq` Prelude.rnf radius

instance Data.ToJSON Circle where
  toJSON Circle' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Center" Data..= center),
            Prelude.Just ("Radius" Data..= radius)
          ]
      )
