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
-- Module      : Amazonka.IoTRoboRunner.Types.CartesianCoordinates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTRoboRunner.Types.CartesianCoordinates where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Cartesian coordinates in 3D space relative to the RoboRunner origin.
--
-- /See:/ 'newCartesianCoordinates' smart constructor.
data CartesianCoordinates = CartesianCoordinates'
  { -- | Z coordinate.
    z :: Prelude.Maybe Prelude.Double,
    -- | X coordinate.
    x :: Prelude.Double,
    -- | Y coordinate.
    y :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CartesianCoordinates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'z', 'cartesianCoordinates_z' - Z coordinate.
--
-- 'x', 'cartesianCoordinates_x' - X coordinate.
--
-- 'y', 'cartesianCoordinates_y' - Y coordinate.
newCartesianCoordinates ::
  -- | 'x'
  Prelude.Double ->
  -- | 'y'
  Prelude.Double ->
  CartesianCoordinates
newCartesianCoordinates pX_ pY_ =
  CartesianCoordinates'
    { z = Prelude.Nothing,
      x = pX_,
      y = pY_
    }

-- | Z coordinate.
cartesianCoordinates_z :: Lens.Lens' CartesianCoordinates (Prelude.Maybe Prelude.Double)
cartesianCoordinates_z = Lens.lens (\CartesianCoordinates' {z} -> z) (\s@CartesianCoordinates' {} a -> s {z = a} :: CartesianCoordinates)

-- | X coordinate.
cartesianCoordinates_x :: Lens.Lens' CartesianCoordinates Prelude.Double
cartesianCoordinates_x = Lens.lens (\CartesianCoordinates' {x} -> x) (\s@CartesianCoordinates' {} a -> s {x = a} :: CartesianCoordinates)

-- | Y coordinate.
cartesianCoordinates_y :: Lens.Lens' CartesianCoordinates Prelude.Double
cartesianCoordinates_y = Lens.lens (\CartesianCoordinates' {y} -> y) (\s@CartesianCoordinates' {} a -> s {y = a} :: CartesianCoordinates)

instance Data.FromJSON CartesianCoordinates where
  parseJSON =
    Data.withObject
      "CartesianCoordinates"
      ( \x ->
          CartesianCoordinates'
            Prelude.<$> (x Data..:? "z")
            Prelude.<*> (x Data..: "x")
            Prelude.<*> (x Data..: "y")
      )

instance Prelude.Hashable CartesianCoordinates where
  hashWithSalt _salt CartesianCoordinates' {..} =
    _salt `Prelude.hashWithSalt` z
      `Prelude.hashWithSalt` x
      `Prelude.hashWithSalt` y

instance Prelude.NFData CartesianCoordinates where
  rnf CartesianCoordinates' {..} =
    Prelude.rnf z
      `Prelude.seq` Prelude.rnf x
      `Prelude.seq` Prelude.rnf y

instance Data.ToJSON CartesianCoordinates where
  toJSON CartesianCoordinates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("z" Data..=) Prelude.<$> z,
            Prelude.Just ("x" Data..= x),
            Prelude.Just ("y" Data..= y)
          ]
      )
