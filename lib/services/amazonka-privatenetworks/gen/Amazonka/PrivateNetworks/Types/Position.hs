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
-- Module      : Amazonka.PrivateNetworks.Types.Position
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types.Position where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types.ElevationReference
import Amazonka.PrivateNetworks.Types.ElevationUnit

-- | Information about a position.
--
-- /See:/ 'newPosition' smart constructor.
data Position = Position'
  { -- | The longitude of the position.
    longitude :: Prelude.Maybe Prelude.Double,
    -- | The latitude of the position.
    latitude :: Prelude.Maybe Prelude.Double,
    -- | The elevation of the equipment at this position.
    elevation :: Prelude.Maybe Prelude.Double,
    -- | The reference point from which elevation is reported.
    elevationReference :: Prelude.Maybe ElevationReference,
    -- | The units used to measure the elevation of the position.
    elevationUnit :: Prelude.Maybe ElevationUnit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Position' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'longitude', 'position_longitude' - The longitude of the position.
--
-- 'latitude', 'position_latitude' - The latitude of the position.
--
-- 'elevation', 'position_elevation' - The elevation of the equipment at this position.
--
-- 'elevationReference', 'position_elevationReference' - The reference point from which elevation is reported.
--
-- 'elevationUnit', 'position_elevationUnit' - The units used to measure the elevation of the position.
newPosition ::
  Position
newPosition =
  Position'
    { longitude = Prelude.Nothing,
      latitude = Prelude.Nothing,
      elevation = Prelude.Nothing,
      elevationReference = Prelude.Nothing,
      elevationUnit = Prelude.Nothing
    }

-- | The longitude of the position.
position_longitude :: Lens.Lens' Position (Prelude.Maybe Prelude.Double)
position_longitude = Lens.lens (\Position' {longitude} -> longitude) (\s@Position' {} a -> s {longitude = a} :: Position)

-- | The latitude of the position.
position_latitude :: Lens.Lens' Position (Prelude.Maybe Prelude.Double)
position_latitude = Lens.lens (\Position' {latitude} -> latitude) (\s@Position' {} a -> s {latitude = a} :: Position)

-- | The elevation of the equipment at this position.
position_elevation :: Lens.Lens' Position (Prelude.Maybe Prelude.Double)
position_elevation = Lens.lens (\Position' {elevation} -> elevation) (\s@Position' {} a -> s {elevation = a} :: Position)

-- | The reference point from which elevation is reported.
position_elevationReference :: Lens.Lens' Position (Prelude.Maybe ElevationReference)
position_elevationReference = Lens.lens (\Position' {elevationReference} -> elevationReference) (\s@Position' {} a -> s {elevationReference = a} :: Position)

-- | The units used to measure the elevation of the position.
position_elevationUnit :: Lens.Lens' Position (Prelude.Maybe ElevationUnit)
position_elevationUnit = Lens.lens (\Position' {elevationUnit} -> elevationUnit) (\s@Position' {} a -> s {elevationUnit = a} :: Position)

instance Data.FromJSON Position where
  parseJSON =
    Data.withObject
      "Position"
      ( \x ->
          Position'
            Prelude.<$> (x Data..:? "longitude")
            Prelude.<*> (x Data..:? "latitude")
            Prelude.<*> (x Data..:? "elevation")
            Prelude.<*> (x Data..:? "elevationReference")
            Prelude.<*> (x Data..:? "elevationUnit")
      )

instance Prelude.Hashable Position where
  hashWithSalt _salt Position' {..} =
    _salt `Prelude.hashWithSalt` longitude
      `Prelude.hashWithSalt` latitude
      `Prelude.hashWithSalt` elevation
      `Prelude.hashWithSalt` elevationReference
      `Prelude.hashWithSalt` elevationUnit

instance Prelude.NFData Position where
  rnf Position' {..} =
    Prelude.rnf longitude
      `Prelude.seq` Prelude.rnf latitude
      `Prelude.seq` Prelude.rnf elevation
      `Prelude.seq` Prelude.rnf elevationReference
      `Prelude.seq` Prelude.rnf elevationUnit

instance Data.ToJSON Position where
  toJSON Position' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("longitude" Data..=) Prelude.<$> longitude,
            ("latitude" Data..=) Prelude.<$> latitude,
            ("elevation" Data..=) Prelude.<$> elevation,
            ("elevationReference" Data..=)
              Prelude.<$> elevationReference,
            ("elevationUnit" Data..=) Prelude.<$> elevationUnit
          ]
      )
