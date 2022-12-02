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
-- Module      : Amazonka.Pinpoint.Types.GPSPointDimension
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.GPSPointDimension where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.GPSCoordinates
import qualified Amazonka.Prelude as Prelude

-- | Specifies GPS-based criteria for including or excluding endpoints from a
-- segment.
--
-- /See:/ 'newGPSPointDimension' smart constructor.
data GPSPointDimension = GPSPointDimension'
  { -- | The range, in kilometers, from the GPS coordinates.
    rangeInKilometers :: Prelude.Maybe Prelude.Double,
    -- | The GPS coordinates to measure distance from.
    coordinates :: GPSCoordinates
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GPSPointDimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rangeInKilometers', 'gPSPointDimension_rangeInKilometers' - The range, in kilometers, from the GPS coordinates.
--
-- 'coordinates', 'gPSPointDimension_coordinates' - The GPS coordinates to measure distance from.
newGPSPointDimension ::
  -- | 'coordinates'
  GPSCoordinates ->
  GPSPointDimension
newGPSPointDimension pCoordinates_ =
  GPSPointDimension'
    { rangeInKilometers =
        Prelude.Nothing,
      coordinates = pCoordinates_
    }

-- | The range, in kilometers, from the GPS coordinates.
gPSPointDimension_rangeInKilometers :: Lens.Lens' GPSPointDimension (Prelude.Maybe Prelude.Double)
gPSPointDimension_rangeInKilometers = Lens.lens (\GPSPointDimension' {rangeInKilometers} -> rangeInKilometers) (\s@GPSPointDimension' {} a -> s {rangeInKilometers = a} :: GPSPointDimension)

-- | The GPS coordinates to measure distance from.
gPSPointDimension_coordinates :: Lens.Lens' GPSPointDimension GPSCoordinates
gPSPointDimension_coordinates = Lens.lens (\GPSPointDimension' {coordinates} -> coordinates) (\s@GPSPointDimension' {} a -> s {coordinates = a} :: GPSPointDimension)

instance Data.FromJSON GPSPointDimension where
  parseJSON =
    Data.withObject
      "GPSPointDimension"
      ( \x ->
          GPSPointDimension'
            Prelude.<$> (x Data..:? "RangeInKilometers")
            Prelude.<*> (x Data..: "Coordinates")
      )

instance Prelude.Hashable GPSPointDimension where
  hashWithSalt _salt GPSPointDimension' {..} =
    _salt `Prelude.hashWithSalt` rangeInKilometers
      `Prelude.hashWithSalt` coordinates

instance Prelude.NFData GPSPointDimension where
  rnf GPSPointDimension' {..} =
    Prelude.rnf rangeInKilometers
      `Prelude.seq` Prelude.rnf coordinates

instance Data.ToJSON GPSPointDimension where
  toJSON GPSPointDimension' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RangeInKilometers" Data..=)
              Prelude.<$> rangeInKilometers,
            Prelude.Just ("Coordinates" Data..= coordinates)
          ]
      )
