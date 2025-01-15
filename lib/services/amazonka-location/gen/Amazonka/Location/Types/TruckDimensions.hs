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
-- Module      : Amazonka.Location.Types.TruckDimensions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.TruckDimensions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.DimensionUnit
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the truck dimensions in the unit of measurement
-- that you specify. Used to filter out roads that can\'t support or allow
-- the specified dimensions for requests that specify @TravelMode@ as
-- @Truck@.
--
-- /See:/ 'newTruckDimensions' smart constructor.
data TruckDimensions = TruckDimensions'
  { -- | The height of the truck.
    --
    -- -   For example, @4.5@.
    --
    -- For routes calculated with a HERE resource, this value must be between 0
    -- and 50 meters.
    height :: Prelude.Maybe Prelude.Double,
    -- | The length of the truck.
    --
    -- -   For example, @15.5@.
    --
    -- For routes calculated with a HERE resource, this value must be between 0
    -- and 300 meters.
    length :: Prelude.Maybe Prelude.Double,
    -- | Specifies the unit of measurement for the truck dimensions.
    --
    -- Default Value: @Meters@
    unit :: Prelude.Maybe DimensionUnit,
    -- | The width of the truck.
    --
    -- -   For example, @4.5@.
    --
    -- For routes calculated with a HERE resource, this value must be between 0
    -- and 50 meters.
    width :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TruckDimensions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'height', 'truckDimensions_height' - The height of the truck.
--
-- -   For example, @4.5@.
--
-- For routes calculated with a HERE resource, this value must be between 0
-- and 50 meters.
--
-- 'length', 'truckDimensions_length' - The length of the truck.
--
-- -   For example, @15.5@.
--
-- For routes calculated with a HERE resource, this value must be between 0
-- and 300 meters.
--
-- 'unit', 'truckDimensions_unit' - Specifies the unit of measurement for the truck dimensions.
--
-- Default Value: @Meters@
--
-- 'width', 'truckDimensions_width' - The width of the truck.
--
-- -   For example, @4.5@.
--
-- For routes calculated with a HERE resource, this value must be between 0
-- and 50 meters.
newTruckDimensions ::
  TruckDimensions
newTruckDimensions =
  TruckDimensions'
    { height = Prelude.Nothing,
      length = Prelude.Nothing,
      unit = Prelude.Nothing,
      width = Prelude.Nothing
    }

-- | The height of the truck.
--
-- -   For example, @4.5@.
--
-- For routes calculated with a HERE resource, this value must be between 0
-- and 50 meters.
truckDimensions_height :: Lens.Lens' TruckDimensions (Prelude.Maybe Prelude.Double)
truckDimensions_height = Lens.lens (\TruckDimensions' {height} -> height) (\s@TruckDimensions' {} a -> s {height = a} :: TruckDimensions)

-- | The length of the truck.
--
-- -   For example, @15.5@.
--
-- For routes calculated with a HERE resource, this value must be between 0
-- and 300 meters.
truckDimensions_length :: Lens.Lens' TruckDimensions (Prelude.Maybe Prelude.Double)
truckDimensions_length = Lens.lens (\TruckDimensions' {length} -> length) (\s@TruckDimensions' {} a -> s {length = a} :: TruckDimensions)

-- | Specifies the unit of measurement for the truck dimensions.
--
-- Default Value: @Meters@
truckDimensions_unit :: Lens.Lens' TruckDimensions (Prelude.Maybe DimensionUnit)
truckDimensions_unit = Lens.lens (\TruckDimensions' {unit} -> unit) (\s@TruckDimensions' {} a -> s {unit = a} :: TruckDimensions)

-- | The width of the truck.
--
-- -   For example, @4.5@.
--
-- For routes calculated with a HERE resource, this value must be between 0
-- and 50 meters.
truckDimensions_width :: Lens.Lens' TruckDimensions (Prelude.Maybe Prelude.Double)
truckDimensions_width = Lens.lens (\TruckDimensions' {width} -> width) (\s@TruckDimensions' {} a -> s {width = a} :: TruckDimensions)

instance Prelude.Hashable TruckDimensions where
  hashWithSalt _salt TruckDimensions' {..} =
    _salt
      `Prelude.hashWithSalt` height
      `Prelude.hashWithSalt` length
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` width

instance Prelude.NFData TruckDimensions where
  rnf TruckDimensions' {..} =
    Prelude.rnf height `Prelude.seq`
      Prelude.rnf length `Prelude.seq`
        Prelude.rnf unit `Prelude.seq`
          Prelude.rnf width

instance Data.ToJSON TruckDimensions where
  toJSON TruckDimensions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Height" Data..=) Prelude.<$> height,
            ("Length" Data..=) Prelude.<$> length,
            ("Unit" Data..=) Prelude.<$> unit,
            ("Width" Data..=) Prelude.<$> width
          ]
      )
