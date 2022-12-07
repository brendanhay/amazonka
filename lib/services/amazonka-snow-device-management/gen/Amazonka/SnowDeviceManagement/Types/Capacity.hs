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
-- Module      : Amazonka.SnowDeviceManagement.Types.Capacity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.Capacity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The physical capacity of the Amazon Web Services Snow Family device.
--
-- /See:/ 'newCapacity' smart constructor.
data Capacity = Capacity'
  { -- | The amount of capacity available for use on the device.
    available :: Prelude.Maybe Prelude.Integer,
    -- | The name of the type of capacity, such as memory.
    name :: Prelude.Maybe Prelude.Text,
    -- | The total capacity on the device.
    total :: Prelude.Maybe Prelude.Integer,
    -- | The amount of capacity used on the device.
    used :: Prelude.Maybe Prelude.Integer,
    -- | The unit of measure for the type of capacity.
    unit :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Capacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'available', 'capacity_available' - The amount of capacity available for use on the device.
--
-- 'name', 'capacity_name' - The name of the type of capacity, such as memory.
--
-- 'total', 'capacity_total' - The total capacity on the device.
--
-- 'used', 'capacity_used' - The amount of capacity used on the device.
--
-- 'unit', 'capacity_unit' - The unit of measure for the type of capacity.
newCapacity ::
  Capacity
newCapacity =
  Capacity'
    { available = Prelude.Nothing,
      name = Prelude.Nothing,
      total = Prelude.Nothing,
      used = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The amount of capacity available for use on the device.
capacity_available :: Lens.Lens' Capacity (Prelude.Maybe Prelude.Integer)
capacity_available = Lens.lens (\Capacity' {available} -> available) (\s@Capacity' {} a -> s {available = a} :: Capacity)

-- | The name of the type of capacity, such as memory.
capacity_name :: Lens.Lens' Capacity (Prelude.Maybe Prelude.Text)
capacity_name = Lens.lens (\Capacity' {name} -> name) (\s@Capacity' {} a -> s {name = a} :: Capacity)

-- | The total capacity on the device.
capacity_total :: Lens.Lens' Capacity (Prelude.Maybe Prelude.Integer)
capacity_total = Lens.lens (\Capacity' {total} -> total) (\s@Capacity' {} a -> s {total = a} :: Capacity)

-- | The amount of capacity used on the device.
capacity_used :: Lens.Lens' Capacity (Prelude.Maybe Prelude.Integer)
capacity_used = Lens.lens (\Capacity' {used} -> used) (\s@Capacity' {} a -> s {used = a} :: Capacity)

-- | The unit of measure for the type of capacity.
capacity_unit :: Lens.Lens' Capacity (Prelude.Maybe Prelude.Text)
capacity_unit = Lens.lens (\Capacity' {unit} -> unit) (\s@Capacity' {} a -> s {unit = a} :: Capacity)

instance Data.FromJSON Capacity where
  parseJSON =
    Data.withObject
      "Capacity"
      ( \x ->
          Capacity'
            Prelude.<$> (x Data..:? "available")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "total")
            Prelude.<*> (x Data..:? "used")
            Prelude.<*> (x Data..:? "unit")
      )

instance Prelude.Hashable Capacity where
  hashWithSalt _salt Capacity' {..} =
    _salt `Prelude.hashWithSalt` available
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` used
      `Prelude.hashWithSalt` unit

instance Prelude.NFData Capacity where
  rnf Capacity' {..} =
    Prelude.rnf available
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf total
      `Prelude.seq` Prelude.rnf used
      `Prelude.seq` Prelude.rnf unit
