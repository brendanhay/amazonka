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
-- Module      : Amazonka.DMS.Types.InventoryData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.InventoryData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a Fleet Advisor collector inventory.
--
-- /See:/ 'newInventoryData' smart constructor.
data InventoryData = InventoryData'
  { -- | The number of schemas in the Fleet Advisor collector inventory.
    numberOfSchemas :: Prelude.Maybe Prelude.Int,
    -- | The number of databases in the Fleet Advisor collector inventory.
    numberOfDatabases :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InventoryData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfSchemas', 'inventoryData_numberOfSchemas' - The number of schemas in the Fleet Advisor collector inventory.
--
-- 'numberOfDatabases', 'inventoryData_numberOfDatabases' - The number of databases in the Fleet Advisor collector inventory.
newInventoryData ::
  InventoryData
newInventoryData =
  InventoryData'
    { numberOfSchemas = Prelude.Nothing,
      numberOfDatabases = Prelude.Nothing
    }

-- | The number of schemas in the Fleet Advisor collector inventory.
inventoryData_numberOfSchemas :: Lens.Lens' InventoryData (Prelude.Maybe Prelude.Int)
inventoryData_numberOfSchemas = Lens.lens (\InventoryData' {numberOfSchemas} -> numberOfSchemas) (\s@InventoryData' {} a -> s {numberOfSchemas = a} :: InventoryData)

-- | The number of databases in the Fleet Advisor collector inventory.
inventoryData_numberOfDatabases :: Lens.Lens' InventoryData (Prelude.Maybe Prelude.Int)
inventoryData_numberOfDatabases = Lens.lens (\InventoryData' {numberOfDatabases} -> numberOfDatabases) (\s@InventoryData' {} a -> s {numberOfDatabases = a} :: InventoryData)

instance Core.FromJSON InventoryData where
  parseJSON =
    Core.withObject
      "InventoryData"
      ( \x ->
          InventoryData'
            Prelude.<$> (x Core..:? "NumberOfSchemas")
            Prelude.<*> (x Core..:? "NumberOfDatabases")
      )

instance Prelude.Hashable InventoryData where
  hashWithSalt _salt InventoryData' {..} =
    _salt `Prelude.hashWithSalt` numberOfSchemas
      `Prelude.hashWithSalt` numberOfDatabases

instance Prelude.NFData InventoryData where
  rnf InventoryData' {..} =
    Prelude.rnf numberOfSchemas
      `Prelude.seq` Prelude.rnf numberOfDatabases
