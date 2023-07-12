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
-- Module      : Amazonka.SSM.Types.InventoryResultEntity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InventoryResultEntity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.InventoryResultItem

-- | Inventory query results.
--
-- /See:/ 'newInventoryResultEntity' smart constructor.
data InventoryResultEntity = InventoryResultEntity'
  { -- | The data section in the inventory result entity JSON.
    data' :: Prelude.Maybe (Prelude.HashMap Prelude.Text InventoryResultItem),
    -- | ID of the inventory result entity. For example, for managed node
    -- inventory the result will be the managed node ID. For EC2 instance
    -- inventory, the result will be the instance ID.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InventoryResultEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'inventoryResultEntity_data' - The data section in the inventory result entity JSON.
--
-- 'id', 'inventoryResultEntity_id' - ID of the inventory result entity. For example, for managed node
-- inventory the result will be the managed node ID. For EC2 instance
-- inventory, the result will be the instance ID.
newInventoryResultEntity ::
  InventoryResultEntity
newInventoryResultEntity =
  InventoryResultEntity'
    { data' = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The data section in the inventory result entity JSON.
inventoryResultEntity_data :: Lens.Lens' InventoryResultEntity (Prelude.Maybe (Prelude.HashMap Prelude.Text InventoryResultItem))
inventoryResultEntity_data = Lens.lens (\InventoryResultEntity' {data'} -> data') (\s@InventoryResultEntity' {} a -> s {data' = a} :: InventoryResultEntity) Prelude.. Lens.mapping Lens.coerced

-- | ID of the inventory result entity. For example, for managed node
-- inventory the result will be the managed node ID. For EC2 instance
-- inventory, the result will be the instance ID.
inventoryResultEntity_id :: Lens.Lens' InventoryResultEntity (Prelude.Maybe Prelude.Text)
inventoryResultEntity_id = Lens.lens (\InventoryResultEntity' {id} -> id) (\s@InventoryResultEntity' {} a -> s {id = a} :: InventoryResultEntity)

instance Data.FromJSON InventoryResultEntity where
  parseJSON =
    Data.withObject
      "InventoryResultEntity"
      ( \x ->
          InventoryResultEntity'
            Prelude.<$> (x Data..:? "Data" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Id")
      )

instance Prelude.Hashable InventoryResultEntity where
  hashWithSalt _salt InventoryResultEntity' {..} =
    _salt
      `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` id

instance Prelude.NFData InventoryResultEntity where
  rnf InventoryResultEntity' {..} =
    Prelude.rnf data' `Prelude.seq` Prelude.rnf id
