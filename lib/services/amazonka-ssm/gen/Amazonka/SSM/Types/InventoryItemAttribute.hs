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
-- Module      : Amazonka.SSM.Types.InventoryItemAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InventoryItemAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.InventoryAttributeDataType

-- | Attributes are the entries within the inventory item content. It
-- contains name and value.
--
-- /See:/ 'newInventoryItemAttribute' smart constructor.
data InventoryItemAttribute = InventoryItemAttribute'
  { -- | Name of the inventory item attribute.
    name :: Prelude.Text,
    -- | The data type of the inventory item attribute.
    dataType :: InventoryAttributeDataType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InventoryItemAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'inventoryItemAttribute_name' - Name of the inventory item attribute.
--
-- 'dataType', 'inventoryItemAttribute_dataType' - The data type of the inventory item attribute.
newInventoryItemAttribute ::
  -- | 'name'
  Prelude.Text ->
  -- | 'dataType'
  InventoryAttributeDataType ->
  InventoryItemAttribute
newInventoryItemAttribute pName_ pDataType_ =
  InventoryItemAttribute'
    { name = pName_,
      dataType = pDataType_
    }

-- | Name of the inventory item attribute.
inventoryItemAttribute_name :: Lens.Lens' InventoryItemAttribute Prelude.Text
inventoryItemAttribute_name = Lens.lens (\InventoryItemAttribute' {name} -> name) (\s@InventoryItemAttribute' {} a -> s {name = a} :: InventoryItemAttribute)

-- | The data type of the inventory item attribute.
inventoryItemAttribute_dataType :: Lens.Lens' InventoryItemAttribute InventoryAttributeDataType
inventoryItemAttribute_dataType = Lens.lens (\InventoryItemAttribute' {dataType} -> dataType) (\s@InventoryItemAttribute' {} a -> s {dataType = a} :: InventoryItemAttribute)

instance Data.FromJSON InventoryItemAttribute where
  parseJSON =
    Data.withObject
      "InventoryItemAttribute"
      ( \x ->
          InventoryItemAttribute'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "DataType")
      )

instance Prelude.Hashable InventoryItemAttribute where
  hashWithSalt _salt InventoryItemAttribute' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dataType

instance Prelude.NFData InventoryItemAttribute where
  rnf InventoryItemAttribute' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf dataType
