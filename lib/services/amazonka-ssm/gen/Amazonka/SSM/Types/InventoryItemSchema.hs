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
-- Module      : Amazonka.SSM.Types.InventoryItemSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InventoryItemSchema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.InventoryItemAttribute

-- | The inventory item schema definition. Users can use this to compose
-- inventory query filters.
--
-- /See:/ 'newInventoryItemSchema' smart constructor.
data InventoryItemSchema = InventoryItemSchema'
  { -- | The alias name of the inventory type. The alias name is used for display
    -- purposes.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The schema version for the inventory item.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the inventory type. Default inventory item type names start
    -- with Amazon Web Services. Custom inventory type names will start with
    -- Custom. Default inventory item types include the following:
    -- @AWS:AWSComponent@, @AWS:Application@, @AWS:InstanceInformation@,
    -- @AWS:Network@, and @AWS:WindowsUpdate@.
    typeName :: Prelude.Text,
    -- | The schema attributes for inventory. This contains data type and
    -- attribute name.
    attributes :: Prelude.NonEmpty InventoryItemAttribute
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InventoryItemSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'inventoryItemSchema_displayName' - The alias name of the inventory type. The alias name is used for display
-- purposes.
--
-- 'version', 'inventoryItemSchema_version' - The schema version for the inventory item.
--
-- 'typeName', 'inventoryItemSchema_typeName' - The name of the inventory type. Default inventory item type names start
-- with Amazon Web Services. Custom inventory type names will start with
-- Custom. Default inventory item types include the following:
-- @AWS:AWSComponent@, @AWS:Application@, @AWS:InstanceInformation@,
-- @AWS:Network@, and @AWS:WindowsUpdate@.
--
-- 'attributes', 'inventoryItemSchema_attributes' - The schema attributes for inventory. This contains data type and
-- attribute name.
newInventoryItemSchema ::
  -- | 'typeName'
  Prelude.Text ->
  -- | 'attributes'
  Prelude.NonEmpty InventoryItemAttribute ->
  InventoryItemSchema
newInventoryItemSchema pTypeName_ pAttributes_ =
  InventoryItemSchema'
    { displayName = Prelude.Nothing,
      version = Prelude.Nothing,
      typeName = pTypeName_,
      attributes = Lens.coerced Lens.# pAttributes_
    }

-- | The alias name of the inventory type. The alias name is used for display
-- purposes.
inventoryItemSchema_displayName :: Lens.Lens' InventoryItemSchema (Prelude.Maybe Prelude.Text)
inventoryItemSchema_displayName = Lens.lens (\InventoryItemSchema' {displayName} -> displayName) (\s@InventoryItemSchema' {} a -> s {displayName = a} :: InventoryItemSchema)

-- | The schema version for the inventory item.
inventoryItemSchema_version :: Lens.Lens' InventoryItemSchema (Prelude.Maybe Prelude.Text)
inventoryItemSchema_version = Lens.lens (\InventoryItemSchema' {version} -> version) (\s@InventoryItemSchema' {} a -> s {version = a} :: InventoryItemSchema)

-- | The name of the inventory type. Default inventory item type names start
-- with Amazon Web Services. Custom inventory type names will start with
-- Custom. Default inventory item types include the following:
-- @AWS:AWSComponent@, @AWS:Application@, @AWS:InstanceInformation@,
-- @AWS:Network@, and @AWS:WindowsUpdate@.
inventoryItemSchema_typeName :: Lens.Lens' InventoryItemSchema Prelude.Text
inventoryItemSchema_typeName = Lens.lens (\InventoryItemSchema' {typeName} -> typeName) (\s@InventoryItemSchema' {} a -> s {typeName = a} :: InventoryItemSchema)

-- | The schema attributes for inventory. This contains data type and
-- attribute name.
inventoryItemSchema_attributes :: Lens.Lens' InventoryItemSchema (Prelude.NonEmpty InventoryItemAttribute)
inventoryItemSchema_attributes = Lens.lens (\InventoryItemSchema' {attributes} -> attributes) (\s@InventoryItemSchema' {} a -> s {attributes = a} :: InventoryItemSchema) Prelude.. Lens.coerced

instance Data.FromJSON InventoryItemSchema where
  parseJSON =
    Data.withObject
      "InventoryItemSchema"
      ( \x ->
          InventoryItemSchema'
            Prelude.<$> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..: "TypeName")
            Prelude.<*> (x Data..: "Attributes")
      )

instance Prelude.Hashable InventoryItemSchema where
  hashWithSalt _salt InventoryItemSchema' {..} =
    _salt
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData InventoryItemSchema where
  rnf InventoryItemSchema' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf attributes
