{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SSM.Types.InventoryItemSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryItemSchema where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.InventoryItemAttribute

-- | The inventory item schema definition. Users can use this to compose
-- inventory query filters.
--
-- /See:/ 'newInventoryItemSchema' smart constructor.
data InventoryItemSchema = InventoryItemSchema'
  { -- | The schema version for the inventory item.
    version :: Prelude.Maybe Prelude.Text,
    -- | The alias name of the inventory type. The alias name is used for display
    -- purposes.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the inventory type. Default inventory item type names start
    -- with AWS. Custom inventory type names will start with Custom. Default
    -- inventory item types include the following: AWS:AWSComponent,
    -- AWS:Application, AWS:InstanceInformation, AWS:Network, and
    -- AWS:WindowsUpdate.
    typeName :: Prelude.Text,
    -- | The schema attributes for inventory. This contains data type and
    -- attribute name.
    attributes :: Prelude.NonEmpty InventoryItemAttribute
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InventoryItemSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'inventoryItemSchema_version' - The schema version for the inventory item.
--
-- 'displayName', 'inventoryItemSchema_displayName' - The alias name of the inventory type. The alias name is used for display
-- purposes.
--
-- 'typeName', 'inventoryItemSchema_typeName' - The name of the inventory type. Default inventory item type names start
-- with AWS. Custom inventory type names will start with Custom. Default
-- inventory item types include the following: AWS:AWSComponent,
-- AWS:Application, AWS:InstanceInformation, AWS:Network, and
-- AWS:WindowsUpdate.
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
    { version = Prelude.Nothing,
      displayName = Prelude.Nothing,
      typeName = pTypeName_,
      attributes = Prelude._Coerce Lens.# pAttributes_
    }

-- | The schema version for the inventory item.
inventoryItemSchema_version :: Lens.Lens' InventoryItemSchema (Prelude.Maybe Prelude.Text)
inventoryItemSchema_version = Lens.lens (\InventoryItemSchema' {version} -> version) (\s@InventoryItemSchema' {} a -> s {version = a} :: InventoryItemSchema)

-- | The alias name of the inventory type. The alias name is used for display
-- purposes.
inventoryItemSchema_displayName :: Lens.Lens' InventoryItemSchema (Prelude.Maybe Prelude.Text)
inventoryItemSchema_displayName = Lens.lens (\InventoryItemSchema' {displayName} -> displayName) (\s@InventoryItemSchema' {} a -> s {displayName = a} :: InventoryItemSchema)

-- | The name of the inventory type. Default inventory item type names start
-- with AWS. Custom inventory type names will start with Custom. Default
-- inventory item types include the following: AWS:AWSComponent,
-- AWS:Application, AWS:InstanceInformation, AWS:Network, and
-- AWS:WindowsUpdate.
inventoryItemSchema_typeName :: Lens.Lens' InventoryItemSchema Prelude.Text
inventoryItemSchema_typeName = Lens.lens (\InventoryItemSchema' {typeName} -> typeName) (\s@InventoryItemSchema' {} a -> s {typeName = a} :: InventoryItemSchema)

-- | The schema attributes for inventory. This contains data type and
-- attribute name.
inventoryItemSchema_attributes :: Lens.Lens' InventoryItemSchema (Prelude.NonEmpty InventoryItemAttribute)
inventoryItemSchema_attributes = Lens.lens (\InventoryItemSchema' {attributes} -> attributes) (\s@InventoryItemSchema' {} a -> s {attributes = a} :: InventoryItemSchema) Prelude.. Prelude._Coerce

instance Prelude.FromJSON InventoryItemSchema where
  parseJSON =
    Prelude.withObject
      "InventoryItemSchema"
      ( \x ->
          InventoryItemSchema'
            Prelude.<$> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..:? "DisplayName")
            Prelude.<*> (x Prelude..: "TypeName")
            Prelude.<*> (x Prelude..: "Attributes")
      )

instance Prelude.Hashable InventoryItemSchema

instance Prelude.NFData InventoryItemSchema
