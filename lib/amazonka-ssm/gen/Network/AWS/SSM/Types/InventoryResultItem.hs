{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryResultItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryResultItem where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The inventory result item.
--
--
--
-- /See:/ 'inventoryResultItem' smart constructor.
data InventoryResultItem = InventoryResultItem'
  { _iriContentHash ::
      !(Maybe Text),
    _iriCaptureTime :: !(Maybe Text),
    _iriTypeName :: !Text,
    _iriSchemaVersion :: !Text,
    _iriContent :: ![Map Text (Text)]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventoryResultItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iriContentHash' - MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The PutInventory API does not update the inventory item type contents if the MD5 hash has not changed since last update.
--
-- * 'iriCaptureTime' - The time inventory item data was captured.
--
-- * 'iriTypeName' - The name of the inventory result item type.
--
-- * 'iriSchemaVersion' - The schema version for the inventory result item/
--
-- * 'iriContent' - Contains all the inventory data of the item type. Results include attribute names and values.
inventoryResultItem ::
  -- | 'iriTypeName'
  Text ->
  -- | 'iriSchemaVersion'
  Text ->
  InventoryResultItem
inventoryResultItem pTypeName_ pSchemaVersion_ =
  InventoryResultItem'
    { _iriContentHash = Nothing,
      _iriCaptureTime = Nothing,
      _iriTypeName = pTypeName_,
      _iriSchemaVersion = pSchemaVersion_,
      _iriContent = mempty
    }

-- | MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The PutInventory API does not update the inventory item type contents if the MD5 hash has not changed since last update.
iriContentHash :: Lens' InventoryResultItem (Maybe Text)
iriContentHash = lens _iriContentHash (\s a -> s {_iriContentHash = a})

-- | The time inventory item data was captured.
iriCaptureTime :: Lens' InventoryResultItem (Maybe Text)
iriCaptureTime = lens _iriCaptureTime (\s a -> s {_iriCaptureTime = a})

-- | The name of the inventory result item type.
iriTypeName :: Lens' InventoryResultItem Text
iriTypeName = lens _iriTypeName (\s a -> s {_iriTypeName = a})

-- | The schema version for the inventory result item/
iriSchemaVersion :: Lens' InventoryResultItem Text
iriSchemaVersion = lens _iriSchemaVersion (\s a -> s {_iriSchemaVersion = a})

-- | Contains all the inventory data of the item type. Results include attribute names and values.
iriContent :: Lens' InventoryResultItem [HashMap Text (Text)]
iriContent = lens _iriContent (\s a -> s {_iriContent = a}) . _Coerce

instance FromJSON InventoryResultItem where
  parseJSON =
    withObject
      "InventoryResultItem"
      ( \x ->
          InventoryResultItem'
            <$> (x .:? "ContentHash")
            <*> (x .:? "CaptureTime")
            <*> (x .: "TypeName")
            <*> (x .: "SchemaVersion")
            <*> (x .:? "Content" .!= mempty)
      )

instance Hashable InventoryResultItem

instance NFData InventoryResultItem
