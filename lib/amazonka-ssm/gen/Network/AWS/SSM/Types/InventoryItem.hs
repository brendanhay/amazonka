{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryItem where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information collected from managed instances based on your inventory policy document
--
--
--
-- /See:/ 'inventoryItem' smart constructor.
data InventoryItem = InventoryItem'
  { _iiContext ::
      !(Maybe (Map Text (Text))),
    _iiContentHash :: !(Maybe Text),
    _iiContent :: !(Maybe [Map Text (Text)]),
    _iiTypeName :: !Text,
    _iiSchemaVersion :: !Text,
    _iiCaptureTime :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventoryItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiContext' - A map of associated properties for a specified inventory type. For example, with this attribute, you can specify the @ExecutionId@ , @ExecutionType@ , @ComplianceType@ properties of the @AWS:ComplianceItem@ type.
--
-- * 'iiContentHash' - MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The PutInventory API does not update the inventory item type contents if the MD5 hash has not changed since last update.
--
-- * 'iiContent' - The inventory data of the inventory type.
--
-- * 'iiTypeName' - The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
--
-- * 'iiSchemaVersion' - The schema version for the inventory item.
--
-- * 'iiCaptureTime' - The time the inventory information was collected.
inventoryItem ::
  -- | 'iiTypeName'
  Text ->
  -- | 'iiSchemaVersion'
  Text ->
  -- | 'iiCaptureTime'
  Text ->
  InventoryItem
inventoryItem pTypeName_ pSchemaVersion_ pCaptureTime_ =
  InventoryItem'
    { _iiContext = Nothing,
      _iiContentHash = Nothing,
      _iiContent = Nothing,
      _iiTypeName = pTypeName_,
      _iiSchemaVersion = pSchemaVersion_,
      _iiCaptureTime = pCaptureTime_
    }

-- | A map of associated properties for a specified inventory type. For example, with this attribute, you can specify the @ExecutionId@ , @ExecutionType@ , @ComplianceType@ properties of the @AWS:ComplianceItem@ type.
iiContext :: Lens' InventoryItem (HashMap Text (Text))
iiContext = lens _iiContext (\s a -> s {_iiContext = a}) . _Default . _Map

-- | MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The PutInventory API does not update the inventory item type contents if the MD5 hash has not changed since last update.
iiContentHash :: Lens' InventoryItem (Maybe Text)
iiContentHash = lens _iiContentHash (\s a -> s {_iiContentHash = a})

-- | The inventory data of the inventory type.
iiContent :: Lens' InventoryItem [HashMap Text (Text)]
iiContent = lens _iiContent (\s a -> s {_iiContent = a}) . _Default . _Coerce

-- | The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
iiTypeName :: Lens' InventoryItem Text
iiTypeName = lens _iiTypeName (\s a -> s {_iiTypeName = a})

-- | The schema version for the inventory item.
iiSchemaVersion :: Lens' InventoryItem Text
iiSchemaVersion = lens _iiSchemaVersion (\s a -> s {_iiSchemaVersion = a})

-- | The time the inventory information was collected.
iiCaptureTime :: Lens' InventoryItem Text
iiCaptureTime = lens _iiCaptureTime (\s a -> s {_iiCaptureTime = a})

instance Hashable InventoryItem

instance NFData InventoryItem

instance ToJSON InventoryItem where
  toJSON InventoryItem' {..} =
    object
      ( catMaybes
          [ ("Context" .=) <$> _iiContext,
            ("ContentHash" .=) <$> _iiContentHash,
            ("Content" .=) <$> _iiContent,
            Just ("TypeName" .= _iiTypeName),
            Just ("SchemaVersion" .= _iiSchemaVersion),
            Just ("CaptureTime" .= _iiCaptureTime)
          ]
      )
