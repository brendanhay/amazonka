{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryItemSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryItemSchema where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.InventoryItemAttribute

-- | The inventory item schema definition. Users can use this to compose inventory query filters.
--
--
--
-- /See:/ 'inventoryItemSchema' smart constructor.
data InventoryItemSchema = InventoryItemSchema'
  { _iisVersion ::
      !(Maybe Text),
    _iisDisplayName :: !(Maybe Text),
    _iisTypeName :: !Text,
    _iisAttributes :: !(List1 InventoryItemAttribute)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventoryItemSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iisVersion' - The schema version for the inventory item.
--
-- * 'iisDisplayName' - The alias name of the inventory type. The alias name is used for display purposes.
--
-- * 'iisTypeName' - The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
--
-- * 'iisAttributes' - The schema attributes for inventory. This contains data type and attribute name.
inventoryItemSchema ::
  -- | 'iisTypeName'
  Text ->
  -- | 'iisAttributes'
  NonEmpty InventoryItemAttribute ->
  InventoryItemSchema
inventoryItemSchema pTypeName_ pAttributes_ =
  InventoryItemSchema'
    { _iisVersion = Nothing,
      _iisDisplayName = Nothing,
      _iisTypeName = pTypeName_,
      _iisAttributes = _List1 # pAttributes_
    }

-- | The schema version for the inventory item.
iisVersion :: Lens' InventoryItemSchema (Maybe Text)
iisVersion = lens _iisVersion (\s a -> s {_iisVersion = a})

-- | The alias name of the inventory type. The alias name is used for display purposes.
iisDisplayName :: Lens' InventoryItemSchema (Maybe Text)
iisDisplayName = lens _iisDisplayName (\s a -> s {_iisDisplayName = a})

-- | The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
iisTypeName :: Lens' InventoryItemSchema Text
iisTypeName = lens _iisTypeName (\s a -> s {_iisTypeName = a})

-- | The schema attributes for inventory. This contains data type and attribute name.
iisAttributes :: Lens' InventoryItemSchema (NonEmpty InventoryItemAttribute)
iisAttributes = lens _iisAttributes (\s a -> s {_iisAttributes = a}) . _List1

instance FromJSON InventoryItemSchema where
  parseJSON =
    withObject
      "InventoryItemSchema"
      ( \x ->
          InventoryItemSchema'
            <$> (x .:? "Version")
            <*> (x .:? "DisplayName")
            <*> (x .: "TypeName")
            <*> (x .: "Attributes")
      )

instance Hashable InventoryItemSchema

instance NFData InventoryItemSchema
