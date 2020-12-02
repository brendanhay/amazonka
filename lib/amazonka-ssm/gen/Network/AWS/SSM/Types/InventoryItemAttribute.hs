{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryItemAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryItemAttribute where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.InventoryAttributeDataType

-- | Attributes are the entries within the inventory item content. It contains name and value.
--
--
--
-- /See:/ 'inventoryItemAttribute' smart constructor.
data InventoryItemAttribute = InventoryItemAttribute'
  { _iiaName ::
      !Text,
    _iiaDataType :: !InventoryAttributeDataType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventoryItemAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiaName' - Name of the inventory item attribute.
--
-- * 'iiaDataType' - The data type of the inventory item attribute.
inventoryItemAttribute ::
  -- | 'iiaName'
  Text ->
  -- | 'iiaDataType'
  InventoryAttributeDataType ->
  InventoryItemAttribute
inventoryItemAttribute pName_ pDataType_ =
  InventoryItemAttribute'
    { _iiaName = pName_,
      _iiaDataType = pDataType_
    }

-- | Name of the inventory item attribute.
iiaName :: Lens' InventoryItemAttribute Text
iiaName = lens _iiaName (\s a -> s {_iiaName = a})

-- | The data type of the inventory item attribute.
iiaDataType :: Lens' InventoryItemAttribute InventoryAttributeDataType
iiaDataType = lens _iiaDataType (\s a -> s {_iiaDataType = a})

instance FromJSON InventoryItemAttribute where
  parseJSON =
    withObject
      "InventoryItemAttribute"
      ( \x ->
          InventoryItemAttribute' <$> (x .: "Name") <*> (x .: "DataType")
      )

instance Hashable InventoryItemAttribute

instance NFData InventoryItemAttribute
