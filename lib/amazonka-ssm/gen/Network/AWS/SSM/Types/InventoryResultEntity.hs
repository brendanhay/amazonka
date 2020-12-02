{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryResultEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryResultEntity where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.InventoryResultItem

-- | Inventory query results.
--
--
--
-- /See:/ 'inventoryResultEntity' smart constructor.
data InventoryResultEntity = InventoryResultEntity'
  { _ireData ::
      !(Maybe (Map Text (InventoryResultItem))),
    _ireId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventoryResultEntity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ireData' - The data section in the inventory result entity JSON.
--
-- * 'ireId' - ID of the inventory result entity. For example, for managed instance inventory the result will be the managed instance ID. For EC2 instance inventory, the result will be the instance ID.
inventoryResultEntity ::
  InventoryResultEntity
inventoryResultEntity =
  InventoryResultEntity' {_ireData = Nothing, _ireId = Nothing}

-- | The data section in the inventory result entity JSON.
ireData :: Lens' InventoryResultEntity (HashMap Text (InventoryResultItem))
ireData = lens _ireData (\s a -> s {_ireData = a}) . _Default . _Map

-- | ID of the inventory result entity. For example, for managed instance inventory the result will be the managed instance ID. For EC2 instance inventory, the result will be the instance ID.
ireId :: Lens' InventoryResultEntity (Maybe Text)
ireId = lens _ireId (\s a -> s {_ireId = a})

instance FromJSON InventoryResultEntity where
  parseJSON =
    withObject
      "InventoryResultEntity"
      ( \x ->
          InventoryResultEntity'
            <$> (x .:? "Data" .!= mempty) <*> (x .:? "Id")
      )

instance Hashable InventoryResultEntity

instance NFData InventoryResultEntity
