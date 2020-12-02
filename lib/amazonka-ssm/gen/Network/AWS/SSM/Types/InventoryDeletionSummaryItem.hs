{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryDeletionSummaryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryDeletionSummaryItem where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Either a count, remaining count, or a version number in a delete inventory summary.
--
--
--
-- /See:/ 'inventoryDeletionSummaryItem' smart constructor.
data InventoryDeletionSummaryItem = InventoryDeletionSummaryItem'
  { _idsiRemainingCount ::
      !(Maybe Int),
    _idsiCount :: !(Maybe Int),
    _idsiVersion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventoryDeletionSummaryItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idsiRemainingCount' - The remaining number of items to delete.
--
-- * 'idsiCount' - A count of the number of deleted items.
--
-- * 'idsiVersion' - The inventory type version.
inventoryDeletionSummaryItem ::
  InventoryDeletionSummaryItem
inventoryDeletionSummaryItem =
  InventoryDeletionSummaryItem'
    { _idsiRemainingCount = Nothing,
      _idsiCount = Nothing,
      _idsiVersion = Nothing
    }

-- | The remaining number of items to delete.
idsiRemainingCount :: Lens' InventoryDeletionSummaryItem (Maybe Int)
idsiRemainingCount = lens _idsiRemainingCount (\s a -> s {_idsiRemainingCount = a})

-- | A count of the number of deleted items.
idsiCount :: Lens' InventoryDeletionSummaryItem (Maybe Int)
idsiCount = lens _idsiCount (\s a -> s {_idsiCount = a})

-- | The inventory type version.
idsiVersion :: Lens' InventoryDeletionSummaryItem (Maybe Text)
idsiVersion = lens _idsiVersion (\s a -> s {_idsiVersion = a})

instance FromJSON InventoryDeletionSummaryItem where
  parseJSON =
    withObject
      "InventoryDeletionSummaryItem"
      ( \x ->
          InventoryDeletionSummaryItem'
            <$> (x .:? "RemainingCount") <*> (x .:? "Count") <*> (x .:? "Version")
      )

instance Hashable InventoryDeletionSummaryItem

instance NFData InventoryDeletionSummaryItem
