{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryDeletionStatusItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryDeletionStatusItem where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.InventoryDeletionStatus
import Network.AWS.SSM.Types.InventoryDeletionSummary

-- | Status information returned by the @DeleteInventory@ action.
--
--
--
-- /See:/ 'inventoryDeletionStatusItem' smart constructor.
data InventoryDeletionStatusItem = InventoryDeletionStatusItem'
  { _idsiTypeName ::
      !(Maybe Text),
    _idsiLastStatusUpdateTime ::
      !(Maybe POSIX),
    _idsiLastStatusMessage ::
      !(Maybe Text),
    _idsiDeletionSummary ::
      !(Maybe InventoryDeletionSummary),
    _idsiLastStatus ::
      !(Maybe InventoryDeletionStatus),
    _idsiDeletionStartTime ::
      !(Maybe POSIX),
    _idsiDeletionId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventoryDeletionStatusItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idsiTypeName' - The name of the inventory data type.
--
-- * 'idsiLastStatusUpdateTime' - The UTC timestamp of when the last status report.
--
-- * 'idsiLastStatusMessage' - Information about the status.
--
-- * 'idsiDeletionSummary' - Information about the delete operation. For more information about this summary, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete Understanding the delete inventory summary> in the /AWS Systems Manager User Guide/ .
--
-- * 'idsiLastStatus' - The status of the operation. Possible values are InProgress and Complete.
--
-- * 'idsiDeletionStartTime' - The UTC timestamp when the delete operation started.
--
-- * 'idsiDeletionId' - The deletion ID returned by the @DeleteInventory@ action.
inventoryDeletionStatusItem ::
  InventoryDeletionStatusItem
inventoryDeletionStatusItem =
  InventoryDeletionStatusItem'
    { _idsiTypeName = Nothing,
      _idsiLastStatusUpdateTime = Nothing,
      _idsiLastStatusMessage = Nothing,
      _idsiDeletionSummary = Nothing,
      _idsiLastStatus = Nothing,
      _idsiDeletionStartTime = Nothing,
      _idsiDeletionId = Nothing
    }

-- | The name of the inventory data type.
idsiTypeName :: Lens' InventoryDeletionStatusItem (Maybe Text)
idsiTypeName = lens _idsiTypeName (\s a -> s {_idsiTypeName = a})

-- | The UTC timestamp of when the last status report.
idsiLastStatusUpdateTime :: Lens' InventoryDeletionStatusItem (Maybe UTCTime)
idsiLastStatusUpdateTime = lens _idsiLastStatusUpdateTime (\s a -> s {_idsiLastStatusUpdateTime = a}) . mapping _Time

-- | Information about the status.
idsiLastStatusMessage :: Lens' InventoryDeletionStatusItem (Maybe Text)
idsiLastStatusMessage = lens _idsiLastStatusMessage (\s a -> s {_idsiLastStatusMessage = a})

-- | Information about the delete operation. For more information about this summary, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete Understanding the delete inventory summary> in the /AWS Systems Manager User Guide/ .
idsiDeletionSummary :: Lens' InventoryDeletionStatusItem (Maybe InventoryDeletionSummary)
idsiDeletionSummary = lens _idsiDeletionSummary (\s a -> s {_idsiDeletionSummary = a})

-- | The status of the operation. Possible values are InProgress and Complete.
idsiLastStatus :: Lens' InventoryDeletionStatusItem (Maybe InventoryDeletionStatus)
idsiLastStatus = lens _idsiLastStatus (\s a -> s {_idsiLastStatus = a})

-- | The UTC timestamp when the delete operation started.
idsiDeletionStartTime :: Lens' InventoryDeletionStatusItem (Maybe UTCTime)
idsiDeletionStartTime = lens _idsiDeletionStartTime (\s a -> s {_idsiDeletionStartTime = a}) . mapping _Time

-- | The deletion ID returned by the @DeleteInventory@ action.
idsiDeletionId :: Lens' InventoryDeletionStatusItem (Maybe Text)
idsiDeletionId = lens _idsiDeletionId (\s a -> s {_idsiDeletionId = a})

instance FromJSON InventoryDeletionStatusItem where
  parseJSON =
    withObject
      "InventoryDeletionStatusItem"
      ( \x ->
          InventoryDeletionStatusItem'
            <$> (x .:? "TypeName")
            <*> (x .:? "LastStatusUpdateTime")
            <*> (x .:? "LastStatusMessage")
            <*> (x .:? "DeletionSummary")
            <*> (x .:? "LastStatus")
            <*> (x .:? "DeletionStartTime")
            <*> (x .:? "DeletionId")
      )

instance Hashable InventoryDeletionStatusItem

instance NFData InventoryDeletionStatusItem
