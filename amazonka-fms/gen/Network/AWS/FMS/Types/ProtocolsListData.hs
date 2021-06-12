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
-- Module      : Network.AWS.FMS.Types.ProtocolsListData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ProtocolsListData where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An AWS Firewall Manager protocols list.
--
-- /See:/ 'newProtocolsListData' smart constructor.
data ProtocolsListData = ProtocolsListData'
  { -- | The time that the AWS Firewall Manager protocols list was last updated.
    lastUpdateTime :: Core.Maybe Core.POSIX,
    -- | The ID of the AWS Firewall Manager protocols list.
    listId :: Core.Maybe Core.Text,
    -- | The time that the AWS Firewall Manager protocols list was created.
    createTime :: Core.Maybe Core.POSIX,
    -- | A map of previous version numbers to their corresponding protocol
    -- arrays.
    previousProtocolsList :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | A unique identifier for each update to the list. When you update the
    -- list, the update token must match the token of the current version of
    -- the application list. You can retrieve the update token by getting the
    -- list.
    listUpdateToken :: Core.Maybe Core.Text,
    -- | The name of the AWS Firewall Manager protocols list.
    listName :: Core.Text,
    -- | An array of protocols in the AWS Firewall Manager protocols list.
    protocolsList :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProtocolsListData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdateTime', 'protocolsListData_lastUpdateTime' - The time that the AWS Firewall Manager protocols list was last updated.
--
-- 'listId', 'protocolsListData_listId' - The ID of the AWS Firewall Manager protocols list.
--
-- 'createTime', 'protocolsListData_createTime' - The time that the AWS Firewall Manager protocols list was created.
--
-- 'previousProtocolsList', 'protocolsListData_previousProtocolsList' - A map of previous version numbers to their corresponding protocol
-- arrays.
--
-- 'listUpdateToken', 'protocolsListData_listUpdateToken' - A unique identifier for each update to the list. When you update the
-- list, the update token must match the token of the current version of
-- the application list. You can retrieve the update token by getting the
-- list.
--
-- 'listName', 'protocolsListData_listName' - The name of the AWS Firewall Manager protocols list.
--
-- 'protocolsList', 'protocolsListData_protocolsList' - An array of protocols in the AWS Firewall Manager protocols list.
newProtocolsListData ::
  -- | 'listName'
  Core.Text ->
  ProtocolsListData
newProtocolsListData pListName_ =
  ProtocolsListData'
    { lastUpdateTime = Core.Nothing,
      listId = Core.Nothing,
      createTime = Core.Nothing,
      previousProtocolsList = Core.Nothing,
      listUpdateToken = Core.Nothing,
      listName = pListName_,
      protocolsList = Core.mempty
    }

-- | The time that the AWS Firewall Manager protocols list was last updated.
protocolsListData_lastUpdateTime :: Lens.Lens' ProtocolsListData (Core.Maybe Core.UTCTime)
protocolsListData_lastUpdateTime = Lens.lens (\ProtocolsListData' {lastUpdateTime} -> lastUpdateTime) (\s@ProtocolsListData' {} a -> s {lastUpdateTime = a} :: ProtocolsListData) Core.. Lens.mapping Core._Time

-- | The ID of the AWS Firewall Manager protocols list.
protocolsListData_listId :: Lens.Lens' ProtocolsListData (Core.Maybe Core.Text)
protocolsListData_listId = Lens.lens (\ProtocolsListData' {listId} -> listId) (\s@ProtocolsListData' {} a -> s {listId = a} :: ProtocolsListData)

-- | The time that the AWS Firewall Manager protocols list was created.
protocolsListData_createTime :: Lens.Lens' ProtocolsListData (Core.Maybe Core.UTCTime)
protocolsListData_createTime = Lens.lens (\ProtocolsListData' {createTime} -> createTime) (\s@ProtocolsListData' {} a -> s {createTime = a} :: ProtocolsListData) Core.. Lens.mapping Core._Time

-- | A map of previous version numbers to their corresponding protocol
-- arrays.
protocolsListData_previousProtocolsList :: Lens.Lens' ProtocolsListData (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
protocolsListData_previousProtocolsList = Lens.lens (\ProtocolsListData' {previousProtocolsList} -> previousProtocolsList) (\s@ProtocolsListData' {} a -> s {previousProtocolsList = a} :: ProtocolsListData) Core.. Lens.mapping Lens._Coerce

-- | A unique identifier for each update to the list. When you update the
-- list, the update token must match the token of the current version of
-- the application list. You can retrieve the update token by getting the
-- list.
protocolsListData_listUpdateToken :: Lens.Lens' ProtocolsListData (Core.Maybe Core.Text)
protocolsListData_listUpdateToken = Lens.lens (\ProtocolsListData' {listUpdateToken} -> listUpdateToken) (\s@ProtocolsListData' {} a -> s {listUpdateToken = a} :: ProtocolsListData)

-- | The name of the AWS Firewall Manager protocols list.
protocolsListData_listName :: Lens.Lens' ProtocolsListData Core.Text
protocolsListData_listName = Lens.lens (\ProtocolsListData' {listName} -> listName) (\s@ProtocolsListData' {} a -> s {listName = a} :: ProtocolsListData)

-- | An array of protocols in the AWS Firewall Manager protocols list.
protocolsListData_protocolsList :: Lens.Lens' ProtocolsListData [Core.Text]
protocolsListData_protocolsList = Lens.lens (\ProtocolsListData' {protocolsList} -> protocolsList) (\s@ProtocolsListData' {} a -> s {protocolsList = a} :: ProtocolsListData) Core.. Lens._Coerce

instance Core.FromJSON ProtocolsListData where
  parseJSON =
    Core.withObject
      "ProtocolsListData"
      ( \x ->
          ProtocolsListData'
            Core.<$> (x Core..:? "LastUpdateTime")
            Core.<*> (x Core..:? "ListId")
            Core.<*> (x Core..:? "CreateTime")
            Core.<*> ( x Core..:? "PreviousProtocolsList"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "ListUpdateToken")
            Core.<*> (x Core..: "ListName")
            Core.<*> (x Core..:? "ProtocolsList" Core..!= Core.mempty)
      )

instance Core.Hashable ProtocolsListData

instance Core.NFData ProtocolsListData

instance Core.ToJSON ProtocolsListData where
  toJSON ProtocolsListData' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LastUpdateTime" Core..=) Core.<$> lastUpdateTime,
            ("ListId" Core..=) Core.<$> listId,
            ("CreateTime" Core..=) Core.<$> createTime,
            ("PreviousProtocolsList" Core..=)
              Core.<$> previousProtocolsList,
            ("ListUpdateToken" Core..=) Core.<$> listUpdateToken,
            Core.Just ("ListName" Core..= listName),
            Core.Just ("ProtocolsList" Core..= protocolsList)
          ]
      )
