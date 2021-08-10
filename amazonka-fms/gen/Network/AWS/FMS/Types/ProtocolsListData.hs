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
import qualified Network.AWS.Prelude as Prelude

-- | An AWS Firewall Manager protocols list.
--
-- /See:/ 'newProtocolsListData' smart constructor.
data ProtocolsListData = ProtocolsListData'
  { -- | The time that the AWS Firewall Manager protocols list was last updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the AWS Firewall Manager protocols list.
    listId :: Prelude.Maybe Prelude.Text,
    -- | The time that the AWS Firewall Manager protocols list was created.
    createTime :: Prelude.Maybe Core.POSIX,
    -- | A map of previous version numbers to their corresponding protocol
    -- arrays.
    previousProtocolsList :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | A unique identifier for each update to the list. When you update the
    -- list, the update token must match the token of the current version of
    -- the application list. You can retrieve the update token by getting the
    -- list.
    listUpdateToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the AWS Firewall Manager protocols list.
    listName :: Prelude.Text,
    -- | An array of protocols in the AWS Firewall Manager protocols list.
    protocolsList :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ProtocolsListData
newProtocolsListData pListName_ =
  ProtocolsListData'
    { lastUpdateTime =
        Prelude.Nothing,
      listId = Prelude.Nothing,
      createTime = Prelude.Nothing,
      previousProtocolsList = Prelude.Nothing,
      listUpdateToken = Prelude.Nothing,
      listName = pListName_,
      protocolsList = Prelude.mempty
    }

-- | The time that the AWS Firewall Manager protocols list was last updated.
protocolsListData_lastUpdateTime :: Lens.Lens' ProtocolsListData (Prelude.Maybe Prelude.UTCTime)
protocolsListData_lastUpdateTime = Lens.lens (\ProtocolsListData' {lastUpdateTime} -> lastUpdateTime) (\s@ProtocolsListData' {} a -> s {lastUpdateTime = a} :: ProtocolsListData) Prelude.. Lens.mapping Core._Time

-- | The ID of the AWS Firewall Manager protocols list.
protocolsListData_listId :: Lens.Lens' ProtocolsListData (Prelude.Maybe Prelude.Text)
protocolsListData_listId = Lens.lens (\ProtocolsListData' {listId} -> listId) (\s@ProtocolsListData' {} a -> s {listId = a} :: ProtocolsListData)

-- | The time that the AWS Firewall Manager protocols list was created.
protocolsListData_createTime :: Lens.Lens' ProtocolsListData (Prelude.Maybe Prelude.UTCTime)
protocolsListData_createTime = Lens.lens (\ProtocolsListData' {createTime} -> createTime) (\s@ProtocolsListData' {} a -> s {createTime = a} :: ProtocolsListData) Prelude.. Lens.mapping Core._Time

-- | A map of previous version numbers to their corresponding protocol
-- arrays.
protocolsListData_previousProtocolsList :: Lens.Lens' ProtocolsListData (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
protocolsListData_previousProtocolsList = Lens.lens (\ProtocolsListData' {previousProtocolsList} -> previousProtocolsList) (\s@ProtocolsListData' {} a -> s {previousProtocolsList = a} :: ProtocolsListData) Prelude.. Lens.mapping Lens._Coerce

-- | A unique identifier for each update to the list. When you update the
-- list, the update token must match the token of the current version of
-- the application list. You can retrieve the update token by getting the
-- list.
protocolsListData_listUpdateToken :: Lens.Lens' ProtocolsListData (Prelude.Maybe Prelude.Text)
protocolsListData_listUpdateToken = Lens.lens (\ProtocolsListData' {listUpdateToken} -> listUpdateToken) (\s@ProtocolsListData' {} a -> s {listUpdateToken = a} :: ProtocolsListData)

-- | The name of the AWS Firewall Manager protocols list.
protocolsListData_listName :: Lens.Lens' ProtocolsListData Prelude.Text
protocolsListData_listName = Lens.lens (\ProtocolsListData' {listName} -> listName) (\s@ProtocolsListData' {} a -> s {listName = a} :: ProtocolsListData)

-- | An array of protocols in the AWS Firewall Manager protocols list.
protocolsListData_protocolsList :: Lens.Lens' ProtocolsListData [Prelude.Text]
protocolsListData_protocolsList = Lens.lens (\ProtocolsListData' {protocolsList} -> protocolsList) (\s@ProtocolsListData' {} a -> s {protocolsList = a} :: ProtocolsListData) Prelude.. Lens._Coerce

instance Core.FromJSON ProtocolsListData where
  parseJSON =
    Core.withObject
      "ProtocolsListData"
      ( \x ->
          ProtocolsListData'
            Prelude.<$> (x Core..:? "LastUpdateTime")
            Prelude.<*> (x Core..:? "ListId")
            Prelude.<*> (x Core..:? "CreateTime")
            Prelude.<*> ( x Core..:? "PreviousProtocolsList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ListUpdateToken")
            Prelude.<*> (x Core..: "ListName")
            Prelude.<*> (x Core..:? "ProtocolsList" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ProtocolsListData

instance Prelude.NFData ProtocolsListData

instance Core.ToJSON ProtocolsListData where
  toJSON ProtocolsListData' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LastUpdateTime" Core..=)
              Prelude.<$> lastUpdateTime,
            ("ListId" Core..=) Prelude.<$> listId,
            ("CreateTime" Core..=) Prelude.<$> createTime,
            ("PreviousProtocolsList" Core..=)
              Prelude.<$> previousProtocolsList,
            ("ListUpdateToken" Core..=)
              Prelude.<$> listUpdateToken,
            Prelude.Just ("ListName" Core..= listName),
            Prelude.Just
              ("ProtocolsList" Core..= protocolsList)
          ]
      )
