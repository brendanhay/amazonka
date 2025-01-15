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
-- Module      : Amazonka.FMS.Types.ProtocolsListData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ProtocolsListData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An Firewall Manager protocols list.
--
-- /See:/ 'newProtocolsListData' smart constructor.
data ProtocolsListData = ProtocolsListData'
  { -- | The time that the Firewall Manager protocols list was created.
    createTime :: Prelude.Maybe Data.POSIX,
    -- | The time that the Firewall Manager protocols list was last updated.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the Firewall Manager protocols list.
    listId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for each update to the list. When you update the
    -- list, the update token must match the token of the current version of
    -- the application list. You can retrieve the update token by getting the
    -- list.
    listUpdateToken :: Prelude.Maybe Prelude.Text,
    -- | A map of previous version numbers to their corresponding protocol
    -- arrays.
    previousProtocolsList :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The name of the Firewall Manager protocols list.
    listName :: Prelude.Text,
    -- | An array of protocols in the Firewall Manager protocols list.
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
-- 'createTime', 'protocolsListData_createTime' - The time that the Firewall Manager protocols list was created.
--
-- 'lastUpdateTime', 'protocolsListData_lastUpdateTime' - The time that the Firewall Manager protocols list was last updated.
--
-- 'listId', 'protocolsListData_listId' - The ID of the Firewall Manager protocols list.
--
-- 'listUpdateToken', 'protocolsListData_listUpdateToken' - A unique identifier for each update to the list. When you update the
-- list, the update token must match the token of the current version of
-- the application list. You can retrieve the update token by getting the
-- list.
--
-- 'previousProtocolsList', 'protocolsListData_previousProtocolsList' - A map of previous version numbers to their corresponding protocol
-- arrays.
--
-- 'listName', 'protocolsListData_listName' - The name of the Firewall Manager protocols list.
--
-- 'protocolsList', 'protocolsListData_protocolsList' - An array of protocols in the Firewall Manager protocols list.
newProtocolsListData ::
  -- | 'listName'
  Prelude.Text ->
  ProtocolsListData
newProtocolsListData pListName_ =
  ProtocolsListData'
    { createTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      listId = Prelude.Nothing,
      listUpdateToken = Prelude.Nothing,
      previousProtocolsList = Prelude.Nothing,
      listName = pListName_,
      protocolsList = Prelude.mempty
    }

-- | The time that the Firewall Manager protocols list was created.
protocolsListData_createTime :: Lens.Lens' ProtocolsListData (Prelude.Maybe Prelude.UTCTime)
protocolsListData_createTime = Lens.lens (\ProtocolsListData' {createTime} -> createTime) (\s@ProtocolsListData' {} a -> s {createTime = a} :: ProtocolsListData) Prelude.. Lens.mapping Data._Time

-- | The time that the Firewall Manager protocols list was last updated.
protocolsListData_lastUpdateTime :: Lens.Lens' ProtocolsListData (Prelude.Maybe Prelude.UTCTime)
protocolsListData_lastUpdateTime = Lens.lens (\ProtocolsListData' {lastUpdateTime} -> lastUpdateTime) (\s@ProtocolsListData' {} a -> s {lastUpdateTime = a} :: ProtocolsListData) Prelude.. Lens.mapping Data._Time

-- | The ID of the Firewall Manager protocols list.
protocolsListData_listId :: Lens.Lens' ProtocolsListData (Prelude.Maybe Prelude.Text)
protocolsListData_listId = Lens.lens (\ProtocolsListData' {listId} -> listId) (\s@ProtocolsListData' {} a -> s {listId = a} :: ProtocolsListData)

-- | A unique identifier for each update to the list. When you update the
-- list, the update token must match the token of the current version of
-- the application list. You can retrieve the update token by getting the
-- list.
protocolsListData_listUpdateToken :: Lens.Lens' ProtocolsListData (Prelude.Maybe Prelude.Text)
protocolsListData_listUpdateToken = Lens.lens (\ProtocolsListData' {listUpdateToken} -> listUpdateToken) (\s@ProtocolsListData' {} a -> s {listUpdateToken = a} :: ProtocolsListData)

-- | A map of previous version numbers to their corresponding protocol
-- arrays.
protocolsListData_previousProtocolsList :: Lens.Lens' ProtocolsListData (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
protocolsListData_previousProtocolsList = Lens.lens (\ProtocolsListData' {previousProtocolsList} -> previousProtocolsList) (\s@ProtocolsListData' {} a -> s {previousProtocolsList = a} :: ProtocolsListData) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Firewall Manager protocols list.
protocolsListData_listName :: Lens.Lens' ProtocolsListData Prelude.Text
protocolsListData_listName = Lens.lens (\ProtocolsListData' {listName} -> listName) (\s@ProtocolsListData' {} a -> s {listName = a} :: ProtocolsListData)

-- | An array of protocols in the Firewall Manager protocols list.
protocolsListData_protocolsList :: Lens.Lens' ProtocolsListData [Prelude.Text]
protocolsListData_protocolsList = Lens.lens (\ProtocolsListData' {protocolsList} -> protocolsList) (\s@ProtocolsListData' {} a -> s {protocolsList = a} :: ProtocolsListData) Prelude.. Lens.coerced

instance Data.FromJSON ProtocolsListData where
  parseJSON =
    Data.withObject
      "ProtocolsListData"
      ( \x ->
          ProtocolsListData'
            Prelude.<$> (x Data..:? "CreateTime")
            Prelude.<*> (x Data..:? "LastUpdateTime")
            Prelude.<*> (x Data..:? "ListId")
            Prelude.<*> (x Data..:? "ListUpdateToken")
            Prelude.<*> ( x
                            Data..:? "PreviousProtocolsList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "ListName")
            Prelude.<*> (x Data..:? "ProtocolsList" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ProtocolsListData where
  hashWithSalt _salt ProtocolsListData' {..} =
    _salt
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` listId
      `Prelude.hashWithSalt` listUpdateToken
      `Prelude.hashWithSalt` previousProtocolsList
      `Prelude.hashWithSalt` listName
      `Prelude.hashWithSalt` protocolsList

instance Prelude.NFData ProtocolsListData where
  rnf ProtocolsListData' {..} =
    Prelude.rnf createTime `Prelude.seq`
      Prelude.rnf lastUpdateTime `Prelude.seq`
        Prelude.rnf listId `Prelude.seq`
          Prelude.rnf listUpdateToken `Prelude.seq`
            Prelude.rnf previousProtocolsList `Prelude.seq`
              Prelude.rnf listName `Prelude.seq`
                Prelude.rnf protocolsList

instance Data.ToJSON ProtocolsListData where
  toJSON ProtocolsListData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreateTime" Data..=) Prelude.<$> createTime,
            ("LastUpdateTime" Data..=)
              Prelude.<$> lastUpdateTime,
            ("ListId" Data..=) Prelude.<$> listId,
            ("ListUpdateToken" Data..=)
              Prelude.<$> listUpdateToken,
            ("PreviousProtocolsList" Data..=)
              Prelude.<$> previousProtocolsList,
            Prelude.Just ("ListName" Data..= listName),
            Prelude.Just
              ("ProtocolsList" Data..= protocolsList)
          ]
      )
