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
-- Module      : Amazonka.FMS.Types.ProtocolsListDataSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ProtocolsListDataSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of the Firewall Manager protocols list.
--
-- /See:/ 'newProtocolsListDataSummary' smart constructor.
data ProtocolsListDataSummary = ProtocolsListDataSummary'
  { -- | The Amazon Resource Name (ARN) of the specified protocols list.
    listArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the specified protocols list.
    listId :: Prelude.Maybe Prelude.Text,
    -- | The name of the specified protocols list.
    listName :: Prelude.Maybe Prelude.Text,
    -- | An array of protocols in the Firewall Manager protocols list.
    protocolsList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtocolsListDataSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listArn', 'protocolsListDataSummary_listArn' - The Amazon Resource Name (ARN) of the specified protocols list.
--
-- 'listId', 'protocolsListDataSummary_listId' - The ID of the specified protocols list.
--
-- 'listName', 'protocolsListDataSummary_listName' - The name of the specified protocols list.
--
-- 'protocolsList', 'protocolsListDataSummary_protocolsList' - An array of protocols in the Firewall Manager protocols list.
newProtocolsListDataSummary ::
  ProtocolsListDataSummary
newProtocolsListDataSummary =
  ProtocolsListDataSummary'
    { listArn =
        Prelude.Nothing,
      listId = Prelude.Nothing,
      listName = Prelude.Nothing,
      protocolsList = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the specified protocols list.
protocolsListDataSummary_listArn :: Lens.Lens' ProtocolsListDataSummary (Prelude.Maybe Prelude.Text)
protocolsListDataSummary_listArn = Lens.lens (\ProtocolsListDataSummary' {listArn} -> listArn) (\s@ProtocolsListDataSummary' {} a -> s {listArn = a} :: ProtocolsListDataSummary)

-- | The ID of the specified protocols list.
protocolsListDataSummary_listId :: Lens.Lens' ProtocolsListDataSummary (Prelude.Maybe Prelude.Text)
protocolsListDataSummary_listId = Lens.lens (\ProtocolsListDataSummary' {listId} -> listId) (\s@ProtocolsListDataSummary' {} a -> s {listId = a} :: ProtocolsListDataSummary)

-- | The name of the specified protocols list.
protocolsListDataSummary_listName :: Lens.Lens' ProtocolsListDataSummary (Prelude.Maybe Prelude.Text)
protocolsListDataSummary_listName = Lens.lens (\ProtocolsListDataSummary' {listName} -> listName) (\s@ProtocolsListDataSummary' {} a -> s {listName = a} :: ProtocolsListDataSummary)

-- | An array of protocols in the Firewall Manager protocols list.
protocolsListDataSummary_protocolsList :: Lens.Lens' ProtocolsListDataSummary (Prelude.Maybe [Prelude.Text])
protocolsListDataSummary_protocolsList = Lens.lens (\ProtocolsListDataSummary' {protocolsList} -> protocolsList) (\s@ProtocolsListDataSummary' {} a -> s {protocolsList = a} :: ProtocolsListDataSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ProtocolsListDataSummary where
  parseJSON =
    Data.withObject
      "ProtocolsListDataSummary"
      ( \x ->
          ProtocolsListDataSummary'
            Prelude.<$> (x Data..:? "ListArn")
            Prelude.<*> (x Data..:? "ListId")
            Prelude.<*> (x Data..:? "ListName")
            Prelude.<*> (x Data..:? "ProtocolsList" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ProtocolsListDataSummary where
  hashWithSalt _salt ProtocolsListDataSummary' {..} =
    _salt `Prelude.hashWithSalt` listArn
      `Prelude.hashWithSalt` listId
      `Prelude.hashWithSalt` listName
      `Prelude.hashWithSalt` protocolsList

instance Prelude.NFData ProtocolsListDataSummary where
  rnf ProtocolsListDataSummary' {..} =
    Prelude.rnf listArn
      `Prelude.seq` Prelude.rnf listId
      `Prelude.seq` Prelude.rnf listName
      `Prelude.seq` Prelude.rnf protocolsList
