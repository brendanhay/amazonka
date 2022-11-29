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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ProtocolsListDataSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details of the Firewall Manager protocols list.
--
-- /See:/ 'newProtocolsListDataSummary' smart constructor.
data ProtocolsListDataSummary = ProtocolsListDataSummary'
  { -- | The ID of the specified protocols list.
    listId :: Prelude.Maybe Prelude.Text,
    -- | The name of the specified protocols list.
    listName :: Prelude.Maybe Prelude.Text,
    -- | An array of protocols in the Firewall Manager protocols list.
    protocolsList :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the specified protocols list.
    listArn :: Prelude.Maybe Prelude.Text
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
-- 'listId', 'protocolsListDataSummary_listId' - The ID of the specified protocols list.
--
-- 'listName', 'protocolsListDataSummary_listName' - The name of the specified protocols list.
--
-- 'protocolsList', 'protocolsListDataSummary_protocolsList' - An array of protocols in the Firewall Manager protocols list.
--
-- 'listArn', 'protocolsListDataSummary_listArn' - The Amazon Resource Name (ARN) of the specified protocols list.
newProtocolsListDataSummary ::
  ProtocolsListDataSummary
newProtocolsListDataSummary =
  ProtocolsListDataSummary'
    { listId = Prelude.Nothing,
      listName = Prelude.Nothing,
      protocolsList = Prelude.Nothing,
      listArn = Prelude.Nothing
    }

-- | The ID of the specified protocols list.
protocolsListDataSummary_listId :: Lens.Lens' ProtocolsListDataSummary (Prelude.Maybe Prelude.Text)
protocolsListDataSummary_listId = Lens.lens (\ProtocolsListDataSummary' {listId} -> listId) (\s@ProtocolsListDataSummary' {} a -> s {listId = a} :: ProtocolsListDataSummary)

-- | The name of the specified protocols list.
protocolsListDataSummary_listName :: Lens.Lens' ProtocolsListDataSummary (Prelude.Maybe Prelude.Text)
protocolsListDataSummary_listName = Lens.lens (\ProtocolsListDataSummary' {listName} -> listName) (\s@ProtocolsListDataSummary' {} a -> s {listName = a} :: ProtocolsListDataSummary)

-- | An array of protocols in the Firewall Manager protocols list.
protocolsListDataSummary_protocolsList :: Lens.Lens' ProtocolsListDataSummary (Prelude.Maybe [Prelude.Text])
protocolsListDataSummary_protocolsList = Lens.lens (\ProtocolsListDataSummary' {protocolsList} -> protocolsList) (\s@ProtocolsListDataSummary' {} a -> s {protocolsList = a} :: ProtocolsListDataSummary) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the specified protocols list.
protocolsListDataSummary_listArn :: Lens.Lens' ProtocolsListDataSummary (Prelude.Maybe Prelude.Text)
protocolsListDataSummary_listArn = Lens.lens (\ProtocolsListDataSummary' {listArn} -> listArn) (\s@ProtocolsListDataSummary' {} a -> s {listArn = a} :: ProtocolsListDataSummary)

instance Core.FromJSON ProtocolsListDataSummary where
  parseJSON =
    Core.withObject
      "ProtocolsListDataSummary"
      ( \x ->
          ProtocolsListDataSummary'
            Prelude.<$> (x Core..:? "ListId")
            Prelude.<*> (x Core..:? "ListName")
            Prelude.<*> (x Core..:? "ProtocolsList" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ListArn")
      )

instance Prelude.Hashable ProtocolsListDataSummary where
  hashWithSalt _salt ProtocolsListDataSummary' {..} =
    _salt `Prelude.hashWithSalt` listId
      `Prelude.hashWithSalt` listName
      `Prelude.hashWithSalt` protocolsList
      `Prelude.hashWithSalt` listArn

instance Prelude.NFData ProtocolsListDataSummary where
  rnf ProtocolsListDataSummary' {..} =
    Prelude.rnf listId
      `Prelude.seq` Prelude.rnf listName
      `Prelude.seq` Prelude.rnf protocolsList
      `Prelude.seq` Prelude.rnf listArn
