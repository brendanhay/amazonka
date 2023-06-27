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
-- Module      : Amazonka.Glue.Types.FederatedTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.FederatedTable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A table that points to an entity outside the Glue Data Catalog.
--
-- /See:/ 'newFederatedTable' smart constructor.
data FederatedTable = FederatedTable'
  { -- | The name of the connection to the external metastore.
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the federated database.
    databaseIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the federated table.
    identifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FederatedTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionName', 'federatedTable_connectionName' - The name of the connection to the external metastore.
--
-- 'databaseIdentifier', 'federatedTable_databaseIdentifier' - A unique identifier for the federated database.
--
-- 'identifier', 'federatedTable_identifier' - A unique identifier for the federated table.
newFederatedTable ::
  FederatedTable
newFederatedTable =
  FederatedTable'
    { connectionName = Prelude.Nothing,
      databaseIdentifier = Prelude.Nothing,
      identifier = Prelude.Nothing
    }

-- | The name of the connection to the external metastore.
federatedTable_connectionName :: Lens.Lens' FederatedTable (Prelude.Maybe Prelude.Text)
federatedTable_connectionName = Lens.lens (\FederatedTable' {connectionName} -> connectionName) (\s@FederatedTable' {} a -> s {connectionName = a} :: FederatedTable)

-- | A unique identifier for the federated database.
federatedTable_databaseIdentifier :: Lens.Lens' FederatedTable (Prelude.Maybe Prelude.Text)
federatedTable_databaseIdentifier = Lens.lens (\FederatedTable' {databaseIdentifier} -> databaseIdentifier) (\s@FederatedTable' {} a -> s {databaseIdentifier = a} :: FederatedTable)

-- | A unique identifier for the federated table.
federatedTable_identifier :: Lens.Lens' FederatedTable (Prelude.Maybe Prelude.Text)
federatedTable_identifier = Lens.lens (\FederatedTable' {identifier} -> identifier) (\s@FederatedTable' {} a -> s {identifier = a} :: FederatedTable)

instance Data.FromJSON FederatedTable where
  parseJSON =
    Data.withObject
      "FederatedTable"
      ( \x ->
          FederatedTable'
            Prelude.<$> (x Data..:? "ConnectionName")
            Prelude.<*> (x Data..:? "DatabaseIdentifier")
            Prelude.<*> (x Data..:? "Identifier")
      )

instance Prelude.Hashable FederatedTable where
  hashWithSalt _salt FederatedTable' {..} =
    _salt
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` databaseIdentifier
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData FederatedTable where
  rnf FederatedTable' {..} =
    Prelude.rnf connectionName
      `Prelude.seq` Prelude.rnf databaseIdentifier
      `Prelude.seq` Prelude.rnf identifier
