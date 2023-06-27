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
-- Module      : Amazonka.Glue.Types.FederatedDatabase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.FederatedDatabase where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A database that points to an entity outside the Glue Data Catalog.
--
-- /See:/ 'newFederatedDatabase' smart constructor.
data FederatedDatabase = FederatedDatabase'
  { -- | The name of the connection to the external metastore.
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the federated database.
    identifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FederatedDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionName', 'federatedDatabase_connectionName' - The name of the connection to the external metastore.
--
-- 'identifier', 'federatedDatabase_identifier' - A unique identifier for the federated database.
newFederatedDatabase ::
  FederatedDatabase
newFederatedDatabase =
  FederatedDatabase'
    { connectionName =
        Prelude.Nothing,
      identifier = Prelude.Nothing
    }

-- | The name of the connection to the external metastore.
federatedDatabase_connectionName :: Lens.Lens' FederatedDatabase (Prelude.Maybe Prelude.Text)
federatedDatabase_connectionName = Lens.lens (\FederatedDatabase' {connectionName} -> connectionName) (\s@FederatedDatabase' {} a -> s {connectionName = a} :: FederatedDatabase)

-- | A unique identifier for the federated database.
federatedDatabase_identifier :: Lens.Lens' FederatedDatabase (Prelude.Maybe Prelude.Text)
federatedDatabase_identifier = Lens.lens (\FederatedDatabase' {identifier} -> identifier) (\s@FederatedDatabase' {} a -> s {identifier = a} :: FederatedDatabase)

instance Data.FromJSON FederatedDatabase where
  parseJSON =
    Data.withObject
      "FederatedDatabase"
      ( \x ->
          FederatedDatabase'
            Prelude.<$> (x Data..:? "ConnectionName")
            Prelude.<*> (x Data..:? "Identifier")
      )

instance Prelude.Hashable FederatedDatabase where
  hashWithSalt _salt FederatedDatabase' {..} =
    _salt
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData FederatedDatabase where
  rnf FederatedDatabase' {..} =
    Prelude.rnf connectionName
      `Prelude.seq` Prelude.rnf identifier

instance Data.ToJSON FederatedDatabase where
  toJSON FederatedDatabase' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConnectionName" Data..=)
              Prelude.<$> connectionName,
            ("Identifier" Data..=) Prelude.<$> identifier
          ]
      )
