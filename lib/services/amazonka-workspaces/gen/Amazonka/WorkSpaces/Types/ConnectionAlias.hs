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
-- Module      : Amazonka.WorkSpaces.Types.ConnectionAlias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.ConnectionAlias where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.ConnectionAliasAssociation
import Amazonka.WorkSpaces.Types.ConnectionAliasState

-- | Describes a connection alias. Connection aliases are used for
-- cross-Region redirection. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces>.
--
-- /See:/ 'newConnectionAlias' smart constructor.
data ConnectionAlias = ConnectionAlias'
  { -- | The association status of the connection alias.
    associations :: Prelude.Maybe (Prelude.NonEmpty ConnectionAliasAssociation),
    -- | The identifier of the connection alias.
    aliasId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the connection alias.
    state :: Prelude.Maybe ConnectionAliasState,
    -- | The connection string specified for the connection alias. The connection
    -- string must be in the form of a fully qualified domain name (FQDN), such
    -- as @www.example.com@.
    connectionString :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Web Services account that owns the
    -- connection alias.
    ownerAccountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associations', 'connectionAlias_associations' - The association status of the connection alias.
--
-- 'aliasId', 'connectionAlias_aliasId' - The identifier of the connection alias.
--
-- 'state', 'connectionAlias_state' - The current state of the connection alias.
--
-- 'connectionString', 'connectionAlias_connectionString' - The connection string specified for the connection alias. The connection
-- string must be in the form of a fully qualified domain name (FQDN), such
-- as @www.example.com@.
--
-- 'ownerAccountId', 'connectionAlias_ownerAccountId' - The identifier of the Amazon Web Services account that owns the
-- connection alias.
newConnectionAlias ::
  ConnectionAlias
newConnectionAlias =
  ConnectionAlias'
    { associations = Prelude.Nothing,
      aliasId = Prelude.Nothing,
      state = Prelude.Nothing,
      connectionString = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing
    }

-- | The association status of the connection alias.
connectionAlias_associations :: Lens.Lens' ConnectionAlias (Prelude.Maybe (Prelude.NonEmpty ConnectionAliasAssociation))
connectionAlias_associations = Lens.lens (\ConnectionAlias' {associations} -> associations) (\s@ConnectionAlias' {} a -> s {associations = a} :: ConnectionAlias) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the connection alias.
connectionAlias_aliasId :: Lens.Lens' ConnectionAlias (Prelude.Maybe Prelude.Text)
connectionAlias_aliasId = Lens.lens (\ConnectionAlias' {aliasId} -> aliasId) (\s@ConnectionAlias' {} a -> s {aliasId = a} :: ConnectionAlias)

-- | The current state of the connection alias.
connectionAlias_state :: Lens.Lens' ConnectionAlias (Prelude.Maybe ConnectionAliasState)
connectionAlias_state = Lens.lens (\ConnectionAlias' {state} -> state) (\s@ConnectionAlias' {} a -> s {state = a} :: ConnectionAlias)

-- | The connection string specified for the connection alias. The connection
-- string must be in the form of a fully qualified domain name (FQDN), such
-- as @www.example.com@.
connectionAlias_connectionString :: Lens.Lens' ConnectionAlias (Prelude.Maybe Prelude.Text)
connectionAlias_connectionString = Lens.lens (\ConnectionAlias' {connectionString} -> connectionString) (\s@ConnectionAlias' {} a -> s {connectionString = a} :: ConnectionAlias)

-- | The identifier of the Amazon Web Services account that owns the
-- connection alias.
connectionAlias_ownerAccountId :: Lens.Lens' ConnectionAlias (Prelude.Maybe Prelude.Text)
connectionAlias_ownerAccountId = Lens.lens (\ConnectionAlias' {ownerAccountId} -> ownerAccountId) (\s@ConnectionAlias' {} a -> s {ownerAccountId = a} :: ConnectionAlias)

instance Core.FromJSON ConnectionAlias where
  parseJSON =
    Core.withObject
      "ConnectionAlias"
      ( \x ->
          ConnectionAlias'
            Prelude.<$> (x Core..:? "Associations")
            Prelude.<*> (x Core..:? "AliasId")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "ConnectionString")
            Prelude.<*> (x Core..:? "OwnerAccountId")
      )

instance Prelude.Hashable ConnectionAlias where
  hashWithSalt _salt ConnectionAlias' {..} =
    _salt `Prelude.hashWithSalt` associations
      `Prelude.hashWithSalt` aliasId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` connectionString
      `Prelude.hashWithSalt` ownerAccountId

instance Prelude.NFData ConnectionAlias where
  rnf ConnectionAlias' {..} =
    Prelude.rnf associations
      `Prelude.seq` Prelude.rnf aliasId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf connectionString
      `Prelude.seq` Prelude.rnf ownerAccountId
