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
-- Module      : Network.AWS.WorkSpaces.Types.ConnectionAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ConnectionAlias where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkSpaces.Types.ConnectionAliasAssociation
import Network.AWS.WorkSpaces.Types.ConnectionAliasState

-- | Describes a connection alias. Connection aliases are used for
-- cross-Region redirection. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces>.
--
-- /See:/ 'newConnectionAlias' smart constructor.
data ConnectionAlias = ConnectionAlias'
  { -- | The current state of the connection alias.
    state :: Core.Maybe ConnectionAliasState,
    -- | The identifier of the connection alias.
    aliasId :: Core.Maybe Core.Text,
    -- | The connection string specified for the connection alias. The connection
    -- string must be in the form of a fully qualified domain name (FQDN), such
    -- as @www.example.com@.
    connectionString :: Core.Maybe Core.Text,
    -- | The identifier of the AWS account that owns the connection alias.
    ownerAccountId :: Core.Maybe Core.Text,
    -- | The association status of the connection alias.
    associations :: Core.Maybe (Core.NonEmpty ConnectionAliasAssociation)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConnectionAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'connectionAlias_state' - The current state of the connection alias.
--
-- 'aliasId', 'connectionAlias_aliasId' - The identifier of the connection alias.
--
-- 'connectionString', 'connectionAlias_connectionString' - The connection string specified for the connection alias. The connection
-- string must be in the form of a fully qualified domain name (FQDN), such
-- as @www.example.com@.
--
-- 'ownerAccountId', 'connectionAlias_ownerAccountId' - The identifier of the AWS account that owns the connection alias.
--
-- 'associations', 'connectionAlias_associations' - The association status of the connection alias.
newConnectionAlias ::
  ConnectionAlias
newConnectionAlias =
  ConnectionAlias'
    { state = Core.Nothing,
      aliasId = Core.Nothing,
      connectionString = Core.Nothing,
      ownerAccountId = Core.Nothing,
      associations = Core.Nothing
    }

-- | The current state of the connection alias.
connectionAlias_state :: Lens.Lens' ConnectionAlias (Core.Maybe ConnectionAliasState)
connectionAlias_state = Lens.lens (\ConnectionAlias' {state} -> state) (\s@ConnectionAlias' {} a -> s {state = a} :: ConnectionAlias)

-- | The identifier of the connection alias.
connectionAlias_aliasId :: Lens.Lens' ConnectionAlias (Core.Maybe Core.Text)
connectionAlias_aliasId = Lens.lens (\ConnectionAlias' {aliasId} -> aliasId) (\s@ConnectionAlias' {} a -> s {aliasId = a} :: ConnectionAlias)

-- | The connection string specified for the connection alias. The connection
-- string must be in the form of a fully qualified domain name (FQDN), such
-- as @www.example.com@.
connectionAlias_connectionString :: Lens.Lens' ConnectionAlias (Core.Maybe Core.Text)
connectionAlias_connectionString = Lens.lens (\ConnectionAlias' {connectionString} -> connectionString) (\s@ConnectionAlias' {} a -> s {connectionString = a} :: ConnectionAlias)

-- | The identifier of the AWS account that owns the connection alias.
connectionAlias_ownerAccountId :: Lens.Lens' ConnectionAlias (Core.Maybe Core.Text)
connectionAlias_ownerAccountId = Lens.lens (\ConnectionAlias' {ownerAccountId} -> ownerAccountId) (\s@ConnectionAlias' {} a -> s {ownerAccountId = a} :: ConnectionAlias)

-- | The association status of the connection alias.
connectionAlias_associations :: Lens.Lens' ConnectionAlias (Core.Maybe (Core.NonEmpty ConnectionAliasAssociation))
connectionAlias_associations = Lens.lens (\ConnectionAlias' {associations} -> associations) (\s@ConnectionAlias' {} a -> s {associations = a} :: ConnectionAlias) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ConnectionAlias where
  parseJSON =
    Core.withObject
      "ConnectionAlias"
      ( \x ->
          ConnectionAlias'
            Core.<$> (x Core..:? "State")
            Core.<*> (x Core..:? "AliasId")
            Core.<*> (x Core..:? "ConnectionString")
            Core.<*> (x Core..:? "OwnerAccountId")
            Core.<*> (x Core..:? "Associations")
      )

instance Core.Hashable ConnectionAlias

instance Core.NFData ConnectionAlias
