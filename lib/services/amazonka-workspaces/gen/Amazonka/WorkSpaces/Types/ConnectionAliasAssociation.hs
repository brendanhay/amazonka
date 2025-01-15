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
-- Module      : Amazonka.WorkSpaces.Types.ConnectionAliasAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.ConnectionAliasAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.AssociationStatus

-- | Describes a connection alias association that is used for cross-Region
-- redirection. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces>.
--
-- /See:/ 'newConnectionAliasAssociation' smart constructor.
data ConnectionAliasAssociation = ConnectionAliasAssociation'
  { -- | The identifier of the Amazon Web Services account that associated the
    -- connection alias with a directory.
    associatedAccountId :: Prelude.Maybe Prelude.Text,
    -- | The association status of the connection alias.
    associationStatus :: Prelude.Maybe AssociationStatus,
    -- | The identifier of the connection alias association. You use the
    -- connection identifier in the DNS TXT record when you\'re configuring
    -- your DNS routing policies.
    connectionIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the directory associated with a connection alias.
    resourceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionAliasAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedAccountId', 'connectionAliasAssociation_associatedAccountId' - The identifier of the Amazon Web Services account that associated the
-- connection alias with a directory.
--
-- 'associationStatus', 'connectionAliasAssociation_associationStatus' - The association status of the connection alias.
--
-- 'connectionIdentifier', 'connectionAliasAssociation_connectionIdentifier' - The identifier of the connection alias association. You use the
-- connection identifier in the DNS TXT record when you\'re configuring
-- your DNS routing policies.
--
-- 'resourceId', 'connectionAliasAssociation_resourceId' - The identifier of the directory associated with a connection alias.
newConnectionAliasAssociation ::
  ConnectionAliasAssociation
newConnectionAliasAssociation =
  ConnectionAliasAssociation'
    { associatedAccountId =
        Prelude.Nothing,
      associationStatus = Prelude.Nothing,
      connectionIdentifier = Prelude.Nothing,
      resourceId = Prelude.Nothing
    }

-- | The identifier of the Amazon Web Services account that associated the
-- connection alias with a directory.
connectionAliasAssociation_associatedAccountId :: Lens.Lens' ConnectionAliasAssociation (Prelude.Maybe Prelude.Text)
connectionAliasAssociation_associatedAccountId = Lens.lens (\ConnectionAliasAssociation' {associatedAccountId} -> associatedAccountId) (\s@ConnectionAliasAssociation' {} a -> s {associatedAccountId = a} :: ConnectionAliasAssociation)

-- | The association status of the connection alias.
connectionAliasAssociation_associationStatus :: Lens.Lens' ConnectionAliasAssociation (Prelude.Maybe AssociationStatus)
connectionAliasAssociation_associationStatus = Lens.lens (\ConnectionAliasAssociation' {associationStatus} -> associationStatus) (\s@ConnectionAliasAssociation' {} a -> s {associationStatus = a} :: ConnectionAliasAssociation)

-- | The identifier of the connection alias association. You use the
-- connection identifier in the DNS TXT record when you\'re configuring
-- your DNS routing policies.
connectionAliasAssociation_connectionIdentifier :: Lens.Lens' ConnectionAliasAssociation (Prelude.Maybe Prelude.Text)
connectionAliasAssociation_connectionIdentifier = Lens.lens (\ConnectionAliasAssociation' {connectionIdentifier} -> connectionIdentifier) (\s@ConnectionAliasAssociation' {} a -> s {connectionIdentifier = a} :: ConnectionAliasAssociation)

-- | The identifier of the directory associated with a connection alias.
connectionAliasAssociation_resourceId :: Lens.Lens' ConnectionAliasAssociation (Prelude.Maybe Prelude.Text)
connectionAliasAssociation_resourceId = Lens.lens (\ConnectionAliasAssociation' {resourceId} -> resourceId) (\s@ConnectionAliasAssociation' {} a -> s {resourceId = a} :: ConnectionAliasAssociation)

instance Data.FromJSON ConnectionAliasAssociation where
  parseJSON =
    Data.withObject
      "ConnectionAliasAssociation"
      ( \x ->
          ConnectionAliasAssociation'
            Prelude.<$> (x Data..:? "AssociatedAccountId")
            Prelude.<*> (x Data..:? "AssociationStatus")
            Prelude.<*> (x Data..:? "ConnectionIdentifier")
            Prelude.<*> (x Data..:? "ResourceId")
      )

instance Prelude.Hashable ConnectionAliasAssociation where
  hashWithSalt _salt ConnectionAliasAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` associatedAccountId
      `Prelude.hashWithSalt` associationStatus
      `Prelude.hashWithSalt` connectionIdentifier
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData ConnectionAliasAssociation where
  rnf ConnectionAliasAssociation' {..} =
    Prelude.rnf associatedAccountId `Prelude.seq`
      Prelude.rnf associationStatus `Prelude.seq`
        Prelude.rnf connectionIdentifier `Prelude.seq`
          Prelude.rnf resourceId
