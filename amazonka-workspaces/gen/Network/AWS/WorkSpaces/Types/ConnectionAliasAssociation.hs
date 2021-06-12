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
-- Module      : Network.AWS.WorkSpaces.Types.ConnectionAliasAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ConnectionAliasAssociation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkSpaces.Types.AssociationStatus

-- | Describes a connection alias association that is used for cross-Region
-- redirection. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces>.
--
-- /See:/ 'newConnectionAliasAssociation' smart constructor.
data ConnectionAliasAssociation = ConnectionAliasAssociation'
  { -- | The identifier of the directory associated with a connection alias.
    resourceId :: Core.Maybe Core.Text,
    -- | The identifier of the connection alias association. You use the
    -- connection identifier in the DNS TXT record when you\'re configuring
    -- your DNS routing policies.
    connectionIdentifier :: Core.Maybe Core.Text,
    -- | The identifier of the AWS account that associated the connection alias
    -- with a directory.
    associatedAccountId :: Core.Maybe Core.Text,
    -- | The association status of the connection alias.
    associationStatus :: Core.Maybe AssociationStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConnectionAliasAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'connectionAliasAssociation_resourceId' - The identifier of the directory associated with a connection alias.
--
-- 'connectionIdentifier', 'connectionAliasAssociation_connectionIdentifier' - The identifier of the connection alias association. You use the
-- connection identifier in the DNS TXT record when you\'re configuring
-- your DNS routing policies.
--
-- 'associatedAccountId', 'connectionAliasAssociation_associatedAccountId' - The identifier of the AWS account that associated the connection alias
-- with a directory.
--
-- 'associationStatus', 'connectionAliasAssociation_associationStatus' - The association status of the connection alias.
newConnectionAliasAssociation ::
  ConnectionAliasAssociation
newConnectionAliasAssociation =
  ConnectionAliasAssociation'
    { resourceId =
        Core.Nothing,
      connectionIdentifier = Core.Nothing,
      associatedAccountId = Core.Nothing,
      associationStatus = Core.Nothing
    }

-- | The identifier of the directory associated with a connection alias.
connectionAliasAssociation_resourceId :: Lens.Lens' ConnectionAliasAssociation (Core.Maybe Core.Text)
connectionAliasAssociation_resourceId = Lens.lens (\ConnectionAliasAssociation' {resourceId} -> resourceId) (\s@ConnectionAliasAssociation' {} a -> s {resourceId = a} :: ConnectionAliasAssociation)

-- | The identifier of the connection alias association. You use the
-- connection identifier in the DNS TXT record when you\'re configuring
-- your DNS routing policies.
connectionAliasAssociation_connectionIdentifier :: Lens.Lens' ConnectionAliasAssociation (Core.Maybe Core.Text)
connectionAliasAssociation_connectionIdentifier = Lens.lens (\ConnectionAliasAssociation' {connectionIdentifier} -> connectionIdentifier) (\s@ConnectionAliasAssociation' {} a -> s {connectionIdentifier = a} :: ConnectionAliasAssociation)

-- | The identifier of the AWS account that associated the connection alias
-- with a directory.
connectionAliasAssociation_associatedAccountId :: Lens.Lens' ConnectionAliasAssociation (Core.Maybe Core.Text)
connectionAliasAssociation_associatedAccountId = Lens.lens (\ConnectionAliasAssociation' {associatedAccountId} -> associatedAccountId) (\s@ConnectionAliasAssociation' {} a -> s {associatedAccountId = a} :: ConnectionAliasAssociation)

-- | The association status of the connection alias.
connectionAliasAssociation_associationStatus :: Lens.Lens' ConnectionAliasAssociation (Core.Maybe AssociationStatus)
connectionAliasAssociation_associationStatus = Lens.lens (\ConnectionAliasAssociation' {associationStatus} -> associationStatus) (\s@ConnectionAliasAssociation' {} a -> s {associationStatus = a} :: ConnectionAliasAssociation)

instance Core.FromJSON ConnectionAliasAssociation where
  parseJSON =
    Core.withObject
      "ConnectionAliasAssociation"
      ( \x ->
          ConnectionAliasAssociation'
            Core.<$> (x Core..:? "ResourceId")
            Core.<*> (x Core..:? "ConnectionIdentifier")
            Core.<*> (x Core..:? "AssociatedAccountId")
            Core.<*> (x Core..:? "AssociationStatus")
      )

instance Core.Hashable ConnectionAliasAssociation

instance Core.NFData ConnectionAliasAssociation
