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
-- Module      : Amazonka.WorkSpaces.Types.ConnectionAliasPermission
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.ConnectionAliasPermission where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the permissions for a connection alias. Connection aliases are
-- used for cross-Region redirection. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces>.
--
-- /See:/ 'newConnectionAliasPermission' smart constructor.
data ConnectionAliasPermission = ConnectionAliasPermission'
  { -- | The identifier of the Amazon Web Services account that the connection
    -- alias is shared with.
    sharedAccountId :: Prelude.Text,
    -- | Indicates whether the specified Amazon Web Services account is allowed
    -- to associate the connection alias with a directory.
    allowAssociation :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionAliasPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sharedAccountId', 'connectionAliasPermission_sharedAccountId' - The identifier of the Amazon Web Services account that the connection
-- alias is shared with.
--
-- 'allowAssociation', 'connectionAliasPermission_allowAssociation' - Indicates whether the specified Amazon Web Services account is allowed
-- to associate the connection alias with a directory.
newConnectionAliasPermission ::
  -- | 'sharedAccountId'
  Prelude.Text ->
  -- | 'allowAssociation'
  Prelude.Bool ->
  ConnectionAliasPermission
newConnectionAliasPermission
  pSharedAccountId_
  pAllowAssociation_ =
    ConnectionAliasPermission'
      { sharedAccountId =
          pSharedAccountId_,
        allowAssociation = pAllowAssociation_
      }

-- | The identifier of the Amazon Web Services account that the connection
-- alias is shared with.
connectionAliasPermission_sharedAccountId :: Lens.Lens' ConnectionAliasPermission Prelude.Text
connectionAliasPermission_sharedAccountId = Lens.lens (\ConnectionAliasPermission' {sharedAccountId} -> sharedAccountId) (\s@ConnectionAliasPermission' {} a -> s {sharedAccountId = a} :: ConnectionAliasPermission)

-- | Indicates whether the specified Amazon Web Services account is allowed
-- to associate the connection alias with a directory.
connectionAliasPermission_allowAssociation :: Lens.Lens' ConnectionAliasPermission Prelude.Bool
connectionAliasPermission_allowAssociation = Lens.lens (\ConnectionAliasPermission' {allowAssociation} -> allowAssociation) (\s@ConnectionAliasPermission' {} a -> s {allowAssociation = a} :: ConnectionAliasPermission)

instance Core.FromJSON ConnectionAliasPermission where
  parseJSON =
    Core.withObject
      "ConnectionAliasPermission"
      ( \x ->
          ConnectionAliasPermission'
            Prelude.<$> (x Core..: "SharedAccountId")
            Prelude.<*> (x Core..: "AllowAssociation")
      )

instance Prelude.Hashable ConnectionAliasPermission where
  hashWithSalt _salt ConnectionAliasPermission' {..} =
    _salt `Prelude.hashWithSalt` sharedAccountId
      `Prelude.hashWithSalt` allowAssociation

instance Prelude.NFData ConnectionAliasPermission where
  rnf ConnectionAliasPermission' {..} =
    Prelude.rnf sharedAccountId
      `Prelude.seq` Prelude.rnf allowAssociation

instance Core.ToJSON ConnectionAliasPermission where
  toJSON ConnectionAliasPermission' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SharedAccountId" Core..= sharedAccountId),
            Prelude.Just
              ("AllowAssociation" Core..= allowAssociation)
          ]
      )
