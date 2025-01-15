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
-- Module      : Amazonka.ManagedBlockChain.Types.ProposalActions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.ProposalActions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types.InviteAction
import Amazonka.ManagedBlockChain.Types.RemoveAction
import qualified Amazonka.Prelude as Prelude

-- | The actions to carry out if a proposal is @APPROVED@.
--
-- Applies only to Hyperledger Fabric.
--
-- /See:/ 'newProposalActions' smart constructor.
data ProposalActions = ProposalActions'
  { -- | The actions to perform for an @APPROVED@ proposal to invite an Amazon
    -- Web Services account to create a member and join the network.
    invitations :: Prelude.Maybe [InviteAction],
    -- | The actions to perform for an @APPROVED@ proposal to remove a member
    -- from the network, which deletes the member and all associated member
    -- resources from the network.
    removals :: Prelude.Maybe [RemoveAction]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProposalActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invitations', 'proposalActions_invitations' - The actions to perform for an @APPROVED@ proposal to invite an Amazon
-- Web Services account to create a member and join the network.
--
-- 'removals', 'proposalActions_removals' - The actions to perform for an @APPROVED@ proposal to remove a member
-- from the network, which deletes the member and all associated member
-- resources from the network.
newProposalActions ::
  ProposalActions
newProposalActions =
  ProposalActions'
    { invitations = Prelude.Nothing,
      removals = Prelude.Nothing
    }

-- | The actions to perform for an @APPROVED@ proposal to invite an Amazon
-- Web Services account to create a member and join the network.
proposalActions_invitations :: Lens.Lens' ProposalActions (Prelude.Maybe [InviteAction])
proposalActions_invitations = Lens.lens (\ProposalActions' {invitations} -> invitations) (\s@ProposalActions' {} a -> s {invitations = a} :: ProposalActions) Prelude.. Lens.mapping Lens.coerced

-- | The actions to perform for an @APPROVED@ proposal to remove a member
-- from the network, which deletes the member and all associated member
-- resources from the network.
proposalActions_removals :: Lens.Lens' ProposalActions (Prelude.Maybe [RemoveAction])
proposalActions_removals = Lens.lens (\ProposalActions' {removals} -> removals) (\s@ProposalActions' {} a -> s {removals = a} :: ProposalActions) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ProposalActions where
  parseJSON =
    Data.withObject
      "ProposalActions"
      ( \x ->
          ProposalActions'
            Prelude.<$> (x Data..:? "Invitations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Removals" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ProposalActions where
  hashWithSalt _salt ProposalActions' {..} =
    _salt
      `Prelude.hashWithSalt` invitations
      `Prelude.hashWithSalt` removals

instance Prelude.NFData ProposalActions where
  rnf ProposalActions' {..} =
    Prelude.rnf invitations `Prelude.seq`
      Prelude.rnf removals

instance Data.ToJSON ProposalActions where
  toJSON ProposalActions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Invitations" Data..=) Prelude.<$> invitations,
            ("Removals" Data..=) Prelude.<$> removals
          ]
      )
