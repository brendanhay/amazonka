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
-- Module      : Amazonka.ManagedBlockChain.Types.VoteSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.VoteSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types.VoteValue
import qualified Amazonka.Prelude as Prelude

-- | Properties of an individual vote that a member cast for a proposal.
--
-- Applies only to Hyperledger Fabric.
--
-- /See:/ 'newVoteSummary' smart constructor.
data VoteSummary = VoteSummary'
  { -- | The unique identifier of the member that cast the vote.
    memberId :: Prelude.Maybe Prelude.Text,
    -- | The name of the member that cast the vote.
    memberName :: Prelude.Maybe Prelude.Text,
    -- | The vote value, either @YES@ or @NO@.
    vote :: Prelude.Maybe VoteValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoteSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberId', 'voteSummary_memberId' - The unique identifier of the member that cast the vote.
--
-- 'memberName', 'voteSummary_memberName' - The name of the member that cast the vote.
--
-- 'vote', 'voteSummary_vote' - The vote value, either @YES@ or @NO@.
newVoteSummary ::
  VoteSummary
newVoteSummary =
  VoteSummary'
    { memberId = Prelude.Nothing,
      memberName = Prelude.Nothing,
      vote = Prelude.Nothing
    }

-- | The unique identifier of the member that cast the vote.
voteSummary_memberId :: Lens.Lens' VoteSummary (Prelude.Maybe Prelude.Text)
voteSummary_memberId = Lens.lens (\VoteSummary' {memberId} -> memberId) (\s@VoteSummary' {} a -> s {memberId = a} :: VoteSummary)

-- | The name of the member that cast the vote.
voteSummary_memberName :: Lens.Lens' VoteSummary (Prelude.Maybe Prelude.Text)
voteSummary_memberName = Lens.lens (\VoteSummary' {memberName} -> memberName) (\s@VoteSummary' {} a -> s {memberName = a} :: VoteSummary)

-- | The vote value, either @YES@ or @NO@.
voteSummary_vote :: Lens.Lens' VoteSummary (Prelude.Maybe VoteValue)
voteSummary_vote = Lens.lens (\VoteSummary' {vote} -> vote) (\s@VoteSummary' {} a -> s {vote = a} :: VoteSummary)

instance Data.FromJSON VoteSummary where
  parseJSON =
    Data.withObject
      "VoteSummary"
      ( \x ->
          VoteSummary'
            Prelude.<$> (x Data..:? "MemberId")
            Prelude.<*> (x Data..:? "MemberName")
            Prelude.<*> (x Data..:? "Vote")
      )

instance Prelude.Hashable VoteSummary where
  hashWithSalt _salt VoteSummary' {..} =
    _salt `Prelude.hashWithSalt` memberId
      `Prelude.hashWithSalt` memberName
      `Prelude.hashWithSalt` vote

instance Prelude.NFData VoteSummary where
  rnf VoteSummary' {..} =
    Prelude.rnf memberId
      `Prelude.seq` Prelude.rnf memberName
      `Prelude.seq` Prelude.rnf vote
