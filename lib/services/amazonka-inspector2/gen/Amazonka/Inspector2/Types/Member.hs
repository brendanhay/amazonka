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
-- Module      : Amazonka.Inspector2.Types.Member
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Member where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.RelationshipStatus
import qualified Amazonka.Prelude as Prelude

-- | Details on a member account in your organization.
--
-- /See:/ 'newMember' smart constructor.
data Member = Member'
  { -- | The Amazon Web Services account ID of the member account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the Amazon Inspector delegated
    -- administrator for this member account.
    delegatedAdminAccountId :: Prelude.Maybe Prelude.Text,
    -- | The status of the member account.
    relationshipStatus :: Prelude.Maybe RelationshipStatus,
    -- | A timestamp showing when the status of this member was last updated.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Member' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'member_accountId' - The Amazon Web Services account ID of the member account.
--
-- 'delegatedAdminAccountId', 'member_delegatedAdminAccountId' - The Amazon Web Services account ID of the Amazon Inspector delegated
-- administrator for this member account.
--
-- 'relationshipStatus', 'member_relationshipStatus' - The status of the member account.
--
-- 'updatedAt', 'member_updatedAt' - A timestamp showing when the status of this member was last updated.
newMember ::
  Member
newMember =
  Member'
    { accountId = Prelude.Nothing,
      delegatedAdminAccountId = Prelude.Nothing,
      relationshipStatus = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The Amazon Web Services account ID of the member account.
member_accountId :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_accountId = Lens.lens (\Member' {accountId} -> accountId) (\s@Member' {} a -> s {accountId = a} :: Member)

-- | The Amazon Web Services account ID of the Amazon Inspector delegated
-- administrator for this member account.
member_delegatedAdminAccountId :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_delegatedAdminAccountId = Lens.lens (\Member' {delegatedAdminAccountId} -> delegatedAdminAccountId) (\s@Member' {} a -> s {delegatedAdminAccountId = a} :: Member)

-- | The status of the member account.
member_relationshipStatus :: Lens.Lens' Member (Prelude.Maybe RelationshipStatus)
member_relationshipStatus = Lens.lens (\Member' {relationshipStatus} -> relationshipStatus) (\s@Member' {} a -> s {relationshipStatus = a} :: Member)

-- | A timestamp showing when the status of this member was last updated.
member_updatedAt :: Lens.Lens' Member (Prelude.Maybe Prelude.UTCTime)
member_updatedAt = Lens.lens (\Member' {updatedAt} -> updatedAt) (\s@Member' {} a -> s {updatedAt = a} :: Member) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Member where
  parseJSON =
    Data.withObject
      "Member"
      ( \x ->
          Member'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "delegatedAdminAccountId")
            Prelude.<*> (x Data..:? "relationshipStatus")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable Member where
  hashWithSalt _salt Member' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` delegatedAdminAccountId
      `Prelude.hashWithSalt` relationshipStatus
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Member where
  rnf Member' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf delegatedAdminAccountId
      `Prelude.seq` Prelude.rnf relationshipStatus
      `Prelude.seq` Prelude.rnf updatedAt
