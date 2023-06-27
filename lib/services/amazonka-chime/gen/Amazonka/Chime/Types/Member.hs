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
-- Module      : Amazonka.Chime.Types.Member
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.Member where

import Amazonka.Chime.Types.MemberType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The member details, such as email address, name, member ID, and member
-- type.
--
-- /See:/ 'newMember' smart constructor.
data Member = Member'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The member email address.
    email :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The member name.
    fullName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The member ID (user ID or bot ID).
    memberId :: Prelude.Maybe Prelude.Text,
    -- | The member type.
    memberType :: Prelude.Maybe MemberType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Member' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'member_accountId' - The Amazon Chime account ID.
--
-- 'email', 'member_email' - The member email address.
--
-- 'fullName', 'member_fullName' - The member name.
--
-- 'memberId', 'member_memberId' - The member ID (user ID or bot ID).
--
-- 'memberType', 'member_memberType' - The member type.
newMember ::
  Member
newMember =
  Member'
    { accountId = Prelude.Nothing,
      email = Prelude.Nothing,
      fullName = Prelude.Nothing,
      memberId = Prelude.Nothing,
      memberType = Prelude.Nothing
    }

-- | The Amazon Chime account ID.
member_accountId :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_accountId = Lens.lens (\Member' {accountId} -> accountId) (\s@Member' {} a -> s {accountId = a} :: Member)

-- | The member email address.
member_email :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_email = Lens.lens (\Member' {email} -> email) (\s@Member' {} a -> s {email = a} :: Member) Prelude.. Lens.mapping Data._Sensitive

-- | The member name.
member_fullName :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_fullName = Lens.lens (\Member' {fullName} -> fullName) (\s@Member' {} a -> s {fullName = a} :: Member) Prelude.. Lens.mapping Data._Sensitive

-- | The member ID (user ID or bot ID).
member_memberId :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_memberId = Lens.lens (\Member' {memberId} -> memberId) (\s@Member' {} a -> s {memberId = a} :: Member)

-- | The member type.
member_memberType :: Lens.Lens' Member (Prelude.Maybe MemberType)
member_memberType = Lens.lens (\Member' {memberType} -> memberType) (\s@Member' {} a -> s {memberType = a} :: Member)

instance Data.FromJSON Member where
  parseJSON =
    Data.withObject
      "Member"
      ( \x ->
          Member'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "Email")
            Prelude.<*> (x Data..:? "FullName")
            Prelude.<*> (x Data..:? "MemberId")
            Prelude.<*> (x Data..:? "MemberType")
      )

instance Prelude.Hashable Member where
  hashWithSalt _salt Member' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` fullName
      `Prelude.hashWithSalt` memberId
      `Prelude.hashWithSalt` memberType

instance Prelude.NFData Member where
  rnf Member' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf fullName
      `Prelude.seq` Prelude.rnf memberId
      `Prelude.seq` Prelude.rnf memberType
