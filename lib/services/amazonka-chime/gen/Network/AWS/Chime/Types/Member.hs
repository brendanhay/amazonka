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
-- Module      : Network.AWS.Chime.Types.Member
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Chime.Types.Member where

import Network.AWS.Chime.Types.MemberType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The member details, such as email address, name, member ID, and member
-- type.
--
-- /See:/ 'newMember' smart constructor.
data Member = Member'
  { -- | The member name.
    fullName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The member email address.
    email :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The member ID (user ID or bot ID).
    memberId :: Prelude.Maybe Prelude.Text,
    -- | The member type.
    memberType :: Prelude.Maybe MemberType,
    -- | The Amazon Chime account ID.
    accountId :: Prelude.Maybe Prelude.Text
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
-- 'fullName', 'member_fullName' - The member name.
--
-- 'email', 'member_email' - The member email address.
--
-- 'memberId', 'member_memberId' - The member ID (user ID or bot ID).
--
-- 'memberType', 'member_memberType' - The member type.
--
-- 'accountId', 'member_accountId' - The Amazon Chime account ID.
newMember ::
  Member
newMember =
  Member'
    { fullName = Prelude.Nothing,
      email = Prelude.Nothing,
      memberId = Prelude.Nothing,
      memberType = Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | The member name.
member_fullName :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_fullName = Lens.lens (\Member' {fullName} -> fullName) (\s@Member' {} a -> s {fullName = a} :: Member) Prelude.. Lens.mapping Core._Sensitive

-- | The member email address.
member_email :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_email = Lens.lens (\Member' {email} -> email) (\s@Member' {} a -> s {email = a} :: Member) Prelude.. Lens.mapping Core._Sensitive

-- | The member ID (user ID or bot ID).
member_memberId :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_memberId = Lens.lens (\Member' {memberId} -> memberId) (\s@Member' {} a -> s {memberId = a} :: Member)

-- | The member type.
member_memberType :: Lens.Lens' Member (Prelude.Maybe MemberType)
member_memberType = Lens.lens (\Member' {memberType} -> memberType) (\s@Member' {} a -> s {memberType = a} :: Member)

-- | The Amazon Chime account ID.
member_accountId :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_accountId = Lens.lens (\Member' {accountId} -> accountId) (\s@Member' {} a -> s {accountId = a} :: Member)

instance Core.FromJSON Member where
  parseJSON =
    Core.withObject
      "Member"
      ( \x ->
          Member'
            Prelude.<$> (x Core..:? "FullName")
            Prelude.<*> (x Core..:? "Email")
            Prelude.<*> (x Core..:? "MemberId")
            Prelude.<*> (x Core..:? "MemberType")
            Prelude.<*> (x Core..:? "AccountId")
      )

instance Prelude.Hashable Member

instance Prelude.NFData Member
