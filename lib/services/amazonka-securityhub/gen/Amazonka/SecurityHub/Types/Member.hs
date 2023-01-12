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
-- Module      : Amazonka.SecurityHub.Types.Member
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Member where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details about a member account.
--
-- /See:/ 'newMember' smart constructor.
data Member = Member'
  { -- | The Amazon Web Services account ID of the member account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the Security Hub administrator
    -- account associated with this member account.
    administratorId :: Prelude.Maybe Prelude.Text,
    -- | The email address of the member account.
    email :: Prelude.Maybe Prelude.Text,
    -- | A timestamp for the date and time when the invitation was sent to the
    -- member account.
    invitedAt :: Prelude.Maybe Data.ISO8601,
    -- | This is replaced by @AdministratorID@.
    --
    -- The Amazon Web Services account ID of the Security Hub administrator
    -- account associated with this member account.
    masterId :: Prelude.Maybe Prelude.Text,
    -- | The status of the relationship between the member account and its
    -- administrator account.
    --
    -- The status can have one of the following values:
    --
    -- -   @CREATED@ - Indicates that the administrator account added the
    --     member account, but has not yet invited the member account.
    --
    -- -   @INVITED@ - Indicates that the administrator account invited the
    --     member account. The member account has not yet responded to the
    --     invitation.
    --
    -- -   @ENABLED@ - Indicates that the member account is currently active.
    --     For manually invited member accounts, indicates that the member
    --     account accepted the invitation.
    --
    -- -   @REMOVED@ - Indicates that the administrator account disassociated
    --     the member account.
    --
    -- -   @RESIGNED@ - Indicates that the member account disassociated
    --     themselves from the administrator account.
    --
    -- -   @DELETED@ - Indicates that the administrator account deleted the
    --     member account.
    --
    -- -   @ACCOUNT_SUSPENDED@ - Indicates that an organization account was
    --     suspended from Amazon Web Services at the same time that the
    --     administrator account tried to enable the organization account as a
    --     member account.
    memberStatus :: Prelude.Maybe Prelude.Text,
    -- | The timestamp for the date and time when the member account was updated.
    updatedAt :: Prelude.Maybe Data.ISO8601
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
-- 'administratorId', 'member_administratorId' - The Amazon Web Services account ID of the Security Hub administrator
-- account associated with this member account.
--
-- 'email', 'member_email' - The email address of the member account.
--
-- 'invitedAt', 'member_invitedAt' - A timestamp for the date and time when the invitation was sent to the
-- member account.
--
-- 'masterId', 'member_masterId' - This is replaced by @AdministratorID@.
--
-- The Amazon Web Services account ID of the Security Hub administrator
-- account associated with this member account.
--
-- 'memberStatus', 'member_memberStatus' - The status of the relationship between the member account and its
-- administrator account.
--
-- The status can have one of the following values:
--
-- -   @CREATED@ - Indicates that the administrator account added the
--     member account, but has not yet invited the member account.
--
-- -   @INVITED@ - Indicates that the administrator account invited the
--     member account. The member account has not yet responded to the
--     invitation.
--
-- -   @ENABLED@ - Indicates that the member account is currently active.
--     For manually invited member accounts, indicates that the member
--     account accepted the invitation.
--
-- -   @REMOVED@ - Indicates that the administrator account disassociated
--     the member account.
--
-- -   @RESIGNED@ - Indicates that the member account disassociated
--     themselves from the administrator account.
--
-- -   @DELETED@ - Indicates that the administrator account deleted the
--     member account.
--
-- -   @ACCOUNT_SUSPENDED@ - Indicates that an organization account was
--     suspended from Amazon Web Services at the same time that the
--     administrator account tried to enable the organization account as a
--     member account.
--
-- 'updatedAt', 'member_updatedAt' - The timestamp for the date and time when the member account was updated.
newMember ::
  Member
newMember =
  Member'
    { accountId = Prelude.Nothing,
      administratorId = Prelude.Nothing,
      email = Prelude.Nothing,
      invitedAt = Prelude.Nothing,
      masterId = Prelude.Nothing,
      memberStatus = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The Amazon Web Services account ID of the member account.
member_accountId :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_accountId = Lens.lens (\Member' {accountId} -> accountId) (\s@Member' {} a -> s {accountId = a} :: Member)

-- | The Amazon Web Services account ID of the Security Hub administrator
-- account associated with this member account.
member_administratorId :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_administratorId = Lens.lens (\Member' {administratorId} -> administratorId) (\s@Member' {} a -> s {administratorId = a} :: Member)

-- | The email address of the member account.
member_email :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_email = Lens.lens (\Member' {email} -> email) (\s@Member' {} a -> s {email = a} :: Member)

-- | A timestamp for the date and time when the invitation was sent to the
-- member account.
member_invitedAt :: Lens.Lens' Member (Prelude.Maybe Prelude.UTCTime)
member_invitedAt = Lens.lens (\Member' {invitedAt} -> invitedAt) (\s@Member' {} a -> s {invitedAt = a} :: Member) Prelude.. Lens.mapping Data._Time

-- | This is replaced by @AdministratorID@.
--
-- The Amazon Web Services account ID of the Security Hub administrator
-- account associated with this member account.
member_masterId :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_masterId = Lens.lens (\Member' {masterId} -> masterId) (\s@Member' {} a -> s {masterId = a} :: Member)

-- | The status of the relationship between the member account and its
-- administrator account.
--
-- The status can have one of the following values:
--
-- -   @CREATED@ - Indicates that the administrator account added the
--     member account, but has not yet invited the member account.
--
-- -   @INVITED@ - Indicates that the administrator account invited the
--     member account. The member account has not yet responded to the
--     invitation.
--
-- -   @ENABLED@ - Indicates that the member account is currently active.
--     For manually invited member accounts, indicates that the member
--     account accepted the invitation.
--
-- -   @REMOVED@ - Indicates that the administrator account disassociated
--     the member account.
--
-- -   @RESIGNED@ - Indicates that the member account disassociated
--     themselves from the administrator account.
--
-- -   @DELETED@ - Indicates that the administrator account deleted the
--     member account.
--
-- -   @ACCOUNT_SUSPENDED@ - Indicates that an organization account was
--     suspended from Amazon Web Services at the same time that the
--     administrator account tried to enable the organization account as a
--     member account.
member_memberStatus :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_memberStatus = Lens.lens (\Member' {memberStatus} -> memberStatus) (\s@Member' {} a -> s {memberStatus = a} :: Member)

-- | The timestamp for the date and time when the member account was updated.
member_updatedAt :: Lens.Lens' Member (Prelude.Maybe Prelude.UTCTime)
member_updatedAt = Lens.lens (\Member' {updatedAt} -> updatedAt) (\s@Member' {} a -> s {updatedAt = a} :: Member) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Member where
  parseJSON =
    Data.withObject
      "Member"
      ( \x ->
          Member'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "AdministratorId")
            Prelude.<*> (x Data..:? "Email")
            Prelude.<*> (x Data..:? "InvitedAt")
            Prelude.<*> (x Data..:? "MasterId")
            Prelude.<*> (x Data..:? "MemberStatus")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable Member where
  hashWithSalt _salt Member' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` administratorId
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` invitedAt
      `Prelude.hashWithSalt` masterId
      `Prelude.hashWithSalt` memberStatus
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Member where
  rnf Member' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf administratorId
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf invitedAt
      `Prelude.seq` Prelude.rnf masterId
      `Prelude.seq` Prelude.rnf memberStatus
      `Prelude.seq` Prelude.rnf updatedAt
