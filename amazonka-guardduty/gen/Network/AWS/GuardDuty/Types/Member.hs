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
-- Module      : Network.AWS.GuardDuty.Types.Member
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Member where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the member account.
--
-- /See:/ 'newMember' smart constructor.
data Member = Member'
  { -- | The detector ID of the member account.
    detectorId :: Core.Maybe Core.Text,
    -- | The timestamp when the invitation was sent.
    invitedAt :: Core.Maybe Core.Text,
    -- | The ID of the member account.
    accountId :: Core.Text,
    -- | The administrator account ID.
    masterId :: Core.Text,
    -- | The email address of the member account.
    email :: Core.Text,
    -- | The status of the relationship between the member and the administrator.
    relationshipStatus :: Core.Text,
    -- | The last-updated timestamp of the member.
    updatedAt :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Member' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'member_detectorId' - The detector ID of the member account.
--
-- 'invitedAt', 'member_invitedAt' - The timestamp when the invitation was sent.
--
-- 'accountId', 'member_accountId' - The ID of the member account.
--
-- 'masterId', 'member_masterId' - The administrator account ID.
--
-- 'email', 'member_email' - The email address of the member account.
--
-- 'relationshipStatus', 'member_relationshipStatus' - The status of the relationship between the member and the administrator.
--
-- 'updatedAt', 'member_updatedAt' - The last-updated timestamp of the member.
newMember ::
  -- | 'accountId'
  Core.Text ->
  -- | 'masterId'
  Core.Text ->
  -- | 'email'
  Core.Text ->
  -- | 'relationshipStatus'
  Core.Text ->
  -- | 'updatedAt'
  Core.Text ->
  Member
newMember
  pAccountId_
  pMasterId_
  pEmail_
  pRelationshipStatus_
  pUpdatedAt_ =
    Member'
      { detectorId = Core.Nothing,
        invitedAt = Core.Nothing,
        accountId = pAccountId_,
        masterId = pMasterId_,
        email = pEmail_,
        relationshipStatus = pRelationshipStatus_,
        updatedAt = pUpdatedAt_
      }

-- | The detector ID of the member account.
member_detectorId :: Lens.Lens' Member (Core.Maybe Core.Text)
member_detectorId = Lens.lens (\Member' {detectorId} -> detectorId) (\s@Member' {} a -> s {detectorId = a} :: Member)

-- | The timestamp when the invitation was sent.
member_invitedAt :: Lens.Lens' Member (Core.Maybe Core.Text)
member_invitedAt = Lens.lens (\Member' {invitedAt} -> invitedAt) (\s@Member' {} a -> s {invitedAt = a} :: Member)

-- | The ID of the member account.
member_accountId :: Lens.Lens' Member Core.Text
member_accountId = Lens.lens (\Member' {accountId} -> accountId) (\s@Member' {} a -> s {accountId = a} :: Member)

-- | The administrator account ID.
member_masterId :: Lens.Lens' Member Core.Text
member_masterId = Lens.lens (\Member' {masterId} -> masterId) (\s@Member' {} a -> s {masterId = a} :: Member)

-- | The email address of the member account.
member_email :: Lens.Lens' Member Core.Text
member_email = Lens.lens (\Member' {email} -> email) (\s@Member' {} a -> s {email = a} :: Member)

-- | The status of the relationship between the member and the administrator.
member_relationshipStatus :: Lens.Lens' Member Core.Text
member_relationshipStatus = Lens.lens (\Member' {relationshipStatus} -> relationshipStatus) (\s@Member' {} a -> s {relationshipStatus = a} :: Member)

-- | The last-updated timestamp of the member.
member_updatedAt :: Lens.Lens' Member Core.Text
member_updatedAt = Lens.lens (\Member' {updatedAt} -> updatedAt) (\s@Member' {} a -> s {updatedAt = a} :: Member)

instance Core.FromJSON Member where
  parseJSON =
    Core.withObject
      "Member"
      ( \x ->
          Member'
            Core.<$> (x Core..:? "detectorId")
            Core.<*> (x Core..:? "invitedAt")
            Core.<*> (x Core..: "accountId")
            Core.<*> (x Core..: "masterId")
            Core.<*> (x Core..: "email")
            Core.<*> (x Core..: "relationshipStatus")
            Core.<*> (x Core..: "updatedAt")
      )

instance Core.Hashable Member

instance Core.NFData Member
