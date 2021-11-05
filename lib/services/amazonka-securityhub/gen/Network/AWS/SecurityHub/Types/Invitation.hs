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
-- Module      : Network.AWS.SecurityHub.Types.Invitation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.Invitation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about an invitation.
--
-- /See:/ 'newInvitation' smart constructor.
data Invitation = Invitation'
  { -- | The timestamp of when the invitation was sent.
    invitedAt :: Prelude.Maybe Core.POSIX,
    -- | The ID of the invitation sent to the member account.
    invitationId :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the Security Hub administrator account that the
    -- invitation was sent from.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the association between the member and
    -- administrator accounts.
    memberStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Invitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invitedAt', 'invitation_invitedAt' - The timestamp of when the invitation was sent.
--
-- 'invitationId', 'invitation_invitationId' - The ID of the invitation sent to the member account.
--
-- 'accountId', 'invitation_accountId' - The account ID of the Security Hub administrator account that the
-- invitation was sent from.
--
-- 'memberStatus', 'invitation_memberStatus' - The current status of the association between the member and
-- administrator accounts.
newInvitation ::
  Invitation
newInvitation =
  Invitation'
    { invitedAt = Prelude.Nothing,
      invitationId = Prelude.Nothing,
      accountId = Prelude.Nothing,
      memberStatus = Prelude.Nothing
    }

-- | The timestamp of when the invitation was sent.
invitation_invitedAt :: Lens.Lens' Invitation (Prelude.Maybe Prelude.UTCTime)
invitation_invitedAt = Lens.lens (\Invitation' {invitedAt} -> invitedAt) (\s@Invitation' {} a -> s {invitedAt = a} :: Invitation) Prelude.. Lens.mapping Core._Time

-- | The ID of the invitation sent to the member account.
invitation_invitationId :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_invitationId = Lens.lens (\Invitation' {invitationId} -> invitationId) (\s@Invitation' {} a -> s {invitationId = a} :: Invitation)

-- | The account ID of the Security Hub administrator account that the
-- invitation was sent from.
invitation_accountId :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_accountId = Lens.lens (\Invitation' {accountId} -> accountId) (\s@Invitation' {} a -> s {accountId = a} :: Invitation)

-- | The current status of the association between the member and
-- administrator accounts.
invitation_memberStatus :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_memberStatus = Lens.lens (\Invitation' {memberStatus} -> memberStatus) (\s@Invitation' {} a -> s {memberStatus = a} :: Invitation)

instance Core.FromJSON Invitation where
  parseJSON =
    Core.withObject
      "Invitation"
      ( \x ->
          Invitation'
            Prelude.<$> (x Core..:? "InvitedAt")
            Prelude.<*> (x Core..:? "InvitationId")
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "MemberStatus")
      )

instance Prelude.Hashable Invitation

instance Prelude.NFData Invitation
