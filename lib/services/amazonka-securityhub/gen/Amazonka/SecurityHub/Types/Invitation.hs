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
-- Module      : Amazonka.SecurityHub.Types.Invitation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Invitation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about an invitation.
--
-- /See:/ 'newInvitation' smart constructor.
data Invitation = Invitation'
  { -- | The account ID of the Security Hub administrator account that the
    -- invitation was sent from.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the invitation sent to the member account.
    invitationId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the invitation was sent.
    invitedAt :: Prelude.Maybe Data.ISO8601,
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
-- 'accountId', 'invitation_accountId' - The account ID of the Security Hub administrator account that the
-- invitation was sent from.
--
-- 'invitationId', 'invitation_invitationId' - The ID of the invitation sent to the member account.
--
-- 'invitedAt', 'invitation_invitedAt' - The timestamp of when the invitation was sent.
--
-- 'memberStatus', 'invitation_memberStatus' - The current status of the association between the member and
-- administrator accounts.
newInvitation ::
  Invitation
newInvitation =
  Invitation'
    { accountId = Prelude.Nothing,
      invitationId = Prelude.Nothing,
      invitedAt = Prelude.Nothing,
      memberStatus = Prelude.Nothing
    }

-- | The account ID of the Security Hub administrator account that the
-- invitation was sent from.
invitation_accountId :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_accountId = Lens.lens (\Invitation' {accountId} -> accountId) (\s@Invitation' {} a -> s {accountId = a} :: Invitation)

-- | The ID of the invitation sent to the member account.
invitation_invitationId :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_invitationId = Lens.lens (\Invitation' {invitationId} -> invitationId) (\s@Invitation' {} a -> s {invitationId = a} :: Invitation)

-- | The timestamp of when the invitation was sent.
invitation_invitedAt :: Lens.Lens' Invitation (Prelude.Maybe Prelude.UTCTime)
invitation_invitedAt = Lens.lens (\Invitation' {invitedAt} -> invitedAt) (\s@Invitation' {} a -> s {invitedAt = a} :: Invitation) Prelude.. Lens.mapping Data._Time

-- | The current status of the association between the member and
-- administrator accounts.
invitation_memberStatus :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_memberStatus = Lens.lens (\Invitation' {memberStatus} -> memberStatus) (\s@Invitation' {} a -> s {memberStatus = a} :: Invitation)

instance Data.FromJSON Invitation where
  parseJSON =
    Data.withObject
      "Invitation"
      ( \x ->
          Invitation'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "InvitationId")
            Prelude.<*> (x Data..:? "InvitedAt")
            Prelude.<*> (x Data..:? "MemberStatus")
      )

instance Prelude.Hashable Invitation where
  hashWithSalt _salt Invitation' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` invitationId
      `Prelude.hashWithSalt` invitedAt
      `Prelude.hashWithSalt` memberStatus

instance Prelude.NFData Invitation where
  rnf Invitation' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf invitationId `Prelude.seq`
        Prelude.rnf invitedAt `Prelude.seq`
          Prelude.rnf memberStatus
