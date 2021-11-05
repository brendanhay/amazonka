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
-- Module      : Amazonka.GuardDuty.Types.Invitation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.Invitation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the invitation to become a member account.
--
-- /See:/ 'newInvitation' smart constructor.
data Invitation = Invitation'
  { -- | The timestamp when the invitation was sent.
    invitedAt :: Prelude.Maybe Prelude.Text,
    -- | The status of the relationship between the inviter and invitee accounts.
    relationshipStatus :: Prelude.Maybe Prelude.Text,
    -- | The ID of the invitation. This value is used to validate the inviter
    -- account to the member account.
    invitationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the account that the invitation was sent from.
    accountId :: Prelude.Maybe Prelude.Text
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
-- 'invitedAt', 'invitation_invitedAt' - The timestamp when the invitation was sent.
--
-- 'relationshipStatus', 'invitation_relationshipStatus' - The status of the relationship between the inviter and invitee accounts.
--
-- 'invitationId', 'invitation_invitationId' - The ID of the invitation. This value is used to validate the inviter
-- account to the member account.
--
-- 'accountId', 'invitation_accountId' - The ID of the account that the invitation was sent from.
newInvitation ::
  Invitation
newInvitation =
  Invitation'
    { invitedAt = Prelude.Nothing,
      relationshipStatus = Prelude.Nothing,
      invitationId = Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | The timestamp when the invitation was sent.
invitation_invitedAt :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_invitedAt = Lens.lens (\Invitation' {invitedAt} -> invitedAt) (\s@Invitation' {} a -> s {invitedAt = a} :: Invitation)

-- | The status of the relationship between the inviter and invitee accounts.
invitation_relationshipStatus :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_relationshipStatus = Lens.lens (\Invitation' {relationshipStatus} -> relationshipStatus) (\s@Invitation' {} a -> s {relationshipStatus = a} :: Invitation)

-- | The ID of the invitation. This value is used to validate the inviter
-- account to the member account.
invitation_invitationId :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_invitationId = Lens.lens (\Invitation' {invitationId} -> invitationId) (\s@Invitation' {} a -> s {invitationId = a} :: Invitation)

-- | The ID of the account that the invitation was sent from.
invitation_accountId :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_accountId = Lens.lens (\Invitation' {accountId} -> accountId) (\s@Invitation' {} a -> s {accountId = a} :: Invitation)

instance Core.FromJSON Invitation where
  parseJSON =
    Core.withObject
      "Invitation"
      ( \x ->
          Invitation'
            Prelude.<$> (x Core..:? "invitedAt")
            Prelude.<*> (x Core..:? "relationshipStatus")
            Prelude.<*> (x Core..:? "invitationId")
            Prelude.<*> (x Core..:? "accountId")
      )

instance Prelude.Hashable Invitation

instance Prelude.NFData Invitation
