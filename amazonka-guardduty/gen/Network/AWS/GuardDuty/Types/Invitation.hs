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
-- Module      : Network.AWS.GuardDuty.Types.Invitation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Invitation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the invitation to become a member account.
--
-- /See:/ 'newInvitation' smart constructor.
data Invitation = Invitation'
  { -- | The ID of the account that the invitation was sent from.
    accountId :: Core.Maybe Core.Text,
    -- | The status of the relationship between the inviter and invitee accounts.
    relationshipStatus :: Core.Maybe Core.Text,
    -- | The ID of the invitation. This value is used to validate the inviter
    -- account to the member account.
    invitationId :: Core.Maybe Core.Text,
    -- | The timestamp when the invitation was sent.
    invitedAt :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Invitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'invitation_accountId' - The ID of the account that the invitation was sent from.
--
-- 'relationshipStatus', 'invitation_relationshipStatus' - The status of the relationship between the inviter and invitee accounts.
--
-- 'invitationId', 'invitation_invitationId' - The ID of the invitation. This value is used to validate the inviter
-- account to the member account.
--
-- 'invitedAt', 'invitation_invitedAt' - The timestamp when the invitation was sent.
newInvitation ::
  Invitation
newInvitation =
  Invitation'
    { accountId = Core.Nothing,
      relationshipStatus = Core.Nothing,
      invitationId = Core.Nothing,
      invitedAt = Core.Nothing
    }

-- | The ID of the account that the invitation was sent from.
invitation_accountId :: Lens.Lens' Invitation (Core.Maybe Core.Text)
invitation_accountId = Lens.lens (\Invitation' {accountId} -> accountId) (\s@Invitation' {} a -> s {accountId = a} :: Invitation)

-- | The status of the relationship between the inviter and invitee accounts.
invitation_relationshipStatus :: Lens.Lens' Invitation (Core.Maybe Core.Text)
invitation_relationshipStatus = Lens.lens (\Invitation' {relationshipStatus} -> relationshipStatus) (\s@Invitation' {} a -> s {relationshipStatus = a} :: Invitation)

-- | The ID of the invitation. This value is used to validate the inviter
-- account to the member account.
invitation_invitationId :: Lens.Lens' Invitation (Core.Maybe Core.Text)
invitation_invitationId = Lens.lens (\Invitation' {invitationId} -> invitationId) (\s@Invitation' {} a -> s {invitationId = a} :: Invitation)

-- | The timestamp when the invitation was sent.
invitation_invitedAt :: Lens.Lens' Invitation (Core.Maybe Core.Text)
invitation_invitedAt = Lens.lens (\Invitation' {invitedAt} -> invitedAt) (\s@Invitation' {} a -> s {invitedAt = a} :: Invitation)

instance Core.FromJSON Invitation where
  parseJSON =
    Core.withObject
      "Invitation"
      ( \x ->
          Invitation'
            Core.<$> (x Core..:? "accountId")
            Core.<*> (x Core..:? "relationshipStatus")
            Core.<*> (x Core..:? "invitationId")
            Core.<*> (x Core..:? "invitedAt")
      )

instance Core.Hashable Invitation

instance Core.NFData Invitation
