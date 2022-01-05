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
-- Module      : Amazonka.MacieV2.Types.Invitation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.Invitation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MacieV2.Types.RelationshipStatus
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an Amazon Macie membership invitation that
-- was received by an account.
--
-- /See:/ 'newInvitation' smart constructor.
data Invitation = Invitation'
  { -- | The status of the relationship between the account that sent the
    -- invitation (/inviter account/) and the account that received the
    -- invitation (/invitee account/).
    relationshipStatus :: Prelude.Maybe RelationshipStatus,
    -- | The date and time, in UTC and extended ISO 8601 format, when the
    -- invitation was sent.
    invitedAt :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier for the invitation. Amazon Macie uses this
    -- identifier to validate the inviter account with the invitee account.
    invitationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID for the account that sent the
    -- invitation.
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
-- 'relationshipStatus', 'invitation_relationshipStatus' - The status of the relationship between the account that sent the
-- invitation (/inviter account/) and the account that received the
-- invitation (/invitee account/).
--
-- 'invitedAt', 'invitation_invitedAt' - The date and time, in UTC and extended ISO 8601 format, when the
-- invitation was sent.
--
-- 'invitationId', 'invitation_invitationId' - The unique identifier for the invitation. Amazon Macie uses this
-- identifier to validate the inviter account with the invitee account.
--
-- 'accountId', 'invitation_accountId' - The Amazon Web Services account ID for the account that sent the
-- invitation.
newInvitation ::
  Invitation
newInvitation =
  Invitation'
    { relationshipStatus = Prelude.Nothing,
      invitedAt = Prelude.Nothing,
      invitationId = Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | The status of the relationship between the account that sent the
-- invitation (/inviter account/) and the account that received the
-- invitation (/invitee account/).
invitation_relationshipStatus :: Lens.Lens' Invitation (Prelude.Maybe RelationshipStatus)
invitation_relationshipStatus = Lens.lens (\Invitation' {relationshipStatus} -> relationshipStatus) (\s@Invitation' {} a -> s {relationshipStatus = a} :: Invitation)

-- | The date and time, in UTC and extended ISO 8601 format, when the
-- invitation was sent.
invitation_invitedAt :: Lens.Lens' Invitation (Prelude.Maybe Prelude.UTCTime)
invitation_invitedAt = Lens.lens (\Invitation' {invitedAt} -> invitedAt) (\s@Invitation' {} a -> s {invitedAt = a} :: Invitation) Prelude.. Lens.mapping Core._Time

-- | The unique identifier for the invitation. Amazon Macie uses this
-- identifier to validate the inviter account with the invitee account.
invitation_invitationId :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_invitationId = Lens.lens (\Invitation' {invitationId} -> invitationId) (\s@Invitation' {} a -> s {invitationId = a} :: Invitation)

-- | The Amazon Web Services account ID for the account that sent the
-- invitation.
invitation_accountId :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_accountId = Lens.lens (\Invitation' {accountId} -> accountId) (\s@Invitation' {} a -> s {accountId = a} :: Invitation)

instance Core.FromJSON Invitation where
  parseJSON =
    Core.withObject
      "Invitation"
      ( \x ->
          Invitation'
            Prelude.<$> (x Core..:? "relationshipStatus")
            Prelude.<*> (x Core..:? "invitedAt")
            Prelude.<*> (x Core..:? "invitationId")
            Prelude.<*> (x Core..:? "accountId")
      )

instance Prelude.Hashable Invitation where
  hashWithSalt _salt Invitation' {..} =
    _salt `Prelude.hashWithSalt` relationshipStatus
      `Prelude.hashWithSalt` invitedAt
      `Prelude.hashWithSalt` invitationId
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData Invitation where
  rnf Invitation' {..} =
    Prelude.rnf relationshipStatus
      `Prelude.seq` Prelude.rnf invitedAt
      `Prelude.seq` Prelude.rnf invitationId
      `Prelude.seq` Prelude.rnf accountId
