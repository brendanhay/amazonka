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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.Invitation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types.RelationshipStatus
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an Amazon Macie membership invitation.
--
-- /See:/ 'newInvitation' smart constructor.
data Invitation = Invitation'
  { -- | The Amazon Web Services account ID for the account that sent the
    -- invitation.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in UTC and extended ISO 8601 format, when the
    -- invitation was sent.
    invitedAt :: Prelude.Maybe Core.POSIX,
    -- | The status of the relationship between the account that sent the
    -- invitation and the account that received the invitation.
    relationshipStatus :: Prelude.Maybe RelationshipStatus,
    -- | The unique identifier for the invitation.
    invitationId :: Prelude.Maybe Prelude.Text
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
-- 'accountId', 'invitation_accountId' - The Amazon Web Services account ID for the account that sent the
-- invitation.
--
-- 'invitedAt', 'invitation_invitedAt' - The date and time, in UTC and extended ISO 8601 format, when the
-- invitation was sent.
--
-- 'relationshipStatus', 'invitation_relationshipStatus' - The status of the relationship between the account that sent the
-- invitation and the account that received the invitation.
--
-- 'invitationId', 'invitation_invitationId' - The unique identifier for the invitation.
newInvitation ::
  Invitation
newInvitation =
  Invitation'
    { accountId = Prelude.Nothing,
      invitedAt = Prelude.Nothing,
      relationshipStatus = Prelude.Nothing,
      invitationId = Prelude.Nothing
    }

-- | The Amazon Web Services account ID for the account that sent the
-- invitation.
invitation_accountId :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_accountId = Lens.lens (\Invitation' {accountId} -> accountId) (\s@Invitation' {} a -> s {accountId = a} :: Invitation)

-- | The date and time, in UTC and extended ISO 8601 format, when the
-- invitation was sent.
invitation_invitedAt :: Lens.Lens' Invitation (Prelude.Maybe Prelude.UTCTime)
invitation_invitedAt = Lens.lens (\Invitation' {invitedAt} -> invitedAt) (\s@Invitation' {} a -> s {invitedAt = a} :: Invitation) Prelude.. Lens.mapping Core._Time

-- | The status of the relationship between the account that sent the
-- invitation and the account that received the invitation.
invitation_relationshipStatus :: Lens.Lens' Invitation (Prelude.Maybe RelationshipStatus)
invitation_relationshipStatus = Lens.lens (\Invitation' {relationshipStatus} -> relationshipStatus) (\s@Invitation' {} a -> s {relationshipStatus = a} :: Invitation)

-- | The unique identifier for the invitation.
invitation_invitationId :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_invitationId = Lens.lens (\Invitation' {invitationId} -> invitationId) (\s@Invitation' {} a -> s {invitationId = a} :: Invitation)

instance Core.FromJSON Invitation where
  parseJSON =
    Core.withObject
      "Invitation"
      ( \x ->
          Invitation'
            Prelude.<$> (x Core..:? "accountId")
            Prelude.<*> (x Core..:? "invitedAt")
            Prelude.<*> (x Core..:? "relationshipStatus")
            Prelude.<*> (x Core..:? "invitationId")
      )

instance Prelude.Hashable Invitation where
  hashWithSalt _salt Invitation' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` invitedAt
      `Prelude.hashWithSalt` relationshipStatus
      `Prelude.hashWithSalt` invitationId

instance Prelude.NFData Invitation where
  rnf Invitation' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf invitedAt
      `Prelude.seq` Prelude.rnf relationshipStatus
      `Prelude.seq` Prelude.rnf invitationId
