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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.Invitation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.RelationshipStatus
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an Amazon Macie membership invitation.
--
-- /See:/ 'newInvitation' smart constructor.
data Invitation = Invitation'
  { -- | The Amazon Web Services account ID for the account that sent the
    -- invitation.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the invitation.
    invitationId :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in UTC and extended ISO 8601 format, when the
    -- invitation was sent.
    invitedAt :: Prelude.Maybe Data.ISO8601,
    -- | The status of the relationship between the account that sent the
    -- invitation and the account that received the invitation.
    relationshipStatus :: Prelude.Maybe RelationshipStatus
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
-- 'invitationId', 'invitation_invitationId' - The unique identifier for the invitation.
--
-- 'invitedAt', 'invitation_invitedAt' - The date and time, in UTC and extended ISO 8601 format, when the
-- invitation was sent.
--
-- 'relationshipStatus', 'invitation_relationshipStatus' - The status of the relationship between the account that sent the
-- invitation and the account that received the invitation.
newInvitation ::
  Invitation
newInvitation =
  Invitation'
    { accountId = Prelude.Nothing,
      invitationId = Prelude.Nothing,
      invitedAt = Prelude.Nothing,
      relationshipStatus = Prelude.Nothing
    }

-- | The Amazon Web Services account ID for the account that sent the
-- invitation.
invitation_accountId :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_accountId = Lens.lens (\Invitation' {accountId} -> accountId) (\s@Invitation' {} a -> s {accountId = a} :: Invitation)

-- | The unique identifier for the invitation.
invitation_invitationId :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_invitationId = Lens.lens (\Invitation' {invitationId} -> invitationId) (\s@Invitation' {} a -> s {invitationId = a} :: Invitation)

-- | The date and time, in UTC and extended ISO 8601 format, when the
-- invitation was sent.
invitation_invitedAt :: Lens.Lens' Invitation (Prelude.Maybe Prelude.UTCTime)
invitation_invitedAt = Lens.lens (\Invitation' {invitedAt} -> invitedAt) (\s@Invitation' {} a -> s {invitedAt = a} :: Invitation) Prelude.. Lens.mapping Data._Time

-- | The status of the relationship between the account that sent the
-- invitation and the account that received the invitation.
invitation_relationshipStatus :: Lens.Lens' Invitation (Prelude.Maybe RelationshipStatus)
invitation_relationshipStatus = Lens.lens (\Invitation' {relationshipStatus} -> relationshipStatus) (\s@Invitation' {} a -> s {relationshipStatus = a} :: Invitation)

instance Data.FromJSON Invitation where
  parseJSON =
    Data.withObject
      "Invitation"
      ( \x ->
          Invitation'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "invitationId")
            Prelude.<*> (x Data..:? "invitedAt")
            Prelude.<*> (x Data..:? "relationshipStatus")
      )

instance Prelude.Hashable Invitation where
  hashWithSalt _salt Invitation' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` invitationId
      `Prelude.hashWithSalt` invitedAt
      `Prelude.hashWithSalt` relationshipStatus

instance Prelude.NFData Invitation where
  rnf Invitation' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf invitationId `Prelude.seq`
        Prelude.rnf invitedAt `Prelude.seq`
          Prelude.rnf relationshipStatus
