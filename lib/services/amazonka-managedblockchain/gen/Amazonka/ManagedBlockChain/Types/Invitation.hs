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
-- Module      : Amazonka.ManagedBlockChain.Types.Invitation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.Invitation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ManagedBlockChain.Types.InvitationStatus
import Amazonka.ManagedBlockChain.Types.NetworkSummary
import qualified Amazonka.Prelude as Prelude

-- | An invitation to an Amazon Web Services account to create a member and
-- join the network.
--
-- Applies only to Hyperledger Fabric.
--
-- /See:/ 'newInvitation' smart constructor.
data Invitation = Invitation'
  { -- | The Amazon Resource Name (ARN) of the invitation. For more information
    -- about ARNs and their format, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the invitation was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The status of the invitation:
    --
    -- -   @PENDING@ - The invitee hasn\'t created a member to join the
    --     network, and the invitation hasn\'t yet expired.
    --
    -- -   @ACCEPTING@ - The invitee has begun creating a member, and creation
    --     hasn\'t yet completed.
    --
    -- -   @ACCEPTED@ - The invitee created a member and joined the network
    --     using the @InvitationID@.
    --
    -- -   @REJECTED@ - The invitee rejected the invitation.
    --
    -- -   @EXPIRED@ - The invitee neither created a member nor rejected the
    --     invitation before the @ExpirationDate@.
    status :: Prelude.Maybe InvitationStatus,
    networkSummary :: Prelude.Maybe NetworkSummary,
    -- | The date and time that the invitation expires. This is the
    -- @CreationDate@ plus the @ProposalDurationInHours@ that is specified in
    -- the @ProposalThresholdPolicy@. After this date and time, the invitee can
    -- no longer create a member and join the network using this
    -- @InvitationId@.
    expirationDate :: Prelude.Maybe Core.POSIX,
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
-- 'arn', 'invitation_arn' - The Amazon Resource Name (ARN) of the invitation. For more information
-- about ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'creationDate', 'invitation_creationDate' - The date and time that the invitation was created.
--
-- 'status', 'invitation_status' - The status of the invitation:
--
-- -   @PENDING@ - The invitee hasn\'t created a member to join the
--     network, and the invitation hasn\'t yet expired.
--
-- -   @ACCEPTING@ - The invitee has begun creating a member, and creation
--     hasn\'t yet completed.
--
-- -   @ACCEPTED@ - The invitee created a member and joined the network
--     using the @InvitationID@.
--
-- -   @REJECTED@ - The invitee rejected the invitation.
--
-- -   @EXPIRED@ - The invitee neither created a member nor rejected the
--     invitation before the @ExpirationDate@.
--
-- 'networkSummary', 'invitation_networkSummary' - Undocumented member.
--
-- 'expirationDate', 'invitation_expirationDate' - The date and time that the invitation expires. This is the
-- @CreationDate@ plus the @ProposalDurationInHours@ that is specified in
-- the @ProposalThresholdPolicy@. After this date and time, the invitee can
-- no longer create a member and join the network using this
-- @InvitationId@.
--
-- 'invitationId', 'invitation_invitationId' - The unique identifier for the invitation.
newInvitation ::
  Invitation
newInvitation =
  Invitation'
    { arn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      status = Prelude.Nothing,
      networkSummary = Prelude.Nothing,
      expirationDate = Prelude.Nothing,
      invitationId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the invitation. For more information
-- about ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
invitation_arn :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_arn = Lens.lens (\Invitation' {arn} -> arn) (\s@Invitation' {} a -> s {arn = a} :: Invitation)

-- | The date and time that the invitation was created.
invitation_creationDate :: Lens.Lens' Invitation (Prelude.Maybe Prelude.UTCTime)
invitation_creationDate = Lens.lens (\Invitation' {creationDate} -> creationDate) (\s@Invitation' {} a -> s {creationDate = a} :: Invitation) Prelude.. Lens.mapping Core._Time

-- | The status of the invitation:
--
-- -   @PENDING@ - The invitee hasn\'t created a member to join the
--     network, and the invitation hasn\'t yet expired.
--
-- -   @ACCEPTING@ - The invitee has begun creating a member, and creation
--     hasn\'t yet completed.
--
-- -   @ACCEPTED@ - The invitee created a member and joined the network
--     using the @InvitationID@.
--
-- -   @REJECTED@ - The invitee rejected the invitation.
--
-- -   @EXPIRED@ - The invitee neither created a member nor rejected the
--     invitation before the @ExpirationDate@.
invitation_status :: Lens.Lens' Invitation (Prelude.Maybe InvitationStatus)
invitation_status = Lens.lens (\Invitation' {status} -> status) (\s@Invitation' {} a -> s {status = a} :: Invitation)

-- | Undocumented member.
invitation_networkSummary :: Lens.Lens' Invitation (Prelude.Maybe NetworkSummary)
invitation_networkSummary = Lens.lens (\Invitation' {networkSummary} -> networkSummary) (\s@Invitation' {} a -> s {networkSummary = a} :: Invitation)

-- | The date and time that the invitation expires. This is the
-- @CreationDate@ plus the @ProposalDurationInHours@ that is specified in
-- the @ProposalThresholdPolicy@. After this date and time, the invitee can
-- no longer create a member and join the network using this
-- @InvitationId@.
invitation_expirationDate :: Lens.Lens' Invitation (Prelude.Maybe Prelude.UTCTime)
invitation_expirationDate = Lens.lens (\Invitation' {expirationDate} -> expirationDate) (\s@Invitation' {} a -> s {expirationDate = a} :: Invitation) Prelude.. Lens.mapping Core._Time

-- | The unique identifier for the invitation.
invitation_invitationId :: Lens.Lens' Invitation (Prelude.Maybe Prelude.Text)
invitation_invitationId = Lens.lens (\Invitation' {invitationId} -> invitationId) (\s@Invitation' {} a -> s {invitationId = a} :: Invitation)

instance Core.FromJSON Invitation where
  parseJSON =
    Core.withObject
      "Invitation"
      ( \x ->
          Invitation'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "NetworkSummary")
            Prelude.<*> (x Core..:? "ExpirationDate")
            Prelude.<*> (x Core..:? "InvitationId")
      )

instance Prelude.Hashable Invitation where
  hashWithSalt _salt Invitation' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` networkSummary
      `Prelude.hashWithSalt` expirationDate
      `Prelude.hashWithSalt` invitationId

instance Prelude.NFData Invitation where
  rnf Invitation' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf networkSummary
      `Prelude.seq` Prelude.rnf expirationDate
      `Prelude.seq` Prelude.rnf invitationId
