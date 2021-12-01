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
-- Module      : Amazonka.RAM.Types.ResourceShareInvitation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ResourceShareInvitation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types.ResourceShareAssociation
import Amazonka.RAM.Types.ResourceShareInvitationStatus

-- | Describes an invitation to join a resource share.
--
-- /See:/ 'newResourceShareInvitation' smart constructor.
data ResourceShareInvitation = ResourceShareInvitation'
  { -- | The status of the invitation.
    status :: Prelude.Maybe ResourceShareInvitationStatus,
    -- | The ID of the Amazon Web Services account that sent the invitation.
    senderAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource share.
    resourceShareArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that received the invitation.
    receiverAccountId :: Prelude.Maybe Prelude.Text,
    -- | To view the resources associated with a pending resource share
    -- invitation, use
    -- <https://docs.aws.amazon.com/ram/latest/APIReference/API_ListPendingInvitationResources.html ListPendingInvitationResources>.
    resourceShareAssociations :: Prelude.Maybe [ResourceShareAssociation],
    -- | The name of the resource share.
    resourceShareName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM user or IAM role that received
    -- the invitation.
    receiverArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the invitation was sent.
    invitationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the invitation.
    resourceShareInvitationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceShareInvitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'resourceShareInvitation_status' - The status of the invitation.
--
-- 'senderAccountId', 'resourceShareInvitation_senderAccountId' - The ID of the Amazon Web Services account that sent the invitation.
--
-- 'resourceShareArn', 'resourceShareInvitation_resourceShareArn' - The Amazon Resource Name (ARN) of the resource share.
--
-- 'receiverAccountId', 'resourceShareInvitation_receiverAccountId' - The ID of the Amazon Web Services account that received the invitation.
--
-- 'resourceShareAssociations', 'resourceShareInvitation_resourceShareAssociations' - To view the resources associated with a pending resource share
-- invitation, use
-- <https://docs.aws.amazon.com/ram/latest/APIReference/API_ListPendingInvitationResources.html ListPendingInvitationResources>.
--
-- 'resourceShareName', 'resourceShareInvitation_resourceShareName' - The name of the resource share.
--
-- 'receiverArn', 'resourceShareInvitation_receiverArn' - The Amazon Resource Name (ARN) of the IAM user or IAM role that received
-- the invitation.
--
-- 'invitationTimestamp', 'resourceShareInvitation_invitationTimestamp' - The date and time when the invitation was sent.
--
-- 'resourceShareInvitationArn', 'resourceShareInvitation_resourceShareInvitationArn' - The Amazon Resource Name (ARN) of the invitation.
newResourceShareInvitation ::
  ResourceShareInvitation
newResourceShareInvitation =
  ResourceShareInvitation'
    { status = Prelude.Nothing,
      senderAccountId = Prelude.Nothing,
      resourceShareArn = Prelude.Nothing,
      receiverAccountId = Prelude.Nothing,
      resourceShareAssociations = Prelude.Nothing,
      resourceShareName = Prelude.Nothing,
      receiverArn = Prelude.Nothing,
      invitationTimestamp = Prelude.Nothing,
      resourceShareInvitationArn = Prelude.Nothing
    }

-- | The status of the invitation.
resourceShareInvitation_status :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe ResourceShareInvitationStatus)
resourceShareInvitation_status = Lens.lens (\ResourceShareInvitation' {status} -> status) (\s@ResourceShareInvitation' {} a -> s {status = a} :: ResourceShareInvitation)

-- | The ID of the Amazon Web Services account that sent the invitation.
resourceShareInvitation_senderAccountId :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe Prelude.Text)
resourceShareInvitation_senderAccountId = Lens.lens (\ResourceShareInvitation' {senderAccountId} -> senderAccountId) (\s@ResourceShareInvitation' {} a -> s {senderAccountId = a} :: ResourceShareInvitation)

-- | The Amazon Resource Name (ARN) of the resource share.
resourceShareInvitation_resourceShareArn :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe Prelude.Text)
resourceShareInvitation_resourceShareArn = Lens.lens (\ResourceShareInvitation' {resourceShareArn} -> resourceShareArn) (\s@ResourceShareInvitation' {} a -> s {resourceShareArn = a} :: ResourceShareInvitation)

-- | The ID of the Amazon Web Services account that received the invitation.
resourceShareInvitation_receiverAccountId :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe Prelude.Text)
resourceShareInvitation_receiverAccountId = Lens.lens (\ResourceShareInvitation' {receiverAccountId} -> receiverAccountId) (\s@ResourceShareInvitation' {} a -> s {receiverAccountId = a} :: ResourceShareInvitation)

-- | To view the resources associated with a pending resource share
-- invitation, use
-- <https://docs.aws.amazon.com/ram/latest/APIReference/API_ListPendingInvitationResources.html ListPendingInvitationResources>.
resourceShareInvitation_resourceShareAssociations :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe [ResourceShareAssociation])
resourceShareInvitation_resourceShareAssociations = Lens.lens (\ResourceShareInvitation' {resourceShareAssociations} -> resourceShareAssociations) (\s@ResourceShareInvitation' {} a -> s {resourceShareAssociations = a} :: ResourceShareInvitation) Prelude.. Lens.mapping Lens.coerced

-- | The name of the resource share.
resourceShareInvitation_resourceShareName :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe Prelude.Text)
resourceShareInvitation_resourceShareName = Lens.lens (\ResourceShareInvitation' {resourceShareName} -> resourceShareName) (\s@ResourceShareInvitation' {} a -> s {resourceShareName = a} :: ResourceShareInvitation)

-- | The Amazon Resource Name (ARN) of the IAM user or IAM role that received
-- the invitation.
resourceShareInvitation_receiverArn :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe Prelude.Text)
resourceShareInvitation_receiverArn = Lens.lens (\ResourceShareInvitation' {receiverArn} -> receiverArn) (\s@ResourceShareInvitation' {} a -> s {receiverArn = a} :: ResourceShareInvitation)

-- | The date and time when the invitation was sent.
resourceShareInvitation_invitationTimestamp :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe Prelude.UTCTime)
resourceShareInvitation_invitationTimestamp = Lens.lens (\ResourceShareInvitation' {invitationTimestamp} -> invitationTimestamp) (\s@ResourceShareInvitation' {} a -> s {invitationTimestamp = a} :: ResourceShareInvitation) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the invitation.
resourceShareInvitation_resourceShareInvitationArn :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe Prelude.Text)
resourceShareInvitation_resourceShareInvitationArn = Lens.lens (\ResourceShareInvitation' {resourceShareInvitationArn} -> resourceShareInvitationArn) (\s@ResourceShareInvitation' {} a -> s {resourceShareInvitationArn = a} :: ResourceShareInvitation)

instance Core.FromJSON ResourceShareInvitation where
  parseJSON =
    Core.withObject
      "ResourceShareInvitation"
      ( \x ->
          ResourceShareInvitation'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "senderAccountId")
            Prelude.<*> (x Core..:? "resourceShareArn")
            Prelude.<*> (x Core..:? "receiverAccountId")
            Prelude.<*> ( x Core..:? "resourceShareAssociations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "resourceShareName")
            Prelude.<*> (x Core..:? "receiverArn")
            Prelude.<*> (x Core..:? "invitationTimestamp")
            Prelude.<*> (x Core..:? "resourceShareInvitationArn")
      )

instance Prelude.Hashable ResourceShareInvitation where
  hashWithSalt salt' ResourceShareInvitation' {..} =
    salt'
      `Prelude.hashWithSalt` resourceShareInvitationArn
      `Prelude.hashWithSalt` invitationTimestamp
      `Prelude.hashWithSalt` receiverArn
      `Prelude.hashWithSalt` resourceShareName
      `Prelude.hashWithSalt` resourceShareAssociations
      `Prelude.hashWithSalt` receiverAccountId
      `Prelude.hashWithSalt` resourceShareArn
      `Prelude.hashWithSalt` senderAccountId
      `Prelude.hashWithSalt` status

instance Prelude.NFData ResourceShareInvitation where
  rnf ResourceShareInvitation' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf resourceShareInvitationArn
      `Prelude.seq` Prelude.rnf invitationTimestamp
      `Prelude.seq` Prelude.rnf receiverArn
      `Prelude.seq` Prelude.rnf resourceShareName
      `Prelude.seq` Prelude.rnf resourceShareAssociations
      `Prelude.seq` Prelude.rnf receiverAccountId
      `Prelude.seq` Prelude.rnf resourceShareArn
      `Prelude.seq` Prelude.rnf senderAccountId
