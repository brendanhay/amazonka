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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ResourceShareInvitation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types.ResourceShareAssociation
import Amazonka.RAM.Types.ResourceShareInvitationStatus

-- | Describes an invitation for an Amazon Web Services account to join a
-- resource share.
--
-- /See:/ 'newResourceShareInvitation' smart constructor.
data ResourceShareInvitation = ResourceShareInvitation'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the resource share
    resourceShareArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of the invitation.
    status :: Prelude.Maybe ResourceShareInvitationStatus,
    -- | To view the resources associated with a pending resource share
    -- invitation, use ListPendingInvitationResources.
    resourceShareAssociations :: Prelude.Maybe [ResourceShareAssociation],
    -- | The date and time when the invitation was sent.
    invitationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ID of the Amazon Web Services account that sent the invitation.
    senderAccountId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that received the invitation.
    receiverAccountId :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the IAM user or role that received the invitation.
    receiverArn :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the invitation.
    resourceShareInvitationArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource share.
    resourceShareName :: Prelude.Maybe Prelude.Text
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
-- 'resourceShareArn', 'resourceShareInvitation_resourceShareArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share
--
-- 'status', 'resourceShareInvitation_status' - The current status of the invitation.
--
-- 'resourceShareAssociations', 'resourceShareInvitation_resourceShareAssociations' - To view the resources associated with a pending resource share
-- invitation, use ListPendingInvitationResources.
--
-- 'invitationTimestamp', 'resourceShareInvitation_invitationTimestamp' - The date and time when the invitation was sent.
--
-- 'senderAccountId', 'resourceShareInvitation_senderAccountId' - The ID of the Amazon Web Services account that sent the invitation.
--
-- 'receiverAccountId', 'resourceShareInvitation_receiverAccountId' - The ID of the Amazon Web Services account that received the invitation.
--
-- 'receiverArn', 'resourceShareInvitation_receiverArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the IAM user or role that received the invitation.
--
-- 'resourceShareInvitationArn', 'resourceShareInvitation_resourceShareInvitationArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the invitation.
--
-- 'resourceShareName', 'resourceShareInvitation_resourceShareName' - The name of the resource share.
newResourceShareInvitation ::
  ResourceShareInvitation
newResourceShareInvitation =
  ResourceShareInvitation'
    { resourceShareArn =
        Prelude.Nothing,
      status = Prelude.Nothing,
      resourceShareAssociations = Prelude.Nothing,
      invitationTimestamp = Prelude.Nothing,
      senderAccountId = Prelude.Nothing,
      receiverAccountId = Prelude.Nothing,
      receiverArn = Prelude.Nothing,
      resourceShareInvitationArn = Prelude.Nothing,
      resourceShareName = Prelude.Nothing
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share
resourceShareInvitation_resourceShareArn :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe Prelude.Text)
resourceShareInvitation_resourceShareArn = Lens.lens (\ResourceShareInvitation' {resourceShareArn} -> resourceShareArn) (\s@ResourceShareInvitation' {} a -> s {resourceShareArn = a} :: ResourceShareInvitation)

-- | The current status of the invitation.
resourceShareInvitation_status :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe ResourceShareInvitationStatus)
resourceShareInvitation_status = Lens.lens (\ResourceShareInvitation' {status} -> status) (\s@ResourceShareInvitation' {} a -> s {status = a} :: ResourceShareInvitation)

-- | To view the resources associated with a pending resource share
-- invitation, use ListPendingInvitationResources.
resourceShareInvitation_resourceShareAssociations :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe [ResourceShareAssociation])
resourceShareInvitation_resourceShareAssociations = Lens.lens (\ResourceShareInvitation' {resourceShareAssociations} -> resourceShareAssociations) (\s@ResourceShareInvitation' {} a -> s {resourceShareAssociations = a} :: ResourceShareInvitation) Prelude.. Lens.mapping Lens.coerced

-- | The date and time when the invitation was sent.
resourceShareInvitation_invitationTimestamp :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe Prelude.UTCTime)
resourceShareInvitation_invitationTimestamp = Lens.lens (\ResourceShareInvitation' {invitationTimestamp} -> invitationTimestamp) (\s@ResourceShareInvitation' {} a -> s {invitationTimestamp = a} :: ResourceShareInvitation) Prelude.. Lens.mapping Data._Time

-- | The ID of the Amazon Web Services account that sent the invitation.
resourceShareInvitation_senderAccountId :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe Prelude.Text)
resourceShareInvitation_senderAccountId = Lens.lens (\ResourceShareInvitation' {senderAccountId} -> senderAccountId) (\s@ResourceShareInvitation' {} a -> s {senderAccountId = a} :: ResourceShareInvitation)

-- | The ID of the Amazon Web Services account that received the invitation.
resourceShareInvitation_receiverAccountId :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe Prelude.Text)
resourceShareInvitation_receiverAccountId = Lens.lens (\ResourceShareInvitation' {receiverAccountId} -> receiverAccountId) (\s@ResourceShareInvitation' {} a -> s {receiverAccountId = a} :: ResourceShareInvitation)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the IAM user or role that received the invitation.
resourceShareInvitation_receiverArn :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe Prelude.Text)
resourceShareInvitation_receiverArn = Lens.lens (\ResourceShareInvitation' {receiverArn} -> receiverArn) (\s@ResourceShareInvitation' {} a -> s {receiverArn = a} :: ResourceShareInvitation)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the invitation.
resourceShareInvitation_resourceShareInvitationArn :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe Prelude.Text)
resourceShareInvitation_resourceShareInvitationArn = Lens.lens (\ResourceShareInvitation' {resourceShareInvitationArn} -> resourceShareInvitationArn) (\s@ResourceShareInvitation' {} a -> s {resourceShareInvitationArn = a} :: ResourceShareInvitation)

-- | The name of the resource share.
resourceShareInvitation_resourceShareName :: Lens.Lens' ResourceShareInvitation (Prelude.Maybe Prelude.Text)
resourceShareInvitation_resourceShareName = Lens.lens (\ResourceShareInvitation' {resourceShareName} -> resourceShareName) (\s@ResourceShareInvitation' {} a -> s {resourceShareName = a} :: ResourceShareInvitation)

instance Data.FromJSON ResourceShareInvitation where
  parseJSON =
    Data.withObject
      "ResourceShareInvitation"
      ( \x ->
          ResourceShareInvitation'
            Prelude.<$> (x Data..:? "resourceShareArn")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> ( x Data..:? "resourceShareAssociations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "invitationTimestamp")
            Prelude.<*> (x Data..:? "senderAccountId")
            Prelude.<*> (x Data..:? "receiverAccountId")
            Prelude.<*> (x Data..:? "receiverArn")
            Prelude.<*> (x Data..:? "resourceShareInvitationArn")
            Prelude.<*> (x Data..:? "resourceShareName")
      )

instance Prelude.Hashable ResourceShareInvitation where
  hashWithSalt _salt ResourceShareInvitation' {..} =
    _salt `Prelude.hashWithSalt` resourceShareArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` resourceShareAssociations
      `Prelude.hashWithSalt` invitationTimestamp
      `Prelude.hashWithSalt` senderAccountId
      `Prelude.hashWithSalt` receiverAccountId
      `Prelude.hashWithSalt` receiverArn
      `Prelude.hashWithSalt` resourceShareInvitationArn
      `Prelude.hashWithSalt` resourceShareName

instance Prelude.NFData ResourceShareInvitation where
  rnf ResourceShareInvitation' {..} =
    Prelude.rnf resourceShareArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf resourceShareAssociations
      `Prelude.seq` Prelude.rnf invitationTimestamp
      `Prelude.seq` Prelude.rnf senderAccountId
      `Prelude.seq` Prelude.rnf receiverAccountId
      `Prelude.seq` Prelude.rnf receiverArn
      `Prelude.seq` Prelude.rnf resourceShareInvitationArn
      `Prelude.seq` Prelude.rnf resourceShareName
