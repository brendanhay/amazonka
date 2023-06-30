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
-- Module      : Amazonka.NetworkManager.Types.Attachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.Attachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.AttachmentState
import Amazonka.NetworkManager.Types.AttachmentType
import Amazonka.NetworkManager.Types.ProposedSegmentChange
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a core network attachment.
--
-- /See:/ 'newAttachment' smart constructor.
data Attachment = Attachment'
  { -- | The ID of the attachment.
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | The policy rule number associated with the attachment.
    attachmentPolicyRuleNumber :: Prelude.Maybe Prelude.Int,
    -- | The type of attachment.
    attachmentType :: Prelude.Maybe AttachmentType,
    -- | The ARN of a core network.
    coreNetworkArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the attachment was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The Region where the edge is located.
    edgeLocation :: Prelude.Maybe Prelude.Text,
    -- | The ID of the attachment account owner.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | The attachment to move from one segment to another.
    proposedSegmentChange :: Prelude.Maybe ProposedSegmentChange,
    -- | The attachment resource ARN.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the segment attachment.
    segmentName :: Prelude.Maybe Prelude.Text,
    -- | The state of the attachment.
    state :: Prelude.Maybe AttachmentState,
    -- | The tags associated with the attachment.
    tags :: Prelude.Maybe [Tag],
    -- | The timestamp when the attachment was last updated.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Attachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentId', 'attachment_attachmentId' - The ID of the attachment.
--
-- 'attachmentPolicyRuleNumber', 'attachment_attachmentPolicyRuleNumber' - The policy rule number associated with the attachment.
--
-- 'attachmentType', 'attachment_attachmentType' - The type of attachment.
--
-- 'coreNetworkArn', 'attachment_coreNetworkArn' - The ARN of a core network.
--
-- 'coreNetworkId', 'attachment_coreNetworkId' - The ID of a core network.
--
-- 'createdAt', 'attachment_createdAt' - The timestamp when the attachment was created.
--
-- 'edgeLocation', 'attachment_edgeLocation' - The Region where the edge is located.
--
-- 'ownerAccountId', 'attachment_ownerAccountId' - The ID of the attachment account owner.
--
-- 'proposedSegmentChange', 'attachment_proposedSegmentChange' - The attachment to move from one segment to another.
--
-- 'resourceArn', 'attachment_resourceArn' - The attachment resource ARN.
--
-- 'segmentName', 'attachment_segmentName' - The name of the segment attachment.
--
-- 'state', 'attachment_state' - The state of the attachment.
--
-- 'tags', 'attachment_tags' - The tags associated with the attachment.
--
-- 'updatedAt', 'attachment_updatedAt' - The timestamp when the attachment was last updated.
newAttachment ::
  Attachment
newAttachment =
  Attachment'
    { attachmentId = Prelude.Nothing,
      attachmentPolicyRuleNumber = Prelude.Nothing,
      attachmentType = Prelude.Nothing,
      coreNetworkArn = Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      edgeLocation = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      proposedSegmentChange = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      segmentName = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The ID of the attachment.
attachment_attachmentId :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_attachmentId = Lens.lens (\Attachment' {attachmentId} -> attachmentId) (\s@Attachment' {} a -> s {attachmentId = a} :: Attachment)

-- | The policy rule number associated with the attachment.
attachment_attachmentPolicyRuleNumber :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Int)
attachment_attachmentPolicyRuleNumber = Lens.lens (\Attachment' {attachmentPolicyRuleNumber} -> attachmentPolicyRuleNumber) (\s@Attachment' {} a -> s {attachmentPolicyRuleNumber = a} :: Attachment)

-- | The type of attachment.
attachment_attachmentType :: Lens.Lens' Attachment (Prelude.Maybe AttachmentType)
attachment_attachmentType = Lens.lens (\Attachment' {attachmentType} -> attachmentType) (\s@Attachment' {} a -> s {attachmentType = a} :: Attachment)

-- | The ARN of a core network.
attachment_coreNetworkArn :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_coreNetworkArn = Lens.lens (\Attachment' {coreNetworkArn} -> coreNetworkArn) (\s@Attachment' {} a -> s {coreNetworkArn = a} :: Attachment)

-- | The ID of a core network.
attachment_coreNetworkId :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_coreNetworkId = Lens.lens (\Attachment' {coreNetworkId} -> coreNetworkId) (\s@Attachment' {} a -> s {coreNetworkId = a} :: Attachment)

-- | The timestamp when the attachment was created.
attachment_createdAt :: Lens.Lens' Attachment (Prelude.Maybe Prelude.UTCTime)
attachment_createdAt = Lens.lens (\Attachment' {createdAt} -> createdAt) (\s@Attachment' {} a -> s {createdAt = a} :: Attachment) Prelude.. Lens.mapping Data._Time

-- | The Region where the edge is located.
attachment_edgeLocation :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_edgeLocation = Lens.lens (\Attachment' {edgeLocation} -> edgeLocation) (\s@Attachment' {} a -> s {edgeLocation = a} :: Attachment)

-- | The ID of the attachment account owner.
attachment_ownerAccountId :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_ownerAccountId = Lens.lens (\Attachment' {ownerAccountId} -> ownerAccountId) (\s@Attachment' {} a -> s {ownerAccountId = a} :: Attachment)

-- | The attachment to move from one segment to another.
attachment_proposedSegmentChange :: Lens.Lens' Attachment (Prelude.Maybe ProposedSegmentChange)
attachment_proposedSegmentChange = Lens.lens (\Attachment' {proposedSegmentChange} -> proposedSegmentChange) (\s@Attachment' {} a -> s {proposedSegmentChange = a} :: Attachment)

-- | The attachment resource ARN.
attachment_resourceArn :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_resourceArn = Lens.lens (\Attachment' {resourceArn} -> resourceArn) (\s@Attachment' {} a -> s {resourceArn = a} :: Attachment)

-- | The name of the segment attachment.
attachment_segmentName :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_segmentName = Lens.lens (\Attachment' {segmentName} -> segmentName) (\s@Attachment' {} a -> s {segmentName = a} :: Attachment)

-- | The state of the attachment.
attachment_state :: Lens.Lens' Attachment (Prelude.Maybe AttachmentState)
attachment_state = Lens.lens (\Attachment' {state} -> state) (\s@Attachment' {} a -> s {state = a} :: Attachment)

-- | The tags associated with the attachment.
attachment_tags :: Lens.Lens' Attachment (Prelude.Maybe [Tag])
attachment_tags = Lens.lens (\Attachment' {tags} -> tags) (\s@Attachment' {} a -> s {tags = a} :: Attachment) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp when the attachment was last updated.
attachment_updatedAt :: Lens.Lens' Attachment (Prelude.Maybe Prelude.UTCTime)
attachment_updatedAt = Lens.lens (\Attachment' {updatedAt} -> updatedAt) (\s@Attachment' {} a -> s {updatedAt = a} :: Attachment) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Attachment where
  parseJSON =
    Data.withObject
      "Attachment"
      ( \x ->
          Attachment'
            Prelude.<$> (x Data..:? "AttachmentId")
            Prelude.<*> (x Data..:? "AttachmentPolicyRuleNumber")
            Prelude.<*> (x Data..:? "AttachmentType")
            Prelude.<*> (x Data..:? "CoreNetworkArn")
            Prelude.<*> (x Data..:? "CoreNetworkId")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "EdgeLocation")
            Prelude.<*> (x Data..:? "OwnerAccountId")
            Prelude.<*> (x Data..:? "ProposedSegmentChange")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "SegmentName")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable Attachment where
  hashWithSalt _salt Attachment' {..} =
    _salt
      `Prelude.hashWithSalt` attachmentId
      `Prelude.hashWithSalt` attachmentPolicyRuleNumber
      `Prelude.hashWithSalt` attachmentType
      `Prelude.hashWithSalt` coreNetworkArn
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` edgeLocation
      `Prelude.hashWithSalt` ownerAccountId
      `Prelude.hashWithSalt` proposedSegmentChange
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` segmentName
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Attachment where
  rnf Attachment' {..} =
    Prelude.rnf attachmentId
      `Prelude.seq` Prelude.rnf attachmentPolicyRuleNumber
      `Prelude.seq` Prelude.rnf attachmentType
      `Prelude.seq` Prelude.rnf coreNetworkArn
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf edgeLocation
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf proposedSegmentChange
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf segmentName
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf updatedAt
