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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.Attachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types.AttachmentState
import Amazonka.NetworkManager.Types.AttachmentType
import Amazonka.NetworkManager.Types.ProposedSegmentChange
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a core network attachment.
--
-- /See:/ 'newAttachment' smart constructor.
data Attachment = Attachment'
  { -- | The tags associated with the attachment.
    tags :: Prelude.Maybe [Tag],
    -- | The attachment to move from one segment to another.
    proposedSegmentChange :: Prelude.Maybe ProposedSegmentChange,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The state of the attachment.
    state :: Prelude.Maybe AttachmentState,
    -- | The Region where the edge is located.
    edgeLocation :: Prelude.Maybe Prelude.Text,
    -- | The ID of the attachment.
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | The name of the segment attachment.
    segmentName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a core network.
    coreNetworkArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the attachment account owner.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | The type of attachment.
    attachmentType :: Prelude.Maybe AttachmentType,
    -- | The attachment resource ARN.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The policy rule number associated with the attachment.
    attachmentPolicyRuleNumber :: Prelude.Maybe Prelude.Int,
    -- | The timestamp when the attachment was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The timestamp when the attachment was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX
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
-- 'tags', 'attachment_tags' - The tags associated with the attachment.
--
-- 'proposedSegmentChange', 'attachment_proposedSegmentChange' - The attachment to move from one segment to another.
--
-- 'coreNetworkId', 'attachment_coreNetworkId' - The ID of a core network.
--
-- 'state', 'attachment_state' - The state of the attachment.
--
-- 'edgeLocation', 'attachment_edgeLocation' - The Region where the edge is located.
--
-- 'attachmentId', 'attachment_attachmentId' - The ID of the attachment.
--
-- 'segmentName', 'attachment_segmentName' - The name of the segment attachment.
--
-- 'coreNetworkArn', 'attachment_coreNetworkArn' - The ARN of a core network.
--
-- 'ownerAccountId', 'attachment_ownerAccountId' - The ID of the attachment account owner.
--
-- 'attachmentType', 'attachment_attachmentType' - The type of attachment.
--
-- 'resourceArn', 'attachment_resourceArn' - The attachment resource ARN.
--
-- 'attachmentPolicyRuleNumber', 'attachment_attachmentPolicyRuleNumber' - The policy rule number associated with the attachment.
--
-- 'createdAt', 'attachment_createdAt' - The timestamp when the attachment was created.
--
-- 'updatedAt', 'attachment_updatedAt' - The timestamp when the attachment was last updated.
newAttachment ::
  Attachment
newAttachment =
  Attachment'
    { tags = Prelude.Nothing,
      proposedSegmentChange = Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      state = Prelude.Nothing,
      edgeLocation = Prelude.Nothing,
      attachmentId = Prelude.Nothing,
      segmentName = Prelude.Nothing,
      coreNetworkArn = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      attachmentType = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      attachmentPolicyRuleNumber = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The tags associated with the attachment.
attachment_tags :: Lens.Lens' Attachment (Prelude.Maybe [Tag])
attachment_tags = Lens.lens (\Attachment' {tags} -> tags) (\s@Attachment' {} a -> s {tags = a} :: Attachment) Prelude.. Lens.mapping Lens.coerced

-- | The attachment to move from one segment to another.
attachment_proposedSegmentChange :: Lens.Lens' Attachment (Prelude.Maybe ProposedSegmentChange)
attachment_proposedSegmentChange = Lens.lens (\Attachment' {proposedSegmentChange} -> proposedSegmentChange) (\s@Attachment' {} a -> s {proposedSegmentChange = a} :: Attachment)

-- | The ID of a core network.
attachment_coreNetworkId :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_coreNetworkId = Lens.lens (\Attachment' {coreNetworkId} -> coreNetworkId) (\s@Attachment' {} a -> s {coreNetworkId = a} :: Attachment)

-- | The state of the attachment.
attachment_state :: Lens.Lens' Attachment (Prelude.Maybe AttachmentState)
attachment_state = Lens.lens (\Attachment' {state} -> state) (\s@Attachment' {} a -> s {state = a} :: Attachment)

-- | The Region where the edge is located.
attachment_edgeLocation :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_edgeLocation = Lens.lens (\Attachment' {edgeLocation} -> edgeLocation) (\s@Attachment' {} a -> s {edgeLocation = a} :: Attachment)

-- | The ID of the attachment.
attachment_attachmentId :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_attachmentId = Lens.lens (\Attachment' {attachmentId} -> attachmentId) (\s@Attachment' {} a -> s {attachmentId = a} :: Attachment)

-- | The name of the segment attachment.
attachment_segmentName :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_segmentName = Lens.lens (\Attachment' {segmentName} -> segmentName) (\s@Attachment' {} a -> s {segmentName = a} :: Attachment)

-- | The ARN of a core network.
attachment_coreNetworkArn :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_coreNetworkArn = Lens.lens (\Attachment' {coreNetworkArn} -> coreNetworkArn) (\s@Attachment' {} a -> s {coreNetworkArn = a} :: Attachment)

-- | The ID of the attachment account owner.
attachment_ownerAccountId :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_ownerAccountId = Lens.lens (\Attachment' {ownerAccountId} -> ownerAccountId) (\s@Attachment' {} a -> s {ownerAccountId = a} :: Attachment)

-- | The type of attachment.
attachment_attachmentType :: Lens.Lens' Attachment (Prelude.Maybe AttachmentType)
attachment_attachmentType = Lens.lens (\Attachment' {attachmentType} -> attachmentType) (\s@Attachment' {} a -> s {attachmentType = a} :: Attachment)

-- | The attachment resource ARN.
attachment_resourceArn :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_resourceArn = Lens.lens (\Attachment' {resourceArn} -> resourceArn) (\s@Attachment' {} a -> s {resourceArn = a} :: Attachment)

-- | The policy rule number associated with the attachment.
attachment_attachmentPolicyRuleNumber :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Int)
attachment_attachmentPolicyRuleNumber = Lens.lens (\Attachment' {attachmentPolicyRuleNumber} -> attachmentPolicyRuleNumber) (\s@Attachment' {} a -> s {attachmentPolicyRuleNumber = a} :: Attachment)

-- | The timestamp when the attachment was created.
attachment_createdAt :: Lens.Lens' Attachment (Prelude.Maybe Prelude.UTCTime)
attachment_createdAt = Lens.lens (\Attachment' {createdAt} -> createdAt) (\s@Attachment' {} a -> s {createdAt = a} :: Attachment) Prelude.. Lens.mapping Core._Time

-- | The timestamp when the attachment was last updated.
attachment_updatedAt :: Lens.Lens' Attachment (Prelude.Maybe Prelude.UTCTime)
attachment_updatedAt = Lens.lens (\Attachment' {updatedAt} -> updatedAt) (\s@Attachment' {} a -> s {updatedAt = a} :: Attachment) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Attachment where
  parseJSON =
    Core.withObject
      "Attachment"
      ( \x ->
          Attachment'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ProposedSegmentChange")
            Prelude.<*> (x Core..:? "CoreNetworkId")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "EdgeLocation")
            Prelude.<*> (x Core..:? "AttachmentId")
            Prelude.<*> (x Core..:? "SegmentName")
            Prelude.<*> (x Core..:? "CoreNetworkArn")
            Prelude.<*> (x Core..:? "OwnerAccountId")
            Prelude.<*> (x Core..:? "AttachmentType")
            Prelude.<*> (x Core..:? "ResourceArn")
            Prelude.<*> (x Core..:? "AttachmentPolicyRuleNumber")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "UpdatedAt")
      )

instance Prelude.Hashable Attachment where
  hashWithSalt _salt Attachment' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` proposedSegmentChange
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` edgeLocation
      `Prelude.hashWithSalt` attachmentId
      `Prelude.hashWithSalt` segmentName
      `Prelude.hashWithSalt` coreNetworkArn
      `Prelude.hashWithSalt` ownerAccountId
      `Prelude.hashWithSalt` attachmentType
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` attachmentPolicyRuleNumber
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Attachment where
  rnf Attachment' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf proposedSegmentChange
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf edgeLocation
      `Prelude.seq` Prelude.rnf attachmentId
      `Prelude.seq` Prelude.rnf segmentName
      `Prelude.seq` Prelude.rnf coreNetworkArn
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf attachmentType
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf attachmentPolicyRuleNumber
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
