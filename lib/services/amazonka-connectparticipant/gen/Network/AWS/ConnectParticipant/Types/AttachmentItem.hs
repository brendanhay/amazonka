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
-- Module      : Network.AWS.ConnectParticipant.Types.AttachmentItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ConnectParticipant.Types.AttachmentItem where

import Network.AWS.ConnectParticipant.Types.ArtifactStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The case-insensitive input to indicate standard MIME type that describes
-- the format of the file that will be uploaded.
--
-- /See:/ 'newAttachmentItem' smart constructor.
data AttachmentItem = AttachmentItem'
  { -- | Status of the attachment.
    status :: Prelude.Maybe ArtifactStatus,
    -- | A case-sensitive name of the attachment being uploaded.
    attachmentName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the attachment.
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | Describes the MIME file type of the attachment. For a list of supported
    -- file types, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html#feature-limits Feature specifications>
    -- in the /Amazon Connect Administrator Guide/.
    contentType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachmentItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'attachmentItem_status' - Status of the attachment.
--
-- 'attachmentName', 'attachmentItem_attachmentName' - A case-sensitive name of the attachment being uploaded.
--
-- 'attachmentId', 'attachmentItem_attachmentId' - A unique identifier for the attachment.
--
-- 'contentType', 'attachmentItem_contentType' - Describes the MIME file type of the attachment. For a list of supported
-- file types, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html#feature-limits Feature specifications>
-- in the /Amazon Connect Administrator Guide/.
newAttachmentItem ::
  AttachmentItem
newAttachmentItem =
  AttachmentItem'
    { status = Prelude.Nothing,
      attachmentName = Prelude.Nothing,
      attachmentId = Prelude.Nothing,
      contentType = Prelude.Nothing
    }

-- | Status of the attachment.
attachmentItem_status :: Lens.Lens' AttachmentItem (Prelude.Maybe ArtifactStatus)
attachmentItem_status = Lens.lens (\AttachmentItem' {status} -> status) (\s@AttachmentItem' {} a -> s {status = a} :: AttachmentItem)

-- | A case-sensitive name of the attachment being uploaded.
attachmentItem_attachmentName :: Lens.Lens' AttachmentItem (Prelude.Maybe Prelude.Text)
attachmentItem_attachmentName = Lens.lens (\AttachmentItem' {attachmentName} -> attachmentName) (\s@AttachmentItem' {} a -> s {attachmentName = a} :: AttachmentItem)

-- | A unique identifier for the attachment.
attachmentItem_attachmentId :: Lens.Lens' AttachmentItem (Prelude.Maybe Prelude.Text)
attachmentItem_attachmentId = Lens.lens (\AttachmentItem' {attachmentId} -> attachmentId) (\s@AttachmentItem' {} a -> s {attachmentId = a} :: AttachmentItem)

-- | Describes the MIME file type of the attachment. For a list of supported
-- file types, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html#feature-limits Feature specifications>
-- in the /Amazon Connect Administrator Guide/.
attachmentItem_contentType :: Lens.Lens' AttachmentItem (Prelude.Maybe Prelude.Text)
attachmentItem_contentType = Lens.lens (\AttachmentItem' {contentType} -> contentType) (\s@AttachmentItem' {} a -> s {contentType = a} :: AttachmentItem)

instance Core.FromJSON AttachmentItem where
  parseJSON =
    Core.withObject
      "AttachmentItem"
      ( \x ->
          AttachmentItem'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "AttachmentName")
            Prelude.<*> (x Core..:? "AttachmentId")
            Prelude.<*> (x Core..:? "ContentType")
      )

instance Prelude.Hashable AttachmentItem

instance Prelude.NFData AttachmentItem
