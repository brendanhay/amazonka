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
-- Module      : Network.AWS.Support.Types.AttachmentDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.AttachmentDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The file name and ID of an attachment to a case communication. You can
-- use the ID to retrieve the attachment with the DescribeAttachment
-- operation.
--
-- /See:/ 'newAttachmentDetails' smart constructor.
data AttachmentDetails = AttachmentDetails'
  { -- | The ID of the attachment.
    attachmentId :: Core.Maybe Core.Text,
    -- | The file name of the attachment.
    fileName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachmentDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentId', 'attachmentDetails_attachmentId' - The ID of the attachment.
--
-- 'fileName', 'attachmentDetails_fileName' - The file name of the attachment.
newAttachmentDetails ::
  AttachmentDetails
newAttachmentDetails =
  AttachmentDetails'
    { attachmentId = Core.Nothing,
      fileName = Core.Nothing
    }

-- | The ID of the attachment.
attachmentDetails_attachmentId :: Lens.Lens' AttachmentDetails (Core.Maybe Core.Text)
attachmentDetails_attachmentId = Lens.lens (\AttachmentDetails' {attachmentId} -> attachmentId) (\s@AttachmentDetails' {} a -> s {attachmentId = a} :: AttachmentDetails)

-- | The file name of the attachment.
attachmentDetails_fileName :: Lens.Lens' AttachmentDetails (Core.Maybe Core.Text)
attachmentDetails_fileName = Lens.lens (\AttachmentDetails' {fileName} -> fileName) (\s@AttachmentDetails' {} a -> s {fileName = a} :: AttachmentDetails)

instance Core.FromJSON AttachmentDetails where
  parseJSON =
    Core.withObject
      "AttachmentDetails"
      ( \x ->
          AttachmentDetails'
            Core.<$> (x Core..:? "attachmentId")
            Core.<*> (x Core..:? "fileName")
      )

instance Core.Hashable AttachmentDetails

instance Core.NFData AttachmentDetails
