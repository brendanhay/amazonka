{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The file name and ID of an attachment to a case communication. You can
-- use the ID to retrieve the attachment with the DescribeAttachment
-- operation.
--
-- /See:/ 'newAttachmentDetails' smart constructor.
data AttachmentDetails = AttachmentDetails'
  { -- | The ID of the attachment.
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | The file name of the attachment.
    fileName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { attachmentId = Prelude.Nothing,
      fileName = Prelude.Nothing
    }

-- | The ID of the attachment.
attachmentDetails_attachmentId :: Lens.Lens' AttachmentDetails (Prelude.Maybe Prelude.Text)
attachmentDetails_attachmentId = Lens.lens (\AttachmentDetails' {attachmentId} -> attachmentId) (\s@AttachmentDetails' {} a -> s {attachmentId = a} :: AttachmentDetails)

-- | The file name of the attachment.
attachmentDetails_fileName :: Lens.Lens' AttachmentDetails (Prelude.Maybe Prelude.Text)
attachmentDetails_fileName = Lens.lens (\AttachmentDetails' {fileName} -> fileName) (\s@AttachmentDetails' {} a -> s {fileName = a} :: AttachmentDetails)

instance Prelude.FromJSON AttachmentDetails where
  parseJSON =
    Prelude.withObject
      "AttachmentDetails"
      ( \x ->
          AttachmentDetails'
            Prelude.<$> (x Prelude..:? "attachmentId")
            Prelude.<*> (x Prelude..:? "fileName")
      )

instance Prelude.Hashable AttachmentDetails

instance Prelude.NFData AttachmentDetails
