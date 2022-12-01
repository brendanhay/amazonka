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
-- Module      : Amazonka.Support.Types.AttachmentDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.AttachmentDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The file name and ID of an attachment to a case communication. You can
-- use the ID to retrieve the attachment with the DescribeAttachment
-- operation.
--
-- /See:/ 'newAttachmentDetails' smart constructor.
data AttachmentDetails = AttachmentDetails'
  { -- | The file name of the attachment.
    fileName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the attachment.
    attachmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachmentDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileName', 'attachmentDetails_fileName' - The file name of the attachment.
--
-- 'attachmentId', 'attachmentDetails_attachmentId' - The ID of the attachment.
newAttachmentDetails ::
  AttachmentDetails
newAttachmentDetails =
  AttachmentDetails'
    { fileName = Prelude.Nothing,
      attachmentId = Prelude.Nothing
    }

-- | The file name of the attachment.
attachmentDetails_fileName :: Lens.Lens' AttachmentDetails (Prelude.Maybe Prelude.Text)
attachmentDetails_fileName = Lens.lens (\AttachmentDetails' {fileName} -> fileName) (\s@AttachmentDetails' {} a -> s {fileName = a} :: AttachmentDetails)

-- | The ID of the attachment.
attachmentDetails_attachmentId :: Lens.Lens' AttachmentDetails (Prelude.Maybe Prelude.Text)
attachmentDetails_attachmentId = Lens.lens (\AttachmentDetails' {attachmentId} -> attachmentId) (\s@AttachmentDetails' {} a -> s {attachmentId = a} :: AttachmentDetails)

instance Core.FromJSON AttachmentDetails where
  parseJSON =
    Core.withObject
      "AttachmentDetails"
      ( \x ->
          AttachmentDetails'
            Prelude.<$> (x Core..:? "fileName")
            Prelude.<*> (x Core..:? "attachmentId")
      )

instance Prelude.Hashable AttachmentDetails where
  hashWithSalt _salt AttachmentDetails' {..} =
    _salt `Prelude.hashWithSalt` fileName
      `Prelude.hashWithSalt` attachmentId

instance Prelude.NFData AttachmentDetails where
  rnf AttachmentDetails' {..} =
    Prelude.rnf fileName
      `Prelude.seq` Prelude.rnf attachmentId
