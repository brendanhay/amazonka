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
-- Module      : Network.AWS.SSM.Types.AttachmentContent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AttachmentContent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.AttachmentHashType

-- | A structure that includes attributes that describe a document
-- attachment.
--
-- /See:/ 'newAttachmentContent' smart constructor.
data AttachmentContent = AttachmentContent'
  { -- | The cryptographic hash value of the document content.
    hash :: Core.Maybe Core.Text,
    -- | The name of an attachment.
    name :: Core.Maybe Core.Text,
    -- | The URL location of the attachment content.
    url :: Core.Maybe Core.Text,
    -- | The size of an attachment in bytes.
    size :: Core.Maybe Core.Integer,
    -- | The hash algorithm used to calculate the hash value.
    hashType :: Core.Maybe AttachmentHashType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachmentContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hash', 'attachmentContent_hash' - The cryptographic hash value of the document content.
--
-- 'name', 'attachmentContent_name' - The name of an attachment.
--
-- 'url', 'attachmentContent_url' - The URL location of the attachment content.
--
-- 'size', 'attachmentContent_size' - The size of an attachment in bytes.
--
-- 'hashType', 'attachmentContent_hashType' - The hash algorithm used to calculate the hash value.
newAttachmentContent ::
  AttachmentContent
newAttachmentContent =
  AttachmentContent'
    { hash = Core.Nothing,
      name = Core.Nothing,
      url = Core.Nothing,
      size = Core.Nothing,
      hashType = Core.Nothing
    }

-- | The cryptographic hash value of the document content.
attachmentContent_hash :: Lens.Lens' AttachmentContent (Core.Maybe Core.Text)
attachmentContent_hash = Lens.lens (\AttachmentContent' {hash} -> hash) (\s@AttachmentContent' {} a -> s {hash = a} :: AttachmentContent)

-- | The name of an attachment.
attachmentContent_name :: Lens.Lens' AttachmentContent (Core.Maybe Core.Text)
attachmentContent_name = Lens.lens (\AttachmentContent' {name} -> name) (\s@AttachmentContent' {} a -> s {name = a} :: AttachmentContent)

-- | The URL location of the attachment content.
attachmentContent_url :: Lens.Lens' AttachmentContent (Core.Maybe Core.Text)
attachmentContent_url = Lens.lens (\AttachmentContent' {url} -> url) (\s@AttachmentContent' {} a -> s {url = a} :: AttachmentContent)

-- | The size of an attachment in bytes.
attachmentContent_size :: Lens.Lens' AttachmentContent (Core.Maybe Core.Integer)
attachmentContent_size = Lens.lens (\AttachmentContent' {size} -> size) (\s@AttachmentContent' {} a -> s {size = a} :: AttachmentContent)

-- | The hash algorithm used to calculate the hash value.
attachmentContent_hashType :: Lens.Lens' AttachmentContent (Core.Maybe AttachmentHashType)
attachmentContent_hashType = Lens.lens (\AttachmentContent' {hashType} -> hashType) (\s@AttachmentContent' {} a -> s {hashType = a} :: AttachmentContent)

instance Core.FromJSON AttachmentContent where
  parseJSON =
    Core.withObject
      "AttachmentContent"
      ( \x ->
          AttachmentContent'
            Core.<$> (x Core..:? "Hash")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Url")
            Core.<*> (x Core..:? "Size")
            Core.<*> (x Core..:? "HashType")
      )

instance Core.Hashable AttachmentContent

instance Core.NFData AttachmentContent
