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
-- Module      : Network.AWS.Glacier.Types.UploadListElement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.UploadListElement where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A list of in-progress multipart uploads for a vault.
--
-- /See:/ 'newUploadListElement' smart constructor.
data UploadListElement = UploadListElement'
  { -- | The part size, in bytes, specified in the Initiate Multipart Upload
    -- request. This is the size of all the parts in the upload except the last
    -- part, which may be smaller than this size.
    partSizeInBytes :: Core.Maybe Core.Integer,
    -- | The UTC time at which the multipart upload was initiated.
    creationDate :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the vault that contains the archive.
    vaultARN :: Core.Maybe Core.Text,
    -- | The description of the archive that was specified in the Initiate
    -- Multipart Upload request.
    archiveDescription :: Core.Maybe Core.Text,
    -- | The ID of a multipart upload.
    multipartUploadId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UploadListElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partSizeInBytes', 'uploadListElement_partSizeInBytes' - The part size, in bytes, specified in the Initiate Multipart Upload
-- request. This is the size of all the parts in the upload except the last
-- part, which may be smaller than this size.
--
-- 'creationDate', 'uploadListElement_creationDate' - The UTC time at which the multipart upload was initiated.
--
-- 'vaultARN', 'uploadListElement_vaultARN' - The Amazon Resource Name (ARN) of the vault that contains the archive.
--
-- 'archiveDescription', 'uploadListElement_archiveDescription' - The description of the archive that was specified in the Initiate
-- Multipart Upload request.
--
-- 'multipartUploadId', 'uploadListElement_multipartUploadId' - The ID of a multipart upload.
newUploadListElement ::
  UploadListElement
newUploadListElement =
  UploadListElement'
    { partSizeInBytes = Core.Nothing,
      creationDate = Core.Nothing,
      vaultARN = Core.Nothing,
      archiveDescription = Core.Nothing,
      multipartUploadId = Core.Nothing
    }

-- | The part size, in bytes, specified in the Initiate Multipart Upload
-- request. This is the size of all the parts in the upload except the last
-- part, which may be smaller than this size.
uploadListElement_partSizeInBytes :: Lens.Lens' UploadListElement (Core.Maybe Core.Integer)
uploadListElement_partSizeInBytes = Lens.lens (\UploadListElement' {partSizeInBytes} -> partSizeInBytes) (\s@UploadListElement' {} a -> s {partSizeInBytes = a} :: UploadListElement)

-- | The UTC time at which the multipart upload was initiated.
uploadListElement_creationDate :: Lens.Lens' UploadListElement (Core.Maybe Core.Text)
uploadListElement_creationDate = Lens.lens (\UploadListElement' {creationDate} -> creationDate) (\s@UploadListElement' {} a -> s {creationDate = a} :: UploadListElement)

-- | The Amazon Resource Name (ARN) of the vault that contains the archive.
uploadListElement_vaultARN :: Lens.Lens' UploadListElement (Core.Maybe Core.Text)
uploadListElement_vaultARN = Lens.lens (\UploadListElement' {vaultARN} -> vaultARN) (\s@UploadListElement' {} a -> s {vaultARN = a} :: UploadListElement)

-- | The description of the archive that was specified in the Initiate
-- Multipart Upload request.
uploadListElement_archiveDescription :: Lens.Lens' UploadListElement (Core.Maybe Core.Text)
uploadListElement_archiveDescription = Lens.lens (\UploadListElement' {archiveDescription} -> archiveDescription) (\s@UploadListElement' {} a -> s {archiveDescription = a} :: UploadListElement)

-- | The ID of a multipart upload.
uploadListElement_multipartUploadId :: Lens.Lens' UploadListElement (Core.Maybe Core.Text)
uploadListElement_multipartUploadId = Lens.lens (\UploadListElement' {multipartUploadId} -> multipartUploadId) (\s@UploadListElement' {} a -> s {multipartUploadId = a} :: UploadListElement)

instance Core.FromJSON UploadListElement where
  parseJSON =
    Core.withObject
      "UploadListElement"
      ( \x ->
          UploadListElement'
            Core.<$> (x Core..:? "PartSizeInBytes")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "VaultARN")
            Core.<*> (x Core..:? "ArchiveDescription")
            Core.<*> (x Core..:? "MultipartUploadId")
      )

instance Core.Hashable UploadListElement

instance Core.NFData UploadListElement
