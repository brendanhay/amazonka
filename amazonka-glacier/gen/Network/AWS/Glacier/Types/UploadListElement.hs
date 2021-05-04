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
-- Module      : Network.AWS.Glacier.Types.UploadListElement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.UploadListElement where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of in-progress multipart uploads for a vault.
--
-- /See:/ 'newUploadListElement' smart constructor.
data UploadListElement = UploadListElement'
  { -- | The part size, in bytes, specified in the Initiate Multipart Upload
    -- request. This is the size of all the parts in the upload except the last
    -- part, which may be smaller than this size.
    partSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The UTC time at which the multipart upload was initiated.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the vault that contains the archive.
    vaultARN :: Prelude.Maybe Prelude.Text,
    -- | The description of the archive that was specified in the Initiate
    -- Multipart Upload request.
    archiveDescription :: Prelude.Maybe Prelude.Text,
    -- | The ID of a multipart upload.
    multipartUploadId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { partSizeInBytes =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      vaultARN = Prelude.Nothing,
      archiveDescription = Prelude.Nothing,
      multipartUploadId = Prelude.Nothing
    }

-- | The part size, in bytes, specified in the Initiate Multipart Upload
-- request. This is the size of all the parts in the upload except the last
-- part, which may be smaller than this size.
uploadListElement_partSizeInBytes :: Lens.Lens' UploadListElement (Prelude.Maybe Prelude.Integer)
uploadListElement_partSizeInBytes = Lens.lens (\UploadListElement' {partSizeInBytes} -> partSizeInBytes) (\s@UploadListElement' {} a -> s {partSizeInBytes = a} :: UploadListElement)

-- | The UTC time at which the multipart upload was initiated.
uploadListElement_creationDate :: Lens.Lens' UploadListElement (Prelude.Maybe Prelude.Text)
uploadListElement_creationDate = Lens.lens (\UploadListElement' {creationDate} -> creationDate) (\s@UploadListElement' {} a -> s {creationDate = a} :: UploadListElement)

-- | The Amazon Resource Name (ARN) of the vault that contains the archive.
uploadListElement_vaultARN :: Lens.Lens' UploadListElement (Prelude.Maybe Prelude.Text)
uploadListElement_vaultARN = Lens.lens (\UploadListElement' {vaultARN} -> vaultARN) (\s@UploadListElement' {} a -> s {vaultARN = a} :: UploadListElement)

-- | The description of the archive that was specified in the Initiate
-- Multipart Upload request.
uploadListElement_archiveDescription :: Lens.Lens' UploadListElement (Prelude.Maybe Prelude.Text)
uploadListElement_archiveDescription = Lens.lens (\UploadListElement' {archiveDescription} -> archiveDescription) (\s@UploadListElement' {} a -> s {archiveDescription = a} :: UploadListElement)

-- | The ID of a multipart upload.
uploadListElement_multipartUploadId :: Lens.Lens' UploadListElement (Prelude.Maybe Prelude.Text)
uploadListElement_multipartUploadId = Lens.lens (\UploadListElement' {multipartUploadId} -> multipartUploadId) (\s@UploadListElement' {} a -> s {multipartUploadId = a} :: UploadListElement)

instance Prelude.FromJSON UploadListElement where
  parseJSON =
    Prelude.withObject
      "UploadListElement"
      ( \x ->
          UploadListElement'
            Prelude.<$> (x Prelude..:? "PartSizeInBytes")
            Prelude.<*> (x Prelude..:? "CreationDate")
            Prelude.<*> (x Prelude..:? "VaultARN")
            Prelude.<*> (x Prelude..:? "ArchiveDescription")
            Prelude.<*> (x Prelude..:? "MultipartUploadId")
      )

instance Prelude.Hashable UploadListElement

instance Prelude.NFData UploadListElement
