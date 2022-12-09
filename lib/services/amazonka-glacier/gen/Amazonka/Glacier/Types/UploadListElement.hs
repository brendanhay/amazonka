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
-- Module      : Amazonka.Glacier.Types.UploadListElement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.UploadListElement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of in-progress multipart uploads for a vault.
--
-- /See:/ 'newUploadListElement' smart constructor.
data UploadListElement = UploadListElement'
  { -- | The description of the archive that was specified in the Initiate
    -- Multipart Upload request.
    archiveDescription :: Prelude.Maybe Prelude.Text,
    -- | The UTC time at which the multipart upload was initiated.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | The ID of a multipart upload.
    multipartUploadId :: Prelude.Maybe Prelude.Text,
    -- | The part size, in bytes, specified in the Initiate Multipart Upload
    -- request. This is the size of all the parts in the upload except the last
    -- part, which may be smaller than this size.
    partSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Resource Name (ARN) of the vault that contains the archive.
    vaultARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadListElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archiveDescription', 'uploadListElement_archiveDescription' - The description of the archive that was specified in the Initiate
-- Multipart Upload request.
--
-- 'creationDate', 'uploadListElement_creationDate' - The UTC time at which the multipart upload was initiated.
--
-- 'multipartUploadId', 'uploadListElement_multipartUploadId' - The ID of a multipart upload.
--
-- 'partSizeInBytes', 'uploadListElement_partSizeInBytes' - The part size, in bytes, specified in the Initiate Multipart Upload
-- request. This is the size of all the parts in the upload except the last
-- part, which may be smaller than this size.
--
-- 'vaultARN', 'uploadListElement_vaultARN' - The Amazon Resource Name (ARN) of the vault that contains the archive.
newUploadListElement ::
  UploadListElement
newUploadListElement =
  UploadListElement'
    { archiveDescription =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      multipartUploadId = Prelude.Nothing,
      partSizeInBytes = Prelude.Nothing,
      vaultARN = Prelude.Nothing
    }

-- | The description of the archive that was specified in the Initiate
-- Multipart Upload request.
uploadListElement_archiveDescription :: Lens.Lens' UploadListElement (Prelude.Maybe Prelude.Text)
uploadListElement_archiveDescription = Lens.lens (\UploadListElement' {archiveDescription} -> archiveDescription) (\s@UploadListElement' {} a -> s {archiveDescription = a} :: UploadListElement)

-- | The UTC time at which the multipart upload was initiated.
uploadListElement_creationDate :: Lens.Lens' UploadListElement (Prelude.Maybe Prelude.Text)
uploadListElement_creationDate = Lens.lens (\UploadListElement' {creationDate} -> creationDate) (\s@UploadListElement' {} a -> s {creationDate = a} :: UploadListElement)

-- | The ID of a multipart upload.
uploadListElement_multipartUploadId :: Lens.Lens' UploadListElement (Prelude.Maybe Prelude.Text)
uploadListElement_multipartUploadId = Lens.lens (\UploadListElement' {multipartUploadId} -> multipartUploadId) (\s@UploadListElement' {} a -> s {multipartUploadId = a} :: UploadListElement)

-- | The part size, in bytes, specified in the Initiate Multipart Upload
-- request. This is the size of all the parts in the upload except the last
-- part, which may be smaller than this size.
uploadListElement_partSizeInBytes :: Lens.Lens' UploadListElement (Prelude.Maybe Prelude.Integer)
uploadListElement_partSizeInBytes = Lens.lens (\UploadListElement' {partSizeInBytes} -> partSizeInBytes) (\s@UploadListElement' {} a -> s {partSizeInBytes = a} :: UploadListElement)

-- | The Amazon Resource Name (ARN) of the vault that contains the archive.
uploadListElement_vaultARN :: Lens.Lens' UploadListElement (Prelude.Maybe Prelude.Text)
uploadListElement_vaultARN = Lens.lens (\UploadListElement' {vaultARN} -> vaultARN) (\s@UploadListElement' {} a -> s {vaultARN = a} :: UploadListElement)

instance Data.FromJSON UploadListElement where
  parseJSON =
    Data.withObject
      "UploadListElement"
      ( \x ->
          UploadListElement'
            Prelude.<$> (x Data..:? "ArchiveDescription")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "MultipartUploadId")
            Prelude.<*> (x Data..:? "PartSizeInBytes")
            Prelude.<*> (x Data..:? "VaultARN")
      )

instance Prelude.Hashable UploadListElement where
  hashWithSalt _salt UploadListElement' {..} =
    _salt `Prelude.hashWithSalt` archiveDescription
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` multipartUploadId
      `Prelude.hashWithSalt` partSizeInBytes
      `Prelude.hashWithSalt` vaultARN

instance Prelude.NFData UploadListElement where
  rnf UploadListElement' {..} =
    Prelude.rnf archiveDescription
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf multipartUploadId
      `Prelude.seq` Prelude.rnf partSizeInBytes
      `Prelude.seq` Prelude.rnf vaultARN
