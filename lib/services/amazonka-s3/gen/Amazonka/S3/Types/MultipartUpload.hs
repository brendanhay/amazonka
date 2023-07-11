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
-- Module      : Amazonka.S3.Types.MultipartUpload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.MultipartUpload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ChecksumAlgorithm
import Amazonka.S3.Types.Initiator
import Amazonka.S3.Types.Owner
import Amazonka.S3.Types.StorageClass

-- | Container for the @MultipartUpload@ for the Amazon S3 object.
--
-- /See:/ 'newMultipartUpload' smart constructor.
data MultipartUpload = MultipartUpload'
  { -- | The algorithm that was used to create a checksum of the object.
    checksumAlgorithm :: Prelude.Maybe ChecksumAlgorithm,
    -- | Date and time at which the multipart upload was initiated.
    initiated :: Prelude.Maybe Data.ISO8601,
    -- | Identifies who initiated the multipart upload.
    initiator :: Prelude.Maybe Initiator,
    -- | Key of the object for which the multipart upload was initiated.
    key :: Prelude.Maybe ObjectKey,
    -- | Specifies the owner of the object that is part of the multipart upload.
    owner :: Prelude.Maybe Owner,
    -- | The class of storage used to store the object.
    storageClass :: Prelude.Maybe StorageClass,
    -- | Upload ID that identifies the multipart upload.
    uploadId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultipartUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'multipartUpload_checksumAlgorithm' - The algorithm that was used to create a checksum of the object.
--
-- 'initiated', 'multipartUpload_initiated' - Date and time at which the multipart upload was initiated.
--
-- 'initiator', 'multipartUpload_initiator' - Identifies who initiated the multipart upload.
--
-- 'key', 'multipartUpload_key' - Key of the object for which the multipart upload was initiated.
--
-- 'owner', 'multipartUpload_owner' - Specifies the owner of the object that is part of the multipart upload.
--
-- 'storageClass', 'multipartUpload_storageClass' - The class of storage used to store the object.
--
-- 'uploadId', 'multipartUpload_uploadId' - Upload ID that identifies the multipart upload.
newMultipartUpload ::
  MultipartUpload
newMultipartUpload =
  MultipartUpload'
    { checksumAlgorithm =
        Prelude.Nothing,
      initiated = Prelude.Nothing,
      initiator = Prelude.Nothing,
      key = Prelude.Nothing,
      owner = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      uploadId = Prelude.Nothing
    }

-- | The algorithm that was used to create a checksum of the object.
multipartUpload_checksumAlgorithm :: Lens.Lens' MultipartUpload (Prelude.Maybe ChecksumAlgorithm)
multipartUpload_checksumAlgorithm = Lens.lens (\MultipartUpload' {checksumAlgorithm} -> checksumAlgorithm) (\s@MultipartUpload' {} a -> s {checksumAlgorithm = a} :: MultipartUpload)

-- | Date and time at which the multipart upload was initiated.
multipartUpload_initiated :: Lens.Lens' MultipartUpload (Prelude.Maybe Prelude.UTCTime)
multipartUpload_initiated = Lens.lens (\MultipartUpload' {initiated} -> initiated) (\s@MultipartUpload' {} a -> s {initiated = a} :: MultipartUpload) Prelude.. Lens.mapping Data._Time

-- | Identifies who initiated the multipart upload.
multipartUpload_initiator :: Lens.Lens' MultipartUpload (Prelude.Maybe Initiator)
multipartUpload_initiator = Lens.lens (\MultipartUpload' {initiator} -> initiator) (\s@MultipartUpload' {} a -> s {initiator = a} :: MultipartUpload)

-- | Key of the object for which the multipart upload was initiated.
multipartUpload_key :: Lens.Lens' MultipartUpload (Prelude.Maybe ObjectKey)
multipartUpload_key = Lens.lens (\MultipartUpload' {key} -> key) (\s@MultipartUpload' {} a -> s {key = a} :: MultipartUpload)

-- | Specifies the owner of the object that is part of the multipart upload.
multipartUpload_owner :: Lens.Lens' MultipartUpload (Prelude.Maybe Owner)
multipartUpload_owner = Lens.lens (\MultipartUpload' {owner} -> owner) (\s@MultipartUpload' {} a -> s {owner = a} :: MultipartUpload)

-- | The class of storage used to store the object.
multipartUpload_storageClass :: Lens.Lens' MultipartUpload (Prelude.Maybe StorageClass)
multipartUpload_storageClass = Lens.lens (\MultipartUpload' {storageClass} -> storageClass) (\s@MultipartUpload' {} a -> s {storageClass = a} :: MultipartUpload)

-- | Upload ID that identifies the multipart upload.
multipartUpload_uploadId :: Lens.Lens' MultipartUpload (Prelude.Maybe Prelude.Text)
multipartUpload_uploadId = Lens.lens (\MultipartUpload' {uploadId} -> uploadId) (\s@MultipartUpload' {} a -> s {uploadId = a} :: MultipartUpload)

instance Data.FromXML MultipartUpload where
  parseXML x =
    MultipartUpload'
      Prelude.<$> (x Data..@? "ChecksumAlgorithm")
      Prelude.<*> (x Data..@? "Initiated")
      Prelude.<*> (x Data..@? "Initiator")
      Prelude.<*> (x Data..@? "Key")
      Prelude.<*> (x Data..@? "Owner")
      Prelude.<*> (x Data..@? "StorageClass")
      Prelude.<*> (x Data..@? "UploadId")

instance Prelude.Hashable MultipartUpload where
  hashWithSalt _salt MultipartUpload' {..} =
    _salt
      `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` initiated
      `Prelude.hashWithSalt` initiator
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` storageClass
      `Prelude.hashWithSalt` uploadId

instance Prelude.NFData MultipartUpload where
  rnf MultipartUpload' {..} =
    Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf initiated
      `Prelude.seq` Prelude.rnf initiator
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf storageClass
      `Prelude.seq` Prelude.rnf uploadId
