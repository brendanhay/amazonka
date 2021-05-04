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
-- Module      : Network.AWS.S3.Types.MultipartUpload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MultipartUpload where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Initiator
import Network.AWS.S3.Types.Owner
import Network.AWS.S3.Types.StorageClass

-- | Container for the @MultipartUpload@ for the Amazon S3 object.
--
-- /See:/ 'newMultipartUpload' smart constructor.
data MultipartUpload = MultipartUpload'
  { -- | Key of the object for which the multipart upload was initiated.
    key :: Prelude.Maybe ObjectKey,
    -- | Upload ID that identifies the multipart upload.
    uploadId :: Prelude.Maybe Prelude.Text,
    -- | The class of storage used to store the object.
    storageClass :: Prelude.Maybe StorageClass,
    -- | Date and time at which the multipart upload was initiated.
    initiated :: Prelude.Maybe Prelude.ISO8601,
    -- | Specifies the owner of the object that is part of the multipart upload.
    owner :: Prelude.Maybe Owner,
    -- | Identifies who initiated the multipart upload.
    initiator :: Prelude.Maybe Initiator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MultipartUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'multipartUpload_key' - Key of the object for which the multipart upload was initiated.
--
-- 'uploadId', 'multipartUpload_uploadId' - Upload ID that identifies the multipart upload.
--
-- 'storageClass', 'multipartUpload_storageClass' - The class of storage used to store the object.
--
-- 'initiated', 'multipartUpload_initiated' - Date and time at which the multipart upload was initiated.
--
-- 'owner', 'multipartUpload_owner' - Specifies the owner of the object that is part of the multipart upload.
--
-- 'initiator', 'multipartUpload_initiator' - Identifies who initiated the multipart upload.
newMultipartUpload ::
  MultipartUpload
newMultipartUpload =
  MultipartUpload'
    { key = Prelude.Nothing,
      uploadId = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      initiated = Prelude.Nothing,
      owner = Prelude.Nothing,
      initiator = Prelude.Nothing
    }

-- | Key of the object for which the multipart upload was initiated.
multipartUpload_key :: Lens.Lens' MultipartUpload (Prelude.Maybe ObjectKey)
multipartUpload_key = Lens.lens (\MultipartUpload' {key} -> key) (\s@MultipartUpload' {} a -> s {key = a} :: MultipartUpload)

-- | Upload ID that identifies the multipart upload.
multipartUpload_uploadId :: Lens.Lens' MultipartUpload (Prelude.Maybe Prelude.Text)
multipartUpload_uploadId = Lens.lens (\MultipartUpload' {uploadId} -> uploadId) (\s@MultipartUpload' {} a -> s {uploadId = a} :: MultipartUpload)

-- | The class of storage used to store the object.
multipartUpload_storageClass :: Lens.Lens' MultipartUpload (Prelude.Maybe StorageClass)
multipartUpload_storageClass = Lens.lens (\MultipartUpload' {storageClass} -> storageClass) (\s@MultipartUpload' {} a -> s {storageClass = a} :: MultipartUpload)

-- | Date and time at which the multipart upload was initiated.
multipartUpload_initiated :: Lens.Lens' MultipartUpload (Prelude.Maybe Prelude.UTCTime)
multipartUpload_initiated = Lens.lens (\MultipartUpload' {initiated} -> initiated) (\s@MultipartUpload' {} a -> s {initiated = a} :: MultipartUpload) Prelude.. Lens.mapping Prelude._Time

-- | Specifies the owner of the object that is part of the multipart upload.
multipartUpload_owner :: Lens.Lens' MultipartUpload (Prelude.Maybe Owner)
multipartUpload_owner = Lens.lens (\MultipartUpload' {owner} -> owner) (\s@MultipartUpload' {} a -> s {owner = a} :: MultipartUpload)

-- | Identifies who initiated the multipart upload.
multipartUpload_initiator :: Lens.Lens' MultipartUpload (Prelude.Maybe Initiator)
multipartUpload_initiator = Lens.lens (\MultipartUpload' {initiator} -> initiator) (\s@MultipartUpload' {} a -> s {initiator = a} :: MultipartUpload)

instance Prelude.FromXML MultipartUpload where
  parseXML x =
    MultipartUpload'
      Prelude.<$> (x Prelude..@? "Key")
      Prelude.<*> (x Prelude..@? "UploadId")
      Prelude.<*> (x Prelude..@? "StorageClass")
      Prelude.<*> (x Prelude..@? "Initiated")
      Prelude.<*> (x Prelude..@? "Owner")
      Prelude.<*> (x Prelude..@? "Initiator")

instance Prelude.Hashable MultipartUpload

instance Prelude.NFData MultipartUpload
