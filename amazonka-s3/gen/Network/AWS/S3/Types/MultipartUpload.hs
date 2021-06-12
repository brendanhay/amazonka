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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Initiator
import Network.AWS.S3.Types.Owner
import Network.AWS.S3.Types.StorageClass

-- | Container for the @MultipartUpload@ for the Amazon S3 object.
--
-- /See:/ 'newMultipartUpload' smart constructor.
data MultipartUpload = MultipartUpload'
  { -- | Key of the object for which the multipart upload was initiated.
    key :: Core.Maybe ObjectKey,
    -- | Upload ID that identifies the multipart upload.
    uploadId :: Core.Maybe Core.Text,
    -- | The class of storage used to store the object.
    storageClass :: Core.Maybe StorageClass,
    -- | Date and time at which the multipart upload was initiated.
    initiated :: Core.Maybe Core.ISO8601,
    -- | Specifies the owner of the object that is part of the multipart upload.
    owner :: Core.Maybe Owner,
    -- | Identifies who initiated the multipart upload.
    initiator :: Core.Maybe Initiator
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { key = Core.Nothing,
      uploadId = Core.Nothing,
      storageClass = Core.Nothing,
      initiated = Core.Nothing,
      owner = Core.Nothing,
      initiator = Core.Nothing
    }

-- | Key of the object for which the multipart upload was initiated.
multipartUpload_key :: Lens.Lens' MultipartUpload (Core.Maybe ObjectKey)
multipartUpload_key = Lens.lens (\MultipartUpload' {key} -> key) (\s@MultipartUpload' {} a -> s {key = a} :: MultipartUpload)

-- | Upload ID that identifies the multipart upload.
multipartUpload_uploadId :: Lens.Lens' MultipartUpload (Core.Maybe Core.Text)
multipartUpload_uploadId = Lens.lens (\MultipartUpload' {uploadId} -> uploadId) (\s@MultipartUpload' {} a -> s {uploadId = a} :: MultipartUpload)

-- | The class of storage used to store the object.
multipartUpload_storageClass :: Lens.Lens' MultipartUpload (Core.Maybe StorageClass)
multipartUpload_storageClass = Lens.lens (\MultipartUpload' {storageClass} -> storageClass) (\s@MultipartUpload' {} a -> s {storageClass = a} :: MultipartUpload)

-- | Date and time at which the multipart upload was initiated.
multipartUpload_initiated :: Lens.Lens' MultipartUpload (Core.Maybe Core.UTCTime)
multipartUpload_initiated = Lens.lens (\MultipartUpload' {initiated} -> initiated) (\s@MultipartUpload' {} a -> s {initiated = a} :: MultipartUpload) Core.. Lens.mapping Core._Time

-- | Specifies the owner of the object that is part of the multipart upload.
multipartUpload_owner :: Lens.Lens' MultipartUpload (Core.Maybe Owner)
multipartUpload_owner = Lens.lens (\MultipartUpload' {owner} -> owner) (\s@MultipartUpload' {} a -> s {owner = a} :: MultipartUpload)

-- | Identifies who initiated the multipart upload.
multipartUpload_initiator :: Lens.Lens' MultipartUpload (Core.Maybe Initiator)
multipartUpload_initiator = Lens.lens (\MultipartUpload' {initiator} -> initiator) (\s@MultipartUpload' {} a -> s {initiator = a} :: MultipartUpload)

instance Core.FromXML MultipartUpload where
  parseXML x =
    MultipartUpload'
      Core.<$> (x Core..@? "Key")
      Core.<*> (x Core..@? "UploadId")
      Core.<*> (x Core..@? "StorageClass")
      Core.<*> (x Core..@? "Initiated")
      Core.<*> (x Core..@? "Owner")
      Core.<*> (x Core..@? "Initiator")

instance Core.Hashable MultipartUpload

instance Core.NFData MultipartUpload
