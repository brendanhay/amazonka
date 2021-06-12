{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.InitiateMultipartUpload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates a multipart upload. Amazon S3 Glacier creates a
-- multipart upload resource and returns its ID in the response. The
-- multipart upload ID is used in subsequent requests to upload parts of an
-- archive (see UploadMultipartPart).
--
-- When you initiate a multipart upload, you specify the part size in
-- number of bytes. The part size must be a megabyte (1024 KB) multiplied
-- by a power of 2-for example, 1048576 (1 MB), 2097152 (2 MB), 4194304 (4
-- MB), 8388608 (8 MB), and so on. The minimum allowable part size is 1 MB,
-- and the maximum is 4 GB.
--
-- Every part you upload to this resource (see UploadMultipartPart), except
-- the last one, must have the same size. The last one can be the same size
-- or smaller. For example, suppose you want to upload a 16.2 MB file. If
-- you initiate the multipart upload with a part size of 4 MB, you will
-- upload four parts of 4 MB each and one part of 0.2 MB.
--
-- You don\'t need to know the size of the archive when you start a
-- multipart upload because Amazon S3 Glacier does not require you to
-- specify the overall archive size.
--
-- After you complete the multipart upload, Amazon S3 Glacier (Glacier)
-- removes the multipart upload resource referenced by the ID. Glacier also
-- removes the multipart upload resource if you cancel the multipart upload
-- or it may be removed if there is no activity for a period of 24 hours.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-archive-mpu.html Uploading Large Archives in Parts (Multipart Upload)>
-- and
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-initiate-upload.html Initiate Multipart Upload>
-- in the /Amazon Glacier Developer Guide/.
module Network.AWS.Glacier.InitiateMultipartUpload
  ( -- * Creating a Request
    InitiateMultipartUpload (..),
    newInitiateMultipartUpload,

    -- * Request Lenses
    initiateMultipartUpload_archiveDescription,
    initiateMultipartUpload_accountId,
    initiateMultipartUpload_vaultName,
    initiateMultipartUpload_partSize,

    -- * Destructuring the Response
    InitiateMultipartUploadResponse (..),
    newInitiateMultipartUploadResponse,

    -- * Response Lenses
    initiateMultipartUploadResponse_uploadId,
    initiateMultipartUploadResponse_location,
    initiateMultipartUploadResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for initiating a multipart upload to an Amazon S3
-- Glacier vault.
--
-- /See:/ 'newInitiateMultipartUpload' smart constructor.
data InitiateMultipartUpload = InitiateMultipartUpload'
  { -- | The archive description that you are uploading in parts.
    --
    -- The part size must be a megabyte (1024 KB) multiplied by a power of 2,
    -- for example 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8
    -- MB), and so on. The minimum allowable part size is 1 MB, and the maximum
    -- is 4 GB (4096 MB).
    archiveDescription :: Core.Maybe Core.Text,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Core.Text,
    -- | The name of the vault.
    vaultName :: Core.Text,
    -- | The size of each part except the last, in bytes. The last part can be
    -- smaller than this part size.
    partSize :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InitiateMultipartUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archiveDescription', 'initiateMultipartUpload_archiveDescription' - The archive description that you are uploading in parts.
--
-- The part size must be a megabyte (1024 KB) multiplied by a power of 2,
-- for example 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8
-- MB), and so on. The minimum allowable part size is 1 MB, and the maximum
-- is 4 GB (4096 MB).
--
-- 'accountId', 'initiateMultipartUpload_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'initiateMultipartUpload_vaultName' - The name of the vault.
--
-- 'partSize', 'initiateMultipartUpload_partSize' - The size of each part except the last, in bytes. The last part can be
-- smaller than this part size.
newInitiateMultipartUpload ::
  -- | 'accountId'
  Core.Text ->
  -- | 'vaultName'
  Core.Text ->
  -- | 'partSize'
  Core.Text ->
  InitiateMultipartUpload
newInitiateMultipartUpload
  pAccountId_
  pVaultName_
  pPartSize_ =
    InitiateMultipartUpload'
      { archiveDescription =
          Core.Nothing,
        accountId = pAccountId_,
        vaultName = pVaultName_,
        partSize = pPartSize_
      }

-- | The archive description that you are uploading in parts.
--
-- The part size must be a megabyte (1024 KB) multiplied by a power of 2,
-- for example 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8
-- MB), and so on. The minimum allowable part size is 1 MB, and the maximum
-- is 4 GB (4096 MB).
initiateMultipartUpload_archiveDescription :: Lens.Lens' InitiateMultipartUpload (Core.Maybe Core.Text)
initiateMultipartUpload_archiveDescription = Lens.lens (\InitiateMultipartUpload' {archiveDescription} -> archiveDescription) (\s@InitiateMultipartUpload' {} a -> s {archiveDescription = a} :: InitiateMultipartUpload)

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
initiateMultipartUpload_accountId :: Lens.Lens' InitiateMultipartUpload Core.Text
initiateMultipartUpload_accountId = Lens.lens (\InitiateMultipartUpload' {accountId} -> accountId) (\s@InitiateMultipartUpload' {} a -> s {accountId = a} :: InitiateMultipartUpload)

-- | The name of the vault.
initiateMultipartUpload_vaultName :: Lens.Lens' InitiateMultipartUpload Core.Text
initiateMultipartUpload_vaultName = Lens.lens (\InitiateMultipartUpload' {vaultName} -> vaultName) (\s@InitiateMultipartUpload' {} a -> s {vaultName = a} :: InitiateMultipartUpload)

-- | The size of each part except the last, in bytes. The last part can be
-- smaller than this part size.
initiateMultipartUpload_partSize :: Lens.Lens' InitiateMultipartUpload Core.Text
initiateMultipartUpload_partSize = Lens.lens (\InitiateMultipartUpload' {partSize} -> partSize) (\s@InitiateMultipartUpload' {} a -> s {partSize = a} :: InitiateMultipartUpload)

instance Core.AWSRequest InitiateMultipartUpload where
  type
    AWSResponse InitiateMultipartUpload =
      InitiateMultipartUploadResponse
  request =
    Request.glacierVersionHeader (Core._serviceVersion defaultService)
      Core.. Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          InitiateMultipartUploadResponse'
            Core.<$> (h Core..#? "x-amz-multipart-upload-id")
            Core.<*> (h Core..#? "Location")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable InitiateMultipartUpload

instance Core.NFData InitiateMultipartUpload

instance Core.ToHeaders InitiateMultipartUpload where
  toHeaders InitiateMultipartUpload' {..} =
    Core.mconcat
      [ "x-amz-archive-description"
          Core.=# archiveDescription,
        "x-amz-part-size" Core.=# partSize
      ]

instance Core.ToJSON InitiateMultipartUpload where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath InitiateMultipartUpload where
  toPath InitiateMultipartUpload' {..} =
    Core.mconcat
      [ "/",
        Core.toBS accountId,
        "/vaults/",
        Core.toBS vaultName,
        "/multipart-uploads"
      ]

instance Core.ToQuery InitiateMultipartUpload where
  toQuery = Core.const Core.mempty

-- | The Amazon S3 Glacier response to your request.
--
-- /See:/ 'newInitiateMultipartUploadResponse' smart constructor.
data InitiateMultipartUploadResponse = InitiateMultipartUploadResponse'
  { -- | The ID of the multipart upload. This value is also included as part of
    -- the location.
    uploadId :: Core.Maybe Core.Text,
    -- | The relative URI path of the multipart upload ID Amazon S3 Glacier
    -- created.
    location :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InitiateMultipartUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uploadId', 'initiateMultipartUploadResponse_uploadId' - The ID of the multipart upload. This value is also included as part of
-- the location.
--
-- 'location', 'initiateMultipartUploadResponse_location' - The relative URI path of the multipart upload ID Amazon S3 Glacier
-- created.
--
-- 'httpStatus', 'initiateMultipartUploadResponse_httpStatus' - The response's http status code.
newInitiateMultipartUploadResponse ::
  -- | 'httpStatus'
  Core.Int ->
  InitiateMultipartUploadResponse
newInitiateMultipartUploadResponse pHttpStatus_ =
  InitiateMultipartUploadResponse'
    { uploadId =
        Core.Nothing,
      location = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the multipart upload. This value is also included as part of
-- the location.
initiateMultipartUploadResponse_uploadId :: Lens.Lens' InitiateMultipartUploadResponse (Core.Maybe Core.Text)
initiateMultipartUploadResponse_uploadId = Lens.lens (\InitiateMultipartUploadResponse' {uploadId} -> uploadId) (\s@InitiateMultipartUploadResponse' {} a -> s {uploadId = a} :: InitiateMultipartUploadResponse)

-- | The relative URI path of the multipart upload ID Amazon S3 Glacier
-- created.
initiateMultipartUploadResponse_location :: Lens.Lens' InitiateMultipartUploadResponse (Core.Maybe Core.Text)
initiateMultipartUploadResponse_location = Lens.lens (\InitiateMultipartUploadResponse' {location} -> location) (\s@InitiateMultipartUploadResponse' {} a -> s {location = a} :: InitiateMultipartUploadResponse)

-- | The response's http status code.
initiateMultipartUploadResponse_httpStatus :: Lens.Lens' InitiateMultipartUploadResponse Core.Int
initiateMultipartUploadResponse_httpStatus = Lens.lens (\InitiateMultipartUploadResponse' {httpStatus} -> httpStatus) (\s@InitiateMultipartUploadResponse' {} a -> s {httpStatus = a} :: InitiateMultipartUploadResponse)

instance Core.NFData InitiateMultipartUploadResponse
