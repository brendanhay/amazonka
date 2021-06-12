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
-- Module      : Network.AWS.Glacier.UploadMultipartPart
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation uploads a part of an archive. You can upload archive
-- parts in any order. You can also upload them in parallel. You can upload
-- up to 10,000 parts for a multipart upload.
--
-- Amazon Glacier rejects your upload part request if any of the following
-- conditions is true:
--
-- -   __SHA256 tree hash does not match__To ensure that part data is not
--     corrupted in transmission, you compute a SHA256 tree hash of the
--     part and include it in your request. Upon receiving the part data,
--     Amazon S3 Glacier also computes a SHA256 tree hash. If these hash
--     values don\'t match, the operation fails. For information about
--     computing a SHA256 tree hash, see
--     <https://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html Computing Checksums>.
--
-- -   __Part size does not match__The size of each part except the last
--     must match the size specified in the corresponding
--     InitiateMultipartUpload request. The size of the last part must be
--     the same size as, or smaller than, the specified size.
--
--     If you upload a part whose size is smaller than the part size you
--     specified in your initiate multipart upload request and that part is
--     not the last part, then the upload part request will succeed.
--     However, the subsequent Complete Multipart Upload request will fail.
--
-- -   __Range does not align__The byte range value in the request does not
--     align with the part size specified in the corresponding initiate
--     request. For example, if you specify a part size of 4194304 bytes (4
--     MB), then 0 to 4194303 bytes (4 MB - 1) and 4194304 (4 MB) to
--     8388607 (8 MB - 1) are valid part ranges. However, if you set a
--     range value of 2 MB to 6 MB, the range does not align with the part
--     size and the upload will fail.
--
-- This operation is idempotent. If you upload the same part multiple
-- times, the data included in the most recent request overwrites the
-- previously uploaded data.
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
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-upload-part.html Upload Part>
-- in the /Amazon Glacier Developer Guide/.
module Network.AWS.Glacier.UploadMultipartPart
  ( -- * Creating a Request
    UploadMultipartPart (..),
    newUploadMultipartPart,

    -- * Request Lenses
    uploadMultipartPart_accountId,
    uploadMultipartPart_vaultName,
    uploadMultipartPart_uploadId,
    uploadMultipartPart_range,
    uploadMultipartPart_checksum,
    uploadMultipartPart_body,

    -- * Destructuring the Response
    UploadMultipartPartResponse (..),
    newUploadMultipartPartResponse,

    -- * Response Lenses
    uploadMultipartPartResponse_checksum,
    uploadMultipartPartResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options to upload a part of an archive in a multipart upload
-- operation.
--
-- /See:/ 'newUploadMultipartPart' smart constructor.
data UploadMultipartPart = UploadMultipartPart'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Core.Text,
    -- | The name of the vault.
    vaultName :: Core.Text,
    -- | The upload ID of the multipart upload.
    uploadId :: Core.Text,
    -- | Identifies the range of bytes in the assembled archive that will be
    -- uploaded in this part. Amazon S3 Glacier uses this information to
    -- assemble the archive in the proper sequence. The format of this header
    -- follows RFC 2616. An example header is Content-Range:bytes 0-4194303\/*.
    range :: Core.Text,
    -- | The SHA256 tree hash of the data being uploaded.
    checksum :: Core.Text,
    -- | The data to upload.
    body :: Core.HashedBody
  }
  deriving (Core.Show, Core.Generic)

-- |
-- Create a value of 'UploadMultipartPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'uploadMultipartPart_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'uploadMultipartPart_vaultName' - The name of the vault.
--
-- 'uploadId', 'uploadMultipartPart_uploadId' - The upload ID of the multipart upload.
--
-- 'range', 'uploadMultipartPart_range' - Identifies the range of bytes in the assembled archive that will be
-- uploaded in this part. Amazon S3 Glacier uses this information to
-- assemble the archive in the proper sequence. The format of this header
-- follows RFC 2616. An example header is Content-Range:bytes 0-4194303\/*.
--
-- 'checksum', 'uploadMultipartPart_checksum' - The SHA256 tree hash of the data being uploaded.
--
-- 'body', 'uploadMultipartPart_body' - The data to upload.
newUploadMultipartPart ::
  -- | 'accountId'
  Core.Text ->
  -- | 'vaultName'
  Core.Text ->
  -- | 'uploadId'
  Core.Text ->
  -- | 'range'
  Core.Text ->
  -- | 'checksum'
  Core.Text ->
  -- | 'body'
  Core.HashedBody ->
  UploadMultipartPart
newUploadMultipartPart
  pAccountId_
  pVaultName_
  pUploadId_
  pRange_
  pChecksum_
  pBody_ =
    UploadMultipartPart'
      { accountId = pAccountId_,
        vaultName = pVaultName_,
        uploadId = pUploadId_,
        range = pRange_,
        checksum = pChecksum_,
        body = pBody_
      }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
uploadMultipartPart_accountId :: Lens.Lens' UploadMultipartPart Core.Text
uploadMultipartPart_accountId = Lens.lens (\UploadMultipartPart' {accountId} -> accountId) (\s@UploadMultipartPart' {} a -> s {accountId = a} :: UploadMultipartPart)

-- | The name of the vault.
uploadMultipartPart_vaultName :: Lens.Lens' UploadMultipartPart Core.Text
uploadMultipartPart_vaultName = Lens.lens (\UploadMultipartPart' {vaultName} -> vaultName) (\s@UploadMultipartPart' {} a -> s {vaultName = a} :: UploadMultipartPart)

-- | The upload ID of the multipart upload.
uploadMultipartPart_uploadId :: Lens.Lens' UploadMultipartPart Core.Text
uploadMultipartPart_uploadId = Lens.lens (\UploadMultipartPart' {uploadId} -> uploadId) (\s@UploadMultipartPart' {} a -> s {uploadId = a} :: UploadMultipartPart)

-- | Identifies the range of bytes in the assembled archive that will be
-- uploaded in this part. Amazon S3 Glacier uses this information to
-- assemble the archive in the proper sequence. The format of this header
-- follows RFC 2616. An example header is Content-Range:bytes 0-4194303\/*.
uploadMultipartPart_range :: Lens.Lens' UploadMultipartPart Core.Text
uploadMultipartPart_range = Lens.lens (\UploadMultipartPart' {range} -> range) (\s@UploadMultipartPart' {} a -> s {range = a} :: UploadMultipartPart)

-- | The SHA256 tree hash of the data being uploaded.
uploadMultipartPart_checksum :: Lens.Lens' UploadMultipartPart Core.Text
uploadMultipartPart_checksum = Lens.lens (\UploadMultipartPart' {checksum} -> checksum) (\s@UploadMultipartPart' {} a -> s {checksum = a} :: UploadMultipartPart)

-- | The data to upload.
uploadMultipartPart_body :: Lens.Lens' UploadMultipartPart Core.HashedBody
uploadMultipartPart_body = Lens.lens (\UploadMultipartPart' {body} -> body) (\s@UploadMultipartPart' {} a -> s {body = a} :: UploadMultipartPart)

instance Core.AWSRequest UploadMultipartPart where
  type
    AWSResponse UploadMultipartPart =
      UploadMultipartPartResponse
  request =
    Request.glacierVersionHeader (Core._serviceVersion defaultService)
      Core.. Request.putBody defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UploadMultipartPartResponse'
            Core.<$> (h Core..#? "x-amz-sha256-tree-hash")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.ToBody UploadMultipartPart where
  toBody UploadMultipartPart' {..} = Core.toBody body

instance Core.ToHeaders UploadMultipartPart where
  toHeaders UploadMultipartPart' {..} =
    Core.mconcat
      [ "Content-Range" Core.=# range,
        "x-amz-sha256-tree-hash" Core.=# checksum
      ]

instance Core.ToPath UploadMultipartPart where
  toPath UploadMultipartPart' {..} =
    Core.mconcat
      [ "/",
        Core.toBS accountId,
        "/vaults/",
        Core.toBS vaultName,
        "/multipart-uploads/",
        Core.toBS uploadId
      ]

instance Core.ToQuery UploadMultipartPart where
  toQuery = Core.const Core.mempty

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'newUploadMultipartPartResponse' smart constructor.
data UploadMultipartPartResponse = UploadMultipartPartResponse'
  { -- | The SHA256 tree hash that Amazon S3 Glacier computed for the uploaded
    -- part.
    checksum :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UploadMultipartPartResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksum', 'uploadMultipartPartResponse_checksum' - The SHA256 tree hash that Amazon S3 Glacier computed for the uploaded
-- part.
--
-- 'httpStatus', 'uploadMultipartPartResponse_httpStatus' - The response's http status code.
newUploadMultipartPartResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UploadMultipartPartResponse
newUploadMultipartPartResponse pHttpStatus_ =
  UploadMultipartPartResponse'
    { checksum =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The SHA256 tree hash that Amazon S3 Glacier computed for the uploaded
-- part.
uploadMultipartPartResponse_checksum :: Lens.Lens' UploadMultipartPartResponse (Core.Maybe Core.Text)
uploadMultipartPartResponse_checksum = Lens.lens (\UploadMultipartPartResponse' {checksum} -> checksum) (\s@UploadMultipartPartResponse' {} a -> s {checksum = a} :: UploadMultipartPartResponse)

-- | The response's http status code.
uploadMultipartPartResponse_httpStatus :: Lens.Lens' UploadMultipartPartResponse Core.Int
uploadMultipartPartResponse_httpStatus = Lens.lens (\UploadMultipartPartResponse' {httpStatus} -> httpStatus) (\s@UploadMultipartPartResponse' {} a -> s {httpStatus = a} :: UploadMultipartPartResponse)

instance Core.NFData UploadMultipartPartResponse
