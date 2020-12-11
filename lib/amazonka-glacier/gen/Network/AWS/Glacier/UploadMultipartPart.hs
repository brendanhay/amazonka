{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.UploadMultipartPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation uploads a part of an archive. You can upload archive parts in any order. You can also upload them in parallel. You can upload up to 10,000 parts for a multipart upload.
--
-- Amazon Glacier rejects your upload part request if any of the following conditions is true:
--
--     * __SHA256 tree hash does not match__ To ensure that part data is not corrupted in transmission, you compute a SHA256 tree hash of the part and include it in your request. Upon receiving the part data, Amazon S3 Glacier also computes a SHA256 tree hash. If these hash values don't match, the operation fails. For information about computing a SHA256 tree hash, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html Computing Checksums> .
--
--
--     * __Part size does not match__ The size of each part except the last must match the size specified in the corresponding 'InitiateMultipartUpload' request. The size of the last part must be the same size as, or smaller than, the specified size.
--
--
--     * __Range does not align__ The byte range value in the request does not align with the part size specified in the corresponding initiate request. For example, if you specify a part size of 4194304 bytes (4 MB), then 0 to 4194303 bytes (4 MB - 1) and 4194304 (4 MB) to 8388607 (8 MB - 1) are valid part ranges. However, if you set a range value of 2 MB to 6 MB, the range does not align with the part size and the upload will fail.
--
--
-- This operation is idempotent. If you upload the same part multiple times, the data included in the most recent request overwrites the previously uploaded data.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-archive-mpu.html Uploading Large Archives in Parts (Multipart Upload)> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-upload-part.html Upload Part > in the /Amazon Glacier Developer Guide/ .
module Network.AWS.Glacier.UploadMultipartPart
  ( -- * Creating a request
    UploadMultipartPart (..),
    mkUploadMultipartPart,

    -- ** Request lenses
    umpChecksum,
    umpRange,
    umpAccountId,
    umpVaultName,
    umpUploadId,
    umpBody,

    -- * Destructuring the response
    UploadMultipartPartResponse (..),
    mkUploadMultipartPartResponse,

    -- ** Response lenses
    umprsChecksum,
    umprsResponseStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Provides options to upload a part of an archive in a multipart upload operation.
--
-- /See:/ 'mkUploadMultipartPart' smart constructor.
data UploadMultipartPart = UploadMultipartPart'
  { checksum ::
      Lude.Maybe Lude.Text,
    range :: Lude.Maybe Lude.Text,
    accountId :: Lude.Text,
    vaultName :: Lude.Text,
    uploadId :: Lude.Text,
    body :: Lude.HashedBody
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'UploadMultipartPart' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
-- * 'body' - The data to upload.
-- * 'checksum' - The SHA256 tree hash of the data being uploaded.
-- * 'range' - Identifies the range of bytes in the assembled archive that will be uploaded in this part. Amazon S3 Glacier uses this information to assemble the archive in the proper sequence. The format of this header follows RFC 2616. An example header is Content-Range:bytes 0-4194303/*.
-- * 'uploadId' - The upload ID of the multipart upload.
-- * 'vaultName' - The name of the vault.
mkUploadMultipartPart ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  -- | 'uploadId'
  Lude.Text ->
  -- | 'body'
  Lude.HashedBody ->
  UploadMultipartPart
mkUploadMultipartPart pAccountId_ pVaultName_ pUploadId_ pBody_ =
  UploadMultipartPart'
    { checksum = Lude.Nothing,
      range = Lude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_,
      uploadId = pUploadId_,
      body = pBody_
    }

-- | The SHA256 tree hash of the data being uploaded.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umpChecksum :: Lens.Lens' UploadMultipartPart (Lude.Maybe Lude.Text)
umpChecksum = Lens.lens (checksum :: UploadMultipartPart -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: UploadMultipartPart)
{-# DEPRECATED umpChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | Identifies the range of bytes in the assembled archive that will be uploaded in this part. Amazon S3 Glacier uses this information to assemble the archive in the proper sequence. The format of this header follows RFC 2616. An example header is Content-Range:bytes 0-4194303/*.
--
-- /Note:/ Consider using 'range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umpRange :: Lens.Lens' UploadMultipartPart (Lude.Maybe Lude.Text)
umpRange = Lens.lens (range :: UploadMultipartPart -> Lude.Maybe Lude.Text) (\s a -> s {range = a} :: UploadMultipartPart)
{-# DEPRECATED umpRange "Use generic-lens or generic-optics with 'range' instead." #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umpAccountId :: Lens.Lens' UploadMultipartPart Lude.Text
umpAccountId = Lens.lens (accountId :: UploadMultipartPart -> Lude.Text) (\s a -> s {accountId = a} :: UploadMultipartPart)
{-# DEPRECATED umpAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umpVaultName :: Lens.Lens' UploadMultipartPart Lude.Text
umpVaultName = Lens.lens (vaultName :: UploadMultipartPart -> Lude.Text) (\s a -> s {vaultName = a} :: UploadMultipartPart)
{-# DEPRECATED umpVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The upload ID of the multipart upload.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umpUploadId :: Lens.Lens' UploadMultipartPart Lude.Text
umpUploadId = Lens.lens (uploadId :: UploadMultipartPart -> Lude.Text) (\s a -> s {uploadId = a} :: UploadMultipartPart)
{-# DEPRECATED umpUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The data to upload.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umpBody :: Lens.Lens' UploadMultipartPart Lude.HashedBody
umpBody = Lens.lens (body :: UploadMultipartPart -> Lude.HashedBody) (\s a -> s {body = a} :: UploadMultipartPart)
{-# DEPRECATED umpBody "Use generic-lens or generic-optics with 'body' instead." #-}

instance Lude.AWSRequest UploadMultipartPart where
  type Rs UploadMultipartPart = UploadMultipartPartResponse
  request = Req.putBody glacierService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UploadMultipartPartResponse'
            Lude.<$> (h Lude..#? "x-amz-sha256-tree-hash")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToBody UploadMultipartPart where
  toBody = Lude.toBody Lude.. body

instance Lude.ToHeaders UploadMultipartPart where
  toHeaders UploadMultipartPart' {..} =
    Lude.mconcat
      [ "x-amz-sha256-tree-hash" Lude.=# checksum,
        "Content-Range" Lude.=# range
      ]

instance Lude.ToPath UploadMultipartPart where
  toPath UploadMultipartPart' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/multipart-uploads/",
        Lude.toBS uploadId
      ]

instance Lude.ToQuery UploadMultipartPart where
  toQuery = Lude.const Lude.mempty

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkUploadMultipartPartResponse' smart constructor.
data UploadMultipartPartResponse = UploadMultipartPartResponse'
  { checksum ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UploadMultipartPartResponse' with the minimum fields required to make a request.
--
-- * 'checksum' - The SHA256 tree hash that Amazon S3 Glacier computed for the uploaded part.
-- * 'responseStatus' - The response status code.
mkUploadMultipartPartResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UploadMultipartPartResponse
mkUploadMultipartPartResponse pResponseStatus_ =
  UploadMultipartPartResponse'
    { checksum = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The SHA256 tree hash that Amazon S3 Glacier computed for the uploaded part.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umprsChecksum :: Lens.Lens' UploadMultipartPartResponse (Lude.Maybe Lude.Text)
umprsChecksum = Lens.lens (checksum :: UploadMultipartPartResponse -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: UploadMultipartPartResponse)
{-# DEPRECATED umprsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umprsResponseStatus :: Lens.Lens' UploadMultipartPartResponse Lude.Int
umprsResponseStatus = Lens.lens (responseStatus :: UploadMultipartPartResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UploadMultipartPartResponse)
{-# DEPRECATED umprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
