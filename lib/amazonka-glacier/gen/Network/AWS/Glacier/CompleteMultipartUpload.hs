{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.CompleteMultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You call this operation to inform Amazon S3 Glacier (Glacier) that all the archive parts have been uploaded and that Glacier can now assemble the archive from the uploaded parts. After assembling and saving the archive to the vault, Glacier returns the URI path of the newly created archive resource. Using the URI path, you can then access the archive. After you upload an archive, you should save the archive ID returned to retrieve the archive at a later point. You can also get the vault inventory to obtain a list of archive IDs in a vault. For more information, see 'InitiateJob' .
--
-- In the request, you must include the computed SHA256 tree hash of the entire archive you have uploaded. For information about computing a SHA256 tree hash, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html Computing Checksums> . On the server side, Glacier also constructs the SHA256 tree hash of the assembled archive. If the values match, Glacier saves the archive to the vault; otherwise, it returns an error, and the operation fails. The 'ListParts' operation returns a list of parts uploaded for a specific multipart upload. It includes checksum information for each uploaded part that can be used to debug a bad checksum issue.
-- Additionally, Glacier also checks for any missing content ranges when assembling the archive, if missing content ranges are found, Glacier returns an error and the operation fails.
-- Complete Multipart Upload is an idempotent operation. After your first successful complete multipart upload, if you call the operation again within a short period, the operation will succeed and return the same archive ID. This is useful in the event you experience a network issue that causes an aborted connection or receive a 500 server error, in which case you can repeat your Complete Multipart Upload request and get the same archive ID without creating duplicate archives. Note, however, that after the multipart upload completes, you cannot call the List Parts operation and the multipart upload will not appear in List Multipart Uploads response, even if idempotent complete is possible.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-archive-mpu.html Uploading Large Archives in Parts (Multipart Upload)> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-complete-upload.html Complete Multipart Upload> in the /Amazon Glacier Developer Guide/ .
module Network.AWS.Glacier.CompleteMultipartUpload
  ( -- * Creating a request
    CompleteMultipartUpload (..),
    mkCompleteMultipartUpload,

    -- ** Request lenses
    cmuChecksum,
    cmuArchiveSize,
    cmuAccountId,
    cmuVaultName,
    cmuUploadId,

    -- * Destructuring the response
    ArchiveCreationOutput (..),
    mkArchiveCreationOutput,

    -- ** Response lenses
    acoArchiveId,
    acoChecksum,
    acoLocation,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Provides options to complete a multipart upload operation. This informs Amazon Glacier that all the archive parts have been uploaded and Amazon S3 Glacier (Glacier) can now assemble the archive from the uploaded parts. After assembling and saving the archive to the vault, Glacier returns the URI path of the newly created archive resource.
--
-- /See:/ 'mkCompleteMultipartUpload' smart constructor.
data CompleteMultipartUpload = CompleteMultipartUpload'
  { checksum ::
      Lude.Maybe Lude.Text,
    archiveSize :: Lude.Maybe Lude.Text,
    accountId :: Lude.Text,
    vaultName :: Lude.Text,
    uploadId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompleteMultipartUpload' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
-- * 'archiveSize' - The total size, in bytes, of the entire archive. This value should be the sum of all the sizes of the individual parts that you uploaded.
-- * 'checksum' - The SHA256 tree hash of the entire archive. It is the tree hash of SHA256 tree hash of the individual parts. If the value you specify in the request does not match the SHA256 tree hash of the final assembled archive as computed by Amazon S3 Glacier (Glacier), Glacier returns an error and the request fails.
-- * 'uploadId' - The upload ID of the multipart upload.
-- * 'vaultName' - The name of the vault.
mkCompleteMultipartUpload ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  -- | 'uploadId'
  Lude.Text ->
  CompleteMultipartUpload
mkCompleteMultipartUpload pAccountId_ pVaultName_ pUploadId_ =
  CompleteMultipartUpload'
    { checksum = Lude.Nothing,
      archiveSize = Lude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_,
      uploadId = pUploadId_
    }

-- | The SHA256 tree hash of the entire archive. It is the tree hash of SHA256 tree hash of the individual parts. If the value you specify in the request does not match the SHA256 tree hash of the final assembled archive as computed by Amazon S3 Glacier (Glacier), Glacier returns an error and the request fails.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuChecksum :: Lens.Lens' CompleteMultipartUpload (Lude.Maybe Lude.Text)
cmuChecksum = Lens.lens (checksum :: CompleteMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: CompleteMultipartUpload)
{-# DEPRECATED cmuChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The total size, in bytes, of the entire archive. This value should be the sum of all the sizes of the individual parts that you uploaded.
--
-- /Note:/ Consider using 'archiveSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuArchiveSize :: Lens.Lens' CompleteMultipartUpload (Lude.Maybe Lude.Text)
cmuArchiveSize = Lens.lens (archiveSize :: CompleteMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {archiveSize = a} :: CompleteMultipartUpload)
{-# DEPRECATED cmuArchiveSize "Use generic-lens or generic-optics with 'archiveSize' instead." #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuAccountId :: Lens.Lens' CompleteMultipartUpload Lude.Text
cmuAccountId = Lens.lens (accountId :: CompleteMultipartUpload -> Lude.Text) (\s a -> s {accountId = a} :: CompleteMultipartUpload)
{-# DEPRECATED cmuAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuVaultName :: Lens.Lens' CompleteMultipartUpload Lude.Text
cmuVaultName = Lens.lens (vaultName :: CompleteMultipartUpload -> Lude.Text) (\s a -> s {vaultName = a} :: CompleteMultipartUpload)
{-# DEPRECATED cmuVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The upload ID of the multipart upload.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuUploadId :: Lens.Lens' CompleteMultipartUpload Lude.Text
cmuUploadId = Lens.lens (uploadId :: CompleteMultipartUpload -> Lude.Text) (\s a -> s {uploadId = a} :: CompleteMultipartUpload)
{-# DEPRECATED cmuUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

instance Lude.AWSRequest CompleteMultipartUpload where
  type Rs CompleteMultipartUpload = ArchiveCreationOutput
  request = Req.postJSON glacierService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ArchiveCreationOutput'
            Lude.<$> (h Lude..#? "x-amz-archive-id")
            Lude.<*> (h Lude..#? "x-amz-sha256-tree-hash")
            Lude.<*> (h Lude..#? "Location")
      )

instance Lude.ToHeaders CompleteMultipartUpload where
  toHeaders CompleteMultipartUpload' {..} =
    Lude.mconcat
      [ "x-amz-sha256-tree-hash" Lude.=# checksum,
        "x-amz-archive-size" Lude.=# archiveSize
      ]

instance Lude.ToJSON CompleteMultipartUpload where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath CompleteMultipartUpload where
  toPath CompleteMultipartUpload' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/multipart-uploads/",
        Lude.toBS uploadId
      ]

instance Lude.ToQuery CompleteMultipartUpload where
  toQuery = Lude.const Lude.mempty
