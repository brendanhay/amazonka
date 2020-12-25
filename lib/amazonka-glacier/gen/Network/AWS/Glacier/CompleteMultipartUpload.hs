{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cmuAccountId,
    cmuVaultName,
    cmuUploadId,
    cmuArchiveSize,
    cmuChecksum,

    -- * Destructuring the response
    Types.ArchiveCreationOutput (..),
    Types.mkArchiveCreationOutput,

    -- ** Response lenses
    Types.acoArchiveId,
    Types.acoChecksum,
    Types.acoLocation,
  )
where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options to complete a multipart upload operation. This informs Amazon Glacier that all the archive parts have been uploaded and Amazon S3 Glacier (Glacier) can now assemble the archive from the uploaded parts. After assembling and saving the archive to the vault, Glacier returns the URI path of the newly created archive resource.
--
-- /See:/ 'mkCompleteMultipartUpload' smart constructor.
data CompleteMultipartUpload = CompleteMultipartUpload'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Types.AccountId,
    -- | The name of the vault.
    vaultName :: Types.VaultName,
    -- | The upload ID of the multipart upload.
    uploadId :: Types.UploadId,
    -- | The total size, in bytes, of the entire archive. This value should be the sum of all the sizes of the individual parts that you uploaded.
    archiveSize :: Core.Maybe Types.ArchiveSize,
    -- | The SHA256 tree hash of the entire archive. It is the tree hash of SHA256 tree hash of the individual parts. If the value you specify in the request does not match the SHA256 tree hash of the final assembled archive as computed by Amazon S3 Glacier (Glacier), Glacier returns an error and the request fails.
    checksum :: Core.Maybe Types.Checksum
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompleteMultipartUpload' value with any optional fields omitted.
mkCompleteMultipartUpload ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'vaultName'
  Types.VaultName ->
  -- | 'uploadId'
  Types.UploadId ->
  CompleteMultipartUpload
mkCompleteMultipartUpload accountId vaultName uploadId =
  CompleteMultipartUpload'
    { accountId,
      vaultName,
      uploadId,
      archiveSize = Core.Nothing,
      checksum = Core.Nothing
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuAccountId :: Lens.Lens' CompleteMultipartUpload Types.AccountId
cmuAccountId = Lens.field @"accountId"
{-# DEPRECATED cmuAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuVaultName :: Lens.Lens' CompleteMultipartUpload Types.VaultName
cmuVaultName = Lens.field @"vaultName"
{-# DEPRECATED cmuVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The upload ID of the multipart upload.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuUploadId :: Lens.Lens' CompleteMultipartUpload Types.UploadId
cmuUploadId = Lens.field @"uploadId"
{-# DEPRECATED cmuUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The total size, in bytes, of the entire archive. This value should be the sum of all the sizes of the individual parts that you uploaded.
--
-- /Note:/ Consider using 'archiveSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuArchiveSize :: Lens.Lens' CompleteMultipartUpload (Core.Maybe Types.ArchiveSize)
cmuArchiveSize = Lens.field @"archiveSize"
{-# DEPRECATED cmuArchiveSize "Use generic-lens or generic-optics with 'archiveSize' instead." #-}

-- | The SHA256 tree hash of the entire archive. It is the tree hash of SHA256 tree hash of the individual parts. If the value you specify in the request does not match the SHA256 tree hash of the final assembled archive as computed by Amazon S3 Glacier (Glacier), Glacier returns an error and the request fails.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuChecksum :: Lens.Lens' CompleteMultipartUpload (Core.Maybe Types.Checksum)
cmuChecksum = Lens.field @"checksum"
{-# DEPRECATED cmuChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

instance Core.FromJSON CompleteMultipartUpload where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest CompleteMultipartUpload where
  type Rs CompleteMultipartUpload = Types.ArchiveCreationOutput
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText accountId) Core.<> ("/vaults/")
                Core.<> (Core.toText vaultName)
                Core.<> ("/multipart-uploads/")
                Core.<> (Core.toText uploadId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-archive-size" archiveSize
            Core.<> (Core.toHeaders "x-amz-sha256-tree-hash" checksum),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ArchiveCreationOutput'
            Core.<$> (Core.parseHeaderMaybe "x-amz-archive-id" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-sha256-tree-hash" h)
            Core.<*> (Core.parseHeaderMaybe "Location" h)
      )
