{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.AbortMultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation aborts a multipart upload identified by the upload ID.
--
-- After the Abort Multipart Upload request succeeds, you cannot upload any more parts to the multipart upload or complete the multipart upload. Aborting a completed upload fails. However, aborting an already-aborted upload will succeed, for a short time. For more information about uploading a part and completing a multipart upload, see 'UploadMultipartPart' and 'CompleteMultipartUpload' .
-- This operation is idempotent.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon S3 Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-abort-upload.html Abort Multipart Upload> in the /Amazon Glacier Developer Guide/ .
module Network.AWS.Glacier.AbortMultipartUpload
  ( -- * Creating a request
    AbortMultipartUpload (..),
    mkAbortMultipartUpload,

    -- ** Request lenses
    amuAccountId,
    amuVaultName,
    amuUploadId,

    -- * Destructuring the response
    AbortMultipartUploadResponse (..),
    mkAbortMultipartUploadResponse,
  )
where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options to abort a multipart upload identified by the upload ID.
--
-- For information about the underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-abort-upload.html Abort Multipart Upload> . For conceptual information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon S3 Glacier> .
--
-- /See:/ 'mkAbortMultipartUpload' smart constructor.
data AbortMultipartUpload = AbortMultipartUpload'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Types.String,
    -- | The name of the vault.
    vaultName :: Types.String,
    -- | The upload ID of the multipart upload to delete.
    uploadId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AbortMultipartUpload' value with any optional fields omitted.
mkAbortMultipartUpload ::
  -- | 'accountId'
  Types.String ->
  -- | 'vaultName'
  Types.String ->
  -- | 'uploadId'
  Types.String ->
  AbortMultipartUpload
mkAbortMultipartUpload accountId vaultName uploadId =
  AbortMultipartUpload' {accountId, vaultName, uploadId}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amuAccountId :: Lens.Lens' AbortMultipartUpload Types.String
amuAccountId = Lens.field @"accountId"
{-# DEPRECATED amuAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amuVaultName :: Lens.Lens' AbortMultipartUpload Types.String
amuVaultName = Lens.field @"vaultName"
{-# DEPRECATED amuVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The upload ID of the multipart upload to delete.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amuUploadId :: Lens.Lens' AbortMultipartUpload Types.String
amuUploadId = Lens.field @"uploadId"
{-# DEPRECATED amuUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

instance Core.AWSRequest AbortMultipartUpload where
  type Rs AbortMultipartUpload = AbortMultipartUploadResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText accountId) Core.<> ("/vaults/")
                Core.<> (Core.toText vaultName)
                Core.<> ("/multipart-uploads/")
                Core.<> (Core.toText uploadId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response = Response.receiveNull AbortMultipartUploadResponse'

-- | /See:/ 'mkAbortMultipartUploadResponse' smart constructor.
data AbortMultipartUploadResponse = AbortMultipartUploadResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AbortMultipartUploadResponse' value with any optional fields omitted.
mkAbortMultipartUploadResponse ::
  AbortMultipartUploadResponse
mkAbortMultipartUploadResponse = AbortMultipartUploadResponse'
