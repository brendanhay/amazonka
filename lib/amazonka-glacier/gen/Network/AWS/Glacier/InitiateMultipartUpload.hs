{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.InitiateMultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates a multipart upload. Amazon S3 Glacier creates a multipart upload resource and returns its ID in the response. The multipart upload ID is used in subsequent requests to upload parts of an archive (see 'UploadMultipartPart' ).
--
-- When you initiate a multipart upload, you specify the part size in number of bytes. The part size must be a megabyte (1024 KB) multiplied by a power of 2-for example, 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8 MB), and so on. The minimum allowable part size is 1 MB, and the maximum is 4 GB.
-- Every part you upload to this resource (see 'UploadMultipartPart' ), except the last one, must have the same size. The last one can be the same size or smaller. For example, suppose you want to upload a 16.2 MB file. If you initiate the multipart upload with a part size of 4 MB, you will upload four parts of 4 MB each and one part of 0.2 MB. 
-- After you complete the multipart upload, Amazon S3 Glacier (Glacier) removes the multipart upload resource referenced by the ID. Glacier also removes the multipart upload resource if you cancel the multipart upload or it may be removed if there is no activity for a period of 24 hours.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-archive-mpu.html Uploading Large Archives in Parts (Multipart Upload)> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-initiate-upload.html Initiate Multipart Upload> in the /Amazon Glacier Developer Guide/ .
module Network.AWS.Glacier.InitiateMultipartUpload
    (
    -- * Creating a request
      InitiateMultipartUpload (..)
    , mkInitiateMultipartUpload
    -- ** Request lenses
    , imuAccountId
    , imuVaultName
    , imuArchiveDescription
    , imuPartSize

    -- * Destructuring the response
    , InitiateMultipartUploadResponse (..)
    , mkInitiateMultipartUploadResponse
    -- ** Response lenses
    , imurrsLocation
    , imurrsUploadId
    , imurrsResponseStatus
    ) where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for initiating a multipart upload to an Amazon S3 Glacier vault.
--
-- /See:/ 'mkInitiateMultipartUpload' smart constructor.
data InitiateMultipartUpload = InitiateMultipartUpload'
  { accountId :: Core.Text
    -- ^ The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID. 
  , vaultName :: Core.Text
    -- ^ The name of the vault.
  , archiveDescription :: Core.Maybe Core.Text
    -- ^ The archive description that you are uploading in parts.
--
-- The part size must be a megabyte (1024 KB) multiplied by a power of 2, for example 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8 MB), and so on. The minimum allowable part size is 1 MB, and the maximum is 4 GB (4096 MB).
  , partSize :: Core.Maybe Core.Text
    -- ^ The size of each part except the last, in bytes. The last part can be smaller than this part size.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InitiateMultipartUpload' value with any optional fields omitted.
mkInitiateMultipartUpload
    :: Core.Text -- ^ 'accountId'
    -> Core.Text -- ^ 'vaultName'
    -> InitiateMultipartUpload
mkInitiateMultipartUpload accountId vaultName
  = InitiateMultipartUpload'{accountId, vaultName,
                             archiveDescription = Core.Nothing, partSize = Core.Nothing}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID. 
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imuAccountId :: Lens.Lens' InitiateMultipartUpload Core.Text
imuAccountId = Lens.field @"accountId"
{-# INLINEABLE imuAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imuVaultName :: Lens.Lens' InitiateMultipartUpload Core.Text
imuVaultName = Lens.field @"vaultName"
{-# INLINEABLE imuVaultName #-}
{-# DEPRECATED vaultName "Use generic-lens or generic-optics with 'vaultName' instead"  #-}

-- | The archive description that you are uploading in parts.
--
-- The part size must be a megabyte (1024 KB) multiplied by a power of 2, for example 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8 MB), and so on. The minimum allowable part size is 1 MB, and the maximum is 4 GB (4096 MB).
--
-- /Note:/ Consider using 'archiveDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imuArchiveDescription :: Lens.Lens' InitiateMultipartUpload (Core.Maybe Core.Text)
imuArchiveDescription = Lens.field @"archiveDescription"
{-# INLINEABLE imuArchiveDescription #-}
{-# DEPRECATED archiveDescription "Use generic-lens or generic-optics with 'archiveDescription' instead"  #-}

-- | The size of each part except the last, in bytes. The last part can be smaller than this part size.
--
-- /Note:/ Consider using 'partSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imuPartSize :: Lens.Lens' InitiateMultipartUpload (Core.Maybe Core.Text)
imuPartSize = Lens.field @"partSize"
{-# INLINEABLE imuPartSize #-}
{-# DEPRECATED partSize "Use generic-lens or generic-optics with 'partSize' instead"  #-}

instance Core.ToQuery InitiateMultipartUpload where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders InitiateMultipartUpload where
        toHeaders InitiateMultipartUpload{..}
          = Core.toHeaders "x-amz-archive-description" archiveDescription
              Core.<> Core.toHeaders "x-amz-part-size" partSize

instance Core.FromJSON InitiateMultipartUpload where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest InitiateMultipartUpload where
        type Rs InitiateMultipartUpload = InitiateMultipartUploadResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/" Core.<> Core.toText accountId Core.<> "/vaults/" Core.<>
                             Core.toText vaultName
                             Core.<> "/multipart-uploads",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 InitiateMultipartUploadResponse' Core.<$>
                   (Core.parseHeaderMaybe "Location" h) Core.<*>
                     Core.parseHeaderMaybe "x-amz-multipart-upload-id" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkInitiateMultipartUploadResponse' smart constructor.
data InitiateMultipartUploadResponse = InitiateMultipartUploadResponse'
  { location :: Core.Maybe Core.Text
    -- ^ The relative URI path of the multipart upload ID Amazon S3 Glacier created.
  , uploadId :: Core.Maybe Core.Text
    -- ^ The ID of the multipart upload. This value is also included as part of the location.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InitiateMultipartUploadResponse' value with any optional fields omitted.
mkInitiateMultipartUploadResponse
    :: Core.Int -- ^ 'responseStatus'
    -> InitiateMultipartUploadResponse
mkInitiateMultipartUploadResponse responseStatus
  = InitiateMultipartUploadResponse'{location = Core.Nothing,
                                     uploadId = Core.Nothing, responseStatus}

-- | The relative URI path of the multipart upload ID Amazon S3 Glacier created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imurrsLocation :: Lens.Lens' InitiateMultipartUploadResponse (Core.Maybe Core.Text)
imurrsLocation = Lens.field @"location"
{-# INLINEABLE imurrsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The ID of the multipart upload. This value is also included as part of the location.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imurrsUploadId :: Lens.Lens' InitiateMultipartUploadResponse (Core.Maybe Core.Text)
imurrsUploadId = Lens.field @"uploadId"
{-# INLINEABLE imurrsUploadId #-}
{-# DEPRECATED uploadId "Use generic-lens or generic-optics with 'uploadId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imurrsResponseStatus :: Lens.Lens' InitiateMultipartUploadResponse Core.Int
imurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE imurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
