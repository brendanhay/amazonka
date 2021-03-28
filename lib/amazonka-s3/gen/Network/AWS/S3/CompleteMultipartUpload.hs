{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.CompleteMultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Completes a multipart upload by assembling previously uploaded parts.
--
-- You first initiate the multipart upload and then upload all parts using the <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart> operation. After successfully uploading all relevant parts of an upload, you call this operation to complete the upload. Upon receiving this request, Amazon S3 concatenates all the parts in ascending order by part number to create a new object. In the Complete Multipart Upload request, you must provide the parts list. You must ensure that the parts list is complete. This operation concatenates the parts that you provide in the list. For each part in the list, you must provide the part number and the @ETag@ value, returned after that part was uploaded.
-- Processing of a Complete Multipart Upload request could take several minutes to complete. After Amazon S3 begins processing the request, it sends an HTTP response header that specifies a 200 OK response. While processing is in progress, Amazon S3 periodically sends white space characters to keep the connection from timing out. Because a request could fail after the initial 200 OK response has been sent, it is important that you check the response body to determine whether the request succeeded.
-- Note that if @CompleteMultipartUpload@ fails, applications should be prepared to retry the failed requests. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ErrorBestPractices.html Amazon S3 Error Best Practices> .
-- For more information about multipart uploads, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/uploadobjusingmpu.html Uploading Objects Using Multipart Upload> .
-- For information about permissions required to use the multipart upload API, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions> .
-- @CompleteMultipartUpload@ has the following special errors:
--
--     * Error code: @EntityTooSmall@ 
--
--     * Description: Your proposed upload is smaller than the minimum allowed object size. Each part must be at least 5 MB in size, except the last part.
--
--
--     * 400 Bad Request
--
--
--
--
--     * Error code: @InvalidPart@ 
--
--     * Description: One or more of the specified parts could not be found. The part might not have been uploaded, or the specified entity tag might not have matched the part's entity tag.
--
--
--     * 400 Bad Request
--
--
--
--
--     * Error code: @InvalidPartOrder@ 
--
--     * Description: The list of parts was not in ascending order. The parts list must be specified in order by part number.
--
--
--     * 400 Bad Request
--
--
--
--
--     * Error code: @NoSuchUpload@ 
--
--     * Description: The specified multipart upload does not exist. The upload ID might be invalid, or the multipart upload might have been aborted or completed.
--
--
--     * 404 Not Found
--
--
--
--
-- The following operations are related to @CompleteMultipartUpload@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads> 
--
--
module Network.AWS.S3.CompleteMultipartUpload
    (
    -- * Creating a request
      CompleteMultipartUpload (..)
    , mkCompleteMultipartUpload
    -- ** Request lenses
    , cBucket
    , cKey
    , cUploadId
    , cExpectedBucketOwner
    , cMultipartUpload
    , cRequestPayer

    -- * Destructuring the response
    , CompleteMultipartUploadResponse (..)
    , mkCompleteMultipartUploadResponse
    -- ** Response lenses
    , crsBucket
    , crsETag
    , crsExpiration
    , crsKey
    , crsLocation
    , crsRequestCharged
    , crsSSEKMSKeyId
    , crsServerSideEncryption
    , crsVersionId
    , crsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkCompleteMultipartUpload' smart constructor.
data CompleteMultipartUpload = CompleteMultipartUpload'
  { bucket :: Types.BucketName
    -- ^ Name of the bucket to which the multipart upload was initiated.
  , key :: Types.Key
    -- ^ Object key for which the multipart upload was initiated.
  , uploadId :: Types.UploadId
    -- ^ ID for the initiated multipart upload.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  , multipartUpload :: Core.Maybe Types.CompletedMultipartUpload
    -- ^ The container for the multipart upload request information.
  , requestPayer :: Core.Maybe Types.RequestPayer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompleteMultipartUpload' value with any optional fields omitted.
mkCompleteMultipartUpload
    :: Types.BucketName -- ^ 'bucket'
    -> Types.Key -- ^ 'key'
    -> Types.UploadId -- ^ 'uploadId'
    -> CompleteMultipartUpload
mkCompleteMultipartUpload bucket key uploadId
  = CompleteMultipartUpload'{bucket, key, uploadId,
                             expectedBucketOwner = Core.Nothing, multipartUpload = Core.Nothing,
                             requestPayer = Core.Nothing}

-- | Name of the bucket to which the multipart upload was initiated.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cBucket :: Lens.Lens' CompleteMultipartUpload Types.BucketName
cBucket = Lens.field @"bucket"
{-# INLINEABLE cBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Object key for which the multipart upload was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cKey :: Lens.Lens' CompleteMultipartUpload Types.Key
cKey = Lens.field @"key"
{-# INLINEABLE cKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | ID for the initiated multipart upload.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cUploadId :: Lens.Lens' CompleteMultipartUpload Types.UploadId
cUploadId = Lens.field @"uploadId"
{-# INLINEABLE cUploadId #-}
{-# DEPRECATED uploadId "Use generic-lens or generic-optics with 'uploadId' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExpectedBucketOwner :: Lens.Lens' CompleteMultipartUpload (Core.Maybe Types.ExpectedBucketOwner)
cExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE cExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

-- | The container for the multipart upload request information.
--
-- /Note:/ Consider using 'multipartUpload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMultipartUpload :: Lens.Lens' CompleteMultipartUpload (Core.Maybe Types.CompletedMultipartUpload)
cMultipartUpload = Lens.field @"multipartUpload"
{-# INLINEABLE cMultipartUpload #-}
{-# DEPRECATED multipartUpload "Use generic-lens or generic-optics with 'multipartUpload' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRequestPayer :: Lens.Lens' CompleteMultipartUpload (Core.Maybe Types.RequestPayer)
cRequestPayer = Lens.field @"requestPayer"
{-# INLINEABLE cRequestPayer #-}
{-# DEPRECATED requestPayer "Use generic-lens or generic-optics with 'requestPayer' instead"  #-}

instance Core.ToQuery CompleteMultipartUpload where
        toQuery CompleteMultipartUpload{..}
          = Core.toQueryPair "uploadId" uploadId

instance Core.ToHeaders CompleteMultipartUpload where
        toHeaders CompleteMultipartUpload{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
              Core.<> Core.toHeaders "x-amz-request-payer" requestPayer

instance Core.AWSRequest CompleteMultipartUpload where
        type Rs CompleteMultipartUpload = CompleteMultipartUploadResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/" Core.<> Core.toText bucket Core.<> "/" Core.<> Core.toText key,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CompleteMultipartUploadResponse' Core.<$>
                   (x Core..@? "Bucket") Core.<*> x Core..@? "ETag" Core.<*>
                     Core.parseHeaderMaybe "x-amz-expiration" h
                     Core.<*> x Core..@? "Key"
                     Core.<*> x Core..@? "Location"
                     Core.<*> Core.parseHeaderMaybe "x-amz-request-charged" h
                     Core.<*>
                     Core.parseHeaderMaybe "x-amz-server-side-encryption-aws-kms-key-id"
                       h
                     Core.<*> Core.parseHeaderMaybe "x-amz-server-side-encryption" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-version-id" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCompleteMultipartUploadResponse' smart constructor.
data CompleteMultipartUploadResponse = CompleteMultipartUploadResponse'
  { bucket :: Core.Maybe Types.BucketName
    -- ^ The name of the bucket that contains the newly created object.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
  , eTag :: Core.Maybe Types.ETag
    -- ^ Entity tag that identifies the newly created object's data. Objects with different object data will have different entity tags. The entity tag is an opaque string. The entity tag may or may not be an MD5 digest of the object data. If the entity tag is not an MD5 digest of the object data, it will contain one or more nonhexadecimal characters and/or will consist of less than 32 or more than 32 hexadecimal digits.
  , expiration :: Core.Maybe Types.Expiration
    -- ^ If the object expiration is configured, this will contain the expiration date (expiry-date) and rule ID (rule-id). The value of rule-id is URL encoded.
  , key :: Core.Maybe Types.Key
    -- ^ The object key of the newly created object.
  , location :: Core.Maybe Types.Location
    -- ^ The URI that identifies the newly created object.
  , requestCharged :: Core.Maybe Types.RequestCharged
  , sSEKMSKeyId :: Core.Maybe Types.SSEKMSKeyId
    -- ^ If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
  , serverSideEncryption :: Core.Maybe Types.ServerSideEncryption
    -- ^ If you specified server-side encryption either with an Amazon S3-managed encryption key or an AWS KMS customer master key (CMK) in your initiate multipart upload request, the response includes this header. It confirms the encryption algorithm that Amazon S3 used to encrypt the object.
  , versionId :: Core.Maybe Types.ObjectVersionId
    -- ^ Version ID of the newly created object, in case the bucket has versioning turned on.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompleteMultipartUploadResponse' value with any optional fields omitted.
mkCompleteMultipartUploadResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CompleteMultipartUploadResponse
mkCompleteMultipartUploadResponse responseStatus
  = CompleteMultipartUploadResponse'{bucket = Core.Nothing,
                                     eTag = Core.Nothing, expiration = Core.Nothing,
                                     key = Core.Nothing, location = Core.Nothing,
                                     requestCharged = Core.Nothing, sSEKMSKeyId = Core.Nothing,
                                     serverSideEncryption = Core.Nothing, versionId = Core.Nothing,
                                     responseStatus}

-- | The name of the bucket that contains the newly created object.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsBucket :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe Types.BucketName)
crsBucket = Lens.field @"bucket"
{-# INLINEABLE crsBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Entity tag that identifies the newly created object's data. Objects with different object data will have different entity tags. The entity tag is an opaque string. The entity tag may or may not be an MD5 digest of the object data. If the entity tag is not an MD5 digest of the object data, it will contain one or more nonhexadecimal characters and/or will consist of less than 32 or more than 32 hexadecimal digits.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsETag :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe Types.ETag)
crsETag = Lens.field @"eTag"
{-# INLINEABLE crsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | If the object expiration is configured, this will contain the expiration date (expiry-date) and rule ID (rule-id). The value of rule-id is URL encoded.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsExpiration :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe Types.Expiration)
crsExpiration = Lens.field @"expiration"
{-# INLINEABLE crsExpiration #-}
{-# DEPRECATED expiration "Use generic-lens or generic-optics with 'expiration' instead"  #-}

-- | The object key of the newly created object.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsKey :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe Types.Key)
crsKey = Lens.field @"key"
{-# INLINEABLE crsKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The URI that identifies the newly created object.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLocation :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe Types.Location)
crsLocation = Lens.field @"location"
{-# INLINEABLE crsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsRequestCharged :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe Types.RequestCharged)
crsRequestCharged = Lens.field @"requestCharged"
{-# INLINEABLE crsRequestCharged #-}
{-# DEPRECATED requestCharged "Use generic-lens or generic-optics with 'requestCharged' instead"  #-}

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsSSEKMSKeyId :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe Types.SSEKMSKeyId)
crsSSEKMSKeyId = Lens.field @"sSEKMSKeyId"
{-# INLINEABLE crsSSEKMSKeyId #-}
{-# DEPRECATED sSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead"  #-}

-- | If you specified server-side encryption either with an Amazon S3-managed encryption key or an AWS KMS customer master key (CMK) in your initiate multipart upload request, the response includes this header. It confirms the encryption algorithm that Amazon S3 used to encrypt the object.
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsServerSideEncryption :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe Types.ServerSideEncryption)
crsServerSideEncryption = Lens.field @"serverSideEncryption"
{-# INLINEABLE crsServerSideEncryption #-}
{-# DEPRECATED serverSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead"  #-}

-- | Version ID of the newly created object, in case the bucket has versioning turned on.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsVersionId :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe Types.ObjectVersionId)
crsVersionId = Lens.field @"versionId"
{-# INLINEABLE crsVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CompleteMultipartUploadResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
