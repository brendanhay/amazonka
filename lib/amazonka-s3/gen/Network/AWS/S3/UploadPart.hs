{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.UploadPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a part in a multipart upload.
--
-- You must initiate a multipart upload (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload> ) before you can upload any part. In response to your initiate request, Amazon S3 returns an upload ID, a unique identifier, that you must include in your upload part request.
-- Part numbers can be any number from 1 to 10,000, inclusive. A part number uniquely identifies a part and also defines its position within the object being created. If you upload a new part using the same part number that was used with a previous part, the previously uploaded part is overwritten. Each part must be at least 5 MB in size, except the last part. There is no size limit on the last part of your multipart upload.
-- To ensure that data is not corrupted when traversing the network, specify the @Content-MD5@ header in the upload part request. Amazon S3 checks the part data against the provided MD5 value. If they do not match, Amazon S3 returns an error. 
-- If the upload request is signed with Signature Version 4, then AWS S3 uses the @x-amz-content-sha256@ header as a checksum instead of @Content-MD5@ . For more information see <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-auth-using-authorization-header.html Authenticating Requests: Using the Authorization Header (AWS Signature Version 4)> . 
-- __Note:__ After you initiate multipart upload and upload one or more parts, you must either complete or abort multipart upload in order to stop getting charged for storage of the uploaded parts. Only after you either complete or abort multipart upload, Amazon S3 frees up the parts storage and stops charging you for the parts storage.
-- For more information on multipart uploads, go to <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html Multipart Upload Overview> in the /Amazon Simple Storage Service Developer Guide / .
-- For information on the permissions required to use the multipart upload API, go to <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions> in the /Amazon Simple Storage Service Developer Guide/ .
-- You can optionally request server-side encryption where Amazon S3 encrypts your data as it writes it to disks in its data centers and decrypts it for you when you access it. You have the option of providing your own encryption key, or you can use the AWS managed encryption keys. If you choose to provide your own encryption key, the request headers you provide in the request must match the headers you used in the request to initiate the upload by using <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload> . For more information, go to <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Using Server-Side Encryption> in the /Amazon Simple Storage Service Developer Guide/ .
-- Server-side encryption is supported by the S3 Multipart Upload actions. Unless you are using a customer-provided encryption key, you don't need to specify the encryption parameters in each UploadPart request. Instead, you only need to specify the server-side encryption parameters in the initial Initiate Multipart request. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload> .
-- If you requested server-side encryption using a customer-provided encryption key in your initiate multipart upload request, you must provide identical encryption information in each part upload using the following headers.
--
--     * x-amz-server-side-encryption-customer-algorithm
--
--
--     * x-amz-server-side-encryption-customer-key
--
--
--     * x-amz-server-side-encryption-customer-key-MD5
--
--
-- __Special Errors__ 
--
--     * 
--     * /Code: NoSuchUpload/ 
--
--
--     * /Cause: The specified multipart upload does not exist. The upload ID might be invalid, or the multipart upload might have been aborted or completed./ 
--
--
--     * /HTTP Status Code: 404 Not Found / 
--
--
--     * /SOAP Fault Code Prefix: Client/ 
--
--
--
--
-- __Related Resources__ 
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload> 
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
module Network.AWS.S3.UploadPart
    (
    -- * Creating a request
      UploadPart (..)
    , mkUploadPart
    -- ** Request lenses
    , upBucket
    , upKey
    , upPartNumber
    , upUploadId
    , upBody
    , upContentLength
    , upContentMD5
    , upExpectedBucketOwner
    , upRequestPayer
    , upSSECustomerAlgorithm
    , upSSECustomerKey
    , upSSECustomerKeyMD5

    -- * Destructuring the response
    , UploadPartResponse (..)
    , mkUploadPartResponse
    -- ** Response lenses
    , uprrsETag
    , uprrsRequestCharged
    , uprrsSSECustomerAlgorithm
    , uprrsSSECustomerKeyMD5
    , uprrsSSEKMSKeyId
    , uprrsServerSideEncryption
    , uprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkUploadPart' smart constructor.
data UploadPart = UploadPart'
  { bucket :: Types.BucketName
    -- ^ The name of the bucket to which the multipart upload was initiated.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
  , key :: Types.Key
    -- ^ Object key for which the multipart upload was initiated.
  , partNumber :: Core.Int
    -- ^ Part number of part being uploaded. This is a positive integer between 1 and 10,000.
  , uploadId :: Types.UploadId
    -- ^ Upload ID identifying the multipart upload whose part is being uploaded.
  , body :: Core.RqBody
    -- ^ Object data.
  , contentLength :: Core.Maybe Core.Integer
    -- ^ Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically.
  , contentMD5 :: Core.Maybe Types.ContentMD5
    -- ^ The base64-encoded 128-bit MD5 digest of the part data. This parameter is auto-populated when using the command from the CLI. This parameter is required if object lock parameters are specified.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  , requestPayer :: Core.Maybe Types.RequestPayer
  , sSECustomerAlgorithm :: Core.Maybe Types.SSECustomerAlgorithm
    -- ^ Specifies the algorithm to use to when encrypting the object (for example, AES256).
  , sSECustomerKey :: Core.Maybe Types.SSECustomerKey
    -- ^ Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm header@ . This must be the same encryption key specified in the initiate multipart upload request.
  , sSECustomerKeyMD5 :: Core.Maybe Types.SSECustomerKeyMD5
    -- ^ Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
  }
  deriving stock (Core.Show, Core.Generic)

-- | Creates a 'UploadPart' value with any optional fields omitted.
mkUploadPart
    :: Types.BucketName -- ^ 'bucket'
    -> Types.Key -- ^ 'key'
    -> Core.Int -- ^ 'partNumber'
    -> Types.UploadId -- ^ 'uploadId'
    -> Core.RqBody -- ^ 'body'
    -> UploadPart
mkUploadPart bucket key partNumber uploadId body
  = UploadPart'{bucket, key, partNumber, uploadId, body,
                contentLength = Core.Nothing, contentMD5 = Core.Nothing,
                expectedBucketOwner = Core.Nothing, requestPayer = Core.Nothing,
                sSECustomerAlgorithm = Core.Nothing, sSECustomerKey = Core.Nothing,
                sSECustomerKeyMD5 = Core.Nothing}

-- | The name of the bucket to which the multipart upload was initiated.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upBucket :: Lens.Lens' UploadPart Types.BucketName
upBucket = Lens.field @"bucket"
{-# INLINEABLE upBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Object key for which the multipart upload was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upKey :: Lens.Lens' UploadPart Types.Key
upKey = Lens.field @"key"
{-# INLINEABLE upKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | Part number of part being uploaded. This is a positive integer between 1 and 10,000.
--
-- /Note:/ Consider using 'partNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPartNumber :: Lens.Lens' UploadPart Core.Int
upPartNumber = Lens.field @"partNumber"
{-# INLINEABLE upPartNumber #-}
{-# DEPRECATED partNumber "Use generic-lens or generic-optics with 'partNumber' instead"  #-}

-- | Upload ID identifying the multipart upload whose part is being uploaded.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upUploadId :: Lens.Lens' UploadPart Types.UploadId
upUploadId = Lens.field @"uploadId"
{-# INLINEABLE upUploadId #-}
{-# DEPRECATED uploadId "Use generic-lens or generic-optics with 'uploadId' instead"  #-}

-- | Object data.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upBody :: Lens.Lens' UploadPart Core.RqBody
upBody = Lens.field @"body"
{-# INLINEABLE upBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically.
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upContentLength :: Lens.Lens' UploadPart (Core.Maybe Core.Integer)
upContentLength = Lens.field @"contentLength"
{-# INLINEABLE upContentLength #-}
{-# DEPRECATED contentLength "Use generic-lens or generic-optics with 'contentLength' instead"  #-}

-- | The base64-encoded 128-bit MD5 digest of the part data. This parameter is auto-populated when using the command from the CLI. This parameter is required if object lock parameters are specified.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upContentMD5 :: Lens.Lens' UploadPart (Core.Maybe Types.ContentMD5)
upContentMD5 = Lens.field @"contentMD5"
{-# INLINEABLE upContentMD5 #-}
{-# DEPRECATED contentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upExpectedBucketOwner :: Lens.Lens' UploadPart (Core.Maybe Types.ExpectedBucketOwner)
upExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE upExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upRequestPayer :: Lens.Lens' UploadPart (Core.Maybe Types.RequestPayer)
upRequestPayer = Lens.field @"requestPayer"
{-# INLINEABLE upRequestPayer #-}
{-# DEPRECATED requestPayer "Use generic-lens or generic-optics with 'requestPayer' instead"  #-}

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSSECustomerAlgorithm :: Lens.Lens' UploadPart (Core.Maybe Types.SSECustomerAlgorithm)
upSSECustomerAlgorithm = Lens.field @"sSECustomerAlgorithm"
{-# INLINEABLE upSSECustomerAlgorithm #-}
{-# DEPRECATED sSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead"  #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm header@ . This must be the same encryption key specified in the initiate multipart upload request.
--
-- /Note:/ Consider using 'sSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSSECustomerKey :: Lens.Lens' UploadPart (Core.Maybe Types.SSECustomerKey)
upSSECustomerKey = Lens.field @"sSECustomerKey"
{-# INLINEABLE upSSECustomerKey #-}
{-# DEPRECATED sSECustomerKey "Use generic-lens or generic-optics with 'sSECustomerKey' instead"  #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSSECustomerKeyMD5 :: Lens.Lens' UploadPart (Core.Maybe Types.SSECustomerKeyMD5)
upSSECustomerKeyMD5 = Lens.field @"sSECustomerKeyMD5"
{-# INLINEABLE upSSECustomerKeyMD5 #-}
{-# DEPRECATED sSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead"  #-}

instance Core.ToQuery UploadPart where
        toQuery UploadPart{..}
          = Core.toQueryPair "partNumber" partNumber Core.<>
              Core.toQueryPair "uploadId" uploadId

instance Core.ToHeaders UploadPart where
        toHeaders UploadPart{..}
          = Core.toHeaders "Content-Length" contentLength Core.<>
              Core.toHeaders "Content-MD5" contentMD5
              Core.<>
              Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
              Core.<> Core.toHeaders "x-amz-request-payer" requestPayer
              Core.<>
              Core.toHeaders "x-amz-server-side-encryption-customer-algorithm"
                sSECustomerAlgorithm
              Core.<>
              Core.toHeaders "x-amz-server-side-encryption-customer-key"
                sSECustomerKey
              Core.<>
              Core.toHeaders "x-amz-server-side-encryption-customer-key-MD5"
                sSECustomerKeyMD5

instance Core.AWSRequest UploadPart where
        type Rs UploadPart = UploadPartResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/" Core.<> Core.toText bucket Core.<> "/" Core.<> Core.toText key,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toBody body}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UploadPartResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*>
                     Core.parseHeaderMaybe "x-amz-request-charged" h
                     Core.<*>
                     Core.parseHeaderMaybe
                       "x-amz-server-side-encryption-customer-algorithm"
                       h
                     Core.<*>
                     Core.parseHeaderMaybe
                       "x-amz-server-side-encryption-customer-key-MD5"
                       h
                     Core.<*>
                     Core.parseHeaderMaybe "x-amz-server-side-encryption-aws-kms-key-id"
                       h
                     Core.<*> Core.parseHeaderMaybe "x-amz-server-side-encryption" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUploadPartResponse' smart constructor.
data UploadPartResponse = UploadPartResponse'
  { eTag :: Core.Maybe Types.ETag
    -- ^ Entity tag for the uploaded object.
  , requestCharged :: Core.Maybe Types.RequestCharged
  , sSECustomerAlgorithm :: Core.Maybe Types.SSECustomerAlgorithm
    -- ^ If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
  , sSECustomerKeyMD5 :: Core.Maybe Types.SSECustomerKeyMD5
    -- ^ If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
  , sSEKMSKeyId :: Core.Maybe Types.SSEKMSKeyId
    -- ^ If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) was used for the object.
  , serverSideEncryption :: Core.Maybe Types.ServerSideEncryption
    -- ^ The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UploadPartResponse' value with any optional fields omitted.
mkUploadPartResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UploadPartResponse
mkUploadPartResponse responseStatus
  = UploadPartResponse'{eTag = Core.Nothing,
                        requestCharged = Core.Nothing, sSECustomerAlgorithm = Core.Nothing,
                        sSECustomerKeyMD5 = Core.Nothing, sSEKMSKeyId = Core.Nothing,
                        serverSideEncryption = Core.Nothing, responseStatus}

-- | Entity tag for the uploaded object.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsETag :: Lens.Lens' UploadPartResponse (Core.Maybe Types.ETag)
uprrsETag = Lens.field @"eTag"
{-# INLINEABLE uprrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsRequestCharged :: Lens.Lens' UploadPartResponse (Core.Maybe Types.RequestCharged)
uprrsRequestCharged = Lens.field @"requestCharged"
{-# INLINEABLE uprrsRequestCharged #-}
{-# DEPRECATED requestCharged "Use generic-lens or generic-optics with 'requestCharged' instead"  #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsSSECustomerAlgorithm :: Lens.Lens' UploadPartResponse (Core.Maybe Types.SSECustomerAlgorithm)
uprrsSSECustomerAlgorithm = Lens.field @"sSECustomerAlgorithm"
{-# INLINEABLE uprrsSSECustomerAlgorithm #-}
{-# DEPRECATED sSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead"  #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsSSECustomerKeyMD5 :: Lens.Lens' UploadPartResponse (Core.Maybe Types.SSECustomerKeyMD5)
uprrsSSECustomerKeyMD5 = Lens.field @"sSECustomerKeyMD5"
{-# INLINEABLE uprrsSSECustomerKeyMD5 #-}
{-# DEPRECATED sSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead"  #-}

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) was used for the object.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsSSEKMSKeyId :: Lens.Lens' UploadPartResponse (Core.Maybe Types.SSEKMSKeyId)
uprrsSSEKMSKeyId = Lens.field @"sSEKMSKeyId"
{-# INLINEABLE uprrsSSEKMSKeyId #-}
{-# DEPRECATED sSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead"  #-}

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsServerSideEncryption :: Lens.Lens' UploadPartResponse (Core.Maybe Types.ServerSideEncryption)
uprrsServerSideEncryption = Lens.field @"serverSideEncryption"
{-# INLINEABLE uprrsServerSideEncryption #-}
{-# DEPRECATED serverSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UploadPartResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
