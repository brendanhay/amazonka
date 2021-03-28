{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.UploadPartCopy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a part by copying data from an existing object as data source. You specify the data source by adding the request header @x-amz-copy-source@ in your request and a byte range by adding the request header @x-amz-copy-source-range@ in your request. 
--
-- The minimum allowable part size for a multipart upload is 5 MB. For more information about multipart upload limits, go to <https://docs.aws.amazon.com/AmazonS3/latest/dev/qfacts.html Quick Facts> in the /Amazon Simple Storage Service Developer Guide/ . 
-- You must initiate a multipart upload before you can upload any part. In response to your initiate request. Amazon S3 returns a unique identifier, the upload ID, that you must include in your upload part request.
-- For more information about using the @UploadPartCopy@ operation, see the following:
--
--     * For conceptual information about multipart uploads, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/uploadobjusingmpu.html Uploading Objects Using Multipart Upload> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--     * For information about permissions required to use the multipart upload API, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--     * For information about copying objects using a single atomic operation vs. the multipart upload, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ObjectOperations.html Operations on Objects> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--     * For information about using server-side encryption with customer-provided encryption keys with the UploadPartCopy operation, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CopyObject.html CopyObject> and <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart> .
--
--
-- Note the following additional considerations about the request headers @x-amz-copy-source-if-match@ , @x-amz-copy-source-if-none-match@ , @x-amz-copy-source-if-unmodified-since@ , and @x-amz-copy-source-if-modified-since@ :
--
--
--     * __Consideration 1__ - If both of the @x-amz-copy-source-if-match@ and @x-amz-copy-source-if-unmodified-since@ headers are present in the request as follows:
-- @x-amz-copy-source-if-match@ condition evaluates to @true@ , and;
-- @x-amz-copy-source-if-unmodified-since@ condition evaluates to @false@ ;
-- Amazon S3 returns @200 OK@ and copies the data. 
--
--
--     * __Consideration 2__ - If both of the @x-amz-copy-source-if-none-match@ and @x-amz-copy-source-if-modified-since@ headers are present in the request as follows:
-- @x-amz-copy-source-if-none-match@ condition evaluates to @false@ , and;
-- @x-amz-copy-source-if-modified-since@ condition evaluates to @true@ ;
-- Amazon S3 returns @412 Precondition Failed@ response code. 
--
--
-- __Versioning__ 
-- If your bucket has versioning enabled, you could have multiple versions of the same object. By default, @x-amz-copy-source@ identifies the current version of the object to copy. If the current version is a delete marker and you don't specify a versionId in the @x-amz-copy-source@ , Amazon S3 returns a 404 error, because the object does not exist. If you specify versionId in the @x-amz-copy-source@ and the versionId is a delete marker, Amazon S3 returns an HTTP 400 error, because you are not allowed to specify a delete marker as a version for the @x-amz-copy-source@ . 
-- You can optionally specify a specific version of the source object to copy by adding the @versionId@ subresource as shown in the following example:
-- @x-amz-copy-source: /bucket/object?versionId=version id@ 
-- __Special Errors__ 
--
--     * 
--     * /Code: NoSuchUpload/ 
--
--
--     * /Cause: The specified multipart upload does not exist. The upload ID might be invalid, or the multipart upload might have been aborted or completed./ 
--
--
--     * /HTTP Status Code: 404 Not Found/ 
--
--
--
--
--     * 
--     * /Code: InvalidRequest/ 
--
--
--     * /Cause: The specified copy source is not supported as a byte-range copy source./ 
--
--
--     * /HTTP Status Code: 400 Bad Request/ 
--
--
--
--
-- __Related Resources__ 
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart> 
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
module Network.AWS.S3.UploadPartCopy
    (
    -- * Creating a request
      UploadPartCopy (..)
    , mkUploadPartCopy
    -- ** Request lenses
    , upcBucket
    , upcCopySource
    , upcKey
    , upcPartNumber
    , upcUploadId
    , upcCopySourceIfMatch
    , upcCopySourceIfModifiedSince
    , upcCopySourceIfNoneMatch
    , upcCopySourceIfUnmodifiedSince
    , upcCopySourceRange
    , upcCopySourceSSECustomerAlgorithm
    , upcCopySourceSSECustomerKey
    , upcCopySourceSSECustomerKeyMD5
    , upcExpectedBucketOwner
    , upcExpectedSourceBucketOwner
    , upcRequestPayer
    , upcSSECustomerAlgorithm
    , upcSSECustomerKey
    , upcSSECustomerKeyMD5

    -- * Destructuring the response
    , UploadPartCopyResponse (..)
    , mkUploadPartCopyResponse
    -- ** Response lenses
    , upcrrsCopyPartResult
    , upcrrsCopySourceVersionId
    , upcrrsRequestCharged
    , upcrrsSSECustomerAlgorithm
    , upcrrsSSECustomerKeyMD5
    , upcrrsSSEKMSKeyId
    , upcrrsServerSideEncryption
    , upcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkUploadPartCopy' smart constructor.
data UploadPartCopy = UploadPartCopy'
  { bucket :: Types.BucketName
    -- ^ The bucket name.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
  , copySource :: Types.CopySource
    -- ^ Specifies the source object for the copy operation. You specify the value in one of two formats, depending on whether you want to access the source object through an <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-points.html access point> :
--
--
--     * For objects not accessed through an access point, specify the name of the source bucket and key of the source object, separated by a slash (/). For example, to copy the object @reports/january.pdf@ from the bucket @awsexamplebucket@ , use @awsexamplebucket/reports/january.pdf@ . The value must be URL encoded.
--
--
--     * For objects accessed through access points, specify the Amazon Resource Name (ARN) of the object as accessed through the access point, in the format @arn:aws:s3:<Region>:<account-id>:accesspoint/<access-point-name>/object/<key>@ . For example, to copy the object @reports/january.pdf@ through access point @my-access-point@ owned by account @123456789012@ in Region @us-west-2@ , use the URL encoding of @arn:aws:s3:us-west-2:123456789012:accesspoint/my-access-point/object/reports/january.pdf@ . The value must be URL encoded.
-- Alternatively, for objects accessed through Amazon S3 on Outposts, specify the ARN of the object as accessed in the format @arn:aws:s3-outposts:<Region>:<account-id>:outpost/<outpost-id>/object/<key>@ . For example, to copy the object @reports/january.pdf@ through outpost @my-outpost@ owned by account @123456789012@ in Region @us-west-2@ , use the URL encoding of @arn:aws:s3-outposts:us-west-2:123456789012:outpost/my-outpost/object/reports/january.pdf@ . The value must be URL encoded. 
--
--
-- To copy a specific version of an object, append @?versionId=<version-id>@ to the value (for example, @awsexamplebucket/reports/january.pdf?versionId=QUpfdndhfd8438MNFDN93jdnJFkdmqnh893@ ). If you don't specify a version ID, Amazon S3 copies the latest version of the source object.
  , key :: Types.ObjectKey
    -- ^ Object key for which the multipart upload was initiated.
  , partNumber :: Core.Int
    -- ^ Part number of part being copied. This is a positive integer between 1 and 10,000.
  , uploadId :: Types.MultipartUploadId
    -- ^ Upload ID identifying the multipart upload whose part is being copied.
  , copySourceIfMatch :: Core.Maybe Types.CopySourceIfMatch
    -- ^ Copies the object if its entity tag (ETag) matches the specified tag.
  , copySourceIfModifiedSince :: Core.Maybe Core.UTCTime
    -- ^ Copies the object if it has been modified since the specified time.
  , copySourceIfNoneMatch :: Core.Maybe Types.CopySourceIfNoneMatch
    -- ^ Copies the object if its entity tag (ETag) is different than the specified ETag.
  , copySourceIfUnmodifiedSince :: Core.Maybe Core.UTCTime
    -- ^ Copies the object if it hasn't been modified since the specified time.
  , copySourceRange :: Core.Maybe Types.CopySourceRange
    -- ^ The range of bytes to copy from the source object. The range value must use the form bytes=first-last, where the first and last are the zero-based byte offsets to copy. For example, bytes=0-9 indicates that you want to copy the first 10 bytes of the source. You can copy a range only if the source object is greater than 5 MB.
  , copySourceSSECustomerAlgorithm :: Core.Maybe Types.CopySourceSSECustomerAlgorithm
    -- ^ Specifies the algorithm to use when decrypting the source object (for example, AES256).
  , copySourceSSECustomerKey :: Core.Maybe Types.CopySourceSSECustomerKey
    -- ^ Specifies the customer-provided encryption key for Amazon S3 to use to decrypt the source object. The encryption key provided in this header must be one that was used when the source object was created.
  , copySourceSSECustomerKeyMD5 :: Core.Maybe Types.CopySourceSSECustomerKeyMD5
    -- ^ Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected destination bucket owner. If the destination bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  , expectedSourceBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected source bucket owner. If the source bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  , requestPayer :: Core.Maybe Types.RequestPayer
  , sSECustomerAlgorithm :: Core.Maybe Types.SSECustomerAlgorithm
    -- ^ Specifies the algorithm to use to when encrypting the object (for example, AES256).
  , sSECustomerKey :: Core.Maybe Types.SSECustomerKey
    -- ^ Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header. This must be the same encryption key specified in the initiate multipart upload request.
  , sSECustomerKeyMD5 :: Core.Maybe Types.SSECustomerKeyMD5
    -- ^ Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UploadPartCopy' value with any optional fields omitted.
mkUploadPartCopy
    :: Types.BucketName -- ^ 'bucket'
    -> Types.CopySource -- ^ 'copySource'
    -> Types.ObjectKey -- ^ 'key'
    -> Core.Int -- ^ 'partNumber'
    -> Types.MultipartUploadId -- ^ 'uploadId'
    -> UploadPartCopy
mkUploadPartCopy bucket copySource key partNumber uploadId
  = UploadPartCopy'{bucket, copySource, key, partNumber, uploadId,
                    copySourceIfMatch = Core.Nothing,
                    copySourceIfModifiedSince = Core.Nothing,
                    copySourceIfNoneMatch = Core.Nothing,
                    copySourceIfUnmodifiedSince = Core.Nothing,
                    copySourceRange = Core.Nothing,
                    copySourceSSECustomerAlgorithm = Core.Nothing,
                    copySourceSSECustomerKey = Core.Nothing,
                    copySourceSSECustomerKeyMD5 = Core.Nothing,
                    expectedBucketOwner = Core.Nothing,
                    expectedSourceBucketOwner = Core.Nothing,
                    requestPayer = Core.Nothing, sSECustomerAlgorithm = Core.Nothing,
                    sSECustomerKey = Core.Nothing, sSECustomerKeyMD5 = Core.Nothing}

-- | The bucket name.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcBucket :: Lens.Lens' UploadPartCopy Types.BucketName
upcBucket = Lens.field @"bucket"
{-# INLINEABLE upcBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Specifies the source object for the copy operation. You specify the value in one of two formats, depending on whether you want to access the source object through an <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-points.html access point> :
--
--
--     * For objects not accessed through an access point, specify the name of the source bucket and key of the source object, separated by a slash (/). For example, to copy the object @reports/january.pdf@ from the bucket @awsexamplebucket@ , use @awsexamplebucket/reports/january.pdf@ . The value must be URL encoded.
--
--
--     * For objects accessed through access points, specify the Amazon Resource Name (ARN) of the object as accessed through the access point, in the format @arn:aws:s3:<Region>:<account-id>:accesspoint/<access-point-name>/object/<key>@ . For example, to copy the object @reports/january.pdf@ through access point @my-access-point@ owned by account @123456789012@ in Region @us-west-2@ , use the URL encoding of @arn:aws:s3:us-west-2:123456789012:accesspoint/my-access-point/object/reports/january.pdf@ . The value must be URL encoded.
-- Alternatively, for objects accessed through Amazon S3 on Outposts, specify the ARN of the object as accessed in the format @arn:aws:s3-outposts:<Region>:<account-id>:outpost/<outpost-id>/object/<key>@ . For example, to copy the object @reports/january.pdf@ through outpost @my-outpost@ owned by account @123456789012@ in Region @us-west-2@ , use the URL encoding of @arn:aws:s3-outposts:us-west-2:123456789012:outpost/my-outpost/object/reports/january.pdf@ . The value must be URL encoded. 
--
--
-- To copy a specific version of an object, append @?versionId=<version-id>@ to the value (for example, @awsexamplebucket/reports/january.pdf?versionId=QUpfdndhfd8438MNFDN93jdnJFkdmqnh893@ ). If you don't specify a version ID, Amazon S3 copies the latest version of the source object.
--
-- /Note:/ Consider using 'copySource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySource :: Lens.Lens' UploadPartCopy Types.CopySource
upcCopySource = Lens.field @"copySource"
{-# INLINEABLE upcCopySource #-}
{-# DEPRECATED copySource "Use generic-lens or generic-optics with 'copySource' instead"  #-}

-- | Object key for which the multipart upload was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcKey :: Lens.Lens' UploadPartCopy Types.ObjectKey
upcKey = Lens.field @"key"
{-# INLINEABLE upcKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | Part number of part being copied. This is a positive integer between 1 and 10,000.
--
-- /Note:/ Consider using 'partNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcPartNumber :: Lens.Lens' UploadPartCopy Core.Int
upcPartNumber = Lens.field @"partNumber"
{-# INLINEABLE upcPartNumber #-}
{-# DEPRECATED partNumber "Use generic-lens or generic-optics with 'partNumber' instead"  #-}

-- | Upload ID identifying the multipart upload whose part is being copied.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcUploadId :: Lens.Lens' UploadPartCopy Types.MultipartUploadId
upcUploadId = Lens.field @"uploadId"
{-# INLINEABLE upcUploadId #-}
{-# DEPRECATED uploadId "Use generic-lens or generic-optics with 'uploadId' instead"  #-}

-- | Copies the object if its entity tag (ETag) matches the specified tag.
--
-- /Note:/ Consider using 'copySourceIfMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySourceIfMatch :: Lens.Lens' UploadPartCopy (Core.Maybe Types.CopySourceIfMatch)
upcCopySourceIfMatch = Lens.field @"copySourceIfMatch"
{-# INLINEABLE upcCopySourceIfMatch #-}
{-# DEPRECATED copySourceIfMatch "Use generic-lens or generic-optics with 'copySourceIfMatch' instead"  #-}

-- | Copies the object if it has been modified since the specified time.
--
-- /Note:/ Consider using 'copySourceIfModifiedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySourceIfModifiedSince :: Lens.Lens' UploadPartCopy (Core.Maybe Core.UTCTime)
upcCopySourceIfModifiedSince = Lens.field @"copySourceIfModifiedSince"
{-# INLINEABLE upcCopySourceIfModifiedSince #-}
{-# DEPRECATED copySourceIfModifiedSince "Use generic-lens or generic-optics with 'copySourceIfModifiedSince' instead"  #-}

-- | Copies the object if its entity tag (ETag) is different than the specified ETag.
--
-- /Note:/ Consider using 'copySourceIfNoneMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySourceIfNoneMatch :: Lens.Lens' UploadPartCopy (Core.Maybe Types.CopySourceIfNoneMatch)
upcCopySourceIfNoneMatch = Lens.field @"copySourceIfNoneMatch"
{-# INLINEABLE upcCopySourceIfNoneMatch #-}
{-# DEPRECATED copySourceIfNoneMatch "Use generic-lens or generic-optics with 'copySourceIfNoneMatch' instead"  #-}

-- | Copies the object if it hasn't been modified since the specified time.
--
-- /Note:/ Consider using 'copySourceIfUnmodifiedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySourceIfUnmodifiedSince :: Lens.Lens' UploadPartCopy (Core.Maybe Core.UTCTime)
upcCopySourceIfUnmodifiedSince = Lens.field @"copySourceIfUnmodifiedSince"
{-# INLINEABLE upcCopySourceIfUnmodifiedSince #-}
{-# DEPRECATED copySourceIfUnmodifiedSince "Use generic-lens or generic-optics with 'copySourceIfUnmodifiedSince' instead"  #-}

-- | The range of bytes to copy from the source object. The range value must use the form bytes=first-last, where the first and last are the zero-based byte offsets to copy. For example, bytes=0-9 indicates that you want to copy the first 10 bytes of the source. You can copy a range only if the source object is greater than 5 MB.
--
-- /Note:/ Consider using 'copySourceRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySourceRange :: Lens.Lens' UploadPartCopy (Core.Maybe Types.CopySourceRange)
upcCopySourceRange = Lens.field @"copySourceRange"
{-# INLINEABLE upcCopySourceRange #-}
{-# DEPRECATED copySourceRange "Use generic-lens or generic-optics with 'copySourceRange' instead"  #-}

-- | Specifies the algorithm to use when decrypting the source object (for example, AES256).
--
-- /Note:/ Consider using 'copySourceSSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySourceSSECustomerAlgorithm :: Lens.Lens' UploadPartCopy (Core.Maybe Types.CopySourceSSECustomerAlgorithm)
upcCopySourceSSECustomerAlgorithm = Lens.field @"copySourceSSECustomerAlgorithm"
{-# INLINEABLE upcCopySourceSSECustomerAlgorithm #-}
{-# DEPRECATED copySourceSSECustomerAlgorithm "Use generic-lens or generic-optics with 'copySourceSSECustomerAlgorithm' instead"  #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use to decrypt the source object. The encryption key provided in this header must be one that was used when the source object was created.
--
-- /Note:/ Consider using 'copySourceSSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySourceSSECustomerKey :: Lens.Lens' UploadPartCopy (Core.Maybe Types.CopySourceSSECustomerKey)
upcCopySourceSSECustomerKey = Lens.field @"copySourceSSECustomerKey"
{-# INLINEABLE upcCopySourceSSECustomerKey #-}
{-# DEPRECATED copySourceSSECustomerKey "Use generic-lens or generic-optics with 'copySourceSSECustomerKey' instead"  #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'copySourceSSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySourceSSECustomerKeyMD5 :: Lens.Lens' UploadPartCopy (Core.Maybe Types.CopySourceSSECustomerKeyMD5)
upcCopySourceSSECustomerKeyMD5 = Lens.field @"copySourceSSECustomerKeyMD5"
{-# INLINEABLE upcCopySourceSSECustomerKeyMD5 #-}
{-# DEPRECATED copySourceSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'copySourceSSECustomerKeyMD5' instead"  #-}

-- | The account id of the expected destination bucket owner. If the destination bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcExpectedBucketOwner :: Lens.Lens' UploadPartCopy (Core.Maybe Types.AccountId)
upcExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE upcExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

-- | The account id of the expected source bucket owner. If the source bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedSourceBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcExpectedSourceBucketOwner :: Lens.Lens' UploadPartCopy (Core.Maybe Types.AccountId)
upcExpectedSourceBucketOwner = Lens.field @"expectedSourceBucketOwner"
{-# INLINEABLE upcExpectedSourceBucketOwner #-}
{-# DEPRECATED expectedSourceBucketOwner "Use generic-lens or generic-optics with 'expectedSourceBucketOwner' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcRequestPayer :: Lens.Lens' UploadPartCopy (Core.Maybe Types.RequestPayer)
upcRequestPayer = Lens.field @"requestPayer"
{-# INLINEABLE upcRequestPayer #-}
{-# DEPRECATED requestPayer "Use generic-lens or generic-optics with 'requestPayer' instead"  #-}

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcSSECustomerAlgorithm :: Lens.Lens' UploadPartCopy (Core.Maybe Types.SSECustomerAlgorithm)
upcSSECustomerAlgorithm = Lens.field @"sSECustomerAlgorithm"
{-# INLINEABLE upcSSECustomerAlgorithm #-}
{-# DEPRECATED sSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead"  #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header. This must be the same encryption key specified in the initiate multipart upload request.
--
-- /Note:/ Consider using 'sSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcSSECustomerKey :: Lens.Lens' UploadPartCopy (Core.Maybe Types.SSECustomerKey)
upcSSECustomerKey = Lens.field @"sSECustomerKey"
{-# INLINEABLE upcSSECustomerKey #-}
{-# DEPRECATED sSECustomerKey "Use generic-lens or generic-optics with 'sSECustomerKey' instead"  #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcSSECustomerKeyMD5 :: Lens.Lens' UploadPartCopy (Core.Maybe Types.SSECustomerKeyMD5)
upcSSECustomerKeyMD5 = Lens.field @"sSECustomerKeyMD5"
{-# INLINEABLE upcSSECustomerKeyMD5 #-}
{-# DEPRECATED sSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead"  #-}

instance Core.ToQuery UploadPartCopy where
        toQuery UploadPartCopy{..}
          = Core.toQueryPair "partNumber" partNumber Core.<>
              Core.toQueryPair "uploadId" uploadId

instance Core.ToHeaders UploadPartCopy where
        toHeaders UploadPartCopy{..}
          = Core.toHeaders "x-amz-copy-source" copySource Core.<>
              Core.toHeaders "x-amz-copy-source-if-match" copySourceIfMatch
              Core.<>
              Core.toHeaders "x-amz-copy-source-if-modified-since"
                copySourceIfModifiedSince
              Core.<>
              Core.toHeaders "x-amz-copy-source-if-none-match"
                copySourceIfNoneMatch
              Core.<>
              Core.toHeaders "x-amz-copy-source-if-unmodified-since"
                copySourceIfUnmodifiedSince
              Core.<> Core.toHeaders "x-amz-copy-source-range" copySourceRange
              Core.<>
              Core.toHeaders
                "x-amz-copy-source-server-side-encryption-customer-algorithm"
                copySourceSSECustomerAlgorithm
              Core.<>
              Core.toHeaders
                "x-amz-copy-source-server-side-encryption-customer-key"
                copySourceSSECustomerKey
              Core.<>
              Core.toHeaders
                "x-amz-copy-source-server-side-encryption-customer-key-MD5"
                copySourceSSECustomerKeyMD5
              Core.<>
              Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
              Core.<>
              Core.toHeaders "x-amz-source-expected-bucket-owner"
                expectedSourceBucketOwner
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

instance Core.AWSRequest UploadPartCopy where
        type Rs UploadPartCopy = UploadPartCopyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/" Core.<> Core.toText bucket Core.<> "/" Core.<> Core.toText key,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 UploadPartCopyResponse' Core.<$>
                   (Core.parseXML x) Core.<*>
                     Core.parseHeaderMaybe "x-amz-copy-source-version-id" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-request-charged" h
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

-- | /See:/ 'mkUploadPartCopyResponse' smart constructor.
data UploadPartCopyResponse = UploadPartCopyResponse'
  { copyPartResult :: Core.Maybe Types.CopyPartResult
    -- ^ Container for all response elements.
  , copySourceVersionId :: Core.Maybe Types.CopySourceVersionId
    -- ^ The version of the source object that was copied, if you have enabled versioning on the source bucket.
  , requestCharged :: Core.Maybe Types.RequestCharged
  , sSECustomerAlgorithm :: Core.Maybe Types.SSECustomerAlgorithm
    -- ^ If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
  , sSECustomerKeyMD5 :: Core.Maybe Types.SSECustomerKeyMD5
    -- ^ If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
  , sSEKMSKeyId :: Core.Maybe Types.SSEKMSKeyId
    -- ^ If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
  , serverSideEncryption :: Core.Maybe Types.ServerSideEncryption
    -- ^ The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UploadPartCopyResponse' value with any optional fields omitted.
mkUploadPartCopyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UploadPartCopyResponse
mkUploadPartCopyResponse responseStatus
  = UploadPartCopyResponse'{copyPartResult = Core.Nothing,
                            copySourceVersionId = Core.Nothing, requestCharged = Core.Nothing,
                            sSECustomerAlgorithm = Core.Nothing,
                            sSECustomerKeyMD5 = Core.Nothing, sSEKMSKeyId = Core.Nothing,
                            serverSideEncryption = Core.Nothing, responseStatus}

-- | Container for all response elements.
--
-- /Note:/ Consider using 'copyPartResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcrrsCopyPartResult :: Lens.Lens' UploadPartCopyResponse (Core.Maybe Types.CopyPartResult)
upcrrsCopyPartResult = Lens.field @"copyPartResult"
{-# INLINEABLE upcrrsCopyPartResult #-}
{-# DEPRECATED copyPartResult "Use generic-lens or generic-optics with 'copyPartResult' instead"  #-}

-- | The version of the source object that was copied, if you have enabled versioning on the source bucket.
--
-- /Note:/ Consider using 'copySourceVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcrrsCopySourceVersionId :: Lens.Lens' UploadPartCopyResponse (Core.Maybe Types.CopySourceVersionId)
upcrrsCopySourceVersionId = Lens.field @"copySourceVersionId"
{-# INLINEABLE upcrrsCopySourceVersionId #-}
{-# DEPRECATED copySourceVersionId "Use generic-lens or generic-optics with 'copySourceVersionId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcrrsRequestCharged :: Lens.Lens' UploadPartCopyResponse (Core.Maybe Types.RequestCharged)
upcrrsRequestCharged = Lens.field @"requestCharged"
{-# INLINEABLE upcrrsRequestCharged #-}
{-# DEPRECATED requestCharged "Use generic-lens or generic-optics with 'requestCharged' instead"  #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcrrsSSECustomerAlgorithm :: Lens.Lens' UploadPartCopyResponse (Core.Maybe Types.SSECustomerAlgorithm)
upcrrsSSECustomerAlgorithm = Lens.field @"sSECustomerAlgorithm"
{-# INLINEABLE upcrrsSSECustomerAlgorithm #-}
{-# DEPRECATED sSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead"  #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcrrsSSECustomerKeyMD5 :: Lens.Lens' UploadPartCopyResponse (Core.Maybe Types.SSECustomerKeyMD5)
upcrrsSSECustomerKeyMD5 = Lens.field @"sSECustomerKeyMD5"
{-# INLINEABLE upcrrsSSECustomerKeyMD5 #-}
{-# DEPRECATED sSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead"  #-}

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcrrsSSEKMSKeyId :: Lens.Lens' UploadPartCopyResponse (Core.Maybe Types.SSEKMSKeyId)
upcrrsSSEKMSKeyId = Lens.field @"sSEKMSKeyId"
{-# INLINEABLE upcrrsSSEKMSKeyId #-}
{-# DEPRECATED sSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead"  #-}

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcrrsServerSideEncryption :: Lens.Lens' UploadPartCopyResponse (Core.Maybe Types.ServerSideEncryption)
upcrrsServerSideEncryption = Lens.field @"serverSideEncryption"
{-# INLINEABLE upcrrsServerSideEncryption #-}
{-# DEPRECATED serverSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcrrsResponseStatus :: Lens.Lens' UploadPartCopyResponse Core.Int
upcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE upcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
