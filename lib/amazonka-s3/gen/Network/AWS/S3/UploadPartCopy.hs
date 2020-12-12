{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.S3.UploadPartCopy
  ( -- * Creating a request
    UploadPartCopy (..),
    mkUploadPartCopy,

    -- ** Request lenses
    upcCopySourceIfModifiedSince,
    upcCopySourceIfUnmodifiedSince,
    upcCopySourceRange,
    upcCopySourceSSECustomerKeyMD5,
    upcCopySourceIfNoneMatch,
    upcSSECustomerAlgorithm,
    upcSSECustomerKey,
    upcRequestPayer,
    upcCopySourceIfMatch,
    upcExpectedSourceBucketOwner,
    upcSSECustomerKeyMD5,
    upcCopySourceSSECustomerKey,
    upcCopySourceSSECustomerAlgorithm,
    upcExpectedBucketOwner,
    upcBucket,
    upcCopySource,
    upcKey,
    upcPartNumber,
    upcUploadId,

    -- * Destructuring the response
    UploadPartCopyResponse (..),
    mkUploadPartCopyResponse,

    -- ** Response lenses
    upcrsRequestCharged,
    upcrsCopyPartResult,
    upcrsSSECustomerAlgorithm,
    upcrsCopySourceVersionId,
    upcrsSSECustomerKeyMD5,
    upcrsSSEKMSKeyId,
    upcrsServerSideEncryption,
    upcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkUploadPartCopy' smart constructor.
data UploadPartCopy = UploadPartCopy'
  { copySourceIfModifiedSince ::
      Lude.Maybe Lude.DateTime,
    copySourceIfUnmodifiedSince :: Lude.Maybe Lude.DateTime,
    copySourceRange :: Lude.Maybe Lude.Text,
    copySourceSSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    copySourceIfNoneMatch :: Lude.Maybe Lude.Text,
    sSECustomerAlgorithm :: Lude.Maybe Lude.Text,
    sSECustomerKey :: Lude.Maybe (Lude.Sensitive Lude.Text),
    requestPayer :: Lude.Maybe RequestPayer,
    copySourceIfMatch :: Lude.Maybe Lude.Text,
    expectedSourceBucketOwner :: Lude.Maybe Lude.Text,
    sSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    copySourceSSECustomerKey ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    copySourceSSECustomerAlgorithm :: Lude.Maybe Lude.Text,
    expectedBucketOwner :: Lude.Maybe Lude.Text,
    bucket :: BucketName,
    copySource :: Lude.Text,
    key :: ObjectKey,
    partNumber :: Lude.Int,
    uploadId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UploadPartCopy' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'copySource' - Specifies the source object for the copy operation. You specify the value in one of two formats, depending on whether you want to access the source object through an <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-points.html access point> :
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
-- * 'copySourceIfMatch' - Copies the object if its entity tag (ETag) matches the specified tag.
-- * 'copySourceIfModifiedSince' - Copies the object if it has been modified since the specified time.
-- * 'copySourceIfNoneMatch' - Copies the object if its entity tag (ETag) is different than the specified ETag.
-- * 'copySourceIfUnmodifiedSince' - Copies the object if it hasn't been modified since the specified time.
-- * 'copySourceRange' - The range of bytes to copy from the source object. The range value must use the form bytes=first-last, where the first and last are the zero-based byte offsets to copy. For example, bytes=0-9 indicates that you want to copy the first 10 bytes of the source. You can copy a range only if the source object is greater than 5 MB.
-- * 'copySourceSSECustomerAlgorithm' - Specifies the algorithm to use when decrypting the source object (for example, AES256).
-- * 'copySourceSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use to decrypt the source object. The encryption key provided in this header must be one that was used when the source object was created.
-- * 'copySourceSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
-- * 'expectedBucketOwner' - The account id of the expected destination bucket owner. If the destination bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'expectedSourceBucketOwner' - The account id of the expected source bucket owner. If the source bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'key' - Object key for which the multipart upload was initiated.
-- * 'partNumber' - Part number of part being copied. This is a positive integer between 1 and 10,000.
-- * 'requestPayer' - Undocumented field.
-- * 'sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for example, AES256).
-- * 'sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header. This must be the same encryption key specified in the initiate multipart upload request.
-- * 'sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
-- * 'uploadId' - Upload ID identifying the multipart upload whose part is being copied.
mkUploadPartCopy ::
  -- | 'bucket'
  BucketName ->
  -- | 'copySource'
  Lude.Text ->
  -- | 'key'
  ObjectKey ->
  -- | 'partNumber'
  Lude.Int ->
  -- | 'uploadId'
  Lude.Text ->
  UploadPartCopy
mkUploadPartCopy
  pBucket_
  pCopySource_
  pKey_
  pPartNumber_
  pUploadId_ =
    UploadPartCopy'
      { copySourceIfModifiedSince = Lude.Nothing,
        copySourceIfUnmodifiedSince = Lude.Nothing,
        copySourceRange = Lude.Nothing,
        copySourceSSECustomerKeyMD5 = Lude.Nothing,
        copySourceIfNoneMatch = Lude.Nothing,
        sSECustomerAlgorithm = Lude.Nothing,
        sSECustomerKey = Lude.Nothing,
        requestPayer = Lude.Nothing,
        copySourceIfMatch = Lude.Nothing,
        expectedSourceBucketOwner = Lude.Nothing,
        sSECustomerKeyMD5 = Lude.Nothing,
        copySourceSSECustomerKey = Lude.Nothing,
        copySourceSSECustomerAlgorithm = Lude.Nothing,
        expectedBucketOwner = Lude.Nothing,
        bucket = pBucket_,
        copySource = pCopySource_,
        key = pKey_,
        partNumber = pPartNumber_,
        uploadId = pUploadId_
      }

-- | Copies the object if it has been modified since the specified time.
--
-- /Note:/ Consider using 'copySourceIfModifiedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySourceIfModifiedSince :: Lens.Lens' UploadPartCopy (Lude.Maybe Lude.DateTime)
upcCopySourceIfModifiedSince = Lens.lens (copySourceIfModifiedSince :: UploadPartCopy -> Lude.Maybe Lude.DateTime) (\s a -> s {copySourceIfModifiedSince = a} :: UploadPartCopy)
{-# DEPRECATED upcCopySourceIfModifiedSince "Use generic-lens or generic-optics with 'copySourceIfModifiedSince' instead." #-}

-- | Copies the object if it hasn't been modified since the specified time.
--
-- /Note:/ Consider using 'copySourceIfUnmodifiedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySourceIfUnmodifiedSince :: Lens.Lens' UploadPartCopy (Lude.Maybe Lude.DateTime)
upcCopySourceIfUnmodifiedSince = Lens.lens (copySourceIfUnmodifiedSince :: UploadPartCopy -> Lude.Maybe Lude.DateTime) (\s a -> s {copySourceIfUnmodifiedSince = a} :: UploadPartCopy)
{-# DEPRECATED upcCopySourceIfUnmodifiedSince "Use generic-lens or generic-optics with 'copySourceIfUnmodifiedSince' instead." #-}

-- | The range of bytes to copy from the source object. The range value must use the form bytes=first-last, where the first and last are the zero-based byte offsets to copy. For example, bytes=0-9 indicates that you want to copy the first 10 bytes of the source. You can copy a range only if the source object is greater than 5 MB.
--
-- /Note:/ Consider using 'copySourceRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySourceRange :: Lens.Lens' UploadPartCopy (Lude.Maybe Lude.Text)
upcCopySourceRange = Lens.lens (copySourceRange :: UploadPartCopy -> Lude.Maybe Lude.Text) (\s a -> s {copySourceRange = a} :: UploadPartCopy)
{-# DEPRECATED upcCopySourceRange "Use generic-lens or generic-optics with 'copySourceRange' instead." #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'copySourceSSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySourceSSECustomerKeyMD5 :: Lens.Lens' UploadPartCopy (Lude.Maybe Lude.Text)
upcCopySourceSSECustomerKeyMD5 = Lens.lens (copySourceSSECustomerKeyMD5 :: UploadPartCopy -> Lude.Maybe Lude.Text) (\s a -> s {copySourceSSECustomerKeyMD5 = a} :: UploadPartCopy)
{-# DEPRECATED upcCopySourceSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'copySourceSSECustomerKeyMD5' instead." #-}

-- | Copies the object if its entity tag (ETag) is different than the specified ETag.
--
-- /Note:/ Consider using 'copySourceIfNoneMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySourceIfNoneMatch :: Lens.Lens' UploadPartCopy (Lude.Maybe Lude.Text)
upcCopySourceIfNoneMatch = Lens.lens (copySourceIfNoneMatch :: UploadPartCopy -> Lude.Maybe Lude.Text) (\s a -> s {copySourceIfNoneMatch = a} :: UploadPartCopy)
{-# DEPRECATED upcCopySourceIfNoneMatch "Use generic-lens or generic-optics with 'copySourceIfNoneMatch' instead." #-}

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcSSECustomerAlgorithm :: Lens.Lens' UploadPartCopy (Lude.Maybe Lude.Text)
upcSSECustomerAlgorithm = Lens.lens (sSECustomerAlgorithm :: UploadPartCopy -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerAlgorithm = a} :: UploadPartCopy)
{-# DEPRECATED upcSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header. This must be the same encryption key specified in the initiate multipart upload request.
--
-- /Note:/ Consider using 'sSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcSSECustomerKey :: Lens.Lens' UploadPartCopy (Lude.Maybe (Lude.Sensitive Lude.Text))
upcSSECustomerKey = Lens.lens (sSECustomerKey :: UploadPartCopy -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSECustomerKey = a} :: UploadPartCopy)
{-# DEPRECATED upcSSECustomerKey "Use generic-lens or generic-optics with 'sSECustomerKey' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcRequestPayer :: Lens.Lens' UploadPartCopy (Lude.Maybe RequestPayer)
upcRequestPayer = Lens.lens (requestPayer :: UploadPartCopy -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: UploadPartCopy)
{-# DEPRECATED upcRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Copies the object if its entity tag (ETag) matches the specified tag.
--
-- /Note:/ Consider using 'copySourceIfMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySourceIfMatch :: Lens.Lens' UploadPartCopy (Lude.Maybe Lude.Text)
upcCopySourceIfMatch = Lens.lens (copySourceIfMatch :: UploadPartCopy -> Lude.Maybe Lude.Text) (\s a -> s {copySourceIfMatch = a} :: UploadPartCopy)
{-# DEPRECATED upcCopySourceIfMatch "Use generic-lens or generic-optics with 'copySourceIfMatch' instead." #-}

-- | The account id of the expected source bucket owner. If the source bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedSourceBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcExpectedSourceBucketOwner :: Lens.Lens' UploadPartCopy (Lude.Maybe Lude.Text)
upcExpectedSourceBucketOwner = Lens.lens (expectedSourceBucketOwner :: UploadPartCopy -> Lude.Maybe Lude.Text) (\s a -> s {expectedSourceBucketOwner = a} :: UploadPartCopy)
{-# DEPRECATED upcExpectedSourceBucketOwner "Use generic-lens or generic-optics with 'expectedSourceBucketOwner' instead." #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcSSECustomerKeyMD5 :: Lens.Lens' UploadPartCopy (Lude.Maybe Lude.Text)
upcSSECustomerKeyMD5 = Lens.lens (sSECustomerKeyMD5 :: UploadPartCopy -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerKeyMD5 = a} :: UploadPartCopy)
{-# DEPRECATED upcSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use to decrypt the source object. The encryption key provided in this header must be one that was used when the source object was created.
--
-- /Note:/ Consider using 'copySourceSSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySourceSSECustomerKey :: Lens.Lens' UploadPartCopy (Lude.Maybe (Lude.Sensitive Lude.Text))
upcCopySourceSSECustomerKey = Lens.lens (copySourceSSECustomerKey :: UploadPartCopy -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {copySourceSSECustomerKey = a} :: UploadPartCopy)
{-# DEPRECATED upcCopySourceSSECustomerKey "Use generic-lens or generic-optics with 'copySourceSSECustomerKey' instead." #-}

-- | Specifies the algorithm to use when decrypting the source object (for example, AES256).
--
-- /Note:/ Consider using 'copySourceSSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcCopySourceSSECustomerAlgorithm :: Lens.Lens' UploadPartCopy (Lude.Maybe Lude.Text)
upcCopySourceSSECustomerAlgorithm = Lens.lens (copySourceSSECustomerAlgorithm :: UploadPartCopy -> Lude.Maybe Lude.Text) (\s a -> s {copySourceSSECustomerAlgorithm = a} :: UploadPartCopy)
{-# DEPRECATED upcCopySourceSSECustomerAlgorithm "Use generic-lens or generic-optics with 'copySourceSSECustomerAlgorithm' instead." #-}

-- | The account id of the expected destination bucket owner. If the destination bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcExpectedBucketOwner :: Lens.Lens' UploadPartCopy (Lude.Maybe Lude.Text)
upcExpectedBucketOwner = Lens.lens (expectedBucketOwner :: UploadPartCopy -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: UploadPartCopy)
{-# DEPRECATED upcExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcBucket :: Lens.Lens' UploadPartCopy BucketName
upcBucket = Lens.lens (bucket :: UploadPartCopy -> BucketName) (\s a -> s {bucket = a} :: UploadPartCopy)
{-# DEPRECATED upcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

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
upcCopySource :: Lens.Lens' UploadPartCopy Lude.Text
upcCopySource = Lens.lens (copySource :: UploadPartCopy -> Lude.Text) (\s a -> s {copySource = a} :: UploadPartCopy)
{-# DEPRECATED upcCopySource "Use generic-lens or generic-optics with 'copySource' instead." #-}

-- | Object key for which the multipart upload was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcKey :: Lens.Lens' UploadPartCopy ObjectKey
upcKey = Lens.lens (key :: UploadPartCopy -> ObjectKey) (\s a -> s {key = a} :: UploadPartCopy)
{-# DEPRECATED upcKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Part number of part being copied. This is a positive integer between 1 and 10,000.
--
-- /Note:/ Consider using 'partNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcPartNumber :: Lens.Lens' UploadPartCopy Lude.Int
upcPartNumber = Lens.lens (partNumber :: UploadPartCopy -> Lude.Int) (\s a -> s {partNumber = a} :: UploadPartCopy)
{-# DEPRECATED upcPartNumber "Use generic-lens or generic-optics with 'partNumber' instead." #-}

-- | Upload ID identifying the multipart upload whose part is being copied.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcUploadId :: Lens.Lens' UploadPartCopy Lude.Text
upcUploadId = Lens.lens (uploadId :: UploadPartCopy -> Lude.Text) (\s a -> s {uploadId = a} :: UploadPartCopy)
{-# DEPRECATED upcUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

instance Lude.AWSRequest UploadPartCopy where
  type Rs UploadPartCopy = UploadPartCopyResponse
  request = Req.put s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          UploadPartCopyResponse'
            Lude.<$> (h Lude..#? "x-amz-request-charged")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-customer-algorithm")
            Lude.<*> (h Lude..#? "x-amz-copy-source-version-id")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-customer-key-MD5")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-aws-kms-key-id")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UploadPartCopy where
  toHeaders UploadPartCopy' {..} =
    Lude.mconcat
      [ "x-amz-copy-source-if-modified-since"
          Lude.=# copySourceIfModifiedSince,
        "x-amz-copy-source-if-unmodified-since"
          Lude.=# copySourceIfUnmodifiedSince,
        "x-amz-copy-source-range" Lude.=# copySourceRange,
        "x-amz-copy-source-server-side-encryption-customer-key-MD5"
          Lude.=# copySourceSSECustomerKeyMD5,
        "x-amz-copy-source-if-none-match" Lude.=# copySourceIfNoneMatch,
        "x-amz-server-side-encryption-customer-algorithm"
          Lude.=# sSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key" Lude.=# sSECustomerKey,
        "x-amz-request-payer" Lude.=# requestPayer,
        "x-amz-copy-source-if-match" Lude.=# copySourceIfMatch,
        "x-amz-source-expected-bucket-owner"
          Lude.=# expectedSourceBucketOwner,
        "x-amz-server-side-encryption-customer-key-MD5"
          Lude.=# sSECustomerKeyMD5,
        "x-amz-copy-source-server-side-encryption-customer-key"
          Lude.=# copySourceSSECustomerKey,
        "x-amz-copy-source-server-side-encryption-customer-algorithm"
          Lude.=# copySourceSSECustomerAlgorithm,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner,
        "x-amz-copy-source" Lude.=# copySource
      ]

instance Lude.ToPath UploadPartCopy where
  toPath UploadPartCopy' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery UploadPartCopy where
  toQuery UploadPartCopy' {..} =
    Lude.mconcat
      ["partNumber" Lude.=: partNumber, "uploadId" Lude.=: uploadId]

-- | /See:/ 'mkUploadPartCopyResponse' smart constructor.
data UploadPartCopyResponse = UploadPartCopyResponse'
  { requestCharged ::
      Lude.Maybe RequestCharged,
    copyPartResult :: Lude.Maybe CopyPartResult,
    sSECustomerAlgorithm :: Lude.Maybe Lude.Text,
    copySourceVersionId :: Lude.Maybe Lude.Text,
    sSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    sSEKMSKeyId ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    serverSideEncryption ::
      Lude.Maybe ServerSideEncryption,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UploadPartCopyResponse' with the minimum fields required to make a request.
--
-- * 'copyPartResult' - Container for all response elements.
-- * 'copySourceVersionId' - The version of the source object that was copied, if you have enabled versioning on the source bucket.
-- * 'requestCharged' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
-- * 'sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
-- * 'sSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
-- * 'serverSideEncryption' - The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
mkUploadPartCopyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UploadPartCopyResponse
mkUploadPartCopyResponse pResponseStatus_ =
  UploadPartCopyResponse'
    { requestCharged = Lude.Nothing,
      copyPartResult = Lude.Nothing,
      sSECustomerAlgorithm = Lude.Nothing,
      copySourceVersionId = Lude.Nothing,
      sSECustomerKeyMD5 = Lude.Nothing,
      sSEKMSKeyId = Lude.Nothing,
      serverSideEncryption = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcrsRequestCharged :: Lens.Lens' UploadPartCopyResponse (Lude.Maybe RequestCharged)
upcrsRequestCharged = Lens.lens (requestCharged :: UploadPartCopyResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: UploadPartCopyResponse)
{-# DEPRECATED upcrsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | Container for all response elements.
--
-- /Note:/ Consider using 'copyPartResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcrsCopyPartResult :: Lens.Lens' UploadPartCopyResponse (Lude.Maybe CopyPartResult)
upcrsCopyPartResult = Lens.lens (copyPartResult :: UploadPartCopyResponse -> Lude.Maybe CopyPartResult) (\s a -> s {copyPartResult = a} :: UploadPartCopyResponse)
{-# DEPRECATED upcrsCopyPartResult "Use generic-lens or generic-optics with 'copyPartResult' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcrsSSECustomerAlgorithm :: Lens.Lens' UploadPartCopyResponse (Lude.Maybe Lude.Text)
upcrsSSECustomerAlgorithm = Lens.lens (sSECustomerAlgorithm :: UploadPartCopyResponse -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerAlgorithm = a} :: UploadPartCopyResponse)
{-# DEPRECATED upcrsSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | The version of the source object that was copied, if you have enabled versioning on the source bucket.
--
-- /Note:/ Consider using 'copySourceVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcrsCopySourceVersionId :: Lens.Lens' UploadPartCopyResponse (Lude.Maybe Lude.Text)
upcrsCopySourceVersionId = Lens.lens (copySourceVersionId :: UploadPartCopyResponse -> Lude.Maybe Lude.Text) (\s a -> s {copySourceVersionId = a} :: UploadPartCopyResponse)
{-# DEPRECATED upcrsCopySourceVersionId "Use generic-lens or generic-optics with 'copySourceVersionId' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcrsSSECustomerKeyMD5 :: Lens.Lens' UploadPartCopyResponse (Lude.Maybe Lude.Text)
upcrsSSECustomerKeyMD5 = Lens.lens (sSECustomerKeyMD5 :: UploadPartCopyResponse -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerKeyMD5 = a} :: UploadPartCopyResponse)
{-# DEPRECATED upcrsSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcrsSSEKMSKeyId :: Lens.Lens' UploadPartCopyResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
upcrsSSEKMSKeyId = Lens.lens (sSEKMSKeyId :: UploadPartCopyResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSKeyId = a} :: UploadPartCopyResponse)
{-# DEPRECATED upcrsSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcrsServerSideEncryption :: Lens.Lens' UploadPartCopyResponse (Lude.Maybe ServerSideEncryption)
upcrsServerSideEncryption = Lens.lens (serverSideEncryption :: UploadPartCopyResponse -> Lude.Maybe ServerSideEncryption) (\s a -> s {serverSideEncryption = a} :: UploadPartCopyResponse)
{-# DEPRECATED upcrsServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcrsResponseStatus :: Lens.Lens' UploadPartCopyResponse Lude.Int
upcrsResponseStatus = Lens.lens (responseStatus :: UploadPartCopyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UploadPartCopyResponse)
{-# DEPRECATED upcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
