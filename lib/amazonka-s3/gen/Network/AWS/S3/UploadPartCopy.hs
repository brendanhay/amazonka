{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
--
-- The minimum allowable part size for a multipart upload is 5 MB. For more information about multipart upload limits, go to <https://docs.aws.amazon.com/AmazonS3/latest/dev/qfacts.html Quick Facts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- You must initiate a multipart upload before you can upload any part. In response to your initiate request. Amazon S3 returns a unique identifier, the upload ID, that you must include in your upload part request.
--
-- For more information about using the @UploadPartCopy@ operation, see the following:
--
--     * For conceptual information about multipart uploads, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/uploadobjusingmpu.html Uploading Objects Using Multipart Upload> in the /Amazon Simple Storage Service Developer Guide/ .
--
--     * For information about permissions required to use the multipart upload API, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions> in the /Amazon Simple Storage Service Developer Guide/ .
--
--     * For information about copying objects using a single atomic operation vs. the multipart upload, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ObjectOperations.html Operations on Objects> in the /Amazon Simple Storage Service Developer Guide/ .
--
--     * For information about using server-side encryption with customer-provided encryption keys with the UploadPartCopy operation, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CopyObject.html CopyObject> and <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart> .
--
--
--
-- Note the following additional considerations about the request headers @x-amz-copy-source-if-match@ , @x-amz-copy-source-if-none-match@ , @x-amz-copy-source-if-unmodified-since@ , and @x-amz-copy-source-if-modified-since@ :
--
--
--
--     * __Consideration 1__ - If both of the @x-amz-copy-source-if-match@ and @x-amz-copy-source-if-unmodified-since@ headers are present in the request as follows:
--
-- @x-amz-copy-source-if-match@ condition evaluates to @true@ , and;
--
-- @x-amz-copy-source-if-unmodified-since@ condition evaluates to @false@ ;
--
-- Amazon S3 returns @200 OK@ and copies the data.
--
--     * __Consideration 2__ - If both of the @x-amz-copy-source-if-none-match@ and @x-amz-copy-source-if-modified-since@ headers are present in the request as follows:
--
-- @x-amz-copy-source-if-none-match@ condition evaluates to @false@ , and;
--
-- @x-amz-copy-source-if-modified-since@ condition evaluates to @true@ ;
--
-- Amazon S3 returns @412 Precondition Failed@ response code.
--
--
--
-- __Versioning__
--
-- If your bucket has versioning enabled, you could have multiple versions of the same object. By default, @x-amz-copy-source@ identifies the current version of the object to copy. If the current version is a delete marker and you don't specify a versionId in the @x-amz-copy-source@ , Amazon S3 returns a 404 error, because the object does not exist. If you specify versionId in the @x-amz-copy-source@ and the versionId is a delete marker, Amazon S3 returns an HTTP 400 error, because you are not allowed to specify a delete marker as a version for the @x-amz-copy-source@ .
--
-- You can optionally specify a specific version of the source object to copy by adding the @versionId@ subresource as shown in the following example:
--
-- @x-amz-copy-source: /bucket/object?versionId=version id@
--
-- __Special Errors__
--
--     *     * /Code: NoSuchUpload/
--
--     * /Cause: The specified multipart upload does not exist. The upload ID might be invalid, or the multipart upload might have been aborted or completed./
--
--     * /HTTP Status Code: 404 Not Found/
--
--
--
--     *     * /Code: InvalidRequest/
--
--     * /Cause: The specified copy source is not supported as a byte-range copy source./
--
--     * /HTTP Status Code: 400 Bad Request/
--
--
--
--
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads>
module Network.AWS.S3.UploadPartCopy
  ( -- * Creating a Request
    uploadPartCopy,
    UploadPartCopy,

    -- * Request Lenses
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

    -- * Destructuring the Response
    uploadPartCopyResponse,
    UploadPartCopyResponse,

    -- * Response Lenses
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

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'uploadPartCopy' smart constructor.
data UploadPartCopy = UploadPartCopy'
  { _upcCopySourceIfModifiedSince ::
      !(Maybe ISO8601),
    _upcCopySourceIfUnmodifiedSince :: !(Maybe ISO8601),
    _upcCopySourceRange :: !(Maybe Text),
    _upcCopySourceSSECustomerKeyMD5 :: !(Maybe Text),
    _upcCopySourceIfNoneMatch :: !(Maybe Text),
    _upcSSECustomerAlgorithm :: !(Maybe Text),
    _upcSSECustomerKey :: !(Maybe (Sensitive Text)),
    _upcRequestPayer :: !(Maybe RequestPayer),
    _upcCopySourceIfMatch :: !(Maybe Text),
    _upcExpectedSourceBucketOwner :: !(Maybe Text),
    _upcSSECustomerKeyMD5 :: !(Maybe Text),
    _upcCopySourceSSECustomerKey :: !(Maybe (Sensitive Text)),
    _upcCopySourceSSECustomerAlgorithm :: !(Maybe Text),
    _upcExpectedBucketOwner :: !(Maybe Text),
    _upcBucket :: !BucketName,
    _upcCopySource :: !Text,
    _upcKey :: !ObjectKey,
    _upcPartNumber :: !Int,
    _upcUploadId :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UploadPartCopy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upcCopySourceIfModifiedSince' - Copies the object if it has been modified since the specified time.
--
-- * 'upcCopySourceIfUnmodifiedSince' - Copies the object if it hasn't been modified since the specified time.
--
-- * 'upcCopySourceRange' - The range of bytes to copy from the source object. The range value must use the form bytes=first-last, where the first and last are the zero-based byte offsets to copy. For example, bytes=0-9 indicates that you want to copy the first 10 bytes of the source. You can copy a range only if the source object is greater than 5 MB.
--
-- * 'upcCopySourceSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- * 'upcCopySourceIfNoneMatch' - Copies the object if its entity tag (ETag) is different than the specified ETag.
--
-- * 'upcSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- * 'upcSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header. This must be the same encryption key specified in the initiate multipart upload request.
--
-- * 'upcRequestPayer' - Undocumented member.
--
-- * 'upcCopySourceIfMatch' - Copies the object if its entity tag (ETag) matches the specified tag.
--
-- * 'upcExpectedSourceBucketOwner' - The account id of the expected source bucket owner. If the source bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'upcSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- * 'upcCopySourceSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use to decrypt the source object. The encryption key provided in this header must be one that was used when the source object was created.
--
-- * 'upcCopySourceSSECustomerAlgorithm' - Specifies the algorithm to use when decrypting the source object (for example, AES256).
--
-- * 'upcExpectedBucketOwner' - The account id of the expected destination bucket owner. If the destination bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'upcBucket' - The bucket name. When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'upcCopySource' - Specifies the source object for the copy operation. You specify the value in one of two formats, depending on whether you want to access the source object through an <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-points.html access point> :     * For objects not accessed through an access point, specify the name of the source bucket and key of the source object, separated by a slash (/). For example, to copy the object @reports/january.pdf@ from the bucket @awsexamplebucket@ , use @awsexamplebucket/reports/january.pdf@ . The value must be URL encoded.     * For objects accessed through access points, specify the Amazon Resource Name (ARN) of the object as accessed through the access point, in the format @arn:aws:s3:<Region>:<account-id>:accesspoint/<access-point-name>/object/<key>@ . For example, to copy the object @reports/january.pdf@ through access point @my-access-point@ owned by account @123456789012@ in Region @us-west-2@ , use the URL encoding of @arn:aws:s3:us-west-2:123456789012:accesspoint/my-access-point/object/reports/january.pdf@ . The value must be URL encoded. Alternatively, for objects accessed through Amazon S3 on Outposts, specify the ARN of the object as accessed in the format @arn:aws:s3-outposts:<Region>:<account-id>:outpost/<outpost-id>/object/<key>@ . For example, to copy the object @reports/january.pdf@ through outpost @my-outpost@ owned by account @123456789012@ in Region @us-west-2@ , use the URL encoding of @arn:aws:s3-outposts:us-west-2:123456789012:outpost/my-outpost/object/reports/january.pdf@ . The value must be URL encoded.  To copy a specific version of an object, append @?versionId=<version-id>@ to the value (for example, @awsexamplebucket/reports/january.pdf?versionId=QUpfdndhfd8438MNFDN93jdnJFkdmqnh893@ ). If you don't specify a version ID, Amazon S3 copies the latest version of the source object.
--
-- * 'upcKey' - Object key for which the multipart upload was initiated.
--
-- * 'upcPartNumber' - Part number of part being copied. This is a positive integer between 1 and 10,000.
--
-- * 'upcUploadId' - Upload ID identifying the multipart upload whose part is being copied.
uploadPartCopy ::
  -- | 'upcBucket'
  BucketName ->
  -- | 'upcCopySource'
  Text ->
  -- | 'upcKey'
  ObjectKey ->
  -- | 'upcPartNumber'
  Int ->
  -- | 'upcUploadId'
  Text ->
  UploadPartCopy
uploadPartCopy pBucket_ pCopySource_ pKey_ pPartNumber_ pUploadId_ =
  UploadPartCopy'
    { _upcCopySourceIfModifiedSince = Nothing,
      _upcCopySourceIfUnmodifiedSince = Nothing,
      _upcCopySourceRange = Nothing,
      _upcCopySourceSSECustomerKeyMD5 = Nothing,
      _upcCopySourceIfNoneMatch = Nothing,
      _upcSSECustomerAlgorithm = Nothing,
      _upcSSECustomerKey = Nothing,
      _upcRequestPayer = Nothing,
      _upcCopySourceIfMatch = Nothing,
      _upcExpectedSourceBucketOwner = Nothing,
      _upcSSECustomerKeyMD5 = Nothing,
      _upcCopySourceSSECustomerKey = Nothing,
      _upcCopySourceSSECustomerAlgorithm = Nothing,
      _upcExpectedBucketOwner = Nothing,
      _upcBucket = pBucket_,
      _upcCopySource = pCopySource_,
      _upcKey = pKey_,
      _upcPartNumber = pPartNumber_,
      _upcUploadId = pUploadId_
    }

-- | Copies the object if it has been modified since the specified time.
upcCopySourceIfModifiedSince :: Lens' UploadPartCopy (Maybe UTCTime)
upcCopySourceIfModifiedSince = lens _upcCopySourceIfModifiedSince (\s a -> s {_upcCopySourceIfModifiedSince = a}) . mapping _Time

-- | Copies the object if it hasn't been modified since the specified time.
upcCopySourceIfUnmodifiedSince :: Lens' UploadPartCopy (Maybe UTCTime)
upcCopySourceIfUnmodifiedSince = lens _upcCopySourceIfUnmodifiedSince (\s a -> s {_upcCopySourceIfUnmodifiedSince = a}) . mapping _Time

-- | The range of bytes to copy from the source object. The range value must use the form bytes=first-last, where the first and last are the zero-based byte offsets to copy. For example, bytes=0-9 indicates that you want to copy the first 10 bytes of the source. You can copy a range only if the source object is greater than 5 MB.
upcCopySourceRange :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceRange = lens _upcCopySourceRange (\s a -> s {_upcCopySourceRange = a})

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
upcCopySourceSSECustomerKeyMD5 :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceSSECustomerKeyMD5 = lens _upcCopySourceSSECustomerKeyMD5 (\s a -> s {_upcCopySourceSSECustomerKeyMD5 = a})

-- | Copies the object if its entity tag (ETag) is different than the specified ETag.
upcCopySourceIfNoneMatch :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceIfNoneMatch = lens _upcCopySourceIfNoneMatch (\s a -> s {_upcCopySourceIfNoneMatch = a})

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
upcSSECustomerAlgorithm :: Lens' UploadPartCopy (Maybe Text)
upcSSECustomerAlgorithm = lens _upcSSECustomerAlgorithm (\s a -> s {_upcSSECustomerAlgorithm = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header. This must be the same encryption key specified in the initiate multipart upload request.
upcSSECustomerKey :: Lens' UploadPartCopy (Maybe Text)
upcSSECustomerKey = lens _upcSSECustomerKey (\s a -> s {_upcSSECustomerKey = a}) . mapping _Sensitive

-- | Undocumented member.
upcRequestPayer :: Lens' UploadPartCopy (Maybe RequestPayer)
upcRequestPayer = lens _upcRequestPayer (\s a -> s {_upcRequestPayer = a})

-- | Copies the object if its entity tag (ETag) matches the specified tag.
upcCopySourceIfMatch :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceIfMatch = lens _upcCopySourceIfMatch (\s a -> s {_upcCopySourceIfMatch = a})

-- | The account id of the expected source bucket owner. If the source bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
upcExpectedSourceBucketOwner :: Lens' UploadPartCopy (Maybe Text)
upcExpectedSourceBucketOwner = lens _upcExpectedSourceBucketOwner (\s a -> s {_upcExpectedSourceBucketOwner = a})

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
upcSSECustomerKeyMD5 :: Lens' UploadPartCopy (Maybe Text)
upcSSECustomerKeyMD5 = lens _upcSSECustomerKeyMD5 (\s a -> s {_upcSSECustomerKeyMD5 = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use to decrypt the source object. The encryption key provided in this header must be one that was used when the source object was created.
upcCopySourceSSECustomerKey :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceSSECustomerKey = lens _upcCopySourceSSECustomerKey (\s a -> s {_upcCopySourceSSECustomerKey = a}) . mapping _Sensitive

-- | Specifies the algorithm to use when decrypting the source object (for example, AES256).
upcCopySourceSSECustomerAlgorithm :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceSSECustomerAlgorithm = lens _upcCopySourceSSECustomerAlgorithm (\s a -> s {_upcCopySourceSSECustomerAlgorithm = a})

-- | The account id of the expected destination bucket owner. If the destination bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
upcExpectedBucketOwner :: Lens' UploadPartCopy (Maybe Text)
upcExpectedBucketOwner = lens _upcExpectedBucketOwner (\s a -> s {_upcExpectedBucketOwner = a})

-- | The bucket name. When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
upcBucket :: Lens' UploadPartCopy BucketName
upcBucket = lens _upcBucket (\s a -> s {_upcBucket = a})

-- | Specifies the source object for the copy operation. You specify the value in one of two formats, depending on whether you want to access the source object through an <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-points.html access point> :     * For objects not accessed through an access point, specify the name of the source bucket and key of the source object, separated by a slash (/). For example, to copy the object @reports/january.pdf@ from the bucket @awsexamplebucket@ , use @awsexamplebucket/reports/january.pdf@ . The value must be URL encoded.     * For objects accessed through access points, specify the Amazon Resource Name (ARN) of the object as accessed through the access point, in the format @arn:aws:s3:<Region>:<account-id>:accesspoint/<access-point-name>/object/<key>@ . For example, to copy the object @reports/january.pdf@ through access point @my-access-point@ owned by account @123456789012@ in Region @us-west-2@ , use the URL encoding of @arn:aws:s3:us-west-2:123456789012:accesspoint/my-access-point/object/reports/january.pdf@ . The value must be URL encoded. Alternatively, for objects accessed through Amazon S3 on Outposts, specify the ARN of the object as accessed in the format @arn:aws:s3-outposts:<Region>:<account-id>:outpost/<outpost-id>/object/<key>@ . For example, to copy the object @reports/january.pdf@ through outpost @my-outpost@ owned by account @123456789012@ in Region @us-west-2@ , use the URL encoding of @arn:aws:s3-outposts:us-west-2:123456789012:outpost/my-outpost/object/reports/january.pdf@ . The value must be URL encoded.  To copy a specific version of an object, append @?versionId=<version-id>@ to the value (for example, @awsexamplebucket/reports/january.pdf?versionId=QUpfdndhfd8438MNFDN93jdnJFkdmqnh893@ ). If you don't specify a version ID, Amazon S3 copies the latest version of the source object.
upcCopySource :: Lens' UploadPartCopy Text
upcCopySource = lens _upcCopySource (\s a -> s {_upcCopySource = a})

-- | Object key for which the multipart upload was initiated.
upcKey :: Lens' UploadPartCopy ObjectKey
upcKey = lens _upcKey (\s a -> s {_upcKey = a})

-- | Part number of part being copied. This is a positive integer between 1 and 10,000.
upcPartNumber :: Lens' UploadPartCopy Int
upcPartNumber = lens _upcPartNumber (\s a -> s {_upcPartNumber = a})

-- | Upload ID identifying the multipart upload whose part is being copied.
upcUploadId :: Lens' UploadPartCopy Text
upcUploadId = lens _upcUploadId (\s a -> s {_upcUploadId = a})

instance AWSRequest UploadPartCopy where
  type Rs UploadPartCopy = UploadPartCopyResponse
  request = put s3
  response =
    receiveXML
      ( \s h x ->
          UploadPartCopyResponse'
            <$> (h .#? "x-amz-request-charged")
            <*> (parseXML x)
            <*> (h .#? "x-amz-server-side-encryption-customer-algorithm")
            <*> (h .#? "x-amz-copy-source-version-id")
            <*> (h .#? "x-amz-server-side-encryption-customer-key-MD5")
            <*> (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
            <*> (h .#? "x-amz-server-side-encryption")
            <*> (pure (fromEnum s))
      )

instance Hashable UploadPartCopy

instance NFData UploadPartCopy

instance ToHeaders UploadPartCopy where
  toHeaders UploadPartCopy' {..} =
    mconcat
      [ "x-amz-copy-source-if-modified-since"
          =# _upcCopySourceIfModifiedSince,
        "x-amz-copy-source-if-unmodified-since"
          =# _upcCopySourceIfUnmodifiedSince,
        "x-amz-copy-source-range" =# _upcCopySourceRange,
        "x-amz-copy-source-server-side-encryption-customer-key-MD5"
          =# _upcCopySourceSSECustomerKeyMD5,
        "x-amz-copy-source-if-none-match" =# _upcCopySourceIfNoneMatch,
        "x-amz-server-side-encryption-customer-algorithm"
          =# _upcSSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key" =# _upcSSECustomerKey,
        "x-amz-request-payer" =# _upcRequestPayer,
        "x-amz-copy-source-if-match" =# _upcCopySourceIfMatch,
        "x-amz-source-expected-bucket-owner"
          =# _upcExpectedSourceBucketOwner,
        "x-amz-server-side-encryption-customer-key-MD5"
          =# _upcSSECustomerKeyMD5,
        "x-amz-copy-source-server-side-encryption-customer-key"
          =# _upcCopySourceSSECustomerKey,
        "x-amz-copy-source-server-side-encryption-customer-algorithm"
          =# _upcCopySourceSSECustomerAlgorithm,
        "x-amz-expected-bucket-owner" =# _upcExpectedBucketOwner,
        "x-amz-copy-source" =# _upcCopySource
      ]

instance ToPath UploadPartCopy where
  toPath UploadPartCopy' {..} =
    mconcat ["/", toBS _upcBucket, "/", toBS _upcKey]

instance ToQuery UploadPartCopy where
  toQuery UploadPartCopy' {..} =
    mconcat
      ["partNumber" =: _upcPartNumber, "uploadId" =: _upcUploadId]

-- | /See:/ 'uploadPartCopyResponse' smart constructor.
data UploadPartCopyResponse = UploadPartCopyResponse'
  { _upcrsRequestCharged ::
      !(Maybe RequestCharged),
    _upcrsCopyPartResult ::
      !(Maybe CopyPartResult),
    _upcrsSSECustomerAlgorithm :: !(Maybe Text),
    _upcrsCopySourceVersionId :: !(Maybe Text),
    _upcrsSSECustomerKeyMD5 :: !(Maybe Text),
    _upcrsSSEKMSKeyId ::
      !(Maybe (Sensitive Text)),
    _upcrsServerSideEncryption ::
      !(Maybe ServerSideEncryption),
    _upcrsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UploadPartCopyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upcrsRequestCharged' - Undocumented member.
--
-- * 'upcrsCopyPartResult' - Container for all response elements.
--
-- * 'upcrsSSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- * 'upcrsCopySourceVersionId' - The version of the source object that was copied, if you have enabled versioning on the source bucket.
--
-- * 'upcrsSSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- * 'upcrsSSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- * 'upcrsServerSideEncryption' - The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- * 'upcrsResponseStatus' - -- | The response status code.
uploadPartCopyResponse ::
  -- | 'upcrsResponseStatus'
  Int ->
  UploadPartCopyResponse
uploadPartCopyResponse pResponseStatus_ =
  UploadPartCopyResponse'
    { _upcrsRequestCharged = Nothing,
      _upcrsCopyPartResult = Nothing,
      _upcrsSSECustomerAlgorithm = Nothing,
      _upcrsCopySourceVersionId = Nothing,
      _upcrsSSECustomerKeyMD5 = Nothing,
      _upcrsSSEKMSKeyId = Nothing,
      _upcrsServerSideEncryption = Nothing,
      _upcrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
upcrsRequestCharged :: Lens' UploadPartCopyResponse (Maybe RequestCharged)
upcrsRequestCharged = lens _upcrsRequestCharged (\s a -> s {_upcrsRequestCharged = a})

-- | Container for all response elements.
upcrsCopyPartResult :: Lens' UploadPartCopyResponse (Maybe CopyPartResult)
upcrsCopyPartResult = lens _upcrsCopyPartResult (\s a -> s {_upcrsCopyPartResult = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
upcrsSSECustomerAlgorithm :: Lens' UploadPartCopyResponse (Maybe Text)
upcrsSSECustomerAlgorithm = lens _upcrsSSECustomerAlgorithm (\s a -> s {_upcrsSSECustomerAlgorithm = a})

-- | The version of the source object that was copied, if you have enabled versioning on the source bucket.
upcrsCopySourceVersionId :: Lens' UploadPartCopyResponse (Maybe Text)
upcrsCopySourceVersionId = lens _upcrsCopySourceVersionId (\s a -> s {_upcrsCopySourceVersionId = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
upcrsSSECustomerKeyMD5 :: Lens' UploadPartCopyResponse (Maybe Text)
upcrsSSECustomerKeyMD5 = lens _upcrsSSECustomerKeyMD5 (\s a -> s {_upcrsSSECustomerKeyMD5 = a})

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
upcrsSSEKMSKeyId :: Lens' UploadPartCopyResponse (Maybe Text)
upcrsSSEKMSKeyId = lens _upcrsSSEKMSKeyId (\s a -> s {_upcrsSSEKMSKeyId = a}) . mapping _Sensitive

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
upcrsServerSideEncryption :: Lens' UploadPartCopyResponse (Maybe ServerSideEncryption)
upcrsServerSideEncryption = lens _upcrsServerSideEncryption (\s a -> s {_upcrsServerSideEncryption = a})

-- | -- | The response status code.
upcrsResponseStatus :: Lens' UploadPartCopyResponse Int
upcrsResponseStatus = lens _upcrsResponseStatus (\s a -> s {_upcrsResponseStatus = a})

instance NFData UploadPartCopyResponse
