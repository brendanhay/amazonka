{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.UploadPartCopy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a part by copying data from an existing object as data source.
-- You specify the data source by adding the request header
-- @x-amz-copy-source@ in your request and a byte range by adding the
-- request header @x-amz-copy-source-range@ in your request.
--
-- The minimum allowable part size for a multipart upload is 5 MB. For more
-- information about multipart upload limits, go to
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/qfacts.html Quick Facts>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- Instead of using an existing object as part data, you might use the
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
-- operation and provide data in your request.
--
-- You must initiate a multipart upload before you can upload any part. In
-- response to your initiate request. Amazon S3 returns a unique
-- identifier, the upload ID, that you must include in your upload part
-- request.
--
-- For more information about using the @UploadPartCopy@ operation, see the
-- following:
--
-- -   For conceptual information about multipart uploads, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/uploadobjusingmpu.html Uploading Objects Using Multipart Upload>
--     in the /Amazon Simple Storage Service Developer Guide/.
--
-- -   For information about permissions required to use the multipart
--     upload API, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions>
--     in the /Amazon Simple Storage Service Developer Guide/.
--
-- -   For information about copying objects using a single atomic
--     operation vs. the multipart upload, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/ObjectOperations.html Operations on Objects>
--     in the /Amazon Simple Storage Service Developer Guide/.
--
-- -   For information about using server-side encryption with
--     customer-provided encryption keys with the UploadPartCopy operation,
--     see
--     <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CopyObject.html CopyObject>
--     and
--     <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>.
--
-- Note the following additional considerations about the request headers
-- @x-amz-copy-source-if-match@, @x-amz-copy-source-if-none-match@,
-- @x-amz-copy-source-if-unmodified-since@, and
-- @x-amz-copy-source-if-modified-since@:
--
-- -   __Consideration 1__ - If both of the @x-amz-copy-source-if-match@
--     and @x-amz-copy-source-if-unmodified-since@ headers are present in
--     the request as follows:
--
--     @x-amz-copy-source-if-match@ condition evaluates to @true@, and;
--
--     @x-amz-copy-source-if-unmodified-since@ condition evaluates to
--     @false@;
--
--     Amazon S3 returns @200 OK@ and copies the data.
--
-- -   __Consideration 2__ - If both of the
--     @x-amz-copy-source-if-none-match@ and
--     @x-amz-copy-source-if-modified-since@ headers are present in the
--     request as follows:
--
--     @x-amz-copy-source-if-none-match@ condition evaluates to @false@,
--     and;
--
--     @x-amz-copy-source-if-modified-since@ condition evaluates to @true@;
--
--     Amazon S3 returns @412 Precondition Failed@ response code.
--
-- __Versioning__
--
-- If your bucket has versioning enabled, you could have multiple versions
-- of the same object. By default, @x-amz-copy-source@ identifies the
-- current version of the object to copy. If the current version is a
-- delete marker and you don\'t specify a versionId in the
-- @x-amz-copy-source@, Amazon S3 returns a 404 error, because the object
-- does not exist. If you specify versionId in the @x-amz-copy-source@ and
-- the versionId is a delete marker, Amazon S3 returns an HTTP 400 error,
-- because you are not allowed to specify a delete marker as a version for
-- the @x-amz-copy-source@.
--
-- You can optionally specify a specific version of the source object to
-- copy by adding the @versionId@ subresource as shown in the following
-- example:
--
-- @x-amz-copy-source: \/bucket\/object?versionId=version id@
--
-- __Special Errors__
--
-- -   -   /Code: NoSuchUpload/
--
--     -   /Cause: The specified multipart upload does not exist. The
--         upload ID might be invalid, or the multipart upload might have
--         been aborted or completed./
--
--     -   /HTTP Status Code: 404 Not Found/
--
-- -   -   /Code: InvalidRequest/
--
--     -   /Cause: The specified copy source is not supported as a
--         byte-range copy source./
--
--     -   /HTTP Status Code: 400 Bad Request/
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads>
module Network.AWS.S3.UploadPartCopy
  ( -- * Creating a Request
    UploadPartCopy (..),
    newUploadPartCopy,

    -- * Request Lenses
    uploadPartCopy_copySourceIfMatch,
    uploadPartCopy_expectedSourceBucketOwner,
    uploadPartCopy_expectedBucketOwner,
    uploadPartCopy_copySourceSSECustomerKey,
    uploadPartCopy_copySourceSSECustomerAlgorithm,
    uploadPartCopy_copySourceIfNoneMatch,
    uploadPartCopy_sSECustomerKeyMD5,
    uploadPartCopy_copySourceIfUnmodifiedSince,
    uploadPartCopy_copySourceRange,
    uploadPartCopy_copySourceIfModifiedSince,
    uploadPartCopy_sSECustomerAlgorithm,
    uploadPartCopy_requestPayer,
    uploadPartCopy_sSECustomerKey,
    uploadPartCopy_copySourceSSECustomerKeyMD5,
    uploadPartCopy_bucket,
    uploadPartCopy_copySource,
    uploadPartCopy_key,
    uploadPartCopy_partNumber,
    uploadPartCopy_uploadId,

    -- * Destructuring the Response
    UploadPartCopyResponse (..),
    newUploadPartCopyResponse,

    -- * Response Lenses
    uploadPartCopyResponse_requestCharged,
    uploadPartCopyResponse_copySourceVersionId,
    uploadPartCopyResponse_copyPartResult,
    uploadPartCopyResponse_sSEKMSKeyId,
    uploadPartCopyResponse_sSECustomerKeyMD5,
    uploadPartCopyResponse_bucketKeyEnabled,
    uploadPartCopyResponse_serverSideEncryption,
    uploadPartCopyResponse_sSECustomerAlgorithm,
    uploadPartCopyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newUploadPartCopy' smart constructor.
data UploadPartCopy = UploadPartCopy'
  { -- | Copies the object if its entity tag (ETag) matches the specified tag.
    copySourceIfMatch :: Core.Maybe Core.Text,
    -- | The account id of the expected source bucket owner. If the source bucket
    -- is owned by a different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedSourceBucketOwner :: Core.Maybe Core.Text,
    -- | The account id of the expected destination bucket owner. If the
    -- destination bucket is owned by a different account, the request will
    -- fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use to
    -- decrypt the source object. The encryption key provided in this header
    -- must be one that was used when the source object was created.
    copySourceSSECustomerKey :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Specifies the algorithm to use when decrypting the source object (for
    -- example, AES256).
    copySourceSSECustomerAlgorithm :: Core.Maybe Core.Text,
    -- | Copies the object if its entity tag (ETag) is different than the
    -- specified ETag.
    copySourceIfNoneMatch :: Core.Maybe Core.Text,
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
    -- 1321. Amazon S3 uses this header for a message integrity check to ensure
    -- that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Core.Maybe Core.Text,
    -- | Copies the object if it hasn\'t been modified since the specified time.
    copySourceIfUnmodifiedSince :: Core.Maybe Core.ISO8601,
    -- | The range of bytes to copy from the source object. The range value must
    -- use the form bytes=first-last, where the first and last are the
    -- zero-based byte offsets to copy. For example, bytes=0-9 indicates that
    -- you want to copy the first 10 bytes of the source. You can copy a range
    -- only if the source object is greater than 5 MB.
    copySourceRange :: Core.Maybe Core.Text,
    -- | Copies the object if it has been modified since the specified time.
    copySourceIfModifiedSince :: Core.Maybe Core.ISO8601,
    -- | Specifies the algorithm to use to when encrypting the object (for
    -- example, AES256).
    sSECustomerAlgorithm :: Core.Maybe Core.Text,
    requestPayer :: Core.Maybe RequestPayer,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use in
    -- encrypting data. This value is used to store the object and then it is
    -- discarded; Amazon S3 does not store the encryption key. The key must be
    -- appropriate for use with the algorithm specified in the
    -- @x-amz-server-side-encryption-customer-algorithm@ header. This must be
    -- the same encryption key specified in the initiate multipart upload
    -- request.
    sSECustomerKey :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
    -- 1321. Amazon S3 uses this header for a message integrity check to ensure
    -- that the encryption key was transmitted without error.
    copySourceSSECustomerKeyMD5 :: Core.Maybe Core.Text,
    -- | The bucket name.
    --
    -- When using this API with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this operation with an access point through the AWS SDKs, you
    -- provide the access point ARN in place of the bucket name. For more
    -- information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    --
    -- When using this API with Amazon S3 on Outposts, you must direct requests
    -- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
    -- form
    -- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
    -- When using this operation using S3 on Outposts through the AWS SDKs, you
    -- provide the Outposts bucket ARN in place of the bucket name. For more
    -- information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    bucket :: BucketName,
    -- | Specifies the source object for the copy operation. You specify the
    -- value in one of two formats, depending on whether you want to access the
    -- source object through an
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-points.html access point>:
    --
    -- -   For objects not accessed through an access point, specify the name
    --     of the source bucket and key of the source object, separated by a
    --     slash (\/). For example, to copy the object @reports\/january.pdf@
    --     from the bucket @awsexamplebucket@, use
    --     @awsexamplebucket\/reports\/january.pdf@. The value must be URL
    --     encoded.
    --
    -- -   For objects accessed through access points, specify the Amazon
    --     Resource Name (ARN) of the object as accessed through the access
    --     point, in the format
    --     @arn:aws:s3:\<Region>:\<account-id>:accesspoint\/\<access-point-name>\/object\/\<key>@.
    --     For example, to copy the object @reports\/january.pdf@ through
    --     access point @my-access-point@ owned by account @123456789012@ in
    --     Region @us-west-2@, use the URL encoding of
    --     @arn:aws:s3:us-west-2:123456789012:accesspoint\/my-access-point\/object\/reports\/january.pdf@.
    --     The value must be URL encoded.
    --
    --     Amazon S3 supports copy operations using access points only when the
    --     source and destination buckets are in the same AWS Region.
    --
    --     Alternatively, for objects accessed through Amazon S3 on Outposts,
    --     specify the ARN of the object as accessed in the format
    --     @arn:aws:s3-outposts:\<Region>:\<account-id>:outpost\/\<outpost-id>\/object\/\<key>@.
    --     For example, to copy the object @reports\/january.pdf@ through
    --     outpost @my-outpost@ owned by account @123456789012@ in Region
    --     @us-west-2@, use the URL encoding of
    --     @arn:aws:s3-outposts:us-west-2:123456789012:outpost\/my-outpost\/object\/reports\/january.pdf@.
    --     The value must be URL encoded.
    --
    -- To copy a specific version of an object, append
    -- @?versionId=\<version-id>@ to the value (for example,
    -- @awsexamplebucket\/reports\/january.pdf?versionId=QUpfdndhfd8438MNFDN93jdnJFkdmqnh893@).
    -- If you don\'t specify a version ID, Amazon S3 copies the latest version
    -- of the source object.
    copySource :: Core.Text,
    -- | Object key for which the multipart upload was initiated.
    key :: ObjectKey,
    -- | Part number of part being copied. This is a positive integer between 1
    -- and 10,000.
    partNumber :: Core.Int,
    -- | Upload ID identifying the multipart upload whose part is being copied.
    uploadId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UploadPartCopy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copySourceIfMatch', 'uploadPartCopy_copySourceIfMatch' - Copies the object if its entity tag (ETag) matches the specified tag.
--
-- 'expectedSourceBucketOwner', 'uploadPartCopy_expectedSourceBucketOwner' - The account id of the expected source bucket owner. If the source bucket
-- is owned by a different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'expectedBucketOwner', 'uploadPartCopy_expectedBucketOwner' - The account id of the expected destination bucket owner. If the
-- destination bucket is owned by a different account, the request will
-- fail with an HTTP @403 (Access Denied)@ error.
--
-- 'copySourceSSECustomerKey', 'uploadPartCopy_copySourceSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header
-- must be one that was used when the source object was created.
--
-- 'copySourceSSECustomerAlgorithm', 'uploadPartCopy_copySourceSSECustomerAlgorithm' - Specifies the algorithm to use when decrypting the source object (for
-- example, AES256).
--
-- 'copySourceIfNoneMatch', 'uploadPartCopy_copySourceIfNoneMatch' - Copies the object if its entity tag (ETag) is different than the
-- specified ETag.
--
-- 'sSECustomerKeyMD5', 'uploadPartCopy_sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'copySourceIfUnmodifiedSince', 'uploadPartCopy_copySourceIfUnmodifiedSince' - Copies the object if it hasn\'t been modified since the specified time.
--
-- 'copySourceRange', 'uploadPartCopy_copySourceRange' - The range of bytes to copy from the source object. The range value must
-- use the form bytes=first-last, where the first and last are the
-- zero-based byte offsets to copy. For example, bytes=0-9 indicates that
-- you want to copy the first 10 bytes of the source. You can copy a range
-- only if the source object is greater than 5 MB.
--
-- 'copySourceIfModifiedSince', 'uploadPartCopy_copySourceIfModifiedSince' - Copies the object if it has been modified since the specified time.
--
-- 'sSECustomerAlgorithm', 'uploadPartCopy_sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
--
-- 'requestPayer', 'uploadPartCopy_requestPayer' - Undocumented member.
--
-- 'sSECustomerKey', 'uploadPartCopy_sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header. This must be
-- the same encryption key specified in the initiate multipart upload
-- request.
--
-- 'copySourceSSECustomerKeyMD5', 'uploadPartCopy_copySourceSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'bucket', 'uploadPartCopy_bucket' - The bucket name.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- When using this API with Amazon S3 on Outposts, you must direct requests
-- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
-- form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this operation using S3 on Outposts through the AWS SDKs, you
-- provide the Outposts bucket ARN in place of the bucket name. For more
-- information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- 'copySource', 'uploadPartCopy_copySource' - Specifies the source object for the copy operation. You specify the
-- value in one of two formats, depending on whether you want to access the
-- source object through an
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-points.html access point>:
--
-- -   For objects not accessed through an access point, specify the name
--     of the source bucket and key of the source object, separated by a
--     slash (\/). For example, to copy the object @reports\/january.pdf@
--     from the bucket @awsexamplebucket@, use
--     @awsexamplebucket\/reports\/january.pdf@. The value must be URL
--     encoded.
--
-- -   For objects accessed through access points, specify the Amazon
--     Resource Name (ARN) of the object as accessed through the access
--     point, in the format
--     @arn:aws:s3:\<Region>:\<account-id>:accesspoint\/\<access-point-name>\/object\/\<key>@.
--     For example, to copy the object @reports\/january.pdf@ through
--     access point @my-access-point@ owned by account @123456789012@ in
--     Region @us-west-2@, use the URL encoding of
--     @arn:aws:s3:us-west-2:123456789012:accesspoint\/my-access-point\/object\/reports\/january.pdf@.
--     The value must be URL encoded.
--
--     Amazon S3 supports copy operations using access points only when the
--     source and destination buckets are in the same AWS Region.
--
--     Alternatively, for objects accessed through Amazon S3 on Outposts,
--     specify the ARN of the object as accessed in the format
--     @arn:aws:s3-outposts:\<Region>:\<account-id>:outpost\/\<outpost-id>\/object\/\<key>@.
--     For example, to copy the object @reports\/january.pdf@ through
--     outpost @my-outpost@ owned by account @123456789012@ in Region
--     @us-west-2@, use the URL encoding of
--     @arn:aws:s3-outposts:us-west-2:123456789012:outpost\/my-outpost\/object\/reports\/january.pdf@.
--     The value must be URL encoded.
--
-- To copy a specific version of an object, append
-- @?versionId=\<version-id>@ to the value (for example,
-- @awsexamplebucket\/reports\/january.pdf?versionId=QUpfdndhfd8438MNFDN93jdnJFkdmqnh893@).
-- If you don\'t specify a version ID, Amazon S3 copies the latest version
-- of the source object.
--
-- 'key', 'uploadPartCopy_key' - Object key for which the multipart upload was initiated.
--
-- 'partNumber', 'uploadPartCopy_partNumber' - Part number of part being copied. This is a positive integer between 1
-- and 10,000.
--
-- 'uploadId', 'uploadPartCopy_uploadId' - Upload ID identifying the multipart upload whose part is being copied.
newUploadPartCopy ::
  -- | 'bucket'
  BucketName ->
  -- | 'copySource'
  Core.Text ->
  -- | 'key'
  ObjectKey ->
  -- | 'partNumber'
  Core.Int ->
  -- | 'uploadId'
  Core.Text ->
  UploadPartCopy
newUploadPartCopy
  pBucket_
  pCopySource_
  pKey_
  pPartNumber_
  pUploadId_ =
    UploadPartCopy'
      { copySourceIfMatch = Core.Nothing,
        expectedSourceBucketOwner = Core.Nothing,
        expectedBucketOwner = Core.Nothing,
        copySourceSSECustomerKey = Core.Nothing,
        copySourceSSECustomerAlgorithm = Core.Nothing,
        copySourceIfNoneMatch = Core.Nothing,
        sSECustomerKeyMD5 = Core.Nothing,
        copySourceIfUnmodifiedSince = Core.Nothing,
        copySourceRange = Core.Nothing,
        copySourceIfModifiedSince = Core.Nothing,
        sSECustomerAlgorithm = Core.Nothing,
        requestPayer = Core.Nothing,
        sSECustomerKey = Core.Nothing,
        copySourceSSECustomerKeyMD5 = Core.Nothing,
        bucket = pBucket_,
        copySource = pCopySource_,
        key = pKey_,
        partNumber = pPartNumber_,
        uploadId = pUploadId_
      }

-- | Copies the object if its entity tag (ETag) matches the specified tag.
uploadPartCopy_copySourceIfMatch :: Lens.Lens' UploadPartCopy (Core.Maybe Core.Text)
uploadPartCopy_copySourceIfMatch = Lens.lens (\UploadPartCopy' {copySourceIfMatch} -> copySourceIfMatch) (\s@UploadPartCopy' {} a -> s {copySourceIfMatch = a} :: UploadPartCopy)

-- | The account id of the expected source bucket owner. If the source bucket
-- is owned by a different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
uploadPartCopy_expectedSourceBucketOwner :: Lens.Lens' UploadPartCopy (Core.Maybe Core.Text)
uploadPartCopy_expectedSourceBucketOwner = Lens.lens (\UploadPartCopy' {expectedSourceBucketOwner} -> expectedSourceBucketOwner) (\s@UploadPartCopy' {} a -> s {expectedSourceBucketOwner = a} :: UploadPartCopy)

-- | The account id of the expected destination bucket owner. If the
-- destination bucket is owned by a different account, the request will
-- fail with an HTTP @403 (Access Denied)@ error.
uploadPartCopy_expectedBucketOwner :: Lens.Lens' UploadPartCopy (Core.Maybe Core.Text)
uploadPartCopy_expectedBucketOwner = Lens.lens (\UploadPartCopy' {expectedBucketOwner} -> expectedBucketOwner) (\s@UploadPartCopy' {} a -> s {expectedBucketOwner = a} :: UploadPartCopy)

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header
-- must be one that was used when the source object was created.
uploadPartCopy_copySourceSSECustomerKey :: Lens.Lens' UploadPartCopy (Core.Maybe Core.Text)
uploadPartCopy_copySourceSSECustomerKey = Lens.lens (\UploadPartCopy' {copySourceSSECustomerKey} -> copySourceSSECustomerKey) (\s@UploadPartCopy' {} a -> s {copySourceSSECustomerKey = a} :: UploadPartCopy) Core.. Lens.mapping Core._Sensitive

-- | Specifies the algorithm to use when decrypting the source object (for
-- example, AES256).
uploadPartCopy_copySourceSSECustomerAlgorithm :: Lens.Lens' UploadPartCopy (Core.Maybe Core.Text)
uploadPartCopy_copySourceSSECustomerAlgorithm = Lens.lens (\UploadPartCopy' {copySourceSSECustomerAlgorithm} -> copySourceSSECustomerAlgorithm) (\s@UploadPartCopy' {} a -> s {copySourceSSECustomerAlgorithm = a} :: UploadPartCopy)

-- | Copies the object if its entity tag (ETag) is different than the
-- specified ETag.
uploadPartCopy_copySourceIfNoneMatch :: Lens.Lens' UploadPartCopy (Core.Maybe Core.Text)
uploadPartCopy_copySourceIfNoneMatch = Lens.lens (\UploadPartCopy' {copySourceIfNoneMatch} -> copySourceIfNoneMatch) (\s@UploadPartCopy' {} a -> s {copySourceIfNoneMatch = a} :: UploadPartCopy)

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
uploadPartCopy_sSECustomerKeyMD5 :: Lens.Lens' UploadPartCopy (Core.Maybe Core.Text)
uploadPartCopy_sSECustomerKeyMD5 = Lens.lens (\UploadPartCopy' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@UploadPartCopy' {} a -> s {sSECustomerKeyMD5 = a} :: UploadPartCopy)

-- | Copies the object if it hasn\'t been modified since the specified time.
uploadPartCopy_copySourceIfUnmodifiedSince :: Lens.Lens' UploadPartCopy (Core.Maybe Core.UTCTime)
uploadPartCopy_copySourceIfUnmodifiedSince = Lens.lens (\UploadPartCopy' {copySourceIfUnmodifiedSince} -> copySourceIfUnmodifiedSince) (\s@UploadPartCopy' {} a -> s {copySourceIfUnmodifiedSince = a} :: UploadPartCopy) Core.. Lens.mapping Core._Time

-- | The range of bytes to copy from the source object. The range value must
-- use the form bytes=first-last, where the first and last are the
-- zero-based byte offsets to copy. For example, bytes=0-9 indicates that
-- you want to copy the first 10 bytes of the source. You can copy a range
-- only if the source object is greater than 5 MB.
uploadPartCopy_copySourceRange :: Lens.Lens' UploadPartCopy (Core.Maybe Core.Text)
uploadPartCopy_copySourceRange = Lens.lens (\UploadPartCopy' {copySourceRange} -> copySourceRange) (\s@UploadPartCopy' {} a -> s {copySourceRange = a} :: UploadPartCopy)

-- | Copies the object if it has been modified since the specified time.
uploadPartCopy_copySourceIfModifiedSince :: Lens.Lens' UploadPartCopy (Core.Maybe Core.UTCTime)
uploadPartCopy_copySourceIfModifiedSince = Lens.lens (\UploadPartCopy' {copySourceIfModifiedSince} -> copySourceIfModifiedSince) (\s@UploadPartCopy' {} a -> s {copySourceIfModifiedSince = a} :: UploadPartCopy) Core.. Lens.mapping Core._Time

-- | Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
uploadPartCopy_sSECustomerAlgorithm :: Lens.Lens' UploadPartCopy (Core.Maybe Core.Text)
uploadPartCopy_sSECustomerAlgorithm = Lens.lens (\UploadPartCopy' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@UploadPartCopy' {} a -> s {sSECustomerAlgorithm = a} :: UploadPartCopy)

-- | Undocumented member.
uploadPartCopy_requestPayer :: Lens.Lens' UploadPartCopy (Core.Maybe RequestPayer)
uploadPartCopy_requestPayer = Lens.lens (\UploadPartCopy' {requestPayer} -> requestPayer) (\s@UploadPartCopy' {} a -> s {requestPayer = a} :: UploadPartCopy)

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header. This must be
-- the same encryption key specified in the initiate multipart upload
-- request.
uploadPartCopy_sSECustomerKey :: Lens.Lens' UploadPartCopy (Core.Maybe Core.Text)
uploadPartCopy_sSECustomerKey = Lens.lens (\UploadPartCopy' {sSECustomerKey} -> sSECustomerKey) (\s@UploadPartCopy' {} a -> s {sSECustomerKey = a} :: UploadPartCopy) Core.. Lens.mapping Core._Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
uploadPartCopy_copySourceSSECustomerKeyMD5 :: Lens.Lens' UploadPartCopy (Core.Maybe Core.Text)
uploadPartCopy_copySourceSSECustomerKeyMD5 = Lens.lens (\UploadPartCopy' {copySourceSSECustomerKeyMD5} -> copySourceSSECustomerKeyMD5) (\s@UploadPartCopy' {} a -> s {copySourceSSECustomerKeyMD5 = a} :: UploadPartCopy)

-- | The bucket name.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- When using this API with Amazon S3 on Outposts, you must direct requests
-- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
-- form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this operation using S3 on Outposts through the AWS SDKs, you
-- provide the Outposts bucket ARN in place of the bucket name. For more
-- information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon Simple Storage Service Developer Guide/.
uploadPartCopy_bucket :: Lens.Lens' UploadPartCopy BucketName
uploadPartCopy_bucket = Lens.lens (\UploadPartCopy' {bucket} -> bucket) (\s@UploadPartCopy' {} a -> s {bucket = a} :: UploadPartCopy)

-- | Specifies the source object for the copy operation. You specify the
-- value in one of two formats, depending on whether you want to access the
-- source object through an
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-points.html access point>:
--
-- -   For objects not accessed through an access point, specify the name
--     of the source bucket and key of the source object, separated by a
--     slash (\/). For example, to copy the object @reports\/january.pdf@
--     from the bucket @awsexamplebucket@, use
--     @awsexamplebucket\/reports\/january.pdf@. The value must be URL
--     encoded.
--
-- -   For objects accessed through access points, specify the Amazon
--     Resource Name (ARN) of the object as accessed through the access
--     point, in the format
--     @arn:aws:s3:\<Region>:\<account-id>:accesspoint\/\<access-point-name>\/object\/\<key>@.
--     For example, to copy the object @reports\/january.pdf@ through
--     access point @my-access-point@ owned by account @123456789012@ in
--     Region @us-west-2@, use the URL encoding of
--     @arn:aws:s3:us-west-2:123456789012:accesspoint\/my-access-point\/object\/reports\/january.pdf@.
--     The value must be URL encoded.
--
--     Amazon S3 supports copy operations using access points only when the
--     source and destination buckets are in the same AWS Region.
--
--     Alternatively, for objects accessed through Amazon S3 on Outposts,
--     specify the ARN of the object as accessed in the format
--     @arn:aws:s3-outposts:\<Region>:\<account-id>:outpost\/\<outpost-id>\/object\/\<key>@.
--     For example, to copy the object @reports\/january.pdf@ through
--     outpost @my-outpost@ owned by account @123456789012@ in Region
--     @us-west-2@, use the URL encoding of
--     @arn:aws:s3-outposts:us-west-2:123456789012:outpost\/my-outpost\/object\/reports\/january.pdf@.
--     The value must be URL encoded.
--
-- To copy a specific version of an object, append
-- @?versionId=\<version-id>@ to the value (for example,
-- @awsexamplebucket\/reports\/january.pdf?versionId=QUpfdndhfd8438MNFDN93jdnJFkdmqnh893@).
-- If you don\'t specify a version ID, Amazon S3 copies the latest version
-- of the source object.
uploadPartCopy_copySource :: Lens.Lens' UploadPartCopy Core.Text
uploadPartCopy_copySource = Lens.lens (\UploadPartCopy' {copySource} -> copySource) (\s@UploadPartCopy' {} a -> s {copySource = a} :: UploadPartCopy)

-- | Object key for which the multipart upload was initiated.
uploadPartCopy_key :: Lens.Lens' UploadPartCopy ObjectKey
uploadPartCopy_key = Lens.lens (\UploadPartCopy' {key} -> key) (\s@UploadPartCopy' {} a -> s {key = a} :: UploadPartCopy)

-- | Part number of part being copied. This is a positive integer between 1
-- and 10,000.
uploadPartCopy_partNumber :: Lens.Lens' UploadPartCopy Core.Int
uploadPartCopy_partNumber = Lens.lens (\UploadPartCopy' {partNumber} -> partNumber) (\s@UploadPartCopy' {} a -> s {partNumber = a} :: UploadPartCopy)

-- | Upload ID identifying the multipart upload whose part is being copied.
uploadPartCopy_uploadId :: Lens.Lens' UploadPartCopy Core.Text
uploadPartCopy_uploadId = Lens.lens (\UploadPartCopy' {uploadId} -> uploadId) (\s@UploadPartCopy' {} a -> s {uploadId = a} :: UploadPartCopy)

instance Core.AWSRequest UploadPartCopy where
  type
    AWSResponse UploadPartCopy =
      UploadPartCopyResponse
  request = Request.put defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          UploadPartCopyResponse'
            Core.<$> (h Core..#? "x-amz-request-charged")
            Core.<*> (h Core..#? "x-amz-copy-source-version-id")
            Core.<*> (Core.parseXML x)
            Core.<*> ( h
                         Core..#? "x-amz-server-side-encryption-aws-kms-key-id"
                     )
            Core.<*> ( h
                         Core..#? "x-amz-server-side-encryption-customer-key-MD5"
                     )
            Core.<*> ( h
                         Core..#? "x-amz-server-side-encryption-bucket-key-enabled"
                     )
            Core.<*> (h Core..#? "x-amz-server-side-encryption")
            Core.<*> ( h
                         Core..#? "x-amz-server-side-encryption-customer-algorithm"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UploadPartCopy

instance Core.NFData UploadPartCopy

instance Core.ToHeaders UploadPartCopy where
  toHeaders UploadPartCopy' {..} =
    Core.mconcat
      [ "x-amz-copy-source-if-match"
          Core.=# copySourceIfMatch,
        "x-amz-source-expected-bucket-owner"
          Core.=# expectedSourceBucketOwner,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "x-amz-copy-source-server-side-encryption-customer-key"
          Core.=# copySourceSSECustomerKey,
        "x-amz-copy-source-server-side-encryption-customer-algorithm"
          Core.=# copySourceSSECustomerAlgorithm,
        "x-amz-copy-source-if-none-match"
          Core.=# copySourceIfNoneMatch,
        "x-amz-server-side-encryption-customer-key-MD5"
          Core.=# sSECustomerKeyMD5,
        "x-amz-copy-source-if-unmodified-since"
          Core.=# copySourceIfUnmodifiedSince,
        "x-amz-copy-source-range" Core.=# copySourceRange,
        "x-amz-copy-source-if-modified-since"
          Core.=# copySourceIfModifiedSince,
        "x-amz-server-side-encryption-customer-algorithm"
          Core.=# sSECustomerAlgorithm,
        "x-amz-request-payer" Core.=# requestPayer,
        "x-amz-server-side-encryption-customer-key"
          Core.=# sSECustomerKey,
        "x-amz-copy-source-server-side-encryption-customer-key-MD5"
          Core.=# copySourceSSECustomerKeyMD5,
        "x-amz-copy-source" Core.=# copySource
      ]

instance Core.ToPath UploadPartCopy where
  toPath UploadPartCopy' {..} =
    Core.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery UploadPartCopy where
  toQuery UploadPartCopy' {..} =
    Core.mconcat
      [ "partNumber" Core.=: partNumber,
        "uploadId" Core.=: uploadId
      ]

-- | /See:/ 'newUploadPartCopyResponse' smart constructor.
data UploadPartCopyResponse = UploadPartCopyResponse'
  { requestCharged :: Core.Maybe RequestCharged,
    -- | The version of the source object that was copied, if you have enabled
    -- versioning on the source bucket.
    copySourceVersionId :: Core.Maybe Core.Text,
    -- | Container for all response elements.
    copyPartResult :: Core.Maybe CopyPartResult,
    -- | If present, specifies the ID of the AWS Key Management Service (AWS KMS)
    -- symmetric customer managed customer master key (CMK) that was used for
    -- the object.
    sSEKMSKeyId :: Core.Maybe (Core.Sensitive Core.Text),
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header to provide round-trip
    -- message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Core.Maybe Core.Text,
    -- | Indicates whether the multipart upload uses an S3 Bucket Key for
    -- server-side encryption with AWS KMS (SSE-KMS).
    bucketKeyEnabled :: Core.Maybe Core.Bool,
    -- | The server-side encryption algorithm used when storing this object in
    -- Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Core.Maybe ServerSideEncryption,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header confirming the
    -- encryption algorithm used.
    sSECustomerAlgorithm :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UploadPartCopyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'uploadPartCopyResponse_requestCharged' - Undocumented member.
--
-- 'copySourceVersionId', 'uploadPartCopyResponse_copySourceVersionId' - The version of the source object that was copied, if you have enabled
-- versioning on the source bucket.
--
-- 'copyPartResult', 'uploadPartCopyResponse_copyPartResult' - Container for all response elements.
--
-- 'sSEKMSKeyId', 'uploadPartCopyResponse_sSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS)
-- symmetric customer managed customer master key (CMK) that was used for
-- the object.
--
-- 'sSECustomerKeyMD5', 'uploadPartCopyResponse_sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
--
-- 'bucketKeyEnabled', 'uploadPartCopyResponse_bucketKeyEnabled' - Indicates whether the multipart upload uses an S3 Bucket Key for
-- server-side encryption with AWS KMS (SSE-KMS).
--
-- 'serverSideEncryption', 'uploadPartCopyResponse_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
--
-- 'sSECustomerAlgorithm', 'uploadPartCopyResponse_sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
--
-- 'httpStatus', 'uploadPartCopyResponse_httpStatus' - The response's http status code.
newUploadPartCopyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UploadPartCopyResponse
newUploadPartCopyResponse pHttpStatus_ =
  UploadPartCopyResponse'
    { requestCharged =
        Core.Nothing,
      copySourceVersionId = Core.Nothing,
      copyPartResult = Core.Nothing,
      sSEKMSKeyId = Core.Nothing,
      sSECustomerKeyMD5 = Core.Nothing,
      bucketKeyEnabled = Core.Nothing,
      serverSideEncryption = Core.Nothing,
      sSECustomerAlgorithm = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
uploadPartCopyResponse_requestCharged :: Lens.Lens' UploadPartCopyResponse (Core.Maybe RequestCharged)
uploadPartCopyResponse_requestCharged = Lens.lens (\UploadPartCopyResponse' {requestCharged} -> requestCharged) (\s@UploadPartCopyResponse' {} a -> s {requestCharged = a} :: UploadPartCopyResponse)

-- | The version of the source object that was copied, if you have enabled
-- versioning on the source bucket.
uploadPartCopyResponse_copySourceVersionId :: Lens.Lens' UploadPartCopyResponse (Core.Maybe Core.Text)
uploadPartCopyResponse_copySourceVersionId = Lens.lens (\UploadPartCopyResponse' {copySourceVersionId} -> copySourceVersionId) (\s@UploadPartCopyResponse' {} a -> s {copySourceVersionId = a} :: UploadPartCopyResponse)

-- | Container for all response elements.
uploadPartCopyResponse_copyPartResult :: Lens.Lens' UploadPartCopyResponse (Core.Maybe CopyPartResult)
uploadPartCopyResponse_copyPartResult = Lens.lens (\UploadPartCopyResponse' {copyPartResult} -> copyPartResult) (\s@UploadPartCopyResponse' {} a -> s {copyPartResult = a} :: UploadPartCopyResponse)

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS)
-- symmetric customer managed customer master key (CMK) that was used for
-- the object.
uploadPartCopyResponse_sSEKMSKeyId :: Lens.Lens' UploadPartCopyResponse (Core.Maybe Core.Text)
uploadPartCopyResponse_sSEKMSKeyId = Lens.lens (\UploadPartCopyResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@UploadPartCopyResponse' {} a -> s {sSEKMSKeyId = a} :: UploadPartCopyResponse) Core.. Lens.mapping Core._Sensitive

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
uploadPartCopyResponse_sSECustomerKeyMD5 :: Lens.Lens' UploadPartCopyResponse (Core.Maybe Core.Text)
uploadPartCopyResponse_sSECustomerKeyMD5 = Lens.lens (\UploadPartCopyResponse' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@UploadPartCopyResponse' {} a -> s {sSECustomerKeyMD5 = a} :: UploadPartCopyResponse)

-- | Indicates whether the multipart upload uses an S3 Bucket Key for
-- server-side encryption with AWS KMS (SSE-KMS).
uploadPartCopyResponse_bucketKeyEnabled :: Lens.Lens' UploadPartCopyResponse (Core.Maybe Core.Bool)
uploadPartCopyResponse_bucketKeyEnabled = Lens.lens (\UploadPartCopyResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@UploadPartCopyResponse' {} a -> s {bucketKeyEnabled = a} :: UploadPartCopyResponse)

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
uploadPartCopyResponse_serverSideEncryption :: Lens.Lens' UploadPartCopyResponse (Core.Maybe ServerSideEncryption)
uploadPartCopyResponse_serverSideEncryption = Lens.lens (\UploadPartCopyResponse' {serverSideEncryption} -> serverSideEncryption) (\s@UploadPartCopyResponse' {} a -> s {serverSideEncryption = a} :: UploadPartCopyResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
uploadPartCopyResponse_sSECustomerAlgorithm :: Lens.Lens' UploadPartCopyResponse (Core.Maybe Core.Text)
uploadPartCopyResponse_sSECustomerAlgorithm = Lens.lens (\UploadPartCopyResponse' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@UploadPartCopyResponse' {} a -> s {sSECustomerAlgorithm = a} :: UploadPartCopyResponse)

-- | The response's http status code.
uploadPartCopyResponse_httpStatus :: Lens.Lens' UploadPartCopyResponse Core.Int
uploadPartCopyResponse_httpStatus = Lens.lens (\UploadPartCopyResponse' {httpStatus} -> httpStatus) (\s@UploadPartCopyResponse' {} a -> s {httpStatus = a} :: UploadPartCopyResponse)

instance Core.NFData UploadPartCopyResponse
