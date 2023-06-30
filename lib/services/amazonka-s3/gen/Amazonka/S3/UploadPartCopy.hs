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
-- Module      : Amazonka.S3.UploadPartCopy
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- For information about maximum and minimum part sizes and other multipart
-- upload specifications, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/qfacts.html Multipart upload limits>
-- in the /Amazon S3 User Guide/.
--
-- Instead of using an existing object as part data, you might use the
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
-- action and provide data in your request.
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
--     in the /Amazon S3 User Guide/.
--
-- -   For information about permissions required to use the multipart
--     upload API, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload and Permissions>
--     in the /Amazon S3 User Guide/.
--
-- -   For information about copying objects using a single atomic action
--     vs. a multipart upload, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/ObjectOperations.html Operations on Objects>
--     in the /Amazon S3 User Guide/.
--
-- -   For information about using server-side encryption with
--     customer-provided encryption keys with the @UploadPartCopy@
--     operation, see
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
module Amazonka.S3.UploadPartCopy
  ( -- * Creating a Request
    UploadPartCopy (..),
    newUploadPartCopy,

    -- * Request Lenses
    uploadPartCopy_copySourceIfMatch,
    uploadPartCopy_copySourceIfModifiedSince,
    uploadPartCopy_copySourceIfNoneMatch,
    uploadPartCopy_copySourceIfUnmodifiedSince,
    uploadPartCopy_copySourceRange,
    uploadPartCopy_copySourceSSECustomerAlgorithm,
    uploadPartCopy_copySourceSSECustomerKey,
    uploadPartCopy_copySourceSSECustomerKeyMD5,
    uploadPartCopy_expectedBucketOwner,
    uploadPartCopy_expectedSourceBucketOwner,
    uploadPartCopy_requestPayer,
    uploadPartCopy_sSECustomerAlgorithm,
    uploadPartCopy_sSECustomerKey,
    uploadPartCopy_sSECustomerKeyMD5,
    uploadPartCopy_bucket,
    uploadPartCopy_copySource,
    uploadPartCopy_key,
    uploadPartCopy_partNumber,
    uploadPartCopy_uploadId,

    -- * Destructuring the Response
    UploadPartCopyResponse (..),
    newUploadPartCopyResponse,

    -- * Response Lenses
    uploadPartCopyResponse_bucketKeyEnabled,
    uploadPartCopyResponse_copyPartResult,
    uploadPartCopyResponse_copySourceVersionId,
    uploadPartCopyResponse_requestCharged,
    uploadPartCopyResponse_sSECustomerAlgorithm,
    uploadPartCopyResponse_sSECustomerKeyMD5,
    uploadPartCopyResponse_sSEKMSKeyId,
    uploadPartCopyResponse_serverSideEncryption,
    uploadPartCopyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newUploadPartCopy' smart constructor.
data UploadPartCopy = UploadPartCopy'
  { -- | Copies the object if its entity tag (ETag) matches the specified tag.
    copySourceIfMatch :: Prelude.Maybe Prelude.Text,
    -- | Copies the object if it has been modified since the specified time.
    copySourceIfModifiedSince :: Prelude.Maybe Data.RFC822,
    -- | Copies the object if its entity tag (ETag) is different than the
    -- specified ETag.
    copySourceIfNoneMatch :: Prelude.Maybe Prelude.Text,
    -- | Copies the object if it hasn\'t been modified since the specified time.
    copySourceIfUnmodifiedSince :: Prelude.Maybe Data.RFC822,
    -- | The range of bytes to copy from the source object. The range value must
    -- use the form bytes=first-last, where the first and last are the
    -- zero-based byte offsets to copy. For example, bytes=0-9 indicates that
    -- you want to copy the first 10 bytes of the source. You can copy a range
    -- only if the source object is greater than 5 MB.
    copySourceRange :: Prelude.Maybe Prelude.Text,
    -- | Specifies the algorithm to use when decrypting the source object (for
    -- example, AES256).
    copySourceSSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use to
    -- decrypt the source object. The encryption key provided in this header
    -- must be one that was used when the source object was created.
    copySourceSSECustomerKey :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
    -- 1321. Amazon S3 uses this header for a message integrity check to ensure
    -- that the encryption key was transmitted without error.
    copySourceSSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected destination bucket owner. If the
    -- destination bucket is owned by a different account, the request fails
    -- with the HTTP status code @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected source bucket owner. If the source bucket
    -- is owned by a different account, the request fails with the HTTP status
    -- code @403 Forbidden@ (access denied).
    expectedSourceBucketOwner :: Prelude.Maybe Prelude.Text,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | Specifies the algorithm to use to when encrypting the object (for
    -- example, AES256).
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use in
    -- encrypting data. This value is used to store the object and then it is
    -- discarded; Amazon S3 does not store the encryption key. The key must be
    -- appropriate for use with the algorithm specified in the
    -- @x-amz-server-side-encryption-customer-algorithm@ header. This must be
    -- the same encryption key specified in the initiate multipart upload
    -- request.
    sSECustomerKey :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
    -- 1321. Amazon S3 uses this header for a message integrity check to ensure
    -- that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | The bucket name.
    --
    -- When using this action with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this action with an access point through the Amazon Web
    -- Services SDKs, you provide the access point ARN in place of the bucket
    -- name. For more information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
    -- in the /Amazon S3 User Guide/.
    --
    -- When using this action with Amazon S3 on Outposts, you must direct
    -- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
    -- takes the form
    -- @ @/@AccessPointName@/@-@/@AccountId@/@.@/@outpostID@/@.s3-outposts.@/@Region@/@.amazonaws.com@.
    -- When using this action with S3 on Outposts through the Amazon Web
    -- Services SDKs, you provide the Outposts bucket ARN in place of the
    -- bucket name. For more information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName,
    -- | Specifies the source object for the copy operation. You specify the
    -- value in one of two formats, depending on whether you want to access the
    -- source object through an
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/access-points.html access point>:
    --
    -- -   For objects not accessed through an access point, specify the name
    --     of the source bucket and key of the source object, separated by a
    --     slash (\/). For example, to copy the object @reports\/january.pdf@
    --     from the bucket @awsexamplebucket@, use
    --     @awsexamplebucket\/reports\/january.pdf@. The value must be
    --     URL-encoded.
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
    --     source and destination buckets are in the same Amazon Web Services
    --     Region.
    --
    --     Alternatively, for objects accessed through Amazon S3 on Outposts,
    --     specify the ARN of the object as accessed in the format
    --     @arn:aws:s3-outposts:\<Region>:\<account-id>:outpost\/\<outpost-id>\/object\/\<key>@.
    --     For example, to copy the object @reports\/january.pdf@ through
    --     outpost @my-outpost@ owned by account @123456789012@ in Region
    --     @us-west-2@, use the URL encoding of
    --     @arn:aws:s3-outposts:us-west-2:123456789012:outpost\/my-outpost\/object\/reports\/january.pdf@.
    --     The value must be URL-encoded.
    --
    -- To copy a specific version of an object, append
    -- @?versionId=\<version-id>@ to the value (for example,
    -- @awsexamplebucket\/reports\/january.pdf?versionId=QUpfdndhfd8438MNFDN93jdnJFkdmqnh893@).
    -- If you don\'t specify a version ID, Amazon S3 copies the latest version
    -- of the source object.
    copySource :: Prelude.Text,
    -- | Object key for which the multipart upload was initiated.
    key :: ObjectKey,
    -- | Part number of part being copied. This is a positive integer between 1
    -- and 10,000.
    partNumber :: Prelude.Int,
    -- | Upload ID identifying the multipart upload whose part is being copied.
    uploadId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'copySourceIfModifiedSince', 'uploadPartCopy_copySourceIfModifiedSince' - Copies the object if it has been modified since the specified time.
--
-- 'copySourceIfNoneMatch', 'uploadPartCopy_copySourceIfNoneMatch' - Copies the object if its entity tag (ETag) is different than the
-- specified ETag.
--
-- 'copySourceIfUnmodifiedSince', 'uploadPartCopy_copySourceIfUnmodifiedSince' - Copies the object if it hasn\'t been modified since the specified time.
--
-- 'copySourceRange', 'uploadPartCopy_copySourceRange' - The range of bytes to copy from the source object. The range value must
-- use the form bytes=first-last, where the first and last are the
-- zero-based byte offsets to copy. For example, bytes=0-9 indicates that
-- you want to copy the first 10 bytes of the source. You can copy a range
-- only if the source object is greater than 5 MB.
--
-- 'copySourceSSECustomerAlgorithm', 'uploadPartCopy_copySourceSSECustomerAlgorithm' - Specifies the algorithm to use when decrypting the source object (for
-- example, AES256).
--
-- 'copySourceSSECustomerKey', 'uploadPartCopy_copySourceSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header
-- must be one that was used when the source object was created.
--
-- 'copySourceSSECustomerKeyMD5', 'uploadPartCopy_copySourceSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'expectedBucketOwner', 'uploadPartCopy_expectedBucketOwner' - The account ID of the expected destination bucket owner. If the
-- destination bucket is owned by a different account, the request fails
-- with the HTTP status code @403 Forbidden@ (access denied).
--
-- 'expectedSourceBucketOwner', 'uploadPartCopy_expectedSourceBucketOwner' - The account ID of the expected source bucket owner. If the source bucket
-- is owned by a different account, the request fails with the HTTP status
-- code @403 Forbidden@ (access denied).
--
-- 'requestPayer', 'uploadPartCopy_requestPayer' - Undocumented member.
--
-- 'sSECustomerAlgorithm', 'uploadPartCopy_sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
--
-- 'sSECustomerKey', 'uploadPartCopy_sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header. This must be
-- the same encryption key specified in the initiate multipart upload
-- request.
--
-- 'sSECustomerKeyMD5', 'uploadPartCopy_sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'bucket', 'uploadPartCopy_bucket' - The bucket name.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
--
-- When using this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- @ @/@AccessPointName@/@-@/@AccountId@/@.@/@outpostID@/@.s3-outposts.@/@Region@/@.amazonaws.com@.
-- When using this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
-- in the /Amazon S3 User Guide/.
--
-- 'copySource', 'uploadPartCopy_copySource' - Specifies the source object for the copy operation. You specify the
-- value in one of two formats, depending on whether you want to access the
-- source object through an
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/access-points.html access point>:
--
-- -   For objects not accessed through an access point, specify the name
--     of the source bucket and key of the source object, separated by a
--     slash (\/). For example, to copy the object @reports\/january.pdf@
--     from the bucket @awsexamplebucket@, use
--     @awsexamplebucket\/reports\/january.pdf@. The value must be
--     URL-encoded.
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
--     source and destination buckets are in the same Amazon Web Services
--     Region.
--
--     Alternatively, for objects accessed through Amazon S3 on Outposts,
--     specify the ARN of the object as accessed in the format
--     @arn:aws:s3-outposts:\<Region>:\<account-id>:outpost\/\<outpost-id>\/object\/\<key>@.
--     For example, to copy the object @reports\/january.pdf@ through
--     outpost @my-outpost@ owned by account @123456789012@ in Region
--     @us-west-2@, use the URL encoding of
--     @arn:aws:s3-outposts:us-west-2:123456789012:outpost\/my-outpost\/object\/reports\/january.pdf@.
--     The value must be URL-encoded.
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
  Prelude.Text ->
  -- | 'key'
  ObjectKey ->
  -- | 'partNumber'
  Prelude.Int ->
  -- | 'uploadId'
  Prelude.Text ->
  UploadPartCopy
newUploadPartCopy
  pBucket_
  pCopySource_
  pKey_
  pPartNumber_
  pUploadId_ =
    UploadPartCopy'
      { copySourceIfMatch =
          Prelude.Nothing,
        copySourceIfModifiedSince = Prelude.Nothing,
        copySourceIfNoneMatch = Prelude.Nothing,
        copySourceIfUnmodifiedSince = Prelude.Nothing,
        copySourceRange = Prelude.Nothing,
        copySourceSSECustomerAlgorithm = Prelude.Nothing,
        copySourceSSECustomerKey = Prelude.Nothing,
        copySourceSSECustomerKeyMD5 = Prelude.Nothing,
        expectedBucketOwner = Prelude.Nothing,
        expectedSourceBucketOwner = Prelude.Nothing,
        requestPayer = Prelude.Nothing,
        sSECustomerAlgorithm = Prelude.Nothing,
        sSECustomerKey = Prelude.Nothing,
        sSECustomerKeyMD5 = Prelude.Nothing,
        bucket = pBucket_,
        copySource = pCopySource_,
        key = pKey_,
        partNumber = pPartNumber_,
        uploadId = pUploadId_
      }

-- | Copies the object if its entity tag (ETag) matches the specified tag.
uploadPartCopy_copySourceIfMatch :: Lens.Lens' UploadPartCopy (Prelude.Maybe Prelude.Text)
uploadPartCopy_copySourceIfMatch = Lens.lens (\UploadPartCopy' {copySourceIfMatch} -> copySourceIfMatch) (\s@UploadPartCopy' {} a -> s {copySourceIfMatch = a} :: UploadPartCopy)

-- | Copies the object if it has been modified since the specified time.
uploadPartCopy_copySourceIfModifiedSince :: Lens.Lens' UploadPartCopy (Prelude.Maybe Prelude.UTCTime)
uploadPartCopy_copySourceIfModifiedSince = Lens.lens (\UploadPartCopy' {copySourceIfModifiedSince} -> copySourceIfModifiedSince) (\s@UploadPartCopy' {} a -> s {copySourceIfModifiedSince = a} :: UploadPartCopy) Prelude.. Lens.mapping Data._Time

-- | Copies the object if its entity tag (ETag) is different than the
-- specified ETag.
uploadPartCopy_copySourceIfNoneMatch :: Lens.Lens' UploadPartCopy (Prelude.Maybe Prelude.Text)
uploadPartCopy_copySourceIfNoneMatch = Lens.lens (\UploadPartCopy' {copySourceIfNoneMatch} -> copySourceIfNoneMatch) (\s@UploadPartCopy' {} a -> s {copySourceIfNoneMatch = a} :: UploadPartCopy)

-- | Copies the object if it hasn\'t been modified since the specified time.
uploadPartCopy_copySourceIfUnmodifiedSince :: Lens.Lens' UploadPartCopy (Prelude.Maybe Prelude.UTCTime)
uploadPartCopy_copySourceIfUnmodifiedSince = Lens.lens (\UploadPartCopy' {copySourceIfUnmodifiedSince} -> copySourceIfUnmodifiedSince) (\s@UploadPartCopy' {} a -> s {copySourceIfUnmodifiedSince = a} :: UploadPartCopy) Prelude.. Lens.mapping Data._Time

-- | The range of bytes to copy from the source object. The range value must
-- use the form bytes=first-last, where the first and last are the
-- zero-based byte offsets to copy. For example, bytes=0-9 indicates that
-- you want to copy the first 10 bytes of the source. You can copy a range
-- only if the source object is greater than 5 MB.
uploadPartCopy_copySourceRange :: Lens.Lens' UploadPartCopy (Prelude.Maybe Prelude.Text)
uploadPartCopy_copySourceRange = Lens.lens (\UploadPartCopy' {copySourceRange} -> copySourceRange) (\s@UploadPartCopy' {} a -> s {copySourceRange = a} :: UploadPartCopy)

-- | Specifies the algorithm to use when decrypting the source object (for
-- example, AES256).
uploadPartCopy_copySourceSSECustomerAlgorithm :: Lens.Lens' UploadPartCopy (Prelude.Maybe Prelude.Text)
uploadPartCopy_copySourceSSECustomerAlgorithm = Lens.lens (\UploadPartCopy' {copySourceSSECustomerAlgorithm} -> copySourceSSECustomerAlgorithm) (\s@UploadPartCopy' {} a -> s {copySourceSSECustomerAlgorithm = a} :: UploadPartCopy)

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header
-- must be one that was used when the source object was created.
uploadPartCopy_copySourceSSECustomerKey :: Lens.Lens' UploadPartCopy (Prelude.Maybe Prelude.Text)
uploadPartCopy_copySourceSSECustomerKey = Lens.lens (\UploadPartCopy' {copySourceSSECustomerKey} -> copySourceSSECustomerKey) (\s@UploadPartCopy' {} a -> s {copySourceSSECustomerKey = a} :: UploadPartCopy) Prelude.. Lens.mapping Data._Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
uploadPartCopy_copySourceSSECustomerKeyMD5 :: Lens.Lens' UploadPartCopy (Prelude.Maybe Prelude.Text)
uploadPartCopy_copySourceSSECustomerKeyMD5 = Lens.lens (\UploadPartCopy' {copySourceSSECustomerKeyMD5} -> copySourceSSECustomerKeyMD5) (\s@UploadPartCopy' {} a -> s {copySourceSSECustomerKeyMD5 = a} :: UploadPartCopy)

-- | The account ID of the expected destination bucket owner. If the
-- destination bucket is owned by a different account, the request fails
-- with the HTTP status code @403 Forbidden@ (access denied).
uploadPartCopy_expectedBucketOwner :: Lens.Lens' UploadPartCopy (Prelude.Maybe Prelude.Text)
uploadPartCopy_expectedBucketOwner = Lens.lens (\UploadPartCopy' {expectedBucketOwner} -> expectedBucketOwner) (\s@UploadPartCopy' {} a -> s {expectedBucketOwner = a} :: UploadPartCopy)

-- | The account ID of the expected source bucket owner. If the source bucket
-- is owned by a different account, the request fails with the HTTP status
-- code @403 Forbidden@ (access denied).
uploadPartCopy_expectedSourceBucketOwner :: Lens.Lens' UploadPartCopy (Prelude.Maybe Prelude.Text)
uploadPartCopy_expectedSourceBucketOwner = Lens.lens (\UploadPartCopy' {expectedSourceBucketOwner} -> expectedSourceBucketOwner) (\s@UploadPartCopy' {} a -> s {expectedSourceBucketOwner = a} :: UploadPartCopy)

-- | Undocumented member.
uploadPartCopy_requestPayer :: Lens.Lens' UploadPartCopy (Prelude.Maybe RequestPayer)
uploadPartCopy_requestPayer = Lens.lens (\UploadPartCopy' {requestPayer} -> requestPayer) (\s@UploadPartCopy' {} a -> s {requestPayer = a} :: UploadPartCopy)

-- | Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
uploadPartCopy_sSECustomerAlgorithm :: Lens.Lens' UploadPartCopy (Prelude.Maybe Prelude.Text)
uploadPartCopy_sSECustomerAlgorithm = Lens.lens (\UploadPartCopy' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@UploadPartCopy' {} a -> s {sSECustomerAlgorithm = a} :: UploadPartCopy)

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header. This must be
-- the same encryption key specified in the initiate multipart upload
-- request.
uploadPartCopy_sSECustomerKey :: Lens.Lens' UploadPartCopy (Prelude.Maybe Prelude.Text)
uploadPartCopy_sSECustomerKey = Lens.lens (\UploadPartCopy' {sSECustomerKey} -> sSECustomerKey) (\s@UploadPartCopy' {} a -> s {sSECustomerKey = a} :: UploadPartCopy) Prelude.. Lens.mapping Data._Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
uploadPartCopy_sSECustomerKeyMD5 :: Lens.Lens' UploadPartCopy (Prelude.Maybe Prelude.Text)
uploadPartCopy_sSECustomerKeyMD5 = Lens.lens (\UploadPartCopy' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@UploadPartCopy' {} a -> s {sSECustomerKeyMD5 = a} :: UploadPartCopy)

-- | The bucket name.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
--
-- When using this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- @ @/@AccessPointName@/@-@/@AccountId@/@.@/@outpostID@/@.s3-outposts.@/@Region@/@.amazonaws.com@.
-- When using this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
-- in the /Amazon S3 User Guide/.
uploadPartCopy_bucket :: Lens.Lens' UploadPartCopy BucketName
uploadPartCopy_bucket = Lens.lens (\UploadPartCopy' {bucket} -> bucket) (\s@UploadPartCopy' {} a -> s {bucket = a} :: UploadPartCopy)

-- | Specifies the source object for the copy operation. You specify the
-- value in one of two formats, depending on whether you want to access the
-- source object through an
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/access-points.html access point>:
--
-- -   For objects not accessed through an access point, specify the name
--     of the source bucket and key of the source object, separated by a
--     slash (\/). For example, to copy the object @reports\/january.pdf@
--     from the bucket @awsexamplebucket@, use
--     @awsexamplebucket\/reports\/january.pdf@. The value must be
--     URL-encoded.
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
--     source and destination buckets are in the same Amazon Web Services
--     Region.
--
--     Alternatively, for objects accessed through Amazon S3 on Outposts,
--     specify the ARN of the object as accessed in the format
--     @arn:aws:s3-outposts:\<Region>:\<account-id>:outpost\/\<outpost-id>\/object\/\<key>@.
--     For example, to copy the object @reports\/january.pdf@ through
--     outpost @my-outpost@ owned by account @123456789012@ in Region
--     @us-west-2@, use the URL encoding of
--     @arn:aws:s3-outposts:us-west-2:123456789012:outpost\/my-outpost\/object\/reports\/january.pdf@.
--     The value must be URL-encoded.
--
-- To copy a specific version of an object, append
-- @?versionId=\<version-id>@ to the value (for example,
-- @awsexamplebucket\/reports\/january.pdf?versionId=QUpfdndhfd8438MNFDN93jdnJFkdmqnh893@).
-- If you don\'t specify a version ID, Amazon S3 copies the latest version
-- of the source object.
uploadPartCopy_copySource :: Lens.Lens' UploadPartCopy Prelude.Text
uploadPartCopy_copySource = Lens.lens (\UploadPartCopy' {copySource} -> copySource) (\s@UploadPartCopy' {} a -> s {copySource = a} :: UploadPartCopy)

-- | Object key for which the multipart upload was initiated.
uploadPartCopy_key :: Lens.Lens' UploadPartCopy ObjectKey
uploadPartCopy_key = Lens.lens (\UploadPartCopy' {key} -> key) (\s@UploadPartCopy' {} a -> s {key = a} :: UploadPartCopy)

-- | Part number of part being copied. This is a positive integer between 1
-- and 10,000.
uploadPartCopy_partNumber :: Lens.Lens' UploadPartCopy Prelude.Int
uploadPartCopy_partNumber = Lens.lens (\UploadPartCopy' {partNumber} -> partNumber) (\s@UploadPartCopy' {} a -> s {partNumber = a} :: UploadPartCopy)

-- | Upload ID identifying the multipart upload whose part is being copied.
uploadPartCopy_uploadId :: Lens.Lens' UploadPartCopy Prelude.Text
uploadPartCopy_uploadId = Lens.lens (\UploadPartCopy' {uploadId} -> uploadId) (\s@UploadPartCopy' {} a -> s {uploadId = a} :: UploadPartCopy)

instance Core.AWSRequest UploadPartCopy where
  type
    AWSResponse UploadPartCopy =
      UploadPartCopyResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.put (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UploadPartCopyResponse'
            Prelude.<$> ( h
                            Data..#? "x-amz-server-side-encryption-bucket-key-enabled"
                        )
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (h Data..#? "x-amz-copy-source-version-id")
            Prelude.<*> (h Data..#? "x-amz-request-charged")
            Prelude.<*> ( h
                            Data..#? "x-amz-server-side-encryption-customer-algorithm"
                        )
            Prelude.<*> ( h
                            Data..#? "x-amz-server-side-encryption-customer-key-MD5"
                        )
            Prelude.<*> ( h
                            Data..#? "x-amz-server-side-encryption-aws-kms-key-id"
                        )
            Prelude.<*> (h Data..#? "x-amz-server-side-encryption")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UploadPartCopy where
  hashWithSalt _salt UploadPartCopy' {..} =
    _salt
      `Prelude.hashWithSalt` copySourceIfMatch
      `Prelude.hashWithSalt` copySourceIfModifiedSince
      `Prelude.hashWithSalt` copySourceIfNoneMatch
      `Prelude.hashWithSalt` copySourceIfUnmodifiedSince
      `Prelude.hashWithSalt` copySourceRange
      `Prelude.hashWithSalt` copySourceSSECustomerAlgorithm
      `Prelude.hashWithSalt` copySourceSSECustomerKey
      `Prelude.hashWithSalt` copySourceSSECustomerKeyMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` expectedSourceBucketOwner
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` sSECustomerAlgorithm
      `Prelude.hashWithSalt` sSECustomerKey
      `Prelude.hashWithSalt` sSECustomerKeyMD5
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` copySource
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` partNumber
      `Prelude.hashWithSalt` uploadId

instance Prelude.NFData UploadPartCopy where
  rnf UploadPartCopy' {..} =
    Prelude.rnf copySourceIfMatch
      `Prelude.seq` Prelude.rnf copySourceIfModifiedSince
      `Prelude.seq` Prelude.rnf copySourceIfNoneMatch
      `Prelude.seq` Prelude.rnf copySourceIfUnmodifiedSince
      `Prelude.seq` Prelude.rnf copySourceRange
      `Prelude.seq` Prelude.rnf copySourceSSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf copySourceSSECustomerKey
      `Prelude.seq` Prelude.rnf copySourceSSECustomerKeyMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf expectedSourceBucketOwner
      `Prelude.seq` Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf sSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf sSECustomerKey
      `Prelude.seq` Prelude.rnf sSECustomerKeyMD5
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf copySource
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf partNumber
      `Prelude.seq` Prelude.rnf uploadId

instance Data.ToHeaders UploadPartCopy where
  toHeaders UploadPartCopy' {..} =
    Prelude.mconcat
      [ "x-amz-copy-source-if-match"
          Data.=# copySourceIfMatch,
        "x-amz-copy-source-if-modified-since"
          Data.=# copySourceIfModifiedSince,
        "x-amz-copy-source-if-none-match"
          Data.=# copySourceIfNoneMatch,
        "x-amz-copy-source-if-unmodified-since"
          Data.=# copySourceIfUnmodifiedSince,
        "x-amz-copy-source-range" Data.=# copySourceRange,
        "x-amz-copy-source-server-side-encryption-customer-algorithm"
          Data.=# copySourceSSECustomerAlgorithm,
        "x-amz-copy-source-server-side-encryption-customer-key"
          Data.=# copySourceSSECustomerKey,
        "x-amz-copy-source-server-side-encryption-customer-key-MD5"
          Data.=# copySourceSSECustomerKeyMD5,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-source-expected-bucket-owner"
          Data.=# expectedSourceBucketOwner,
        "x-amz-request-payer" Data.=# requestPayer,
        "x-amz-server-side-encryption-customer-algorithm"
          Data.=# sSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key"
          Data.=# sSECustomerKey,
        "x-amz-server-side-encryption-customer-key-MD5"
          Data.=# sSECustomerKeyMD5,
        "x-amz-copy-source" Data.=# copySource
      ]

instance Data.ToPath UploadPartCopy where
  toPath UploadPartCopy' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery UploadPartCopy where
  toQuery UploadPartCopy' {..} =
    Prelude.mconcat
      [ "partNumber" Data.=: partNumber,
        "uploadId" Data.=: uploadId
      ]

-- | /See:/ 'newUploadPartCopyResponse' smart constructor.
data UploadPartCopyResponse = UploadPartCopyResponse'
  { -- | Indicates whether the multipart upload uses an S3 Bucket Key for
    -- server-side encryption with Amazon Web Services KMS (SSE-KMS).
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Container for all response elements.
    copyPartResult :: Prelude.Maybe CopyPartResult,
    -- | The version of the source object that was copied, if you have enabled
    -- versioning on the source bucket.
    copySourceVersionId :: Prelude.Maybe Prelude.Text,
    requestCharged :: Prelude.Maybe RequestCharged,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header confirming the
    -- encryption algorithm used.
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header to provide round-trip
    -- message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | If present, specifies the ID of the Amazon Web Services Key Management
    -- Service (Amazon Web Services KMS) symmetric customer managed key that
    -- was used for the object.
    sSEKMSKeyId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The server-side encryption algorithm used when storing this object in
    -- Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Prelude.Maybe ServerSideEncryption,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadPartCopyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketKeyEnabled', 'uploadPartCopyResponse_bucketKeyEnabled' - Indicates whether the multipart upload uses an S3 Bucket Key for
-- server-side encryption with Amazon Web Services KMS (SSE-KMS).
--
-- 'copyPartResult', 'uploadPartCopyResponse_copyPartResult' - Container for all response elements.
--
-- 'copySourceVersionId', 'uploadPartCopyResponse_copySourceVersionId' - The version of the source object that was copied, if you have enabled
-- versioning on the source bucket.
--
-- 'requestCharged', 'uploadPartCopyResponse_requestCharged' - Undocumented member.
--
-- 'sSECustomerAlgorithm', 'uploadPartCopyResponse_sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
--
-- 'sSECustomerKeyMD5', 'uploadPartCopyResponse_sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
--
-- 'sSEKMSKeyId', 'uploadPartCopyResponse_sSEKMSKeyId' - If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key that
-- was used for the object.
--
-- 'serverSideEncryption', 'uploadPartCopyResponse_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
--
-- 'httpStatus', 'uploadPartCopyResponse_httpStatus' - The response's http status code.
newUploadPartCopyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UploadPartCopyResponse
newUploadPartCopyResponse pHttpStatus_ =
  UploadPartCopyResponse'
    { bucketKeyEnabled =
        Prelude.Nothing,
      copyPartResult = Prelude.Nothing,
      copySourceVersionId = Prelude.Nothing,
      requestCharged = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      sSEKMSKeyId = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether the multipart upload uses an S3 Bucket Key for
-- server-side encryption with Amazon Web Services KMS (SSE-KMS).
uploadPartCopyResponse_bucketKeyEnabled :: Lens.Lens' UploadPartCopyResponse (Prelude.Maybe Prelude.Bool)
uploadPartCopyResponse_bucketKeyEnabled = Lens.lens (\UploadPartCopyResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@UploadPartCopyResponse' {} a -> s {bucketKeyEnabled = a} :: UploadPartCopyResponse)

-- | Container for all response elements.
uploadPartCopyResponse_copyPartResult :: Lens.Lens' UploadPartCopyResponse (Prelude.Maybe CopyPartResult)
uploadPartCopyResponse_copyPartResult = Lens.lens (\UploadPartCopyResponse' {copyPartResult} -> copyPartResult) (\s@UploadPartCopyResponse' {} a -> s {copyPartResult = a} :: UploadPartCopyResponse)

-- | The version of the source object that was copied, if you have enabled
-- versioning on the source bucket.
uploadPartCopyResponse_copySourceVersionId :: Lens.Lens' UploadPartCopyResponse (Prelude.Maybe Prelude.Text)
uploadPartCopyResponse_copySourceVersionId = Lens.lens (\UploadPartCopyResponse' {copySourceVersionId} -> copySourceVersionId) (\s@UploadPartCopyResponse' {} a -> s {copySourceVersionId = a} :: UploadPartCopyResponse)

-- | Undocumented member.
uploadPartCopyResponse_requestCharged :: Lens.Lens' UploadPartCopyResponse (Prelude.Maybe RequestCharged)
uploadPartCopyResponse_requestCharged = Lens.lens (\UploadPartCopyResponse' {requestCharged} -> requestCharged) (\s@UploadPartCopyResponse' {} a -> s {requestCharged = a} :: UploadPartCopyResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
uploadPartCopyResponse_sSECustomerAlgorithm :: Lens.Lens' UploadPartCopyResponse (Prelude.Maybe Prelude.Text)
uploadPartCopyResponse_sSECustomerAlgorithm = Lens.lens (\UploadPartCopyResponse' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@UploadPartCopyResponse' {} a -> s {sSECustomerAlgorithm = a} :: UploadPartCopyResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
uploadPartCopyResponse_sSECustomerKeyMD5 :: Lens.Lens' UploadPartCopyResponse (Prelude.Maybe Prelude.Text)
uploadPartCopyResponse_sSECustomerKeyMD5 = Lens.lens (\UploadPartCopyResponse' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@UploadPartCopyResponse' {} a -> s {sSECustomerKeyMD5 = a} :: UploadPartCopyResponse)

-- | If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key that
-- was used for the object.
uploadPartCopyResponse_sSEKMSKeyId :: Lens.Lens' UploadPartCopyResponse (Prelude.Maybe Prelude.Text)
uploadPartCopyResponse_sSEKMSKeyId = Lens.lens (\UploadPartCopyResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@UploadPartCopyResponse' {} a -> s {sSEKMSKeyId = a} :: UploadPartCopyResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
uploadPartCopyResponse_serverSideEncryption :: Lens.Lens' UploadPartCopyResponse (Prelude.Maybe ServerSideEncryption)
uploadPartCopyResponse_serverSideEncryption = Lens.lens (\UploadPartCopyResponse' {serverSideEncryption} -> serverSideEncryption) (\s@UploadPartCopyResponse' {} a -> s {serverSideEncryption = a} :: UploadPartCopyResponse)

-- | The response's http status code.
uploadPartCopyResponse_httpStatus :: Lens.Lens' UploadPartCopyResponse Prelude.Int
uploadPartCopyResponse_httpStatus = Lens.lens (\UploadPartCopyResponse' {httpStatus} -> httpStatus) (\s@UploadPartCopyResponse' {} a -> s {httpStatus = a} :: UploadPartCopyResponse)

instance Prelude.NFData UploadPartCopyResponse where
  rnf UploadPartCopyResponse' {..} =
    Prelude.rnf bucketKeyEnabled
      `Prelude.seq` Prelude.rnf copyPartResult
      `Prelude.seq` Prelude.rnf copySourceVersionId
      `Prelude.seq` Prelude.rnf requestCharged
      `Prelude.seq` Prelude.rnf sSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf sSECustomerKeyMD5
      `Prelude.seq` Prelude.rnf sSEKMSKeyId
      `Prelude.seq` Prelude.rnf serverSideEncryption
      `Prelude.seq` Prelude.rnf httpStatus
