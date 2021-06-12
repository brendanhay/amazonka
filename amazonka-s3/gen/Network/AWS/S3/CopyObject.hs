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
-- Module      : Network.AWS.S3.CopyObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a copy of an object that is already stored in Amazon S3.
--
-- You can store individual objects of up to 5 TB in Amazon S3. You create
-- a copy of your object up to 5 GB in size in a single atomic operation
-- using this API. However, to copy an object greater than 5 GB, you must
-- use the multipart upload Upload Part - Copy API. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/CopyingObjctsUsingRESTMPUapi.html Copy Object Using the REST Multipart Upload API>.
--
-- All copy requests must be authenticated. Additionally, you must have
-- /read/ access to the source object and /write/ access to the destination
-- bucket. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication>.
-- Both the Region that you want to copy the object from and the Region
-- that you want to copy the object to must be enabled for your account.
--
-- A copy request might return an error when Amazon S3 receives the copy
-- request or while Amazon S3 is copying the files. If the error occurs
-- before the copy operation starts, you receive a standard Amazon S3
-- error. If the error occurs during the copy operation, the error response
-- is embedded in the @200 OK@ response. This means that a @200 OK@
-- response can contain either a success or an error. Design your
-- application to parse the contents of the response and handle it
-- appropriately.
--
-- If the copy is successful, you receive a response with information about
-- the copied object.
--
-- If the request is an HTTP 1.1 request, the response is chunk encoded. If
-- it were not, it would not contain the content-length, and you would need
-- to read the entire body.
--
-- The copy request charge is based on the storage class and Region that
-- you specify for the destination object. For pricing information, see
-- <https://aws.amazon.com/s3/pricing/ Amazon S3 pricing>.
--
-- Amazon S3 transfer acceleration does not support cross-Region copies. If
-- you request a cross-Region copy using a transfer acceleration endpoint,
-- you get a 400 @Bad Request@ error. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/transfer-acceleration.html Transfer Acceleration>.
--
-- __Metadata__
--
-- When copying an object, you can preserve all metadata (default) or
-- specify new metadata. However, the ACL is not preserved and is set to
-- private for the user making the request. To override the default ACL
-- setting, specify a new ACL when generating a copy request. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3_ACLs_UsingACLs.html Using ACLs>.
--
-- To specify whether you want the object metadata copied from the source
-- object or replaced with metadata provided in the request, you can
-- optionally add the @x-amz-metadata-directive@ header. When you grant
-- permissions, you can use the @s3:x-amz-metadata-directive@ condition key
-- to enforce certain metadata behavior when objects are uploaded. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/amazon-s3-policy-keys.html Specifying Conditions in a Policy>
-- in the /Amazon S3 Developer Guide/. For a complete list of Amazon
-- S3-specific condition keys, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/list_amazons3.html Actions, Resources, and Condition Keys for Amazon S3>.
--
-- __@x-amz-copy-source-if@ Headers__
--
-- To only copy an object under certain conditions, such as whether the
-- @Etag@ matches or whether the object was modified before or after a
-- specified date, use the following request parameters:
--
-- -   @x-amz-copy-source-if-match@
--
-- -   @x-amz-copy-source-if-none-match@
--
-- -   @x-amz-copy-source-if-unmodified-since@
--
-- -   @x-amz-copy-source-if-modified-since@
--
-- If both the @x-amz-copy-source-if-match@ and
-- @x-amz-copy-source-if-unmodified-since@ headers are present in the
-- request and evaluate as follows, Amazon S3 returns @200 OK@ and copies
-- the data:
--
-- -   @x-amz-copy-source-if-match@ condition evaluates to true
--
-- -   @x-amz-copy-source-if-unmodified-since@ condition evaluates to false
--
-- If both the @x-amz-copy-source-if-none-match@ and
-- @x-amz-copy-source-if-modified-since@ headers are present in the request
-- and evaluate as follows, Amazon S3 returns the @412 Precondition Failed@
-- response code:
--
-- -   @x-amz-copy-source-if-none-match@ condition evaluates to false
--
-- -   @x-amz-copy-source-if-modified-since@ condition evaluates to true
--
-- All headers with the @x-amz-@ prefix, including @x-amz-copy-source@,
-- must be signed.
--
-- __Server-side encryption__
--
-- When you perform a CopyObject operation, you can optionally use the
-- appropriate encryption-related headers to encrypt the object using
-- server-side encryption with AWS managed encryption keys (SSE-S3 or
-- SSE-KMS) or a customer-provided encryption key. With server-side
-- encryption, Amazon S3 encrypts your data as it writes it to disks in its
-- data centers and decrypts the data when you access it. For more
-- information about server-side encryption, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html Using Server-Side Encryption>.
--
-- If a target object uses SSE-KMS, you can enable an S3 Bucket Key for the
-- object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-key.html Amazon S3 Bucket Keys>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- __Access Control List (ACL)-Specific Request Headers__
--
-- When copying an object, you can optionally use headers to grant
-- ACL-based permissions. By default, all objects are private. Only the
-- owner has full access control. When adding a new object, you can grant
-- permissions to individual AWS accounts or to predefined groups defined
-- by Amazon S3. These permissions are then added to the ACL on the object.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-using-rest-api.html Managing ACLs Using the REST API>.
--
-- __Storage Class Options__
--
-- You can use the @CopyObject@ operation to change the storage class of an
-- object that is already stored in Amazon S3 using the @StorageClass@
-- parameter. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 Service Developer Guide/.
--
-- __Versioning__
--
-- By default, @x-amz-copy-source@ identifies the current version of an
-- object to copy. If the current version is a delete marker, Amazon S3
-- behaves as if the object was deleted. To copy a different version, use
-- the @versionId@ subresource.
--
-- If you enable versioning on the target bucket, Amazon S3 generates a
-- unique version ID for the object being copied. This version ID is
-- different from the version ID of the source object. Amazon S3 returns
-- the version ID of the copied object in the @x-amz-version-id@ response
-- header in the response.
--
-- If you do not enable versioning or suspend it on the target bucket, the
-- version ID that Amazon S3 generates is always null.
--
-- If the source object\'s storage class is GLACIER, you must restore a
-- copy of this object before you can use it as a source object for the
-- copy operation. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject>.
--
-- The following operations are related to @CopyObject@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/CopyingObjectsExamples.html Copying Objects>.
module Network.AWS.S3.CopyObject
  ( -- * Creating a Request
    CopyObject (..),
    newCopyObject,

    -- * Request Lenses
    copyObject_copySourceIfMatch,
    copyObject_websiteRedirectLocation,
    copyObject_grantRead,
    copyObject_expectedSourceBucketOwner,
    copyObject_contentType,
    copyObject_expectedBucketOwner,
    copyObject_contentDisposition,
    copyObject_copySourceSSECustomerKey,
    copyObject_copySourceSSECustomerAlgorithm,
    copyObject_copySourceIfNoneMatch,
    copyObject_contentLanguage,
    copyObject_sSEKMSEncryptionContext,
    copyObject_metadata,
    copyObject_contentEncoding,
    copyObject_sSEKMSKeyId,
    copyObject_sSECustomerKeyMD5,
    copyObject_taggingDirective,
    copyObject_storageClass,
    copyObject_copySourceIfUnmodifiedSince,
    copyObject_copySourceIfModifiedSince,
    copyObject_bucketKeyEnabled,
    copyObject_grantWriteACP,
    copyObject_serverSideEncryption,
    copyObject_objectLockLegalHoldStatus,
    copyObject_grantReadACP,
    copyObject_acl,
    copyObject_sSECustomerAlgorithm,
    copyObject_requestPayer,
    copyObject_sSECustomerKey,
    copyObject_cacheControl,
    copyObject_expires,
    copyObject_objectLockMode,
    copyObject_objectLockRetainUntilDate,
    copyObject_tagging,
    copyObject_grantFullControl,
    copyObject_copySourceSSECustomerKeyMD5,
    copyObject_metadataDirective,
    copyObject_bucket,
    copyObject_copySource,
    copyObject_key,

    -- * Destructuring the Response
    CopyObjectResponse (..),
    newCopyObjectResponse,

    -- * Response Lenses
    copyObjectResponse_requestCharged,
    copyObjectResponse_copySourceVersionId,
    copyObjectResponse_expiration,
    copyObjectResponse_sSEKMSEncryptionContext,
    copyObjectResponse_sSEKMSKeyId,
    copyObjectResponse_sSECustomerKeyMD5,
    copyObjectResponse_versionId,
    copyObjectResponse_bucketKeyEnabled,
    copyObjectResponse_copyObjectResult,
    copyObjectResponse_serverSideEncryption,
    copyObjectResponse_sSECustomerAlgorithm,
    copyObjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newCopyObject' smart constructor.
data CopyObject = CopyObject'
  { -- | Copies the object if its entity tag (ETag) matches the specified tag.
    copySourceIfMatch :: Core.Maybe Core.Text,
    -- | If the bucket is configured as a website, redirects requests for this
    -- object to another object in the same bucket or to an external URL.
    -- Amazon S3 stores the value of this header in the object metadata.
    websiteRedirectLocation :: Core.Maybe Core.Text,
    -- | Allows grantee to read the object data and its metadata.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantRead :: Core.Maybe Core.Text,
    -- | The account id of the expected source bucket owner. If the source bucket
    -- is owned by a different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedSourceBucketOwner :: Core.Maybe Core.Text,
    -- | A standard MIME type describing the format of the object data.
    contentType :: Core.Maybe Core.Text,
    -- | The account id of the expected destination bucket owner. If the
    -- destination bucket is owned by a different account, the request will
    -- fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | Specifies presentational information for the object.
    contentDisposition :: Core.Maybe Core.Text,
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
    -- | The language the content is in.
    contentLanguage :: Core.Maybe Core.Text,
    -- | Specifies the AWS KMS Encryption Context to use for object encryption.
    -- The value of this header is a base64-encoded UTF-8 string holding JSON
    -- with the encryption context key-value pairs.
    sSEKMSEncryptionContext :: Core.Maybe (Core.Sensitive Core.Text),
    -- | A map of metadata to store with the object in S3.
    metadata :: Core.HashMap Core.Text Core.Text,
    -- | Specifies what content encodings have been applied to the object and
    -- thus what decoding mechanisms must be applied to obtain the media-type
    -- referenced by the Content-Type header field.
    contentEncoding :: Core.Maybe Core.Text,
    -- | Specifies the AWS KMS key ID to use for object encryption. All GET and
    -- PUT requests for an object protected by AWS KMS will fail if not made
    -- via SSL or using SigV4. For information about configuring using any of
    -- the officially supported AWS SDKs and AWS CLI, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication>
    -- in the /Amazon S3 Developer Guide/.
    sSEKMSKeyId :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
    -- 1321. Amazon S3 uses this header for a message integrity check to ensure
    -- that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Core.Maybe Core.Text,
    -- | Specifies whether the object tag-set are copied from the source object
    -- or replaced with tag-set provided in the request.
    taggingDirective :: Core.Maybe TaggingDirective,
    -- | By default, Amazon S3 uses the STANDARD Storage Class to store newly
    -- created objects. The STANDARD storage class provides high durability and
    -- high availability. Depending on performance needs, you can specify a
    -- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
    -- Storage Class. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
    -- in the /Amazon S3 Service Developer Guide/.
    storageClass :: Core.Maybe StorageClass,
    -- | Copies the object if it hasn\'t been modified since the specified time.
    copySourceIfUnmodifiedSince :: Core.Maybe Core.ISO8601,
    -- | Copies the object if it has been modified since the specified time.
    copySourceIfModifiedSince :: Core.Maybe Core.ISO8601,
    -- | Specifies whether Amazon S3 should use an S3 Bucket Key for object
    -- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
    -- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
    -- object encryption with SSE-KMS.
    --
    -- Specifying this header with a COPY operation doesn’t affect bucket-level
    -- settings for S3 Bucket Key.
    bucketKeyEnabled :: Core.Maybe Core.Bool,
    -- | Allows grantee to write the ACL for the applicable object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantWriteACP :: Core.Maybe Core.Text,
    -- | The server-side encryption algorithm used when storing this object in
    -- Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Core.Maybe ServerSideEncryption,
    -- | Specifies whether you want to apply a Legal Hold to the copied object.
    objectLockLegalHoldStatus :: Core.Maybe ObjectLockLegalHoldStatus,
    -- | Allows grantee to read the object ACL.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantReadACP :: Core.Maybe Core.Text,
    -- | The canned ACL to apply to the object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    acl :: Core.Maybe ObjectCannedACL,
    -- | Specifies the algorithm to use to when encrypting the object (for
    -- example, AES256).
    sSECustomerAlgorithm :: Core.Maybe Core.Text,
    requestPayer :: Core.Maybe RequestPayer,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use in
    -- encrypting data. This value is used to store the object and then it is
    -- discarded; Amazon S3 does not store the encryption key. The key must be
    -- appropriate for use with the algorithm specified in the
    -- @x-amz-server-side-encryption-customer-algorithm@ header.
    sSECustomerKey :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Specifies caching behavior along the request\/reply chain.
    cacheControl :: Core.Maybe Core.Text,
    -- | The date and time at which the object is no longer cacheable.
    expires :: Core.Maybe Core.ISO8601,
    -- | The Object Lock mode that you want to apply to the copied object.
    objectLockMode :: Core.Maybe ObjectLockMode,
    -- | The date and time when you want the copied object\'s Object Lock to
    -- expire.
    objectLockRetainUntilDate :: Core.Maybe Core.ISO8601,
    -- | The tag-set for the object destination object this value must be used in
    -- conjunction with the @TaggingDirective@. The tag-set must be encoded as
    -- URL Query parameters.
    tagging :: Core.Maybe Core.Text,
    -- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
    -- object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantFullControl :: Core.Maybe Core.Text,
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
    -- 1321. Amazon S3 uses this header for a message integrity check to ensure
    -- that the encryption key was transmitted without error.
    copySourceSSECustomerKeyMD5 :: Core.Maybe Core.Text,
    -- | Specifies whether the metadata is copied from the source object or
    -- replaced with metadata provided in the request.
    metadataDirective :: Core.Maybe MetadataDirective,
    -- | The name of the destination bucket.
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
    --     of the source bucket and the key of the source object, separated by
    --     a slash (\/). For example, to copy the object @reports\/january.pdf@
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
    -- | The key of the destination object.
    key :: ObjectKey
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CopyObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copySourceIfMatch', 'copyObject_copySourceIfMatch' - Copies the object if its entity tag (ETag) matches the specified tag.
--
-- 'websiteRedirectLocation', 'copyObject_websiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
--
-- 'grantRead', 'copyObject_grantRead' - Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'expectedSourceBucketOwner', 'copyObject_expectedSourceBucketOwner' - The account id of the expected source bucket owner. If the source bucket
-- is owned by a different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'contentType', 'copyObject_contentType' - A standard MIME type describing the format of the object data.
--
-- 'expectedBucketOwner', 'copyObject_expectedBucketOwner' - The account id of the expected destination bucket owner. If the
-- destination bucket is owned by a different account, the request will
-- fail with an HTTP @403 (Access Denied)@ error.
--
-- 'contentDisposition', 'copyObject_contentDisposition' - Specifies presentational information for the object.
--
-- 'copySourceSSECustomerKey', 'copyObject_copySourceSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header
-- must be one that was used when the source object was created.
--
-- 'copySourceSSECustomerAlgorithm', 'copyObject_copySourceSSECustomerAlgorithm' - Specifies the algorithm to use when decrypting the source object (for
-- example, AES256).
--
-- 'copySourceIfNoneMatch', 'copyObject_copySourceIfNoneMatch' - Copies the object if its entity tag (ETag) is different than the
-- specified ETag.
--
-- 'contentLanguage', 'copyObject_contentLanguage' - The language the content is in.
--
-- 'sSEKMSEncryptionContext', 'copyObject_sSEKMSEncryptionContext' - Specifies the AWS KMS Encryption Context to use for object encryption.
-- The value of this header is a base64-encoded UTF-8 string holding JSON
-- with the encryption context key-value pairs.
--
-- 'metadata', 'copyObject_metadata' - A map of metadata to store with the object in S3.
--
-- 'contentEncoding', 'copyObject_contentEncoding' - Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
--
-- 'sSEKMSKeyId', 'copyObject_sSEKMSKeyId' - Specifies the AWS KMS key ID to use for object encryption. All GET and
-- PUT requests for an object protected by AWS KMS will fail if not made
-- via SSL or using SigV4. For information about configuring using any of
-- the officially supported AWS SDKs and AWS CLI, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication>
-- in the /Amazon S3 Developer Guide/.
--
-- 'sSECustomerKeyMD5', 'copyObject_sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'taggingDirective', 'copyObject_taggingDirective' - Specifies whether the object tag-set are copied from the source object
-- or replaced with tag-set provided in the request.
--
-- 'storageClass', 'copyObject_storageClass' - By default, Amazon S3 uses the STANDARD Storage Class to store newly
-- created objects. The STANDARD storage class provides high durability and
-- high availability. Depending on performance needs, you can specify a
-- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
-- Storage Class. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 Service Developer Guide/.
--
-- 'copySourceIfUnmodifiedSince', 'copyObject_copySourceIfUnmodifiedSince' - Copies the object if it hasn\'t been modified since the specified time.
--
-- 'copySourceIfModifiedSince', 'copyObject_copySourceIfModifiedSince' - Copies the object if it has been modified since the specified time.
--
-- 'bucketKeyEnabled', 'copyObject_bucketKeyEnabled' - Specifies whether Amazon S3 should use an S3 Bucket Key for object
-- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
-- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
-- object encryption with SSE-KMS.
--
-- Specifying this header with a COPY operation doesn’t affect bucket-level
-- settings for S3 Bucket Key.
--
-- 'grantWriteACP', 'copyObject_grantWriteACP' - Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'serverSideEncryption', 'copyObject_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
--
-- 'objectLockLegalHoldStatus', 'copyObject_objectLockLegalHoldStatus' - Specifies whether you want to apply a Legal Hold to the copied object.
--
-- 'grantReadACP', 'copyObject_grantReadACP' - Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'acl', 'copyObject_acl' - The canned ACL to apply to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'sSECustomerAlgorithm', 'copyObject_sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
--
-- 'requestPayer', 'copyObject_requestPayer' - Undocumented member.
--
-- 'sSECustomerKey', 'copyObject_sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- 'cacheControl', 'copyObject_cacheControl' - Specifies caching behavior along the request\/reply chain.
--
-- 'expires', 'copyObject_expires' - The date and time at which the object is no longer cacheable.
--
-- 'objectLockMode', 'copyObject_objectLockMode' - The Object Lock mode that you want to apply to the copied object.
--
-- 'objectLockRetainUntilDate', 'copyObject_objectLockRetainUntilDate' - The date and time when you want the copied object\'s Object Lock to
-- expire.
--
-- 'tagging', 'copyObject_tagging' - The tag-set for the object destination object this value must be used in
-- conjunction with the @TaggingDirective@. The tag-set must be encoded as
-- URL Query parameters.
--
-- 'grantFullControl', 'copyObject_grantFullControl' - Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'copySourceSSECustomerKeyMD5', 'copyObject_copySourceSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'metadataDirective', 'copyObject_metadataDirective' - Specifies whether the metadata is copied from the source object or
-- replaced with metadata provided in the request.
--
-- 'bucket', 'copyObject_bucket' - The name of the destination bucket.
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
-- 'copySource', 'copyObject_copySource' - Specifies the source object for the copy operation. You specify the
-- value in one of two formats, depending on whether you want to access the
-- source object through an
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-points.html access point>:
--
-- -   For objects not accessed through an access point, specify the name
--     of the source bucket and the key of the source object, separated by
--     a slash (\/). For example, to copy the object @reports\/january.pdf@
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
-- 'key', 'copyObject_key' - The key of the destination object.
newCopyObject ::
  -- | 'bucket'
  BucketName ->
  -- | 'copySource'
  Core.Text ->
  -- | 'key'
  ObjectKey ->
  CopyObject
newCopyObject pBucket_ pCopySource_ pKey_ =
  CopyObject'
    { copySourceIfMatch = Core.Nothing,
      websiteRedirectLocation = Core.Nothing,
      grantRead = Core.Nothing,
      expectedSourceBucketOwner = Core.Nothing,
      contentType = Core.Nothing,
      expectedBucketOwner = Core.Nothing,
      contentDisposition = Core.Nothing,
      copySourceSSECustomerKey = Core.Nothing,
      copySourceSSECustomerAlgorithm = Core.Nothing,
      copySourceIfNoneMatch = Core.Nothing,
      contentLanguage = Core.Nothing,
      sSEKMSEncryptionContext = Core.Nothing,
      metadata = Core.mempty,
      contentEncoding = Core.Nothing,
      sSEKMSKeyId = Core.Nothing,
      sSECustomerKeyMD5 = Core.Nothing,
      taggingDirective = Core.Nothing,
      storageClass = Core.Nothing,
      copySourceIfUnmodifiedSince = Core.Nothing,
      copySourceIfModifiedSince = Core.Nothing,
      bucketKeyEnabled = Core.Nothing,
      grantWriteACP = Core.Nothing,
      serverSideEncryption = Core.Nothing,
      objectLockLegalHoldStatus = Core.Nothing,
      grantReadACP = Core.Nothing,
      acl = Core.Nothing,
      sSECustomerAlgorithm = Core.Nothing,
      requestPayer = Core.Nothing,
      sSECustomerKey = Core.Nothing,
      cacheControl = Core.Nothing,
      expires = Core.Nothing,
      objectLockMode = Core.Nothing,
      objectLockRetainUntilDate = Core.Nothing,
      tagging = Core.Nothing,
      grantFullControl = Core.Nothing,
      copySourceSSECustomerKeyMD5 = Core.Nothing,
      metadataDirective = Core.Nothing,
      bucket = pBucket_,
      copySource = pCopySource_,
      key = pKey_
    }

-- | Copies the object if its entity tag (ETag) matches the specified tag.
copyObject_copySourceIfMatch :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_copySourceIfMatch = Lens.lens (\CopyObject' {copySourceIfMatch} -> copySourceIfMatch) (\s@CopyObject' {} a -> s {copySourceIfMatch = a} :: CopyObject)

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
copyObject_websiteRedirectLocation :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_websiteRedirectLocation = Lens.lens (\CopyObject' {websiteRedirectLocation} -> websiteRedirectLocation) (\s@CopyObject' {} a -> s {websiteRedirectLocation = a} :: CopyObject)

-- | Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
copyObject_grantRead :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_grantRead = Lens.lens (\CopyObject' {grantRead} -> grantRead) (\s@CopyObject' {} a -> s {grantRead = a} :: CopyObject)

-- | The account id of the expected source bucket owner. If the source bucket
-- is owned by a different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
copyObject_expectedSourceBucketOwner :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_expectedSourceBucketOwner = Lens.lens (\CopyObject' {expectedSourceBucketOwner} -> expectedSourceBucketOwner) (\s@CopyObject' {} a -> s {expectedSourceBucketOwner = a} :: CopyObject)

-- | A standard MIME type describing the format of the object data.
copyObject_contentType :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_contentType = Lens.lens (\CopyObject' {contentType} -> contentType) (\s@CopyObject' {} a -> s {contentType = a} :: CopyObject)

-- | The account id of the expected destination bucket owner. If the
-- destination bucket is owned by a different account, the request will
-- fail with an HTTP @403 (Access Denied)@ error.
copyObject_expectedBucketOwner :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_expectedBucketOwner = Lens.lens (\CopyObject' {expectedBucketOwner} -> expectedBucketOwner) (\s@CopyObject' {} a -> s {expectedBucketOwner = a} :: CopyObject)

-- | Specifies presentational information for the object.
copyObject_contentDisposition :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_contentDisposition = Lens.lens (\CopyObject' {contentDisposition} -> contentDisposition) (\s@CopyObject' {} a -> s {contentDisposition = a} :: CopyObject)

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header
-- must be one that was used when the source object was created.
copyObject_copySourceSSECustomerKey :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_copySourceSSECustomerKey = Lens.lens (\CopyObject' {copySourceSSECustomerKey} -> copySourceSSECustomerKey) (\s@CopyObject' {} a -> s {copySourceSSECustomerKey = a} :: CopyObject) Core.. Lens.mapping Core._Sensitive

-- | Specifies the algorithm to use when decrypting the source object (for
-- example, AES256).
copyObject_copySourceSSECustomerAlgorithm :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_copySourceSSECustomerAlgorithm = Lens.lens (\CopyObject' {copySourceSSECustomerAlgorithm} -> copySourceSSECustomerAlgorithm) (\s@CopyObject' {} a -> s {copySourceSSECustomerAlgorithm = a} :: CopyObject)

-- | Copies the object if its entity tag (ETag) is different than the
-- specified ETag.
copyObject_copySourceIfNoneMatch :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_copySourceIfNoneMatch = Lens.lens (\CopyObject' {copySourceIfNoneMatch} -> copySourceIfNoneMatch) (\s@CopyObject' {} a -> s {copySourceIfNoneMatch = a} :: CopyObject)

-- | The language the content is in.
copyObject_contentLanguage :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_contentLanguage = Lens.lens (\CopyObject' {contentLanguage} -> contentLanguage) (\s@CopyObject' {} a -> s {contentLanguage = a} :: CopyObject)

-- | Specifies the AWS KMS Encryption Context to use for object encryption.
-- The value of this header is a base64-encoded UTF-8 string holding JSON
-- with the encryption context key-value pairs.
copyObject_sSEKMSEncryptionContext :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_sSEKMSEncryptionContext = Lens.lens (\CopyObject' {sSEKMSEncryptionContext} -> sSEKMSEncryptionContext) (\s@CopyObject' {} a -> s {sSEKMSEncryptionContext = a} :: CopyObject) Core.. Lens.mapping Core._Sensitive

-- | A map of metadata to store with the object in S3.
copyObject_metadata :: Lens.Lens' CopyObject (Core.HashMap Core.Text Core.Text)
copyObject_metadata = Lens.lens (\CopyObject' {metadata} -> metadata) (\s@CopyObject' {} a -> s {metadata = a} :: CopyObject) Core.. Lens._Coerce

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
copyObject_contentEncoding :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_contentEncoding = Lens.lens (\CopyObject' {contentEncoding} -> contentEncoding) (\s@CopyObject' {} a -> s {contentEncoding = a} :: CopyObject)

-- | Specifies the AWS KMS key ID to use for object encryption. All GET and
-- PUT requests for an object protected by AWS KMS will fail if not made
-- via SSL or using SigV4. For information about configuring using any of
-- the officially supported AWS SDKs and AWS CLI, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication>
-- in the /Amazon S3 Developer Guide/.
copyObject_sSEKMSKeyId :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_sSEKMSKeyId = Lens.lens (\CopyObject' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@CopyObject' {} a -> s {sSEKMSKeyId = a} :: CopyObject) Core.. Lens.mapping Core._Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
copyObject_sSECustomerKeyMD5 :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_sSECustomerKeyMD5 = Lens.lens (\CopyObject' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@CopyObject' {} a -> s {sSECustomerKeyMD5 = a} :: CopyObject)

-- | Specifies whether the object tag-set are copied from the source object
-- or replaced with tag-set provided in the request.
copyObject_taggingDirective :: Lens.Lens' CopyObject (Core.Maybe TaggingDirective)
copyObject_taggingDirective = Lens.lens (\CopyObject' {taggingDirective} -> taggingDirective) (\s@CopyObject' {} a -> s {taggingDirective = a} :: CopyObject)

-- | By default, Amazon S3 uses the STANDARD Storage Class to store newly
-- created objects. The STANDARD storage class provides high durability and
-- high availability. Depending on performance needs, you can specify a
-- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
-- Storage Class. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 Service Developer Guide/.
copyObject_storageClass :: Lens.Lens' CopyObject (Core.Maybe StorageClass)
copyObject_storageClass = Lens.lens (\CopyObject' {storageClass} -> storageClass) (\s@CopyObject' {} a -> s {storageClass = a} :: CopyObject)

-- | Copies the object if it hasn\'t been modified since the specified time.
copyObject_copySourceIfUnmodifiedSince :: Lens.Lens' CopyObject (Core.Maybe Core.UTCTime)
copyObject_copySourceIfUnmodifiedSince = Lens.lens (\CopyObject' {copySourceIfUnmodifiedSince} -> copySourceIfUnmodifiedSince) (\s@CopyObject' {} a -> s {copySourceIfUnmodifiedSince = a} :: CopyObject) Core.. Lens.mapping Core._Time

-- | Copies the object if it has been modified since the specified time.
copyObject_copySourceIfModifiedSince :: Lens.Lens' CopyObject (Core.Maybe Core.UTCTime)
copyObject_copySourceIfModifiedSince = Lens.lens (\CopyObject' {copySourceIfModifiedSince} -> copySourceIfModifiedSince) (\s@CopyObject' {} a -> s {copySourceIfModifiedSince = a} :: CopyObject) Core.. Lens.mapping Core._Time

-- | Specifies whether Amazon S3 should use an S3 Bucket Key for object
-- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
-- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
-- object encryption with SSE-KMS.
--
-- Specifying this header with a COPY operation doesn’t affect bucket-level
-- settings for S3 Bucket Key.
copyObject_bucketKeyEnabled :: Lens.Lens' CopyObject (Core.Maybe Core.Bool)
copyObject_bucketKeyEnabled = Lens.lens (\CopyObject' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@CopyObject' {} a -> s {bucketKeyEnabled = a} :: CopyObject)

-- | Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
copyObject_grantWriteACP :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_grantWriteACP = Lens.lens (\CopyObject' {grantWriteACP} -> grantWriteACP) (\s@CopyObject' {} a -> s {grantWriteACP = a} :: CopyObject)

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
copyObject_serverSideEncryption :: Lens.Lens' CopyObject (Core.Maybe ServerSideEncryption)
copyObject_serverSideEncryption = Lens.lens (\CopyObject' {serverSideEncryption} -> serverSideEncryption) (\s@CopyObject' {} a -> s {serverSideEncryption = a} :: CopyObject)

-- | Specifies whether you want to apply a Legal Hold to the copied object.
copyObject_objectLockLegalHoldStatus :: Lens.Lens' CopyObject (Core.Maybe ObjectLockLegalHoldStatus)
copyObject_objectLockLegalHoldStatus = Lens.lens (\CopyObject' {objectLockLegalHoldStatus} -> objectLockLegalHoldStatus) (\s@CopyObject' {} a -> s {objectLockLegalHoldStatus = a} :: CopyObject)

-- | Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
copyObject_grantReadACP :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_grantReadACP = Lens.lens (\CopyObject' {grantReadACP} -> grantReadACP) (\s@CopyObject' {} a -> s {grantReadACP = a} :: CopyObject)

-- | The canned ACL to apply to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
copyObject_acl :: Lens.Lens' CopyObject (Core.Maybe ObjectCannedACL)
copyObject_acl = Lens.lens (\CopyObject' {acl} -> acl) (\s@CopyObject' {} a -> s {acl = a} :: CopyObject)

-- | Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
copyObject_sSECustomerAlgorithm :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_sSECustomerAlgorithm = Lens.lens (\CopyObject' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@CopyObject' {} a -> s {sSECustomerAlgorithm = a} :: CopyObject)

-- | Undocumented member.
copyObject_requestPayer :: Lens.Lens' CopyObject (Core.Maybe RequestPayer)
copyObject_requestPayer = Lens.lens (\CopyObject' {requestPayer} -> requestPayer) (\s@CopyObject' {} a -> s {requestPayer = a} :: CopyObject)

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
copyObject_sSECustomerKey :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_sSECustomerKey = Lens.lens (\CopyObject' {sSECustomerKey} -> sSECustomerKey) (\s@CopyObject' {} a -> s {sSECustomerKey = a} :: CopyObject) Core.. Lens.mapping Core._Sensitive

-- | Specifies caching behavior along the request\/reply chain.
copyObject_cacheControl :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_cacheControl = Lens.lens (\CopyObject' {cacheControl} -> cacheControl) (\s@CopyObject' {} a -> s {cacheControl = a} :: CopyObject)

-- | The date and time at which the object is no longer cacheable.
copyObject_expires :: Lens.Lens' CopyObject (Core.Maybe Core.UTCTime)
copyObject_expires = Lens.lens (\CopyObject' {expires} -> expires) (\s@CopyObject' {} a -> s {expires = a} :: CopyObject) Core.. Lens.mapping Core._Time

-- | The Object Lock mode that you want to apply to the copied object.
copyObject_objectLockMode :: Lens.Lens' CopyObject (Core.Maybe ObjectLockMode)
copyObject_objectLockMode = Lens.lens (\CopyObject' {objectLockMode} -> objectLockMode) (\s@CopyObject' {} a -> s {objectLockMode = a} :: CopyObject)

-- | The date and time when you want the copied object\'s Object Lock to
-- expire.
copyObject_objectLockRetainUntilDate :: Lens.Lens' CopyObject (Core.Maybe Core.UTCTime)
copyObject_objectLockRetainUntilDate = Lens.lens (\CopyObject' {objectLockRetainUntilDate} -> objectLockRetainUntilDate) (\s@CopyObject' {} a -> s {objectLockRetainUntilDate = a} :: CopyObject) Core.. Lens.mapping Core._Time

-- | The tag-set for the object destination object this value must be used in
-- conjunction with the @TaggingDirective@. The tag-set must be encoded as
-- URL Query parameters.
copyObject_tagging :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_tagging = Lens.lens (\CopyObject' {tagging} -> tagging) (\s@CopyObject' {} a -> s {tagging = a} :: CopyObject)

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
--
-- This action is not supported by Amazon S3 on Outposts.
copyObject_grantFullControl :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_grantFullControl = Lens.lens (\CopyObject' {grantFullControl} -> grantFullControl) (\s@CopyObject' {} a -> s {grantFullControl = a} :: CopyObject)

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
copyObject_copySourceSSECustomerKeyMD5 :: Lens.Lens' CopyObject (Core.Maybe Core.Text)
copyObject_copySourceSSECustomerKeyMD5 = Lens.lens (\CopyObject' {copySourceSSECustomerKeyMD5} -> copySourceSSECustomerKeyMD5) (\s@CopyObject' {} a -> s {copySourceSSECustomerKeyMD5 = a} :: CopyObject)

-- | Specifies whether the metadata is copied from the source object or
-- replaced with metadata provided in the request.
copyObject_metadataDirective :: Lens.Lens' CopyObject (Core.Maybe MetadataDirective)
copyObject_metadataDirective = Lens.lens (\CopyObject' {metadataDirective} -> metadataDirective) (\s@CopyObject' {} a -> s {metadataDirective = a} :: CopyObject)

-- | The name of the destination bucket.
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
copyObject_bucket :: Lens.Lens' CopyObject BucketName
copyObject_bucket = Lens.lens (\CopyObject' {bucket} -> bucket) (\s@CopyObject' {} a -> s {bucket = a} :: CopyObject)

-- | Specifies the source object for the copy operation. You specify the
-- value in one of two formats, depending on whether you want to access the
-- source object through an
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-points.html access point>:
--
-- -   For objects not accessed through an access point, specify the name
--     of the source bucket and the key of the source object, separated by
--     a slash (\/). For example, to copy the object @reports\/january.pdf@
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
copyObject_copySource :: Lens.Lens' CopyObject Core.Text
copyObject_copySource = Lens.lens (\CopyObject' {copySource} -> copySource) (\s@CopyObject' {} a -> s {copySource = a} :: CopyObject)

-- | The key of the destination object.
copyObject_key :: Lens.Lens' CopyObject ObjectKey
copyObject_key = Lens.lens (\CopyObject' {key} -> key) (\s@CopyObject' {} a -> s {key = a} :: CopyObject)

instance Core.AWSRequest CopyObject where
  type AWSResponse CopyObject = CopyObjectResponse
  request = Request.put defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CopyObjectResponse'
            Core.<$> (h Core..#? "x-amz-request-charged")
            Core.<*> (h Core..#? "x-amz-copy-source-version-id")
            Core.<*> (h Core..#? "x-amz-expiration")
            Core.<*> (h Core..#? "x-amz-server-side-encryption-context")
            Core.<*> ( h
                         Core..#? "x-amz-server-side-encryption-aws-kms-key-id"
                     )
            Core.<*> ( h
                         Core..#? "x-amz-server-side-encryption-customer-key-MD5"
                     )
            Core.<*> (h Core..#? "x-amz-version-id")
            Core.<*> ( h
                         Core..#? "x-amz-server-side-encryption-bucket-key-enabled"
                     )
            Core.<*> (Core.parseXML x)
            Core.<*> (h Core..#? "x-amz-server-side-encryption")
            Core.<*> ( h
                         Core..#? "x-amz-server-side-encryption-customer-algorithm"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CopyObject

instance Core.NFData CopyObject

instance Core.ToHeaders CopyObject where
  toHeaders CopyObject' {..} =
    Core.mconcat
      [ "x-amz-copy-source-if-match"
          Core.=# copySourceIfMatch,
        "x-amz-website-redirect-location"
          Core.=# websiteRedirectLocation,
        "x-amz-grant-read" Core.=# grantRead,
        "x-amz-source-expected-bucket-owner"
          Core.=# expectedSourceBucketOwner,
        "Content-Type" Core.=# contentType,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "Content-Disposition" Core.=# contentDisposition,
        "x-amz-copy-source-server-side-encryption-customer-key"
          Core.=# copySourceSSECustomerKey,
        "x-amz-copy-source-server-side-encryption-customer-algorithm"
          Core.=# copySourceSSECustomerAlgorithm,
        "x-amz-copy-source-if-none-match"
          Core.=# copySourceIfNoneMatch,
        "Content-Language" Core.=# contentLanguage,
        "x-amz-server-side-encryption-context"
          Core.=# sSEKMSEncryptionContext,
        "x-amz-meta-" Core.=# metadata,
        "Content-Encoding" Core.=# contentEncoding,
        "x-amz-server-side-encryption-aws-kms-key-id"
          Core.=# sSEKMSKeyId,
        "x-amz-server-side-encryption-customer-key-MD5"
          Core.=# sSECustomerKeyMD5,
        "x-amz-tagging-directive" Core.=# taggingDirective,
        "x-amz-storage-class" Core.=# storageClass,
        "x-amz-copy-source-if-unmodified-since"
          Core.=# copySourceIfUnmodifiedSince,
        "x-amz-copy-source-if-modified-since"
          Core.=# copySourceIfModifiedSince,
        "x-amz-server-side-encryption-bucket-key-enabled"
          Core.=# bucketKeyEnabled,
        "x-amz-grant-write-acp" Core.=# grantWriteACP,
        "x-amz-server-side-encryption"
          Core.=# serverSideEncryption,
        "x-amz-object-lock-legal-hold"
          Core.=# objectLockLegalHoldStatus,
        "x-amz-grant-read-acp" Core.=# grantReadACP,
        "x-amz-acl" Core.=# acl,
        "x-amz-server-side-encryption-customer-algorithm"
          Core.=# sSECustomerAlgorithm,
        "x-amz-request-payer" Core.=# requestPayer,
        "x-amz-server-side-encryption-customer-key"
          Core.=# sSECustomerKey,
        "Cache-Control" Core.=# cacheControl,
        "Expires" Core.=# expires,
        "x-amz-object-lock-mode" Core.=# objectLockMode,
        "x-amz-object-lock-retain-until-date"
          Core.=# objectLockRetainUntilDate,
        "x-amz-tagging" Core.=# tagging,
        "x-amz-grant-full-control" Core.=# grantFullControl,
        "x-amz-copy-source-server-side-encryption-customer-key-MD5"
          Core.=# copySourceSSECustomerKeyMD5,
        "x-amz-metadata-directive" Core.=# metadataDirective,
        "x-amz-copy-source" Core.=# copySource
      ]

instance Core.ToPath CopyObject where
  toPath CopyObject' {..} =
    Core.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery CopyObject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCopyObjectResponse' smart constructor.
data CopyObjectResponse = CopyObjectResponse'
  { requestCharged :: Core.Maybe RequestCharged,
    -- | Version of the copied object in the destination bucket.
    copySourceVersionId :: Core.Maybe Core.Text,
    -- | If the object expiration is configured, the response includes this
    -- header.
    expiration :: Core.Maybe Core.Text,
    -- | If present, specifies the AWS KMS Encryption Context to use for object
    -- encryption. The value of this header is a base64-encoded UTF-8 string
    -- holding JSON with the encryption context key-value pairs.
    sSEKMSEncryptionContext :: Core.Maybe (Core.Sensitive Core.Text),
    -- | If present, specifies the ID of the AWS Key Management Service (AWS KMS)
    -- symmetric customer managed customer master key (CMK) that was used for
    -- the object.
    sSEKMSKeyId :: Core.Maybe (Core.Sensitive Core.Text),
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header to provide round-trip
    -- message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Core.Maybe Core.Text,
    -- | Version ID of the newly created copy.
    versionId :: Core.Maybe ObjectVersionId,
    -- | Indicates whether the copied object uses an S3 Bucket Key for
    -- server-side encryption with AWS KMS (SSE-KMS).
    bucketKeyEnabled :: Core.Maybe Core.Bool,
    -- | Container for all response elements.
    copyObjectResult :: Core.Maybe CopyObjectResult,
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
-- Create a value of 'CopyObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'copyObjectResponse_requestCharged' - Undocumented member.
--
-- 'copySourceVersionId', 'copyObjectResponse_copySourceVersionId' - Version of the copied object in the destination bucket.
--
-- 'expiration', 'copyObjectResponse_expiration' - If the object expiration is configured, the response includes this
-- header.
--
-- 'sSEKMSEncryptionContext', 'copyObjectResponse_sSEKMSEncryptionContext' - If present, specifies the AWS KMS Encryption Context to use for object
-- encryption. The value of this header is a base64-encoded UTF-8 string
-- holding JSON with the encryption context key-value pairs.
--
-- 'sSEKMSKeyId', 'copyObjectResponse_sSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS)
-- symmetric customer managed customer master key (CMK) that was used for
-- the object.
--
-- 'sSECustomerKeyMD5', 'copyObjectResponse_sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
--
-- 'versionId', 'copyObjectResponse_versionId' - Version ID of the newly created copy.
--
-- 'bucketKeyEnabled', 'copyObjectResponse_bucketKeyEnabled' - Indicates whether the copied object uses an S3 Bucket Key for
-- server-side encryption with AWS KMS (SSE-KMS).
--
-- 'copyObjectResult', 'copyObjectResponse_copyObjectResult' - Container for all response elements.
--
-- 'serverSideEncryption', 'copyObjectResponse_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
--
-- 'sSECustomerAlgorithm', 'copyObjectResponse_sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
--
-- 'httpStatus', 'copyObjectResponse_httpStatus' - The response's http status code.
newCopyObjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CopyObjectResponse
newCopyObjectResponse pHttpStatus_ =
  CopyObjectResponse'
    { requestCharged = Core.Nothing,
      copySourceVersionId = Core.Nothing,
      expiration = Core.Nothing,
      sSEKMSEncryptionContext = Core.Nothing,
      sSEKMSKeyId = Core.Nothing,
      sSECustomerKeyMD5 = Core.Nothing,
      versionId = Core.Nothing,
      bucketKeyEnabled = Core.Nothing,
      copyObjectResult = Core.Nothing,
      serverSideEncryption = Core.Nothing,
      sSECustomerAlgorithm = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
copyObjectResponse_requestCharged :: Lens.Lens' CopyObjectResponse (Core.Maybe RequestCharged)
copyObjectResponse_requestCharged = Lens.lens (\CopyObjectResponse' {requestCharged} -> requestCharged) (\s@CopyObjectResponse' {} a -> s {requestCharged = a} :: CopyObjectResponse)

-- | Version of the copied object in the destination bucket.
copyObjectResponse_copySourceVersionId :: Lens.Lens' CopyObjectResponse (Core.Maybe Core.Text)
copyObjectResponse_copySourceVersionId = Lens.lens (\CopyObjectResponse' {copySourceVersionId} -> copySourceVersionId) (\s@CopyObjectResponse' {} a -> s {copySourceVersionId = a} :: CopyObjectResponse)

-- | If the object expiration is configured, the response includes this
-- header.
copyObjectResponse_expiration :: Lens.Lens' CopyObjectResponse (Core.Maybe Core.Text)
copyObjectResponse_expiration = Lens.lens (\CopyObjectResponse' {expiration} -> expiration) (\s@CopyObjectResponse' {} a -> s {expiration = a} :: CopyObjectResponse)

-- | If present, specifies the AWS KMS Encryption Context to use for object
-- encryption. The value of this header is a base64-encoded UTF-8 string
-- holding JSON with the encryption context key-value pairs.
copyObjectResponse_sSEKMSEncryptionContext :: Lens.Lens' CopyObjectResponse (Core.Maybe Core.Text)
copyObjectResponse_sSEKMSEncryptionContext = Lens.lens (\CopyObjectResponse' {sSEKMSEncryptionContext} -> sSEKMSEncryptionContext) (\s@CopyObjectResponse' {} a -> s {sSEKMSEncryptionContext = a} :: CopyObjectResponse) Core.. Lens.mapping Core._Sensitive

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS)
-- symmetric customer managed customer master key (CMK) that was used for
-- the object.
copyObjectResponse_sSEKMSKeyId :: Lens.Lens' CopyObjectResponse (Core.Maybe Core.Text)
copyObjectResponse_sSEKMSKeyId = Lens.lens (\CopyObjectResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@CopyObjectResponse' {} a -> s {sSEKMSKeyId = a} :: CopyObjectResponse) Core.. Lens.mapping Core._Sensitive

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
copyObjectResponse_sSECustomerKeyMD5 :: Lens.Lens' CopyObjectResponse (Core.Maybe Core.Text)
copyObjectResponse_sSECustomerKeyMD5 = Lens.lens (\CopyObjectResponse' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@CopyObjectResponse' {} a -> s {sSECustomerKeyMD5 = a} :: CopyObjectResponse)

-- | Version ID of the newly created copy.
copyObjectResponse_versionId :: Lens.Lens' CopyObjectResponse (Core.Maybe ObjectVersionId)
copyObjectResponse_versionId = Lens.lens (\CopyObjectResponse' {versionId} -> versionId) (\s@CopyObjectResponse' {} a -> s {versionId = a} :: CopyObjectResponse)

-- | Indicates whether the copied object uses an S3 Bucket Key for
-- server-side encryption with AWS KMS (SSE-KMS).
copyObjectResponse_bucketKeyEnabled :: Lens.Lens' CopyObjectResponse (Core.Maybe Core.Bool)
copyObjectResponse_bucketKeyEnabled = Lens.lens (\CopyObjectResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@CopyObjectResponse' {} a -> s {bucketKeyEnabled = a} :: CopyObjectResponse)

-- | Container for all response elements.
copyObjectResponse_copyObjectResult :: Lens.Lens' CopyObjectResponse (Core.Maybe CopyObjectResult)
copyObjectResponse_copyObjectResult = Lens.lens (\CopyObjectResponse' {copyObjectResult} -> copyObjectResult) (\s@CopyObjectResponse' {} a -> s {copyObjectResult = a} :: CopyObjectResponse)

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
copyObjectResponse_serverSideEncryption :: Lens.Lens' CopyObjectResponse (Core.Maybe ServerSideEncryption)
copyObjectResponse_serverSideEncryption = Lens.lens (\CopyObjectResponse' {serverSideEncryption} -> serverSideEncryption) (\s@CopyObjectResponse' {} a -> s {serverSideEncryption = a} :: CopyObjectResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
copyObjectResponse_sSECustomerAlgorithm :: Lens.Lens' CopyObjectResponse (Core.Maybe Core.Text)
copyObjectResponse_sSECustomerAlgorithm = Lens.lens (\CopyObjectResponse' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@CopyObjectResponse' {} a -> s {sSECustomerAlgorithm = a} :: CopyObjectResponse)

-- | The response's http status code.
copyObjectResponse_httpStatus :: Lens.Lens' CopyObjectResponse Core.Int
copyObjectResponse_httpStatus = Lens.lens (\CopyObjectResponse' {httpStatus} -> httpStatus) (\s@CopyObjectResponse' {} a -> s {httpStatus = a} :: CopyObjectResponse)

instance Core.NFData CopyObjectResponse
