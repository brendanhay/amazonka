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
-- Module      : Amazonka.S3.CopyObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a copy of an object that is already stored in Amazon S3.
--
-- You can store individual objects of up to 5 TB in Amazon S3. You create
-- a copy of your object up to 5 GB in size in a single atomic action using
-- this API. However, to copy an object greater than 5 GB, you must use the
-- multipart upload Upload Part - Copy (UploadPartCopy) API. For more
-- information, see
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
-- before the copy action starts, you receive a standard Amazon S3 error.
-- If the error occurs during the copy operation, the error response is
-- embedded in the @200 OK@ response. This means that a @200 OK@ response
-- can contain either a success or an error. Design your application to
-- parse the contents of the response and handle it appropriately.
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
-- <http://aws.amazon.com/s3/pricing/ Amazon S3 pricing>.
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
-- in the /Amazon S3 User Guide/. For a complete list of Amazon S3-specific
-- condition keys, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/list_amazons3.html Actions, Resources, and Condition Keys for Amazon S3>.
--
-- __x-amz-copy-source-if Headers__
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
-- server-side encryption with Amazon Web Services managed encryption keys
-- (SSE-S3 or SSE-KMS) or a customer-provided encryption key. With
-- server-side encryption, Amazon S3 encrypts your data as it writes it to
-- disks in its data centers and decrypts the data when you access it. For
-- more information about server-side encryption, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html Using Server-Side Encryption>.
--
-- If a target object uses SSE-KMS, you can enable an S3 Bucket Key for the
-- object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-key.html Amazon S3 Bucket Keys>
-- in the /Amazon S3 User Guide/.
--
-- __Access Control List (ACL)-Specific Request Headers__
--
-- When copying an object, you can optionally use headers to grant
-- ACL-based permissions. By default, all objects are private. Only the
-- owner has full access control. When adding a new object, you can grant
-- permissions to individual Amazon Web Services accounts or to predefined
-- groups defined by Amazon S3. These permissions are then added to the ACL
-- on the object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-using-rest-api.html Managing ACLs Using the REST API>.
--
-- If the bucket that you\'re copying objects to uses the bucket owner
-- enforced setting for S3 Object Ownership, ACLs are disabled and no
-- longer affect permissions. Buckets that use this setting only accept PUT
-- requests that don\'t specify an ACL or PUT requests that specify bucket
-- owner full control ACLs, such as the @bucket-owner-full-control@ canned
-- ACL or an equivalent form of this ACL expressed in the XML format.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/about-object-ownership.html Controlling ownership of objects and disabling ACLs>
-- in the /Amazon S3 User Guide/.
--
-- If your bucket uses the bucket owner enforced setting for Object
-- Ownership, all objects written to the bucket by any account will be
-- owned by the bucket owner.
--
-- __Checksums__
--
-- When copying an object, if it has a checksum, that checksum will be
-- copied to the new object by default. When you copy the object over, you
-- may optionally specify a different checksum algorithm to use with the
-- @x-amz-checksum-algorithm@ header.
--
-- __Storage Class Options__
--
-- You can use the @CopyObject@ action to change the storage class of an
-- object that is already stored in Amazon S3 using the @StorageClass@
-- parameter. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 User Guide/.
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
module Amazonka.S3.CopyObject
  ( -- * Creating a Request
    CopyObject (..),
    newCopyObject,

    -- * Request Lenses
    copyObject_acl,
    copyObject_bucketKeyEnabled,
    copyObject_cacheControl,
    copyObject_checksumAlgorithm,
    copyObject_contentDisposition,
    copyObject_contentEncoding,
    copyObject_contentLanguage,
    copyObject_contentType,
    copyObject_copySourceIfMatch,
    copyObject_copySourceIfModifiedSince,
    copyObject_copySourceIfNoneMatch,
    copyObject_copySourceIfUnmodifiedSince,
    copyObject_copySourceSSECustomerAlgorithm,
    copyObject_copySourceSSECustomerKey,
    copyObject_copySourceSSECustomerKeyMD5,
    copyObject_expectedBucketOwner,
    copyObject_expectedSourceBucketOwner,
    copyObject_expires,
    copyObject_grantFullControl,
    copyObject_grantRead,
    copyObject_grantReadACP,
    copyObject_grantWriteACP,
    copyObject_metadata,
    copyObject_metadataDirective,
    copyObject_objectLockLegalHoldStatus,
    copyObject_objectLockMode,
    copyObject_objectLockRetainUntilDate,
    copyObject_requestPayer,
    copyObject_sSECustomerAlgorithm,
    copyObject_sSECustomerKey,
    copyObject_sSECustomerKeyMD5,
    copyObject_sSEKMSEncryptionContext,
    copyObject_sSEKMSKeyId,
    copyObject_serverSideEncryption,
    copyObject_storageClass,
    copyObject_tagging,
    copyObject_taggingDirective,
    copyObject_websiteRedirectLocation,
    copyObject_bucket,
    copyObject_copySource,
    copyObject_key,

    -- * Destructuring the Response
    CopyObjectResponse (..),
    newCopyObjectResponse,

    -- * Response Lenses
    copyObjectResponse_bucketKeyEnabled,
    copyObjectResponse_copyObjectResult,
    copyObjectResponse_copySourceVersionId,
    copyObjectResponse_expiration,
    copyObjectResponse_requestCharged,
    copyObjectResponse_sSECustomerAlgorithm,
    copyObjectResponse_sSECustomerKeyMD5,
    copyObjectResponse_sSEKMSEncryptionContext,
    copyObjectResponse_sSEKMSKeyId,
    copyObjectResponse_serverSideEncryption,
    copyObjectResponse_versionId,
    copyObjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newCopyObject' smart constructor.
data CopyObject = CopyObject'
  { -- | The canned ACL to apply to the object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    acl :: Prelude.Maybe ObjectCannedACL,
    -- | Specifies whether Amazon S3 should use an S3 Bucket Key for object
    -- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
    -- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
    -- object encryption with SSE-KMS.
    --
    -- Specifying this header with a COPY action doesn’t affect bucket-level
    -- settings for S3 Bucket Key.
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies caching behavior along the request\/reply chain.
    cacheControl :: Prelude.Maybe Prelude.Text,
    -- | Indicates the algorithm you want Amazon S3 to use to create the checksum
    -- for the object. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumAlgorithm :: Prelude.Maybe ChecksumAlgorithm,
    -- | Specifies presentational information for the object.
    contentDisposition :: Prelude.Maybe Prelude.Text,
    -- | Specifies what content encodings have been applied to the object and
    -- thus what decoding mechanisms must be applied to obtain the media-type
    -- referenced by the Content-Type header field.
    contentEncoding :: Prelude.Maybe Prelude.Text,
    -- | The language the content is in.
    contentLanguage :: Prelude.Maybe Prelude.Text,
    -- | A standard MIME type describing the format of the object data.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | Copies the object if its entity tag (ETag) matches the specified tag.
    copySourceIfMatch :: Prelude.Maybe Prelude.Text,
    -- | Copies the object if it has been modified since the specified time.
    copySourceIfModifiedSince :: Prelude.Maybe Data.RFC822,
    -- | Copies the object if its entity tag (ETag) is different than the
    -- specified ETag.
    copySourceIfNoneMatch :: Prelude.Maybe Prelude.Text,
    -- | Copies the object if it hasn\'t been modified since the specified time.
    copySourceIfUnmodifiedSince :: Prelude.Maybe Data.RFC822,
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
    -- | The date and time at which the object is no longer cacheable.
    expires :: Prelude.Maybe Data.RFC822,
    -- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
    -- object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantFullControl :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee to read the object data and its metadata.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantRead :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee to read the object ACL.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantReadACP :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee to write the ACL for the applicable object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantWriteACP :: Prelude.Maybe Prelude.Text,
    -- | A map of metadata to store with the object in S3.
    metadata :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | Specifies whether the metadata is copied from the source object or
    -- replaced with metadata provided in the request.
    metadataDirective :: Prelude.Maybe MetadataDirective,
    -- | Specifies whether you want to apply a legal hold to the copied object.
    objectLockLegalHoldStatus :: Prelude.Maybe ObjectLockLegalHoldStatus,
    -- | The Object Lock mode that you want to apply to the copied object.
    objectLockMode :: Prelude.Maybe ObjectLockMode,
    -- | The date and time when you want the copied object\'s Object Lock to
    -- expire.
    objectLockRetainUntilDate :: Prelude.Maybe Data.ISO8601,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | Specifies the algorithm to use to when encrypting the object (for
    -- example, AES256).
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use in
    -- encrypting data. This value is used to store the object and then it is
    -- discarded; Amazon S3 does not store the encryption key. The key must be
    -- appropriate for use with the algorithm specified in the
    -- @x-amz-server-side-encryption-customer-algorithm@ header.
    sSECustomerKey :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
    -- 1321. Amazon S3 uses this header for a message integrity check to ensure
    -- that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Amazon Web Services KMS Encryption Context to use for
    -- object encryption. The value of this header is a base64-encoded UTF-8
    -- string holding JSON with the encryption context key-value pairs.
    sSEKMSEncryptionContext :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Specifies the Amazon Web Services KMS key ID to use for object
    -- encryption. All GET and PUT requests for an object protected by Amazon
    -- Web Services KMS will fail if not made via SSL or using SigV4. For
    -- information about configuring using any of the officially supported
    -- Amazon Web Services SDKs and Amazon Web Services CLI, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication>
    -- in the /Amazon S3 User Guide/.
    sSEKMSKeyId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The server-side encryption algorithm used when storing this object in
    -- Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Prelude.Maybe ServerSideEncryption,
    -- | By default, Amazon S3 uses the STANDARD Storage Class to store newly
    -- created objects. The STANDARD storage class provides high durability and
    -- high availability. Depending on performance needs, you can specify a
    -- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
    -- Storage Class. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
    -- in the /Amazon S3 User Guide/.
    storageClass :: Prelude.Maybe StorageClass,
    -- | The tag-set for the object destination object this value must be used in
    -- conjunction with the @TaggingDirective@. The tag-set must be encoded as
    -- URL Query parameters.
    tagging :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the object tag-set are copied from the source object
    -- or replaced with tag-set provided in the request.
    taggingDirective :: Prelude.Maybe TaggingDirective,
    -- | If the bucket is configured as a website, redirects requests for this
    -- object to another object in the same bucket or to an external URL.
    -- Amazon S3 stores the value of this header in the object metadata.
    websiteRedirectLocation :: Prelude.Maybe Prelude.Text,
    -- | The name of the destination bucket.
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
    --     of the source bucket and the key of the source object, separated by
    --     a slash (\/). For example, to copy the object @reports\/january.pdf@
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
    -- | The key of the destination object.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acl', 'copyObject_acl' - The canned ACL to apply to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'bucketKeyEnabled', 'copyObject_bucketKeyEnabled' - Specifies whether Amazon S3 should use an S3 Bucket Key for object
-- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
-- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
-- object encryption with SSE-KMS.
--
-- Specifying this header with a COPY action doesn’t affect bucket-level
-- settings for S3 Bucket Key.
--
-- 'cacheControl', 'copyObject_cacheControl' - Specifies caching behavior along the request\/reply chain.
--
-- 'checksumAlgorithm', 'copyObject_checksumAlgorithm' - Indicates the algorithm you want Amazon S3 to use to create the checksum
-- for the object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'contentDisposition', 'copyObject_contentDisposition' - Specifies presentational information for the object.
--
-- 'contentEncoding', 'copyObject_contentEncoding' - Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
--
-- 'contentLanguage', 'copyObject_contentLanguage' - The language the content is in.
--
-- 'contentType', 'copyObject_contentType' - A standard MIME type describing the format of the object data.
--
-- 'copySourceIfMatch', 'copyObject_copySourceIfMatch' - Copies the object if its entity tag (ETag) matches the specified tag.
--
-- 'copySourceIfModifiedSince', 'copyObject_copySourceIfModifiedSince' - Copies the object if it has been modified since the specified time.
--
-- 'copySourceIfNoneMatch', 'copyObject_copySourceIfNoneMatch' - Copies the object if its entity tag (ETag) is different than the
-- specified ETag.
--
-- 'copySourceIfUnmodifiedSince', 'copyObject_copySourceIfUnmodifiedSince' - Copies the object if it hasn\'t been modified since the specified time.
--
-- 'copySourceSSECustomerAlgorithm', 'copyObject_copySourceSSECustomerAlgorithm' - Specifies the algorithm to use when decrypting the source object (for
-- example, AES256).
--
-- 'copySourceSSECustomerKey', 'copyObject_copySourceSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header
-- must be one that was used when the source object was created.
--
-- 'copySourceSSECustomerKeyMD5', 'copyObject_copySourceSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'expectedBucketOwner', 'copyObject_expectedBucketOwner' - The account ID of the expected destination bucket owner. If the
-- destination bucket is owned by a different account, the request fails
-- with the HTTP status code @403 Forbidden@ (access denied).
--
-- 'expectedSourceBucketOwner', 'copyObject_expectedSourceBucketOwner' - The account ID of the expected source bucket owner. If the source bucket
-- is owned by a different account, the request fails with the HTTP status
-- code @403 Forbidden@ (access denied).
--
-- 'expires', 'copyObject_expires' - The date and time at which the object is no longer cacheable.
--
-- 'grantFullControl', 'copyObject_grantFullControl' - Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'grantRead', 'copyObject_grantRead' - Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'grantReadACP', 'copyObject_grantReadACP' - Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'grantWriteACP', 'copyObject_grantWriteACP' - Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'metadata', 'copyObject_metadata' - A map of metadata to store with the object in S3.
--
-- 'metadataDirective', 'copyObject_metadataDirective' - Specifies whether the metadata is copied from the source object or
-- replaced with metadata provided in the request.
--
-- 'objectLockLegalHoldStatus', 'copyObject_objectLockLegalHoldStatus' - Specifies whether you want to apply a legal hold to the copied object.
--
-- 'objectLockMode', 'copyObject_objectLockMode' - The Object Lock mode that you want to apply to the copied object.
--
-- 'objectLockRetainUntilDate', 'copyObject_objectLockRetainUntilDate' - The date and time when you want the copied object\'s Object Lock to
-- expire.
--
-- 'requestPayer', 'copyObject_requestPayer' - Undocumented member.
--
-- 'sSECustomerAlgorithm', 'copyObject_sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
--
-- 'sSECustomerKey', 'copyObject_sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- 'sSECustomerKeyMD5', 'copyObject_sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'sSEKMSEncryptionContext', 'copyObject_sSEKMSEncryptionContext' - Specifies the Amazon Web Services KMS Encryption Context to use for
-- object encryption. The value of this header is a base64-encoded UTF-8
-- string holding JSON with the encryption context key-value pairs.
--
-- 'sSEKMSKeyId', 'copyObject_sSEKMSKeyId' - Specifies the Amazon Web Services KMS key ID to use for object
-- encryption. All GET and PUT requests for an object protected by Amazon
-- Web Services KMS will fail if not made via SSL or using SigV4. For
-- information about configuring using any of the officially supported
-- Amazon Web Services SDKs and Amazon Web Services CLI, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication>
-- in the /Amazon S3 User Guide/.
--
-- 'serverSideEncryption', 'copyObject_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
--
-- 'storageClass', 'copyObject_storageClass' - By default, Amazon S3 uses the STANDARD Storage Class to store newly
-- created objects. The STANDARD storage class provides high durability and
-- high availability. Depending on performance needs, you can specify a
-- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
-- Storage Class. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 User Guide/.
--
-- 'tagging', 'copyObject_tagging' - The tag-set for the object destination object this value must be used in
-- conjunction with the @TaggingDirective@. The tag-set must be encoded as
-- URL Query parameters.
--
-- 'taggingDirective', 'copyObject_taggingDirective' - Specifies whether the object tag-set are copied from the source object
-- or replaced with tag-set provided in the request.
--
-- 'websiteRedirectLocation', 'copyObject_websiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
--
-- 'bucket', 'copyObject_bucket' - The name of the destination bucket.
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
-- 'copySource', 'copyObject_copySource' - Specifies the source object for the copy operation. You specify the
-- value in one of two formats, depending on whether you want to access the
-- source object through an
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/access-points.html access point>:
--
-- -   For objects not accessed through an access point, specify the name
--     of the source bucket and the key of the source object, separated by
--     a slash (\/). For example, to copy the object @reports\/january.pdf@
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
-- 'key', 'copyObject_key' - The key of the destination object.
newCopyObject ::
  -- | 'bucket'
  BucketName ->
  -- | 'copySource'
  Prelude.Text ->
  -- | 'key'
  ObjectKey ->
  CopyObject
newCopyObject pBucket_ pCopySource_ pKey_ =
  CopyObject'
    { acl = Prelude.Nothing,
      bucketKeyEnabled = Prelude.Nothing,
      cacheControl = Prelude.Nothing,
      checksumAlgorithm = Prelude.Nothing,
      contentDisposition = Prelude.Nothing,
      contentEncoding = Prelude.Nothing,
      contentLanguage = Prelude.Nothing,
      contentType = Prelude.Nothing,
      copySourceIfMatch = Prelude.Nothing,
      copySourceIfModifiedSince = Prelude.Nothing,
      copySourceIfNoneMatch = Prelude.Nothing,
      copySourceIfUnmodifiedSince = Prelude.Nothing,
      copySourceSSECustomerAlgorithm = Prelude.Nothing,
      copySourceSSECustomerKey = Prelude.Nothing,
      copySourceSSECustomerKeyMD5 = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      expectedSourceBucketOwner = Prelude.Nothing,
      expires = Prelude.Nothing,
      grantFullControl = Prelude.Nothing,
      grantRead = Prelude.Nothing,
      grantReadACP = Prelude.Nothing,
      grantWriteACP = Prelude.Nothing,
      metadata = Prelude.mempty,
      metadataDirective = Prelude.Nothing,
      objectLockLegalHoldStatus = Prelude.Nothing,
      objectLockMode = Prelude.Nothing,
      objectLockRetainUntilDate = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      sSECustomerKey = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      sSEKMSEncryptionContext = Prelude.Nothing,
      sSEKMSKeyId = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      tagging = Prelude.Nothing,
      taggingDirective = Prelude.Nothing,
      websiteRedirectLocation = Prelude.Nothing,
      bucket = pBucket_,
      copySource = pCopySource_,
      key = pKey_
    }

-- | The canned ACL to apply to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
copyObject_acl :: Lens.Lens' CopyObject (Prelude.Maybe ObjectCannedACL)
copyObject_acl = Lens.lens (\CopyObject' {acl} -> acl) (\s@CopyObject' {} a -> s {acl = a} :: CopyObject)

-- | Specifies whether Amazon S3 should use an S3 Bucket Key for object
-- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
-- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
-- object encryption with SSE-KMS.
--
-- Specifying this header with a COPY action doesn’t affect bucket-level
-- settings for S3 Bucket Key.
copyObject_bucketKeyEnabled :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Bool)
copyObject_bucketKeyEnabled = Lens.lens (\CopyObject' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@CopyObject' {} a -> s {bucketKeyEnabled = a} :: CopyObject)

-- | Specifies caching behavior along the request\/reply chain.
copyObject_cacheControl :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_cacheControl = Lens.lens (\CopyObject' {cacheControl} -> cacheControl) (\s@CopyObject' {} a -> s {cacheControl = a} :: CopyObject)

-- | Indicates the algorithm you want Amazon S3 to use to create the checksum
-- for the object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
copyObject_checksumAlgorithm :: Lens.Lens' CopyObject (Prelude.Maybe ChecksumAlgorithm)
copyObject_checksumAlgorithm = Lens.lens (\CopyObject' {checksumAlgorithm} -> checksumAlgorithm) (\s@CopyObject' {} a -> s {checksumAlgorithm = a} :: CopyObject)

-- | Specifies presentational information for the object.
copyObject_contentDisposition :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_contentDisposition = Lens.lens (\CopyObject' {contentDisposition} -> contentDisposition) (\s@CopyObject' {} a -> s {contentDisposition = a} :: CopyObject)

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
copyObject_contentEncoding :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_contentEncoding = Lens.lens (\CopyObject' {contentEncoding} -> contentEncoding) (\s@CopyObject' {} a -> s {contentEncoding = a} :: CopyObject)

-- | The language the content is in.
copyObject_contentLanguage :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_contentLanguage = Lens.lens (\CopyObject' {contentLanguage} -> contentLanguage) (\s@CopyObject' {} a -> s {contentLanguage = a} :: CopyObject)

-- | A standard MIME type describing the format of the object data.
copyObject_contentType :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_contentType = Lens.lens (\CopyObject' {contentType} -> contentType) (\s@CopyObject' {} a -> s {contentType = a} :: CopyObject)

-- | Copies the object if its entity tag (ETag) matches the specified tag.
copyObject_copySourceIfMatch :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_copySourceIfMatch = Lens.lens (\CopyObject' {copySourceIfMatch} -> copySourceIfMatch) (\s@CopyObject' {} a -> s {copySourceIfMatch = a} :: CopyObject)

-- | Copies the object if it has been modified since the specified time.
copyObject_copySourceIfModifiedSince :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.UTCTime)
copyObject_copySourceIfModifiedSince = Lens.lens (\CopyObject' {copySourceIfModifiedSince} -> copySourceIfModifiedSince) (\s@CopyObject' {} a -> s {copySourceIfModifiedSince = a} :: CopyObject) Prelude.. Lens.mapping Data._Time

-- | Copies the object if its entity tag (ETag) is different than the
-- specified ETag.
copyObject_copySourceIfNoneMatch :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_copySourceIfNoneMatch = Lens.lens (\CopyObject' {copySourceIfNoneMatch} -> copySourceIfNoneMatch) (\s@CopyObject' {} a -> s {copySourceIfNoneMatch = a} :: CopyObject)

-- | Copies the object if it hasn\'t been modified since the specified time.
copyObject_copySourceIfUnmodifiedSince :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.UTCTime)
copyObject_copySourceIfUnmodifiedSince = Lens.lens (\CopyObject' {copySourceIfUnmodifiedSince} -> copySourceIfUnmodifiedSince) (\s@CopyObject' {} a -> s {copySourceIfUnmodifiedSince = a} :: CopyObject) Prelude.. Lens.mapping Data._Time

-- | Specifies the algorithm to use when decrypting the source object (for
-- example, AES256).
copyObject_copySourceSSECustomerAlgorithm :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_copySourceSSECustomerAlgorithm = Lens.lens (\CopyObject' {copySourceSSECustomerAlgorithm} -> copySourceSSECustomerAlgorithm) (\s@CopyObject' {} a -> s {copySourceSSECustomerAlgorithm = a} :: CopyObject)

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header
-- must be one that was used when the source object was created.
copyObject_copySourceSSECustomerKey :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_copySourceSSECustomerKey = Lens.lens (\CopyObject' {copySourceSSECustomerKey} -> copySourceSSECustomerKey) (\s@CopyObject' {} a -> s {copySourceSSECustomerKey = a} :: CopyObject) Prelude.. Lens.mapping Data._Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
copyObject_copySourceSSECustomerKeyMD5 :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_copySourceSSECustomerKeyMD5 = Lens.lens (\CopyObject' {copySourceSSECustomerKeyMD5} -> copySourceSSECustomerKeyMD5) (\s@CopyObject' {} a -> s {copySourceSSECustomerKeyMD5 = a} :: CopyObject)

-- | The account ID of the expected destination bucket owner. If the
-- destination bucket is owned by a different account, the request fails
-- with the HTTP status code @403 Forbidden@ (access denied).
copyObject_expectedBucketOwner :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_expectedBucketOwner = Lens.lens (\CopyObject' {expectedBucketOwner} -> expectedBucketOwner) (\s@CopyObject' {} a -> s {expectedBucketOwner = a} :: CopyObject)

-- | The account ID of the expected source bucket owner. If the source bucket
-- is owned by a different account, the request fails with the HTTP status
-- code @403 Forbidden@ (access denied).
copyObject_expectedSourceBucketOwner :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_expectedSourceBucketOwner = Lens.lens (\CopyObject' {expectedSourceBucketOwner} -> expectedSourceBucketOwner) (\s@CopyObject' {} a -> s {expectedSourceBucketOwner = a} :: CopyObject)

-- | The date and time at which the object is no longer cacheable.
copyObject_expires :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.UTCTime)
copyObject_expires = Lens.lens (\CopyObject' {expires} -> expires) (\s@CopyObject' {} a -> s {expires = a} :: CopyObject) Prelude.. Lens.mapping Data._Time

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
--
-- This action is not supported by Amazon S3 on Outposts.
copyObject_grantFullControl :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_grantFullControl = Lens.lens (\CopyObject' {grantFullControl} -> grantFullControl) (\s@CopyObject' {} a -> s {grantFullControl = a} :: CopyObject)

-- | Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
copyObject_grantRead :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_grantRead = Lens.lens (\CopyObject' {grantRead} -> grantRead) (\s@CopyObject' {} a -> s {grantRead = a} :: CopyObject)

-- | Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
copyObject_grantReadACP :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_grantReadACP = Lens.lens (\CopyObject' {grantReadACP} -> grantReadACP) (\s@CopyObject' {} a -> s {grantReadACP = a} :: CopyObject)

-- | Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
copyObject_grantWriteACP :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_grantWriteACP = Lens.lens (\CopyObject' {grantWriteACP} -> grantWriteACP) (\s@CopyObject' {} a -> s {grantWriteACP = a} :: CopyObject)

-- | A map of metadata to store with the object in S3.
copyObject_metadata :: Lens.Lens' CopyObject (Prelude.HashMap Prelude.Text Prelude.Text)
copyObject_metadata = Lens.lens (\CopyObject' {metadata} -> metadata) (\s@CopyObject' {} a -> s {metadata = a} :: CopyObject) Prelude.. Lens.coerced

-- | Specifies whether the metadata is copied from the source object or
-- replaced with metadata provided in the request.
copyObject_metadataDirective :: Lens.Lens' CopyObject (Prelude.Maybe MetadataDirective)
copyObject_metadataDirective = Lens.lens (\CopyObject' {metadataDirective} -> metadataDirective) (\s@CopyObject' {} a -> s {metadataDirective = a} :: CopyObject)

-- | Specifies whether you want to apply a legal hold to the copied object.
copyObject_objectLockLegalHoldStatus :: Lens.Lens' CopyObject (Prelude.Maybe ObjectLockLegalHoldStatus)
copyObject_objectLockLegalHoldStatus = Lens.lens (\CopyObject' {objectLockLegalHoldStatus} -> objectLockLegalHoldStatus) (\s@CopyObject' {} a -> s {objectLockLegalHoldStatus = a} :: CopyObject)

-- | The Object Lock mode that you want to apply to the copied object.
copyObject_objectLockMode :: Lens.Lens' CopyObject (Prelude.Maybe ObjectLockMode)
copyObject_objectLockMode = Lens.lens (\CopyObject' {objectLockMode} -> objectLockMode) (\s@CopyObject' {} a -> s {objectLockMode = a} :: CopyObject)

-- | The date and time when you want the copied object\'s Object Lock to
-- expire.
copyObject_objectLockRetainUntilDate :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.UTCTime)
copyObject_objectLockRetainUntilDate = Lens.lens (\CopyObject' {objectLockRetainUntilDate} -> objectLockRetainUntilDate) (\s@CopyObject' {} a -> s {objectLockRetainUntilDate = a} :: CopyObject) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
copyObject_requestPayer :: Lens.Lens' CopyObject (Prelude.Maybe RequestPayer)
copyObject_requestPayer = Lens.lens (\CopyObject' {requestPayer} -> requestPayer) (\s@CopyObject' {} a -> s {requestPayer = a} :: CopyObject)

-- | Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
copyObject_sSECustomerAlgorithm :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_sSECustomerAlgorithm = Lens.lens (\CopyObject' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@CopyObject' {} a -> s {sSECustomerAlgorithm = a} :: CopyObject)

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
copyObject_sSECustomerKey :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_sSECustomerKey = Lens.lens (\CopyObject' {sSECustomerKey} -> sSECustomerKey) (\s@CopyObject' {} a -> s {sSECustomerKey = a} :: CopyObject) Prelude.. Lens.mapping Data._Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
copyObject_sSECustomerKeyMD5 :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_sSECustomerKeyMD5 = Lens.lens (\CopyObject' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@CopyObject' {} a -> s {sSECustomerKeyMD5 = a} :: CopyObject)

-- | Specifies the Amazon Web Services KMS Encryption Context to use for
-- object encryption. The value of this header is a base64-encoded UTF-8
-- string holding JSON with the encryption context key-value pairs.
copyObject_sSEKMSEncryptionContext :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_sSEKMSEncryptionContext = Lens.lens (\CopyObject' {sSEKMSEncryptionContext} -> sSEKMSEncryptionContext) (\s@CopyObject' {} a -> s {sSEKMSEncryptionContext = a} :: CopyObject) Prelude.. Lens.mapping Data._Sensitive

-- | Specifies the Amazon Web Services KMS key ID to use for object
-- encryption. All GET and PUT requests for an object protected by Amazon
-- Web Services KMS will fail if not made via SSL or using SigV4. For
-- information about configuring using any of the officially supported
-- Amazon Web Services SDKs and Amazon Web Services CLI, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication>
-- in the /Amazon S3 User Guide/.
copyObject_sSEKMSKeyId :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_sSEKMSKeyId = Lens.lens (\CopyObject' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@CopyObject' {} a -> s {sSEKMSKeyId = a} :: CopyObject) Prelude.. Lens.mapping Data._Sensitive

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
copyObject_serverSideEncryption :: Lens.Lens' CopyObject (Prelude.Maybe ServerSideEncryption)
copyObject_serverSideEncryption = Lens.lens (\CopyObject' {serverSideEncryption} -> serverSideEncryption) (\s@CopyObject' {} a -> s {serverSideEncryption = a} :: CopyObject)

-- | By default, Amazon S3 uses the STANDARD Storage Class to store newly
-- created objects. The STANDARD storage class provides high durability and
-- high availability. Depending on performance needs, you can specify a
-- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
-- Storage Class. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 User Guide/.
copyObject_storageClass :: Lens.Lens' CopyObject (Prelude.Maybe StorageClass)
copyObject_storageClass = Lens.lens (\CopyObject' {storageClass} -> storageClass) (\s@CopyObject' {} a -> s {storageClass = a} :: CopyObject)

-- | The tag-set for the object destination object this value must be used in
-- conjunction with the @TaggingDirective@. The tag-set must be encoded as
-- URL Query parameters.
copyObject_tagging :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_tagging = Lens.lens (\CopyObject' {tagging} -> tagging) (\s@CopyObject' {} a -> s {tagging = a} :: CopyObject)

-- | Specifies whether the object tag-set are copied from the source object
-- or replaced with tag-set provided in the request.
copyObject_taggingDirective :: Lens.Lens' CopyObject (Prelude.Maybe TaggingDirective)
copyObject_taggingDirective = Lens.lens (\CopyObject' {taggingDirective} -> taggingDirective) (\s@CopyObject' {} a -> s {taggingDirective = a} :: CopyObject)

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
copyObject_websiteRedirectLocation :: Lens.Lens' CopyObject (Prelude.Maybe Prelude.Text)
copyObject_websiteRedirectLocation = Lens.lens (\CopyObject' {websiteRedirectLocation} -> websiteRedirectLocation) (\s@CopyObject' {} a -> s {websiteRedirectLocation = a} :: CopyObject)

-- | The name of the destination bucket.
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
copyObject_bucket :: Lens.Lens' CopyObject BucketName
copyObject_bucket = Lens.lens (\CopyObject' {bucket} -> bucket) (\s@CopyObject' {} a -> s {bucket = a} :: CopyObject)

-- | Specifies the source object for the copy operation. You specify the
-- value in one of two formats, depending on whether you want to access the
-- source object through an
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/access-points.html access point>:
--
-- -   For objects not accessed through an access point, specify the name
--     of the source bucket and the key of the source object, separated by
--     a slash (\/). For example, to copy the object @reports\/january.pdf@
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
copyObject_copySource :: Lens.Lens' CopyObject Prelude.Text
copyObject_copySource = Lens.lens (\CopyObject' {copySource} -> copySource) (\s@CopyObject' {} a -> s {copySource = a} :: CopyObject)

-- | The key of the destination object.
copyObject_key :: Lens.Lens' CopyObject ObjectKey
copyObject_key = Lens.lens (\CopyObject' {key} -> key) (\s@CopyObject' {} a -> s {key = a} :: CopyObject)

instance Core.AWSRequest CopyObject where
  type AWSResponse CopyObject = CopyObjectResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.put (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CopyObjectResponse'
            Prelude.<$> ( h
                            Data..#? "x-amz-server-side-encryption-bucket-key-enabled"
                        )
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (h Data..#? "x-amz-copy-source-version-id")
            Prelude.<*> (h Data..#? "x-amz-expiration")
            Prelude.<*> (h Data..#? "x-amz-request-charged")
            Prelude.<*> ( h
                            Data..#? "x-amz-server-side-encryption-customer-algorithm"
                        )
            Prelude.<*> ( h
                            Data..#? "x-amz-server-side-encryption-customer-key-MD5"
                        )
            Prelude.<*> (h Data..#? "x-amz-server-side-encryption-context")
            Prelude.<*> ( h
                            Data..#? "x-amz-server-side-encryption-aws-kms-key-id"
                        )
            Prelude.<*> (h Data..#? "x-amz-server-side-encryption")
            Prelude.<*> (h Data..#? "x-amz-version-id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyObject where
  hashWithSalt _salt CopyObject' {..} =
    _salt
      `Prelude.hashWithSalt` acl
      `Prelude.hashWithSalt` bucketKeyEnabled
      `Prelude.hashWithSalt` cacheControl
      `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` contentDisposition
      `Prelude.hashWithSalt` contentEncoding
      `Prelude.hashWithSalt` contentLanguage
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` copySourceIfMatch
      `Prelude.hashWithSalt` copySourceIfModifiedSince
      `Prelude.hashWithSalt` copySourceIfNoneMatch
      `Prelude.hashWithSalt` copySourceIfUnmodifiedSince
      `Prelude.hashWithSalt` copySourceSSECustomerAlgorithm
      `Prelude.hashWithSalt` copySourceSSECustomerKey
      `Prelude.hashWithSalt` copySourceSSECustomerKeyMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` expectedSourceBucketOwner
      `Prelude.hashWithSalt` expires
      `Prelude.hashWithSalt` grantFullControl
      `Prelude.hashWithSalt` grantRead
      `Prelude.hashWithSalt` grantReadACP
      `Prelude.hashWithSalt` grantWriteACP
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` metadataDirective
      `Prelude.hashWithSalt` objectLockLegalHoldStatus
      `Prelude.hashWithSalt` objectLockMode
      `Prelude.hashWithSalt` objectLockRetainUntilDate
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` sSECustomerAlgorithm
      `Prelude.hashWithSalt` sSECustomerKey
      `Prelude.hashWithSalt` sSECustomerKeyMD5
      `Prelude.hashWithSalt` sSEKMSEncryptionContext
      `Prelude.hashWithSalt` sSEKMSKeyId
      `Prelude.hashWithSalt` serverSideEncryption
      `Prelude.hashWithSalt` storageClass
      `Prelude.hashWithSalt` tagging
      `Prelude.hashWithSalt` taggingDirective
      `Prelude.hashWithSalt` websiteRedirectLocation
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` copySource
      `Prelude.hashWithSalt` key

instance Prelude.NFData CopyObject where
  rnf CopyObject' {..} =
    Prelude.rnf acl
      `Prelude.seq` Prelude.rnf bucketKeyEnabled
      `Prelude.seq` Prelude.rnf cacheControl
      `Prelude.seq` Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf contentDisposition
      `Prelude.seq` Prelude.rnf contentEncoding
      `Prelude.seq` Prelude.rnf contentLanguage
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf copySourceIfMatch
      `Prelude.seq` Prelude.rnf copySourceIfModifiedSince
      `Prelude.seq` Prelude.rnf copySourceIfNoneMatch
      `Prelude.seq` Prelude.rnf copySourceIfUnmodifiedSince
      `Prelude.seq` Prelude.rnf copySourceSSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf copySourceSSECustomerKey
      `Prelude.seq` Prelude.rnf copySourceSSECustomerKeyMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf
        expectedSourceBucketOwner
      `Prelude.seq` Prelude.rnf expires
      `Prelude.seq` Prelude.rnf grantFullControl
      `Prelude.seq` Prelude.rnf grantRead
      `Prelude.seq` Prelude.rnf grantReadACP
      `Prelude.seq` Prelude.rnf grantWriteACP
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf
        metadataDirective
      `Prelude.seq` Prelude.rnf
        objectLockLegalHoldStatus
      `Prelude.seq` Prelude.rnf
        objectLockMode
      `Prelude.seq` Prelude.rnf
        objectLockRetainUntilDate
      `Prelude.seq` Prelude.rnf
        requestPayer
      `Prelude.seq` Prelude.rnf
        sSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf
        sSECustomerKey
      `Prelude.seq` Prelude.rnf
        sSECustomerKeyMD5
      `Prelude.seq` Prelude.rnf
        sSEKMSEncryptionContext
      `Prelude.seq` Prelude.rnf
        sSEKMSKeyId
      `Prelude.seq` Prelude.rnf
        serverSideEncryption
      `Prelude.seq` Prelude.rnf
        storageClass
      `Prelude.seq` Prelude.rnf
        tagging
      `Prelude.seq` Prelude.rnf
        taggingDirective
      `Prelude.seq` Prelude.rnf
        websiteRedirectLocation
      `Prelude.seq` Prelude.rnf
        bucket
      `Prelude.seq` Prelude.rnf
        copySource
      `Prelude.seq` Prelude.rnf
        key

instance Data.ToHeaders CopyObject where
  toHeaders CopyObject' {..} =
    Prelude.mconcat
      [ "x-amz-acl" Data.=# acl,
        "x-amz-server-side-encryption-bucket-key-enabled"
          Data.=# bucketKeyEnabled,
        "Cache-Control" Data.=# cacheControl,
        "x-amz-checksum-algorithm" Data.=# checksumAlgorithm,
        "Content-Disposition" Data.=# contentDisposition,
        "Content-Encoding" Data.=# contentEncoding,
        "Content-Language" Data.=# contentLanguage,
        "Content-Type" Data.=# contentType,
        "x-amz-copy-source-if-match"
          Data.=# copySourceIfMatch,
        "x-amz-copy-source-if-modified-since"
          Data.=# copySourceIfModifiedSince,
        "x-amz-copy-source-if-none-match"
          Data.=# copySourceIfNoneMatch,
        "x-amz-copy-source-if-unmodified-since"
          Data.=# copySourceIfUnmodifiedSince,
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
        "Expires" Data.=# expires,
        "x-amz-grant-full-control" Data.=# grantFullControl,
        "x-amz-grant-read" Data.=# grantRead,
        "x-amz-grant-read-acp" Data.=# grantReadACP,
        "x-amz-grant-write-acp" Data.=# grantWriteACP,
        "x-amz-meta-" Data.=# metadata,
        "x-amz-metadata-directive" Data.=# metadataDirective,
        "x-amz-object-lock-legal-hold"
          Data.=# objectLockLegalHoldStatus,
        "x-amz-object-lock-mode" Data.=# objectLockMode,
        "x-amz-object-lock-retain-until-date"
          Data.=# objectLockRetainUntilDate,
        "x-amz-request-payer" Data.=# requestPayer,
        "x-amz-server-side-encryption-customer-algorithm"
          Data.=# sSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key"
          Data.=# sSECustomerKey,
        "x-amz-server-side-encryption-customer-key-MD5"
          Data.=# sSECustomerKeyMD5,
        "x-amz-server-side-encryption-context"
          Data.=# sSEKMSEncryptionContext,
        "x-amz-server-side-encryption-aws-kms-key-id"
          Data.=# sSEKMSKeyId,
        "x-amz-server-side-encryption"
          Data.=# serverSideEncryption,
        "x-amz-storage-class" Data.=# storageClass,
        "x-amz-tagging" Data.=# tagging,
        "x-amz-tagging-directive" Data.=# taggingDirective,
        "x-amz-website-redirect-location"
          Data.=# websiteRedirectLocation,
        "x-amz-copy-source" Data.=# copySource
      ]

instance Data.ToPath CopyObject where
  toPath CopyObject' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery CopyObject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCopyObjectResponse' smart constructor.
data CopyObjectResponse = CopyObjectResponse'
  { -- | Indicates whether the copied object uses an S3 Bucket Key for
    -- server-side encryption with Amazon Web Services KMS (SSE-KMS).
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Container for all response elements.
    copyObjectResult :: Prelude.Maybe CopyObjectResult,
    -- | Version of the copied object in the destination bucket.
    copySourceVersionId :: Prelude.Maybe Prelude.Text,
    -- | If the object expiration is configured, the response includes this
    -- header.
    expiration :: Prelude.Maybe Prelude.Text,
    requestCharged :: Prelude.Maybe RequestCharged,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header confirming the
    -- encryption algorithm used.
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header to provide round-trip
    -- message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | If present, specifies the Amazon Web Services KMS Encryption Context to
    -- use for object encryption. The value of this header is a base64-encoded
    -- UTF-8 string holding JSON with the encryption context key-value pairs.
    sSEKMSEncryptionContext :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | If present, specifies the ID of the Amazon Web Services Key Management
    -- Service (Amazon Web Services KMS) symmetric customer managed key that
    -- was used for the object.
    sSEKMSKeyId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The server-side encryption algorithm used when storing this object in
    -- Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Prelude.Maybe ServerSideEncryption,
    -- | Version ID of the newly created copy.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketKeyEnabled', 'copyObjectResponse_bucketKeyEnabled' - Indicates whether the copied object uses an S3 Bucket Key for
-- server-side encryption with Amazon Web Services KMS (SSE-KMS).
--
-- 'copyObjectResult', 'copyObjectResponse_copyObjectResult' - Container for all response elements.
--
-- 'copySourceVersionId', 'copyObjectResponse_copySourceVersionId' - Version of the copied object in the destination bucket.
--
-- 'expiration', 'copyObjectResponse_expiration' - If the object expiration is configured, the response includes this
-- header.
--
-- 'requestCharged', 'copyObjectResponse_requestCharged' - Undocumented member.
--
-- 'sSECustomerAlgorithm', 'copyObjectResponse_sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
--
-- 'sSECustomerKeyMD5', 'copyObjectResponse_sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
--
-- 'sSEKMSEncryptionContext', 'copyObjectResponse_sSEKMSEncryptionContext' - If present, specifies the Amazon Web Services KMS Encryption Context to
-- use for object encryption. The value of this header is a base64-encoded
-- UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- 'sSEKMSKeyId', 'copyObjectResponse_sSEKMSKeyId' - If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key that
-- was used for the object.
--
-- 'serverSideEncryption', 'copyObjectResponse_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
--
-- 'versionId', 'copyObjectResponse_versionId' - Version ID of the newly created copy.
--
-- 'httpStatus', 'copyObjectResponse_httpStatus' - The response's http status code.
newCopyObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CopyObjectResponse
newCopyObjectResponse pHttpStatus_ =
  CopyObjectResponse'
    { bucketKeyEnabled =
        Prelude.Nothing,
      copyObjectResult = Prelude.Nothing,
      copySourceVersionId = Prelude.Nothing,
      expiration = Prelude.Nothing,
      requestCharged = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      sSEKMSEncryptionContext = Prelude.Nothing,
      sSEKMSKeyId = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      versionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether the copied object uses an S3 Bucket Key for
-- server-side encryption with Amazon Web Services KMS (SSE-KMS).
copyObjectResponse_bucketKeyEnabled :: Lens.Lens' CopyObjectResponse (Prelude.Maybe Prelude.Bool)
copyObjectResponse_bucketKeyEnabled = Lens.lens (\CopyObjectResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@CopyObjectResponse' {} a -> s {bucketKeyEnabled = a} :: CopyObjectResponse)

-- | Container for all response elements.
copyObjectResponse_copyObjectResult :: Lens.Lens' CopyObjectResponse (Prelude.Maybe CopyObjectResult)
copyObjectResponse_copyObjectResult = Lens.lens (\CopyObjectResponse' {copyObjectResult} -> copyObjectResult) (\s@CopyObjectResponse' {} a -> s {copyObjectResult = a} :: CopyObjectResponse)

-- | Version of the copied object in the destination bucket.
copyObjectResponse_copySourceVersionId :: Lens.Lens' CopyObjectResponse (Prelude.Maybe Prelude.Text)
copyObjectResponse_copySourceVersionId = Lens.lens (\CopyObjectResponse' {copySourceVersionId} -> copySourceVersionId) (\s@CopyObjectResponse' {} a -> s {copySourceVersionId = a} :: CopyObjectResponse)

-- | If the object expiration is configured, the response includes this
-- header.
copyObjectResponse_expiration :: Lens.Lens' CopyObjectResponse (Prelude.Maybe Prelude.Text)
copyObjectResponse_expiration = Lens.lens (\CopyObjectResponse' {expiration} -> expiration) (\s@CopyObjectResponse' {} a -> s {expiration = a} :: CopyObjectResponse)

-- | Undocumented member.
copyObjectResponse_requestCharged :: Lens.Lens' CopyObjectResponse (Prelude.Maybe RequestCharged)
copyObjectResponse_requestCharged = Lens.lens (\CopyObjectResponse' {requestCharged} -> requestCharged) (\s@CopyObjectResponse' {} a -> s {requestCharged = a} :: CopyObjectResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
copyObjectResponse_sSECustomerAlgorithm :: Lens.Lens' CopyObjectResponse (Prelude.Maybe Prelude.Text)
copyObjectResponse_sSECustomerAlgorithm = Lens.lens (\CopyObjectResponse' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@CopyObjectResponse' {} a -> s {sSECustomerAlgorithm = a} :: CopyObjectResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
copyObjectResponse_sSECustomerKeyMD5 :: Lens.Lens' CopyObjectResponse (Prelude.Maybe Prelude.Text)
copyObjectResponse_sSECustomerKeyMD5 = Lens.lens (\CopyObjectResponse' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@CopyObjectResponse' {} a -> s {sSECustomerKeyMD5 = a} :: CopyObjectResponse)

-- | If present, specifies the Amazon Web Services KMS Encryption Context to
-- use for object encryption. The value of this header is a base64-encoded
-- UTF-8 string holding JSON with the encryption context key-value pairs.
copyObjectResponse_sSEKMSEncryptionContext :: Lens.Lens' CopyObjectResponse (Prelude.Maybe Prelude.Text)
copyObjectResponse_sSEKMSEncryptionContext = Lens.lens (\CopyObjectResponse' {sSEKMSEncryptionContext} -> sSEKMSEncryptionContext) (\s@CopyObjectResponse' {} a -> s {sSEKMSEncryptionContext = a} :: CopyObjectResponse) Prelude.. Lens.mapping Data._Sensitive

-- | If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key that
-- was used for the object.
copyObjectResponse_sSEKMSKeyId :: Lens.Lens' CopyObjectResponse (Prelude.Maybe Prelude.Text)
copyObjectResponse_sSEKMSKeyId = Lens.lens (\CopyObjectResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@CopyObjectResponse' {} a -> s {sSEKMSKeyId = a} :: CopyObjectResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
copyObjectResponse_serverSideEncryption :: Lens.Lens' CopyObjectResponse (Prelude.Maybe ServerSideEncryption)
copyObjectResponse_serverSideEncryption = Lens.lens (\CopyObjectResponse' {serverSideEncryption} -> serverSideEncryption) (\s@CopyObjectResponse' {} a -> s {serverSideEncryption = a} :: CopyObjectResponse)

-- | Version ID of the newly created copy.
copyObjectResponse_versionId :: Lens.Lens' CopyObjectResponse (Prelude.Maybe ObjectVersionId)
copyObjectResponse_versionId = Lens.lens (\CopyObjectResponse' {versionId} -> versionId) (\s@CopyObjectResponse' {} a -> s {versionId = a} :: CopyObjectResponse)

-- | The response's http status code.
copyObjectResponse_httpStatus :: Lens.Lens' CopyObjectResponse Prelude.Int
copyObjectResponse_httpStatus = Lens.lens (\CopyObjectResponse' {httpStatus} -> httpStatus) (\s@CopyObjectResponse' {} a -> s {httpStatus = a} :: CopyObjectResponse)

instance Prelude.NFData CopyObjectResponse where
  rnf CopyObjectResponse' {..} =
    Prelude.rnf bucketKeyEnabled
      `Prelude.seq` Prelude.rnf copyObjectResult
      `Prelude.seq` Prelude.rnf copySourceVersionId
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf requestCharged
      `Prelude.seq` Prelude.rnf sSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf sSECustomerKeyMD5
      `Prelude.seq` Prelude.rnf sSEKMSEncryptionContext
      `Prelude.seq` Prelude.rnf sSEKMSKeyId
      `Prelude.seq` Prelude.rnf serverSideEncryption
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf httpStatus
