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
-- Module      : Amazonka.S3.PutObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an object to a bucket. You must have WRITE permissions on a bucket
-- to add an object to it.
--
-- Amazon S3 never adds partial objects; if you receive a success response,
-- Amazon S3 added the entire object to the bucket.
--
-- Amazon S3 is a distributed system. If it receives multiple write
-- requests for the same object simultaneously, it overwrites all but the
-- last object written. Amazon S3 does not provide object locking; if you
-- need this, make sure to build it into your application layer or use
-- versioning instead.
--
-- To ensure that data is not corrupted traversing the network, use the
-- @Content-MD5@ header. When you use this header, Amazon S3 checks the
-- object against the provided MD5 value and, if they do not match, returns
-- an error. Additionally, you can calculate the MD5 while putting an
-- object to Amazon S3 and compare the returned ETag to the calculated MD5
-- value.
--
-- -   To successfully complete the @PutObject@ request, you must have the
--     @s3:PutObject@ in your IAM permissions.
--
-- -   To successfully change the objects acl of your @PutObject@ request,
--     you must have the @s3:PutObjectAcl@ in your IAM permissions.
--
-- -   The @Content-MD5@ header is required for any request to upload an
--     object with a retention period configured using Amazon S3 Object
--     Lock. For more information about Amazon S3 Object Lock, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock-overview.html Amazon S3 Object Lock Overview>
--     in the /Amazon S3 User Guide/.
--
-- __Server-side Encryption__
--
-- You can optionally request server-side encryption. With server-side
-- encryption, Amazon S3 encrypts your data as it writes it to disks in its
-- data centers and decrypts the data when you access it. You have the
-- option to provide your own encryption key or use Amazon Web Services
-- managed encryption keys (SSE-S3 or SSE-KMS). For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Using Server-Side Encryption>.
--
-- If you request server-side encryption using Amazon Web Services Key
-- Management Service (SSE-KMS), you can enable an S3 Bucket Key at the
-- object-level. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-key.html Amazon S3 Bucket Keys>
-- in the /Amazon S3 User Guide/.
--
-- __Access Control List (ACL)-Specific Request Headers__
--
-- You can use headers to grant ACL- based permissions. By default, all
-- objects are private. Only the owner has full access control. When adding
-- a new object, you can grant permissions to individual Amazon Web
-- Services accounts or to predefined groups defined by Amazon S3. These
-- permissions are then added to the ACL on the object. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-using-rest-api.html Managing ACLs Using the REST API>.
--
-- If the bucket that you\'re uploading objects to uses the bucket owner
-- enforced setting for S3 Object Ownership, ACLs are disabled and no
-- longer affect permissions. Buckets that use this setting only accept PUT
-- requests that don\'t specify an ACL or PUT requests that specify bucket
-- owner full control ACLs, such as the @bucket-owner-full-control@ canned
-- ACL or an equivalent form of this ACL expressed in the XML format. PUT
-- requests that contain other ACLs (for example, custom grants to certain
-- Amazon Web Services accounts) fail and return a @400@ error with the
-- error code @AccessControlListNotSupported@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/about-object-ownership.html Controlling ownership of objects and disabling ACLs>
-- in the /Amazon S3 User Guide/.
--
-- If your bucket uses the bucket owner enforced setting for Object
-- Ownership, all objects written to the bucket by any account will be
-- owned by the bucket owner.
--
-- __Storage Class Options__
--
-- By default, Amazon S3 uses the STANDARD Storage Class to store newly
-- created objects. The STANDARD storage class provides high durability and
-- high availability. Depending on performance needs, you can specify a
-- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
-- Storage Class. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 User Guide/.
--
-- __Versioning__
--
-- If you enable versioning for a bucket, Amazon S3 automatically generates
-- a unique version ID for the object being stored. Amazon S3 returns this
-- ID in the response. When you enable versioning for a bucket, if Amazon
-- S3 receives multiple write requests for the same object simultaneously,
-- it stores all of the objects.
--
-- For more information about versioning, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/AddingObjectstoVersioningEnabledBuckets.html Adding Objects to Versioning Enabled Buckets>.
-- For information about returning the versioning state of a bucket, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketVersioning.html GetBucketVersioning>.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CopyObject.html CopyObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
module Amazonka.S3.PutObject
  ( -- * Creating a Request
    PutObject (..),
    newPutObject,

    -- * Request Lenses
    putObject_acl,
    putObject_bucketKeyEnabled,
    putObject_cacheControl,
    putObject_checksumAlgorithm,
    putObject_checksumCRC32,
    putObject_checksumCRC32C,
    putObject_checksumSHA1,
    putObject_checksumSHA256,
    putObject_contentDisposition,
    putObject_contentEncoding,
    putObject_contentLanguage,
    putObject_contentLength,
    putObject_contentMD5,
    putObject_contentType,
    putObject_expectedBucketOwner,
    putObject_expires,
    putObject_grantFullControl,
    putObject_grantRead,
    putObject_grantReadACP,
    putObject_grantWriteACP,
    putObject_metadata,
    putObject_objectLockLegalHoldStatus,
    putObject_objectLockMode,
    putObject_objectLockRetainUntilDate,
    putObject_requestPayer,
    putObject_sSECustomerAlgorithm,
    putObject_sSECustomerKey,
    putObject_sSECustomerKeyMD5,
    putObject_sSEKMSEncryptionContext,
    putObject_sSEKMSKeyId,
    putObject_serverSideEncryption,
    putObject_storageClass,
    putObject_tagging,
    putObject_websiteRedirectLocation,
    putObject_bucket,
    putObject_key,
    putObject_body,

    -- * Destructuring the Response
    PutObjectResponse (..),
    newPutObjectResponse,

    -- * Response Lenses
    putObjectResponse_bucketKeyEnabled,
    putObjectResponse_checksumCRC32,
    putObjectResponse_checksumCRC32C,
    putObjectResponse_checksumSHA1,
    putObjectResponse_checksumSHA256,
    putObjectResponse_eTag,
    putObjectResponse_expiration,
    putObjectResponse_requestCharged,
    putObjectResponse_sSECustomerAlgorithm,
    putObjectResponse_sSECustomerKeyMD5,
    putObjectResponse_sSEKMSEncryptionContext,
    putObjectResponse_sSEKMSKeyId,
    putObjectResponse_serverSideEncryption,
    putObjectResponse_versionId,
    putObjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutObject' smart constructor.
data PutObject = PutObject'
  { -- | The canned ACL to apply to the object. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    acl :: Prelude.Maybe ObjectCannedACL,
    -- | Specifies whether Amazon S3 should use an S3 Bucket Key for object
    -- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
    -- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
    -- object encryption with SSE-KMS.
    --
    -- Specifying this header with a PUT action doesn’t affect bucket-level
    -- settings for S3 Bucket Key.
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Can be used to specify caching behavior along the request\/reply chain.
    -- For more information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
    cacheControl :: Prelude.Maybe Prelude.Text,
    -- | Indicates the algorithm used to create the checksum for the object when
    -- using the SDK. This header will not provide any additional functionality
    -- if not using the SDK. When sending this header, there must be a
    -- corresponding @x-amz-checksum@ or @x-amz-trailer@ header sent.
    -- Otherwise, Amazon S3 fails the request with the HTTP status code
    -- @400 Bad Request@. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    --
    -- If you provide an individual checksum, Amazon S3 ignores any provided
    -- @ChecksumAlgorithm@ parameter.
    checksumAlgorithm :: Prelude.Maybe ChecksumAlgorithm,
    -- | This header can be used as a data integrity check to verify that the
    -- data received is the same data that was originally sent. This header
    -- specifies the base64-encoded, 32-bit CRC32 checksum of the object. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumCRC32 :: Prelude.Maybe Prelude.Text,
    -- | This header can be used as a data integrity check to verify that the
    -- data received is the same data that was originally sent. This header
    -- specifies the base64-encoded, 32-bit CRC32C checksum of the object. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumCRC32C :: Prelude.Maybe Prelude.Text,
    -- | This header can be used as a data integrity check to verify that the
    -- data received is the same data that was originally sent. This header
    -- specifies the base64-encoded, 160-bit SHA-1 digest of the object. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumSHA1 :: Prelude.Maybe Prelude.Text,
    -- | This header can be used as a data integrity check to verify that the
    -- data received is the same data that was originally sent. This header
    -- specifies the base64-encoded, 256-bit SHA-256 digest of the object. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumSHA256 :: Prelude.Maybe Prelude.Text,
    -- | Specifies presentational information for the object. For more
    -- information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1>.
    contentDisposition :: Prelude.Maybe Prelude.Text,
    -- | Specifies what content encodings have been applied to the object and
    -- thus what decoding mechanisms must be applied to obtain the media-type
    -- referenced by the Content-Type header field. For more information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11>.
    contentEncoding :: Prelude.Maybe Prelude.Text,
    -- | The language the content is in.
    contentLanguage :: Prelude.Maybe Prelude.Text,
    -- | Size of the body in bytes. This parameter is useful when the size of the
    -- body cannot be determined automatically. For more information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13>.
    contentLength :: Prelude.Maybe Prelude.Integer,
    -- | The base64-encoded 128-bit MD5 digest of the message (without the
    -- headers) according to RFC 1864. This header can be used as a message
    -- integrity check to verify that the data is the same data that was
    -- originally sent. Although it is optional, we recommend using the
    -- Content-MD5 mechanism as an end-to-end integrity check. For more
    -- information about REST request authentication, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication>.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | A standard MIME type describing the format of the contents. For more
    -- information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17>.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the object is no longer cacheable. For more
    -- information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21>.
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
    -- | Specifies whether a legal hold will be applied to this object. For more
    -- information about S3 Object Lock, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
    objectLockLegalHoldStatus :: Prelude.Maybe ObjectLockLegalHoldStatus,
    -- | The Object Lock mode that you want to apply to this object.
    objectLockMode :: Prelude.Maybe ObjectLockMode,
    -- | The date and time when you want this object\'s Object Lock to expire.
    -- Must be formatted as a timestamp parameter.
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
    -- | If @x-amz-server-side-encryption@ is present and has the value of
    -- @aws:kms@, this header specifies the ID of the Amazon Web Services Key
    -- Management Service (Amazon Web Services KMS) symmetrical customer
    -- managed key that was used for the object. If you specify
    -- @x-amz-server-side-encryption:aws:kms@, but do not
    -- provide@ x-amz-server-side-encryption-aws-kms-key-id@, Amazon S3 uses
    -- the Amazon Web Services managed key to protect the data. If the KMS key
    -- does not exist in the same account issuing the command, you must use the
    -- full ARN and not just the ID.
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
    -- | The tag-set for the object. The tag-set must be encoded as URL Query
    -- parameters. (For example, \"Key1=Value1\")
    tagging :: Prelude.Maybe Prelude.Text,
    -- | If the bucket is configured as a website, redirects requests for this
    -- object to another object in the same bucket or to an external URL.
    -- Amazon S3 stores the value of this header in the object metadata. For
    -- information about object metadata, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html Object Key and Metadata>.
    --
    -- In the following example, the request header sets the redirect to an
    -- object (anotherPage.html) in the same bucket:
    --
    -- @x-amz-website-redirect-location: \/anotherPage.html@
    --
    -- In the following example, the request header sets the object redirect to
    -- another website:
    --
    -- @x-amz-website-redirect-location: http:\/\/www.example.com\/@
    --
    -- For more information about website hosting in Amazon S3, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3>
    -- and
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html How to Configure Website Page Redirects>.
    websiteRedirectLocation :: Prelude.Maybe Prelude.Text,
    -- | The bucket name to which the PUT action was initiated.
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
    -- | Object key for which the PUT action was initiated.
    key :: ObjectKey,
    -- | Object data.
    body :: Data.RequestBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acl', 'putObject_acl' - The canned ACL to apply to the object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'bucketKeyEnabled', 'putObject_bucketKeyEnabled' - Specifies whether Amazon S3 should use an S3 Bucket Key for object
-- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
-- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
-- object encryption with SSE-KMS.
--
-- Specifying this header with a PUT action doesn’t affect bucket-level
-- settings for S3 Bucket Key.
--
-- 'cacheControl', 'putObject_cacheControl' - Can be used to specify caching behavior along the request\/reply chain.
-- For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
--
-- 'checksumAlgorithm', 'putObject_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
-- using the SDK. This header will not provide any additional functionality
-- if not using the SDK. When sending this header, there must be a
-- corresponding @x-amz-checksum@ or @x-amz-trailer@ header sent.
-- Otherwise, Amazon S3 fails the request with the HTTP status code
-- @400 Bad Request@. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- If you provide an individual checksum, Amazon S3 ignores any provided
-- @ChecksumAlgorithm@ parameter.
--
-- 'checksumCRC32', 'putObject_checksumCRC32' - This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 32-bit CRC32 checksum of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumCRC32C', 'putObject_checksumCRC32C' - This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 32-bit CRC32C checksum of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumSHA1', 'putObject_checksumSHA1' - This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 160-bit SHA-1 digest of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumSHA256', 'putObject_checksumSHA256' - This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 256-bit SHA-256 digest of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'contentDisposition', 'putObject_contentDisposition' - Specifies presentational information for the object. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1>.
--
-- 'contentEncoding', 'putObject_contentEncoding' - Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field. For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11>.
--
-- 'contentLanguage', 'putObject_contentLanguage' - The language the content is in.
--
-- 'contentLength', 'putObject_contentLength' - Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically. For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13>.
--
-- 'contentMD5', 'putObject_contentMD5' - The base64-encoded 128-bit MD5 digest of the message (without the
-- headers) according to RFC 1864. This header can be used as a message
-- integrity check to verify that the data is the same data that was
-- originally sent. Although it is optional, we recommend using the
-- Content-MD5 mechanism as an end-to-end integrity check. For more
-- information about REST request authentication, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication>.
--
-- 'contentType', 'putObject_contentType' - A standard MIME type describing the format of the contents. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17>.
--
-- 'expectedBucketOwner', 'putObject_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'expires', 'putObject_expires' - The date and time at which the object is no longer cacheable. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21>.
--
-- 'grantFullControl', 'putObject_grantFullControl' - Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'grantRead', 'putObject_grantRead' - Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'grantReadACP', 'putObject_grantReadACP' - Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'grantWriteACP', 'putObject_grantWriteACP' - Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'metadata', 'putObject_metadata' - A map of metadata to store with the object in S3.
--
-- 'objectLockLegalHoldStatus', 'putObject_objectLockLegalHoldStatus' - Specifies whether a legal hold will be applied to this object. For more
-- information about S3 Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
--
-- 'objectLockMode', 'putObject_objectLockMode' - The Object Lock mode that you want to apply to this object.
--
-- 'objectLockRetainUntilDate', 'putObject_objectLockRetainUntilDate' - The date and time when you want this object\'s Object Lock to expire.
-- Must be formatted as a timestamp parameter.
--
-- 'requestPayer', 'putObject_requestPayer' - Undocumented member.
--
-- 'sSECustomerAlgorithm', 'putObject_sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
--
-- 'sSECustomerKey', 'putObject_sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- 'sSECustomerKeyMD5', 'putObject_sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'sSEKMSEncryptionContext', 'putObject_sSEKMSEncryptionContext' - Specifies the Amazon Web Services KMS Encryption Context to use for
-- object encryption. The value of this header is a base64-encoded UTF-8
-- string holding JSON with the encryption context key-value pairs.
--
-- 'sSEKMSKeyId', 'putObject_sSEKMSKeyId' - If @x-amz-server-side-encryption@ is present and has the value of
-- @aws:kms@, this header specifies the ID of the Amazon Web Services Key
-- Management Service (Amazon Web Services KMS) symmetrical customer
-- managed key that was used for the object. If you specify
-- @x-amz-server-side-encryption:aws:kms@, but do not
-- provide@ x-amz-server-side-encryption-aws-kms-key-id@, Amazon S3 uses
-- the Amazon Web Services managed key to protect the data. If the KMS key
-- does not exist in the same account issuing the command, you must use the
-- full ARN and not just the ID.
--
-- 'serverSideEncryption', 'putObject_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
--
-- 'storageClass', 'putObject_storageClass' - By default, Amazon S3 uses the STANDARD Storage Class to store newly
-- created objects. The STANDARD storage class provides high durability and
-- high availability. Depending on performance needs, you can specify a
-- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
-- Storage Class. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 User Guide/.
--
-- 'tagging', 'putObject_tagging' - The tag-set for the object. The tag-set must be encoded as URL Query
-- parameters. (For example, \"Key1=Value1\")
--
-- 'websiteRedirectLocation', 'putObject_websiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata. For
-- information about object metadata, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html Object Key and Metadata>.
--
-- In the following example, the request header sets the redirect to an
-- object (anotherPage.html) in the same bucket:
--
-- @x-amz-website-redirect-location: \/anotherPage.html@
--
-- In the following example, the request header sets the object redirect to
-- another website:
--
-- @x-amz-website-redirect-location: http:\/\/www.example.com\/@
--
-- For more information about website hosting in Amazon S3, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html How to Configure Website Page Redirects>.
--
-- 'bucket', 'putObject_bucket' - The bucket name to which the PUT action was initiated.
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
-- 'key', 'putObject_key' - Object key for which the PUT action was initiated.
--
-- 'body', 'putObject_body' - Object data.
newPutObject ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  -- | 'body'
  Data.RequestBody ->
  PutObject
newPutObject pBucket_ pKey_ pBody_ =
  PutObject'
    { acl = Prelude.Nothing,
      bucketKeyEnabled = Prelude.Nothing,
      cacheControl = Prelude.Nothing,
      checksumAlgorithm = Prelude.Nothing,
      checksumCRC32 = Prelude.Nothing,
      checksumCRC32C = Prelude.Nothing,
      checksumSHA1 = Prelude.Nothing,
      checksumSHA256 = Prelude.Nothing,
      contentDisposition = Prelude.Nothing,
      contentEncoding = Prelude.Nothing,
      contentLanguage = Prelude.Nothing,
      contentLength = Prelude.Nothing,
      contentMD5 = Prelude.Nothing,
      contentType = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      expires = Prelude.Nothing,
      grantFullControl = Prelude.Nothing,
      grantRead = Prelude.Nothing,
      grantReadACP = Prelude.Nothing,
      grantWriteACP = Prelude.Nothing,
      metadata = Prelude.mempty,
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
      websiteRedirectLocation = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_,
      body = pBody_
    }

-- | The canned ACL to apply to the object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
--
-- This action is not supported by Amazon S3 on Outposts.
putObject_acl :: Lens.Lens' PutObject (Prelude.Maybe ObjectCannedACL)
putObject_acl = Lens.lens (\PutObject' {acl} -> acl) (\s@PutObject' {} a -> s {acl = a} :: PutObject)

-- | Specifies whether Amazon S3 should use an S3 Bucket Key for object
-- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
-- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
-- object encryption with SSE-KMS.
--
-- Specifying this header with a PUT action doesn’t affect bucket-level
-- settings for S3 Bucket Key.
putObject_bucketKeyEnabled :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Bool)
putObject_bucketKeyEnabled = Lens.lens (\PutObject' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@PutObject' {} a -> s {bucketKeyEnabled = a} :: PutObject)

-- | Can be used to specify caching behavior along the request\/reply chain.
-- For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
putObject_cacheControl :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_cacheControl = Lens.lens (\PutObject' {cacheControl} -> cacheControl) (\s@PutObject' {} a -> s {cacheControl = a} :: PutObject)

-- | Indicates the algorithm used to create the checksum for the object when
-- using the SDK. This header will not provide any additional functionality
-- if not using the SDK. When sending this header, there must be a
-- corresponding @x-amz-checksum@ or @x-amz-trailer@ header sent.
-- Otherwise, Amazon S3 fails the request with the HTTP status code
-- @400 Bad Request@. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- If you provide an individual checksum, Amazon S3 ignores any provided
-- @ChecksumAlgorithm@ parameter.
putObject_checksumAlgorithm :: Lens.Lens' PutObject (Prelude.Maybe ChecksumAlgorithm)
putObject_checksumAlgorithm = Lens.lens (\PutObject' {checksumAlgorithm} -> checksumAlgorithm) (\s@PutObject' {} a -> s {checksumAlgorithm = a} :: PutObject)

-- | This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 32-bit CRC32 checksum of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
putObject_checksumCRC32 :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_checksumCRC32 = Lens.lens (\PutObject' {checksumCRC32} -> checksumCRC32) (\s@PutObject' {} a -> s {checksumCRC32 = a} :: PutObject)

-- | This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 32-bit CRC32C checksum of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
putObject_checksumCRC32C :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_checksumCRC32C = Lens.lens (\PutObject' {checksumCRC32C} -> checksumCRC32C) (\s@PutObject' {} a -> s {checksumCRC32C = a} :: PutObject)

-- | This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 160-bit SHA-1 digest of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
putObject_checksumSHA1 :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_checksumSHA1 = Lens.lens (\PutObject' {checksumSHA1} -> checksumSHA1) (\s@PutObject' {} a -> s {checksumSHA1 = a} :: PutObject)

-- | This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 256-bit SHA-256 digest of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
putObject_checksumSHA256 :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_checksumSHA256 = Lens.lens (\PutObject' {checksumSHA256} -> checksumSHA256) (\s@PutObject' {} a -> s {checksumSHA256 = a} :: PutObject)

-- | Specifies presentational information for the object. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1>.
putObject_contentDisposition :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_contentDisposition = Lens.lens (\PutObject' {contentDisposition} -> contentDisposition) (\s@PutObject' {} a -> s {contentDisposition = a} :: PutObject)

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field. For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11>.
putObject_contentEncoding :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_contentEncoding = Lens.lens (\PutObject' {contentEncoding} -> contentEncoding) (\s@PutObject' {} a -> s {contentEncoding = a} :: PutObject)

-- | The language the content is in.
putObject_contentLanguage :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_contentLanguage = Lens.lens (\PutObject' {contentLanguage} -> contentLanguage) (\s@PutObject' {} a -> s {contentLanguage = a} :: PutObject)

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically. For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13>.
putObject_contentLength :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Integer)
putObject_contentLength = Lens.lens (\PutObject' {contentLength} -> contentLength) (\s@PutObject' {} a -> s {contentLength = a} :: PutObject)

-- | The base64-encoded 128-bit MD5 digest of the message (without the
-- headers) according to RFC 1864. This header can be used as a message
-- integrity check to verify that the data is the same data that was
-- originally sent. Although it is optional, we recommend using the
-- Content-MD5 mechanism as an end-to-end integrity check. For more
-- information about REST request authentication, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication>.
putObject_contentMD5 :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_contentMD5 = Lens.lens (\PutObject' {contentMD5} -> contentMD5) (\s@PutObject' {} a -> s {contentMD5 = a} :: PutObject)

-- | A standard MIME type describing the format of the contents. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17>.
putObject_contentType :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_contentType = Lens.lens (\PutObject' {contentType} -> contentType) (\s@PutObject' {} a -> s {contentType = a} :: PutObject)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putObject_expectedBucketOwner :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_expectedBucketOwner = Lens.lens (\PutObject' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutObject' {} a -> s {expectedBucketOwner = a} :: PutObject)

-- | The date and time at which the object is no longer cacheable. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21>.
putObject_expires :: Lens.Lens' PutObject (Prelude.Maybe Prelude.UTCTime)
putObject_expires = Lens.lens (\PutObject' {expires} -> expires) (\s@PutObject' {} a -> s {expires = a} :: PutObject) Prelude.. Lens.mapping Data._Time

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
--
-- This action is not supported by Amazon S3 on Outposts.
putObject_grantFullControl :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_grantFullControl = Lens.lens (\PutObject' {grantFullControl} -> grantFullControl) (\s@PutObject' {} a -> s {grantFullControl = a} :: PutObject)

-- | Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
putObject_grantRead :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_grantRead = Lens.lens (\PutObject' {grantRead} -> grantRead) (\s@PutObject' {} a -> s {grantRead = a} :: PutObject)

-- | Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
putObject_grantReadACP :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_grantReadACP = Lens.lens (\PutObject' {grantReadACP} -> grantReadACP) (\s@PutObject' {} a -> s {grantReadACP = a} :: PutObject)

-- | Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
putObject_grantWriteACP :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_grantWriteACP = Lens.lens (\PutObject' {grantWriteACP} -> grantWriteACP) (\s@PutObject' {} a -> s {grantWriteACP = a} :: PutObject)

-- | A map of metadata to store with the object in S3.
putObject_metadata :: Lens.Lens' PutObject (Prelude.HashMap Prelude.Text Prelude.Text)
putObject_metadata = Lens.lens (\PutObject' {metadata} -> metadata) (\s@PutObject' {} a -> s {metadata = a} :: PutObject) Prelude.. Lens.coerced

-- | Specifies whether a legal hold will be applied to this object. For more
-- information about S3 Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
putObject_objectLockLegalHoldStatus :: Lens.Lens' PutObject (Prelude.Maybe ObjectLockLegalHoldStatus)
putObject_objectLockLegalHoldStatus = Lens.lens (\PutObject' {objectLockLegalHoldStatus} -> objectLockLegalHoldStatus) (\s@PutObject' {} a -> s {objectLockLegalHoldStatus = a} :: PutObject)

-- | The Object Lock mode that you want to apply to this object.
putObject_objectLockMode :: Lens.Lens' PutObject (Prelude.Maybe ObjectLockMode)
putObject_objectLockMode = Lens.lens (\PutObject' {objectLockMode} -> objectLockMode) (\s@PutObject' {} a -> s {objectLockMode = a} :: PutObject)

-- | The date and time when you want this object\'s Object Lock to expire.
-- Must be formatted as a timestamp parameter.
putObject_objectLockRetainUntilDate :: Lens.Lens' PutObject (Prelude.Maybe Prelude.UTCTime)
putObject_objectLockRetainUntilDate = Lens.lens (\PutObject' {objectLockRetainUntilDate} -> objectLockRetainUntilDate) (\s@PutObject' {} a -> s {objectLockRetainUntilDate = a} :: PutObject) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
putObject_requestPayer :: Lens.Lens' PutObject (Prelude.Maybe RequestPayer)
putObject_requestPayer = Lens.lens (\PutObject' {requestPayer} -> requestPayer) (\s@PutObject' {} a -> s {requestPayer = a} :: PutObject)

-- | Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
putObject_sSECustomerAlgorithm :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_sSECustomerAlgorithm = Lens.lens (\PutObject' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@PutObject' {} a -> s {sSECustomerAlgorithm = a} :: PutObject)

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
putObject_sSECustomerKey :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_sSECustomerKey = Lens.lens (\PutObject' {sSECustomerKey} -> sSECustomerKey) (\s@PutObject' {} a -> s {sSECustomerKey = a} :: PutObject) Prelude.. Lens.mapping Data._Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
putObject_sSECustomerKeyMD5 :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_sSECustomerKeyMD5 = Lens.lens (\PutObject' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@PutObject' {} a -> s {sSECustomerKeyMD5 = a} :: PutObject)

-- | Specifies the Amazon Web Services KMS Encryption Context to use for
-- object encryption. The value of this header is a base64-encoded UTF-8
-- string holding JSON with the encryption context key-value pairs.
putObject_sSEKMSEncryptionContext :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_sSEKMSEncryptionContext = Lens.lens (\PutObject' {sSEKMSEncryptionContext} -> sSEKMSEncryptionContext) (\s@PutObject' {} a -> s {sSEKMSEncryptionContext = a} :: PutObject) Prelude.. Lens.mapping Data._Sensitive

-- | If @x-amz-server-side-encryption@ is present and has the value of
-- @aws:kms@, this header specifies the ID of the Amazon Web Services Key
-- Management Service (Amazon Web Services KMS) symmetrical customer
-- managed key that was used for the object. If you specify
-- @x-amz-server-side-encryption:aws:kms@, but do not
-- provide@ x-amz-server-side-encryption-aws-kms-key-id@, Amazon S3 uses
-- the Amazon Web Services managed key to protect the data. If the KMS key
-- does not exist in the same account issuing the command, you must use the
-- full ARN and not just the ID.
putObject_sSEKMSKeyId :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_sSEKMSKeyId = Lens.lens (\PutObject' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@PutObject' {} a -> s {sSEKMSKeyId = a} :: PutObject) Prelude.. Lens.mapping Data._Sensitive

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
putObject_serverSideEncryption :: Lens.Lens' PutObject (Prelude.Maybe ServerSideEncryption)
putObject_serverSideEncryption = Lens.lens (\PutObject' {serverSideEncryption} -> serverSideEncryption) (\s@PutObject' {} a -> s {serverSideEncryption = a} :: PutObject)

-- | By default, Amazon S3 uses the STANDARD Storage Class to store newly
-- created objects. The STANDARD storage class provides high durability and
-- high availability. Depending on performance needs, you can specify a
-- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
-- Storage Class. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 User Guide/.
putObject_storageClass :: Lens.Lens' PutObject (Prelude.Maybe StorageClass)
putObject_storageClass = Lens.lens (\PutObject' {storageClass} -> storageClass) (\s@PutObject' {} a -> s {storageClass = a} :: PutObject)

-- | The tag-set for the object. The tag-set must be encoded as URL Query
-- parameters. (For example, \"Key1=Value1\")
putObject_tagging :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_tagging = Lens.lens (\PutObject' {tagging} -> tagging) (\s@PutObject' {} a -> s {tagging = a} :: PutObject)

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata. For
-- information about object metadata, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html Object Key and Metadata>.
--
-- In the following example, the request header sets the redirect to an
-- object (anotherPage.html) in the same bucket:
--
-- @x-amz-website-redirect-location: \/anotherPage.html@
--
-- In the following example, the request header sets the object redirect to
-- another website:
--
-- @x-amz-website-redirect-location: http:\/\/www.example.com\/@
--
-- For more information about website hosting in Amazon S3, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html How to Configure Website Page Redirects>.
putObject_websiteRedirectLocation :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_websiteRedirectLocation = Lens.lens (\PutObject' {websiteRedirectLocation} -> websiteRedirectLocation) (\s@PutObject' {} a -> s {websiteRedirectLocation = a} :: PutObject)

-- | The bucket name to which the PUT action was initiated.
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
putObject_bucket :: Lens.Lens' PutObject BucketName
putObject_bucket = Lens.lens (\PutObject' {bucket} -> bucket) (\s@PutObject' {} a -> s {bucket = a} :: PutObject)

-- | Object key for which the PUT action was initiated.
putObject_key :: Lens.Lens' PutObject ObjectKey
putObject_key = Lens.lens (\PutObject' {key} -> key) (\s@PutObject' {} a -> s {key = a} :: PutObject)

-- | Object data.
putObject_body :: Lens.Lens' PutObject Data.RequestBody
putObject_body = Lens.lens (\PutObject' {body} -> body) (\s@PutObject' {} a -> s {body = a} :: PutObject)

instance Core.AWSRequest PutObject where
  type AWSResponse PutObject = PutObjectResponse
  request overrides =
    Request.expectHeader
      Prelude.. Request.s3vhost
      Prelude.. Request.putBody (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutObjectResponse'
            Prelude.<$> ( h
                            Data..#? "x-amz-server-side-encryption-bucket-key-enabled"
                        )
            Prelude.<*> (h Data..#? "x-amz-checksum-crc32")
            Prelude.<*> (h Data..#? "x-amz-checksum-crc32c")
            Prelude.<*> (h Data..#? "x-amz-checksum-sha1")
            Prelude.<*> (h Data..#? "x-amz-checksum-sha256")
            Prelude.<*> (h Data..#? "ETag")
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

instance Data.ToBody PutObject where
  toBody PutObject' {..} = Data.toBody body

instance Data.ToHeaders PutObject where
  toHeaders PutObject' {..} =
    Prelude.mconcat
      [ "x-amz-acl" Data.=# acl,
        "x-amz-server-side-encryption-bucket-key-enabled"
          Data.=# bucketKeyEnabled,
        "Cache-Control" Data.=# cacheControl,
        "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "x-amz-checksum-crc32" Data.=# checksumCRC32,
        "x-amz-checksum-crc32c" Data.=# checksumCRC32C,
        "x-amz-checksum-sha1" Data.=# checksumSHA1,
        "x-amz-checksum-sha256" Data.=# checksumSHA256,
        "Content-Disposition" Data.=# contentDisposition,
        "Content-Encoding" Data.=# contentEncoding,
        "Content-Language" Data.=# contentLanguage,
        "Content-Length" Data.=# contentLength,
        "Content-MD5" Data.=# contentMD5,
        "Content-Type" Data.=# contentType,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "Expires" Data.=# expires,
        "x-amz-grant-full-control" Data.=# grantFullControl,
        "x-amz-grant-read" Data.=# grantRead,
        "x-amz-grant-read-acp" Data.=# grantReadACP,
        "x-amz-grant-write-acp" Data.=# grantWriteACP,
        "x-amz-meta-" Data.=# metadata,
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
        "x-amz-website-redirect-location"
          Data.=# websiteRedirectLocation
      ]

instance Data.ToPath PutObject where
  toPath PutObject' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery PutObject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutObjectResponse' smart constructor.
data PutObjectResponse = PutObjectResponse'
  { -- | Indicates whether the uploaded object uses an S3 Bucket Key for
    -- server-side encryption with Amazon Web Services KMS (SSE-KMS).
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The base64-encoded, 32-bit CRC32 checksum of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumCRC32 :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded, 32-bit CRC32C checksum of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumCRC32C :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded, 160-bit SHA-1 digest of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumSHA1 :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded, 256-bit SHA-256 digest of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumSHA256 :: Prelude.Maybe Prelude.Text,
    -- | Entity tag for the uploaded object.
    eTag :: Prelude.Maybe ETag,
    -- | If the expiration is configured for the object (see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>),
    -- the response includes this header. It includes the @expiry-date@ and
    -- @rule-id@ key-value pairs that provide information about object
    -- expiration. The value of the @rule-id@ is URL-encoded.
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
    -- | If @x-amz-server-side-encryption@ is present and has the value of
    -- @aws:kms@, this header specifies the ID of the Amazon Web Services Key
    -- Management Service (Amazon Web Services KMS) symmetric customer managed
    -- key that was used for the object.
    sSEKMSKeyId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | If you specified server-side encryption either with an Amazon Web
    -- Services KMS key or Amazon S3-managed encryption key in your PUT
    -- request, the response includes this header. It confirms the encryption
    -- algorithm that Amazon S3 used to encrypt the object.
    serverSideEncryption :: Prelude.Maybe ServerSideEncryption,
    -- | Version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketKeyEnabled', 'putObjectResponse_bucketKeyEnabled' - Indicates whether the uploaded object uses an S3 Bucket Key for
-- server-side encryption with Amazon Web Services KMS (SSE-KMS).
--
-- 'checksumCRC32', 'putObjectResponse_checksumCRC32' - The base64-encoded, 32-bit CRC32 checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumCRC32C', 'putObjectResponse_checksumCRC32C' - The base64-encoded, 32-bit CRC32C checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumSHA1', 'putObjectResponse_checksumSHA1' - The base64-encoded, 160-bit SHA-1 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumSHA256', 'putObjectResponse_checksumSHA256' - The base64-encoded, 256-bit SHA-256 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'eTag', 'putObjectResponse_eTag' - Entity tag for the uploaded object.
--
-- 'expiration', 'putObjectResponse_expiration' - If the expiration is configured for the object (see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>),
-- the response includes this header. It includes the @expiry-date@ and
-- @rule-id@ key-value pairs that provide information about object
-- expiration. The value of the @rule-id@ is URL-encoded.
--
-- 'requestCharged', 'putObjectResponse_requestCharged' - Undocumented member.
--
-- 'sSECustomerAlgorithm', 'putObjectResponse_sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
--
-- 'sSECustomerKeyMD5', 'putObjectResponse_sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
--
-- 'sSEKMSEncryptionContext', 'putObjectResponse_sSEKMSEncryptionContext' - If present, specifies the Amazon Web Services KMS Encryption Context to
-- use for object encryption. The value of this header is a base64-encoded
-- UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- 'sSEKMSKeyId', 'putObjectResponse_sSEKMSKeyId' - If @x-amz-server-side-encryption@ is present and has the value of
-- @aws:kms@, this header specifies the ID of the Amazon Web Services Key
-- Management Service (Amazon Web Services KMS) symmetric customer managed
-- key that was used for the object.
--
-- 'serverSideEncryption', 'putObjectResponse_serverSideEncryption' - If you specified server-side encryption either with an Amazon Web
-- Services KMS key or Amazon S3-managed encryption key in your PUT
-- request, the response includes this header. It confirms the encryption
-- algorithm that Amazon S3 used to encrypt the object.
--
-- 'versionId', 'putObjectResponse_versionId' - Version of the object.
--
-- 'httpStatus', 'putObjectResponse_httpStatus' - The response's http status code.
newPutObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutObjectResponse
newPutObjectResponse pHttpStatus_ =
  PutObjectResponse'
    { bucketKeyEnabled =
        Prelude.Nothing,
      checksumCRC32 = Prelude.Nothing,
      checksumCRC32C = Prelude.Nothing,
      checksumSHA1 = Prelude.Nothing,
      checksumSHA256 = Prelude.Nothing,
      eTag = Prelude.Nothing,
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

-- | Indicates whether the uploaded object uses an S3 Bucket Key for
-- server-side encryption with Amazon Web Services KMS (SSE-KMS).
putObjectResponse_bucketKeyEnabled :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Bool)
putObjectResponse_bucketKeyEnabled = Lens.lens (\PutObjectResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@PutObjectResponse' {} a -> s {bucketKeyEnabled = a} :: PutObjectResponse)

-- | The base64-encoded, 32-bit CRC32 checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
putObjectResponse_checksumCRC32 :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_checksumCRC32 = Lens.lens (\PutObjectResponse' {checksumCRC32} -> checksumCRC32) (\s@PutObjectResponse' {} a -> s {checksumCRC32 = a} :: PutObjectResponse)

-- | The base64-encoded, 32-bit CRC32C checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
putObjectResponse_checksumCRC32C :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_checksumCRC32C = Lens.lens (\PutObjectResponse' {checksumCRC32C} -> checksumCRC32C) (\s@PutObjectResponse' {} a -> s {checksumCRC32C = a} :: PutObjectResponse)

-- | The base64-encoded, 160-bit SHA-1 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
putObjectResponse_checksumSHA1 :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_checksumSHA1 = Lens.lens (\PutObjectResponse' {checksumSHA1} -> checksumSHA1) (\s@PutObjectResponse' {} a -> s {checksumSHA1 = a} :: PutObjectResponse)

-- | The base64-encoded, 256-bit SHA-256 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
putObjectResponse_checksumSHA256 :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_checksumSHA256 = Lens.lens (\PutObjectResponse' {checksumSHA256} -> checksumSHA256) (\s@PutObjectResponse' {} a -> s {checksumSHA256 = a} :: PutObjectResponse)

-- | Entity tag for the uploaded object.
putObjectResponse_eTag :: Lens.Lens' PutObjectResponse (Prelude.Maybe ETag)
putObjectResponse_eTag = Lens.lens (\PutObjectResponse' {eTag} -> eTag) (\s@PutObjectResponse' {} a -> s {eTag = a} :: PutObjectResponse)

-- | If the expiration is configured for the object (see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>),
-- the response includes this header. It includes the @expiry-date@ and
-- @rule-id@ key-value pairs that provide information about object
-- expiration. The value of the @rule-id@ is URL-encoded.
putObjectResponse_expiration :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_expiration = Lens.lens (\PutObjectResponse' {expiration} -> expiration) (\s@PutObjectResponse' {} a -> s {expiration = a} :: PutObjectResponse)

-- | Undocumented member.
putObjectResponse_requestCharged :: Lens.Lens' PutObjectResponse (Prelude.Maybe RequestCharged)
putObjectResponse_requestCharged = Lens.lens (\PutObjectResponse' {requestCharged} -> requestCharged) (\s@PutObjectResponse' {} a -> s {requestCharged = a} :: PutObjectResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
putObjectResponse_sSECustomerAlgorithm :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_sSECustomerAlgorithm = Lens.lens (\PutObjectResponse' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@PutObjectResponse' {} a -> s {sSECustomerAlgorithm = a} :: PutObjectResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
putObjectResponse_sSECustomerKeyMD5 :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_sSECustomerKeyMD5 = Lens.lens (\PutObjectResponse' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@PutObjectResponse' {} a -> s {sSECustomerKeyMD5 = a} :: PutObjectResponse)

-- | If present, specifies the Amazon Web Services KMS Encryption Context to
-- use for object encryption. The value of this header is a base64-encoded
-- UTF-8 string holding JSON with the encryption context key-value pairs.
putObjectResponse_sSEKMSEncryptionContext :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_sSEKMSEncryptionContext = Lens.lens (\PutObjectResponse' {sSEKMSEncryptionContext} -> sSEKMSEncryptionContext) (\s@PutObjectResponse' {} a -> s {sSEKMSEncryptionContext = a} :: PutObjectResponse) Prelude.. Lens.mapping Data._Sensitive

-- | If @x-amz-server-side-encryption@ is present and has the value of
-- @aws:kms@, this header specifies the ID of the Amazon Web Services Key
-- Management Service (Amazon Web Services KMS) symmetric customer managed
-- key that was used for the object.
putObjectResponse_sSEKMSKeyId :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_sSEKMSKeyId = Lens.lens (\PutObjectResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@PutObjectResponse' {} a -> s {sSEKMSKeyId = a} :: PutObjectResponse) Prelude.. Lens.mapping Data._Sensitive

-- | If you specified server-side encryption either with an Amazon Web
-- Services KMS key or Amazon S3-managed encryption key in your PUT
-- request, the response includes this header. It confirms the encryption
-- algorithm that Amazon S3 used to encrypt the object.
putObjectResponse_serverSideEncryption :: Lens.Lens' PutObjectResponse (Prelude.Maybe ServerSideEncryption)
putObjectResponse_serverSideEncryption = Lens.lens (\PutObjectResponse' {serverSideEncryption} -> serverSideEncryption) (\s@PutObjectResponse' {} a -> s {serverSideEncryption = a} :: PutObjectResponse)

-- | Version of the object.
putObjectResponse_versionId :: Lens.Lens' PutObjectResponse (Prelude.Maybe ObjectVersionId)
putObjectResponse_versionId = Lens.lens (\PutObjectResponse' {versionId} -> versionId) (\s@PutObjectResponse' {} a -> s {versionId = a} :: PutObjectResponse)

-- | The response's http status code.
putObjectResponse_httpStatus :: Lens.Lens' PutObjectResponse Prelude.Int
putObjectResponse_httpStatus = Lens.lens (\PutObjectResponse' {httpStatus} -> httpStatus) (\s@PutObjectResponse' {} a -> s {httpStatus = a} :: PutObjectResponse)

instance Prelude.NFData PutObjectResponse where
  rnf PutObjectResponse' {..} =
    Prelude.rnf bucketKeyEnabled
      `Prelude.seq` Prelude.rnf checksumCRC32
      `Prelude.seq` Prelude.rnf checksumCRC32C
      `Prelude.seq` Prelude.rnf checksumSHA1
      `Prelude.seq` Prelude.rnf checksumSHA256
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf requestCharged
      `Prelude.seq` Prelude.rnf sSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf sSECustomerKeyMD5
      `Prelude.seq` Prelude.rnf sSEKMSEncryptionContext
      `Prelude.seq` Prelude.rnf sSEKMSKeyId
      `Prelude.seq` Prelude.rnf serverSideEncryption
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf httpStatus
