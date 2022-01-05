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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    putObject_contentLength,
    putObject_objectLockMode,
    putObject_expires,
    putObject_grantReadACP,
    putObject_sSECustomerAlgorithm,
    putObject_sSECustomerKey,
    putObject_requestPayer,
    putObject_grantWriteACP,
    putObject_bucketKeyEnabled,
    putObject_websiteRedirectLocation,
    putObject_grantRead,
    putObject_storageClass,
    putObject_sSECustomerKeyMD5,
    putObject_sSEKMSKeyId,
    putObject_grantFullControl,
    putObject_contentEncoding,
    putObject_tagging,
    putObject_contentMD5,
    putObject_objectLockRetainUntilDate,
    putObject_metadata,
    putObject_sSEKMSEncryptionContext,
    putObject_cacheControl,
    putObject_contentLanguage,
    putObject_objectLockLegalHoldStatus,
    putObject_acl,
    putObject_contentDisposition,
    putObject_expectedBucketOwner,
    putObject_serverSideEncryption,
    putObject_contentType,
    putObject_bucket,
    putObject_key,
    putObject_body,

    -- * Destructuring the Response
    PutObjectResponse (..),
    newPutObjectResponse,

    -- * Response Lenses
    putObjectResponse_requestCharged,
    putObjectResponse_eTag,
    putObjectResponse_versionId,
    putObjectResponse_expiration,
    putObjectResponse_sSECustomerAlgorithm,
    putObjectResponse_bucketKeyEnabled,
    putObjectResponse_sSECustomerKeyMD5,
    putObjectResponse_sSEKMSKeyId,
    putObjectResponse_sSEKMSEncryptionContext,
    putObjectResponse_serverSideEncryption,
    putObjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutObject' smart constructor.
data PutObject = PutObject'
  { -- | Size of the body in bytes. This parameter is useful when the size of the
    -- body cannot be determined automatically. For more information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13>.
    contentLength :: Prelude.Maybe Prelude.Integer,
    -- | The Object Lock mode that you want to apply to this object.
    objectLockMode :: Prelude.Maybe ObjectLockMode,
    -- | The date and time at which the object is no longer cacheable. For more
    -- information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21>.
    expires :: Prelude.Maybe Core.ISO8601,
    -- | Allows grantee to read the object ACL.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantReadACP :: Prelude.Maybe Prelude.Text,
    -- | Specifies the algorithm to use to when encrypting the object (for
    -- example, AES256).
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use in
    -- encrypting data. This value is used to store the object and then it is
    -- discarded; Amazon S3 does not store the encryption key. The key must be
    -- appropriate for use with the algorithm specified in the
    -- @x-amz-server-side-encryption-customer-algorithm@ header.
    sSECustomerKey :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | Allows grantee to write the ACL for the applicable object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantWriteACP :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether Amazon S3 should use an S3 Bucket Key for object
    -- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
    -- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
    -- object encryption with SSE-KMS.
    --
    -- Specifying this header with a PUT action doesn’t affect bucket-level
    -- settings for S3 Bucket Key.
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
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
    -- | Allows grantee to read the object data and its metadata.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantRead :: Prelude.Maybe Prelude.Text,
    -- | By default, Amazon S3 uses the STANDARD Storage Class to store newly
    -- created objects. The STANDARD storage class provides high durability and
    -- high availability. Depending on performance needs, you can specify a
    -- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
    -- Storage Class. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
    -- in the /Amazon S3 User Guide/.
    storageClass :: Prelude.Maybe StorageClass,
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
    -- 1321. Amazon S3 uses this header for a message integrity check to ensure
    -- that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | If @x-amz-server-side-encryption@ is present and has the value of
    -- @aws:kms@, this header specifies the ID of the Amazon Web Services Key
    -- Management Service (Amazon Web Services KMS) symmetrical customer
    -- managed key that was used for the object. If you specify
    -- @x-amz-server-side-encryption:aws:kms@, but do not
    -- provide@ x-amz-server-side-encryption-aws-kms-key-id@, Amazon S3 uses
    -- the Amazon Web Services managed key to protect the data. If the KMS key
    -- does not exist in the same account issuing the command, you must use the
    -- full ARN and not just the ID.
    sSEKMSKeyId :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
    -- object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantFullControl :: Prelude.Maybe Prelude.Text,
    -- | Specifies what content encodings have been applied to the object and
    -- thus what decoding mechanisms must be applied to obtain the media-type
    -- referenced by the Content-Type header field. For more information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11>.
    contentEncoding :: Prelude.Maybe Prelude.Text,
    -- | The tag-set for the object. The tag-set must be encoded as URL Query
    -- parameters. (For example, \"Key1=Value1\")
    tagging :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded 128-bit MD5 digest of the message (without the
    -- headers) according to RFC 1864. This header can be used as a message
    -- integrity check to verify that the data is the same data that was
    -- originally sent. Although it is optional, we recommend using the
    -- Content-MD5 mechanism as an end-to-end integrity check. For more
    -- information about REST request authentication, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication>.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The date and time when you want this object\'s Object Lock to expire.
    -- Must be formatted as a timestamp parameter.
    objectLockRetainUntilDate :: Prelude.Maybe Core.ISO8601,
    -- | A map of metadata to store with the object in S3.
    metadata :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | Specifies the Amazon Web Services KMS Encryption Context to use for
    -- object encryption. The value of this header is a base64-encoded UTF-8
    -- string holding JSON with the encryption context key-value pairs.
    sSEKMSEncryptionContext :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Can be used to specify caching behavior along the request\/reply chain.
    -- For more information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
    cacheControl :: Prelude.Maybe Prelude.Text,
    -- | The language the content is in.
    contentLanguage :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether a legal hold will be applied to this object. For more
    -- information about S3 Object Lock, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
    objectLockLegalHoldStatus :: Prelude.Maybe ObjectLockLegalHoldStatus,
    -- | The canned ACL to apply to the object. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    acl :: Prelude.Maybe ObjectCannedACL,
    -- | Specifies presentational information for the object. For more
    -- information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1>.
    contentDisposition :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The server-side encryption algorithm used when storing this object in
    -- Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Prelude.Maybe ServerSideEncryption,
    -- | A standard MIME type describing the format of the contents. For more
    -- information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17>.
    contentType :: Prelude.Maybe Prelude.Text,
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
    -- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
    -- When using this action using S3 on Outposts through the Amazon Web
    -- Services SDKs, you provide the Outposts bucket ARN in place of the
    -- bucket name. For more information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using S3 on Outposts>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName,
    -- | Object key for which the PUT action was initiated.
    key :: ObjectKey,
    -- | Object data.
    body :: Core.RequestBody
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
-- 'contentLength', 'putObject_contentLength' - Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically. For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13>.
--
-- 'objectLockMode', 'putObject_objectLockMode' - The Object Lock mode that you want to apply to this object.
--
-- 'expires', 'putObject_expires' - The date and time at which the object is no longer cacheable. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21>.
--
-- 'grantReadACP', 'putObject_grantReadACP' - Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
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
-- 'requestPayer', 'putObject_requestPayer' - Undocumented member.
--
-- 'grantWriteACP', 'putObject_grantWriteACP' - Allows grantee to write the ACL for the applicable object.
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
-- 'grantRead', 'putObject_grantRead' - Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'storageClass', 'putObject_storageClass' - By default, Amazon S3 uses the STANDARD Storage Class to store newly
-- created objects. The STANDARD storage class provides high durability and
-- high availability. Depending on performance needs, you can specify a
-- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
-- Storage Class. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 User Guide/.
--
-- 'sSECustomerKeyMD5', 'putObject_sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
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
-- 'grantFullControl', 'putObject_grantFullControl' - Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'contentEncoding', 'putObject_contentEncoding' - Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field. For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11>.
--
-- 'tagging', 'putObject_tagging' - The tag-set for the object. The tag-set must be encoded as URL Query
-- parameters. (For example, \"Key1=Value1\")
--
-- 'contentMD5', 'putObject_contentMD5' - The base64-encoded 128-bit MD5 digest of the message (without the
-- headers) according to RFC 1864. This header can be used as a message
-- integrity check to verify that the data is the same data that was
-- originally sent. Although it is optional, we recommend using the
-- Content-MD5 mechanism as an end-to-end integrity check. For more
-- information about REST request authentication, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication>.
--
-- 'objectLockRetainUntilDate', 'putObject_objectLockRetainUntilDate' - The date and time when you want this object\'s Object Lock to expire.
-- Must be formatted as a timestamp parameter.
--
-- 'metadata', 'putObject_metadata' - A map of metadata to store with the object in S3.
--
-- 'sSEKMSEncryptionContext', 'putObject_sSEKMSEncryptionContext' - Specifies the Amazon Web Services KMS Encryption Context to use for
-- object encryption. The value of this header is a base64-encoded UTF-8
-- string holding JSON with the encryption context key-value pairs.
--
-- 'cacheControl', 'putObject_cacheControl' - Can be used to specify caching behavior along the request\/reply chain.
-- For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
--
-- 'contentLanguage', 'putObject_contentLanguage' - The language the content is in.
--
-- 'objectLockLegalHoldStatus', 'putObject_objectLockLegalHoldStatus' - Specifies whether a legal hold will be applied to this object. For more
-- information about S3 Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
--
-- 'acl', 'putObject_acl' - The canned ACL to apply to the object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'contentDisposition', 'putObject_contentDisposition' - Specifies presentational information for the object. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1>.
--
-- 'expectedBucketOwner', 'putObject_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'serverSideEncryption', 'putObject_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
--
-- 'contentType', 'putObject_contentType' - A standard MIME type describing the format of the contents. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17>.
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
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this action using S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using S3 on Outposts>
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
  Core.RequestBody ->
  PutObject
newPutObject pBucket_ pKey_ pBody_ =
  PutObject'
    { contentLength = Prelude.Nothing,
      objectLockMode = Prelude.Nothing,
      expires = Prelude.Nothing,
      grantReadACP = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      sSECustomerKey = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      grantWriteACP = Prelude.Nothing,
      bucketKeyEnabled = Prelude.Nothing,
      websiteRedirectLocation = Prelude.Nothing,
      grantRead = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      sSEKMSKeyId = Prelude.Nothing,
      grantFullControl = Prelude.Nothing,
      contentEncoding = Prelude.Nothing,
      tagging = Prelude.Nothing,
      contentMD5 = Prelude.Nothing,
      objectLockRetainUntilDate = Prelude.Nothing,
      metadata = Prelude.mempty,
      sSEKMSEncryptionContext = Prelude.Nothing,
      cacheControl = Prelude.Nothing,
      contentLanguage = Prelude.Nothing,
      objectLockLegalHoldStatus = Prelude.Nothing,
      acl = Prelude.Nothing,
      contentDisposition = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      contentType = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_,
      body = pBody_
    }

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically. For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13>.
putObject_contentLength :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Integer)
putObject_contentLength = Lens.lens (\PutObject' {contentLength} -> contentLength) (\s@PutObject' {} a -> s {contentLength = a} :: PutObject)

-- | The Object Lock mode that you want to apply to this object.
putObject_objectLockMode :: Lens.Lens' PutObject (Prelude.Maybe ObjectLockMode)
putObject_objectLockMode = Lens.lens (\PutObject' {objectLockMode} -> objectLockMode) (\s@PutObject' {} a -> s {objectLockMode = a} :: PutObject)

-- | The date and time at which the object is no longer cacheable. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21>.
putObject_expires :: Lens.Lens' PutObject (Prelude.Maybe Prelude.UTCTime)
putObject_expires = Lens.lens (\PutObject' {expires} -> expires) (\s@PutObject' {} a -> s {expires = a} :: PutObject) Prelude.. Lens.mapping Core._Time

-- | Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
putObject_grantReadACP :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_grantReadACP = Lens.lens (\PutObject' {grantReadACP} -> grantReadACP) (\s@PutObject' {} a -> s {grantReadACP = a} :: PutObject)

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
putObject_sSECustomerKey = Lens.lens (\PutObject' {sSECustomerKey} -> sSECustomerKey) (\s@PutObject' {} a -> s {sSECustomerKey = a} :: PutObject) Prelude.. Lens.mapping Core._Sensitive

-- | Undocumented member.
putObject_requestPayer :: Lens.Lens' PutObject (Prelude.Maybe RequestPayer)
putObject_requestPayer = Lens.lens (\PutObject' {requestPayer} -> requestPayer) (\s@PutObject' {} a -> s {requestPayer = a} :: PutObject)

-- | Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
putObject_grantWriteACP :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_grantWriteACP = Lens.lens (\PutObject' {grantWriteACP} -> grantWriteACP) (\s@PutObject' {} a -> s {grantWriteACP = a} :: PutObject)

-- | Specifies whether Amazon S3 should use an S3 Bucket Key for object
-- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
-- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
-- object encryption with SSE-KMS.
--
-- Specifying this header with a PUT action doesn’t affect bucket-level
-- settings for S3 Bucket Key.
putObject_bucketKeyEnabled :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Bool)
putObject_bucketKeyEnabled = Lens.lens (\PutObject' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@PutObject' {} a -> s {bucketKeyEnabled = a} :: PutObject)

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

-- | Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
putObject_grantRead :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_grantRead = Lens.lens (\PutObject' {grantRead} -> grantRead) (\s@PutObject' {} a -> s {grantRead = a} :: PutObject)

-- | By default, Amazon S3 uses the STANDARD Storage Class to store newly
-- created objects. The STANDARD storage class provides high durability and
-- high availability. Depending on performance needs, you can specify a
-- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
-- Storage Class. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 User Guide/.
putObject_storageClass :: Lens.Lens' PutObject (Prelude.Maybe StorageClass)
putObject_storageClass = Lens.lens (\PutObject' {storageClass} -> storageClass) (\s@PutObject' {} a -> s {storageClass = a} :: PutObject)

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
putObject_sSECustomerKeyMD5 :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_sSECustomerKeyMD5 = Lens.lens (\PutObject' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@PutObject' {} a -> s {sSECustomerKeyMD5 = a} :: PutObject)

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
putObject_sSEKMSKeyId = Lens.lens (\PutObject' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@PutObject' {} a -> s {sSEKMSKeyId = a} :: PutObject) Prelude.. Lens.mapping Core._Sensitive

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
--
-- This action is not supported by Amazon S3 on Outposts.
putObject_grantFullControl :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_grantFullControl = Lens.lens (\PutObject' {grantFullControl} -> grantFullControl) (\s@PutObject' {} a -> s {grantFullControl = a} :: PutObject)

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field. For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11>.
putObject_contentEncoding :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_contentEncoding = Lens.lens (\PutObject' {contentEncoding} -> contentEncoding) (\s@PutObject' {} a -> s {contentEncoding = a} :: PutObject)

-- | The tag-set for the object. The tag-set must be encoded as URL Query
-- parameters. (For example, \"Key1=Value1\")
putObject_tagging :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_tagging = Lens.lens (\PutObject' {tagging} -> tagging) (\s@PutObject' {} a -> s {tagging = a} :: PutObject)

-- | The base64-encoded 128-bit MD5 digest of the message (without the
-- headers) according to RFC 1864. This header can be used as a message
-- integrity check to verify that the data is the same data that was
-- originally sent. Although it is optional, we recommend using the
-- Content-MD5 mechanism as an end-to-end integrity check. For more
-- information about REST request authentication, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication>.
putObject_contentMD5 :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_contentMD5 = Lens.lens (\PutObject' {contentMD5} -> contentMD5) (\s@PutObject' {} a -> s {contentMD5 = a} :: PutObject)

-- | The date and time when you want this object\'s Object Lock to expire.
-- Must be formatted as a timestamp parameter.
putObject_objectLockRetainUntilDate :: Lens.Lens' PutObject (Prelude.Maybe Prelude.UTCTime)
putObject_objectLockRetainUntilDate = Lens.lens (\PutObject' {objectLockRetainUntilDate} -> objectLockRetainUntilDate) (\s@PutObject' {} a -> s {objectLockRetainUntilDate = a} :: PutObject) Prelude.. Lens.mapping Core._Time

-- | A map of metadata to store with the object in S3.
putObject_metadata :: Lens.Lens' PutObject (Prelude.HashMap Prelude.Text Prelude.Text)
putObject_metadata = Lens.lens (\PutObject' {metadata} -> metadata) (\s@PutObject' {} a -> s {metadata = a} :: PutObject) Prelude.. Lens.coerced

-- | Specifies the Amazon Web Services KMS Encryption Context to use for
-- object encryption. The value of this header is a base64-encoded UTF-8
-- string holding JSON with the encryption context key-value pairs.
putObject_sSEKMSEncryptionContext :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_sSEKMSEncryptionContext = Lens.lens (\PutObject' {sSEKMSEncryptionContext} -> sSEKMSEncryptionContext) (\s@PutObject' {} a -> s {sSEKMSEncryptionContext = a} :: PutObject) Prelude.. Lens.mapping Core._Sensitive

-- | Can be used to specify caching behavior along the request\/reply chain.
-- For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
putObject_cacheControl :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_cacheControl = Lens.lens (\PutObject' {cacheControl} -> cacheControl) (\s@PutObject' {} a -> s {cacheControl = a} :: PutObject)

-- | The language the content is in.
putObject_contentLanguage :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_contentLanguage = Lens.lens (\PutObject' {contentLanguage} -> contentLanguage) (\s@PutObject' {} a -> s {contentLanguage = a} :: PutObject)

-- | Specifies whether a legal hold will be applied to this object. For more
-- information about S3 Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
putObject_objectLockLegalHoldStatus :: Lens.Lens' PutObject (Prelude.Maybe ObjectLockLegalHoldStatus)
putObject_objectLockLegalHoldStatus = Lens.lens (\PutObject' {objectLockLegalHoldStatus} -> objectLockLegalHoldStatus) (\s@PutObject' {} a -> s {objectLockLegalHoldStatus = a} :: PutObject)

-- | The canned ACL to apply to the object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
--
-- This action is not supported by Amazon S3 on Outposts.
putObject_acl :: Lens.Lens' PutObject (Prelude.Maybe ObjectCannedACL)
putObject_acl = Lens.lens (\PutObject' {acl} -> acl) (\s@PutObject' {} a -> s {acl = a} :: PutObject)

-- | Specifies presentational information for the object. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1>.
putObject_contentDisposition :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_contentDisposition = Lens.lens (\PutObject' {contentDisposition} -> contentDisposition) (\s@PutObject' {} a -> s {contentDisposition = a} :: PutObject)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putObject_expectedBucketOwner :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_expectedBucketOwner = Lens.lens (\PutObject' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutObject' {} a -> s {expectedBucketOwner = a} :: PutObject)

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
putObject_serverSideEncryption :: Lens.Lens' PutObject (Prelude.Maybe ServerSideEncryption)
putObject_serverSideEncryption = Lens.lens (\PutObject' {serverSideEncryption} -> serverSideEncryption) (\s@PutObject' {} a -> s {serverSideEncryption = a} :: PutObject)

-- | A standard MIME type describing the format of the contents. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17>.
putObject_contentType :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_contentType = Lens.lens (\PutObject' {contentType} -> contentType) (\s@PutObject' {} a -> s {contentType = a} :: PutObject)

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
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this action using S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon S3 User Guide/.
putObject_bucket :: Lens.Lens' PutObject BucketName
putObject_bucket = Lens.lens (\PutObject' {bucket} -> bucket) (\s@PutObject' {} a -> s {bucket = a} :: PutObject)

-- | Object key for which the PUT action was initiated.
putObject_key :: Lens.Lens' PutObject ObjectKey
putObject_key = Lens.lens (\PutObject' {key} -> key) (\s@PutObject' {} a -> s {key = a} :: PutObject)

-- | Object data.
putObject_body :: Lens.Lens' PutObject Core.RequestBody
putObject_body = Lens.lens (\PutObject' {body} -> body) (\s@PutObject' {} a -> s {body = a} :: PutObject)

instance Core.AWSRequest PutObject where
  type AWSResponse PutObject = PutObjectResponse
  request =
    Request.expectHeader
      Prelude.. Request.s3vhost
      Prelude.. Request.putBody defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutObjectResponse'
            Prelude.<$> (h Core..#? "x-amz-request-charged")
            Prelude.<*> (h Core..#? "ETag")
            Prelude.<*> (h Core..#? "x-amz-version-id")
            Prelude.<*> (h Core..#? "x-amz-expiration")
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-customer-algorithm"
                        )
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-bucket-key-enabled"
                        )
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-customer-key-MD5"
                        )
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-aws-kms-key-id"
                        )
            Prelude.<*> (h Core..#? "x-amz-server-side-encryption-context")
            Prelude.<*> (h Core..#? "x-amz-server-side-encryption")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Core.ToBody PutObject where
  toBody PutObject' {..} = Core.toBody body

instance Core.ToHeaders PutObject where
  toHeaders PutObject' {..} =
    Prelude.mconcat
      [ "Content-Length" Core.=# contentLength,
        "x-amz-object-lock-mode" Core.=# objectLockMode,
        "Expires" Core.=# expires,
        "x-amz-grant-read-acp" Core.=# grantReadACP,
        "x-amz-server-side-encryption-customer-algorithm"
          Core.=# sSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key"
          Core.=# sSECustomerKey,
        "x-amz-request-payer" Core.=# requestPayer,
        "x-amz-grant-write-acp" Core.=# grantWriteACP,
        "x-amz-server-side-encryption-bucket-key-enabled"
          Core.=# bucketKeyEnabled,
        "x-amz-website-redirect-location"
          Core.=# websiteRedirectLocation,
        "x-amz-grant-read" Core.=# grantRead,
        "x-amz-storage-class" Core.=# storageClass,
        "x-amz-server-side-encryption-customer-key-MD5"
          Core.=# sSECustomerKeyMD5,
        "x-amz-server-side-encryption-aws-kms-key-id"
          Core.=# sSEKMSKeyId,
        "x-amz-grant-full-control" Core.=# grantFullControl,
        "Content-Encoding" Core.=# contentEncoding,
        "x-amz-tagging" Core.=# tagging,
        "Content-MD5" Core.=# contentMD5,
        "x-amz-object-lock-retain-until-date"
          Core.=# objectLockRetainUntilDate,
        "x-amz-meta-" Core.=# metadata,
        "x-amz-server-side-encryption-context"
          Core.=# sSEKMSEncryptionContext,
        "Cache-Control" Core.=# cacheControl,
        "Content-Language" Core.=# contentLanguage,
        "x-amz-object-lock-legal-hold"
          Core.=# objectLockLegalHoldStatus,
        "x-amz-acl" Core.=# acl,
        "Content-Disposition" Core.=# contentDisposition,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "x-amz-server-side-encryption"
          Core.=# serverSideEncryption,
        "Content-Type" Core.=# contentType
      ]

instance Core.ToPath PutObject where
  toPath PutObject' {..} =
    Prelude.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery PutObject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutObjectResponse' smart constructor.
data PutObjectResponse = PutObjectResponse'
  { requestCharged :: Prelude.Maybe RequestCharged,
    -- | Entity tag for the uploaded object.
    eTag :: Prelude.Maybe ETag,
    -- | Version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | If the expiration is configured for the object (see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>),
    -- the response includes this header. It includes the expiry-date and
    -- rule-id key-value pairs that provide information about object
    -- expiration. The value of the rule-id is URL encoded.
    expiration :: Prelude.Maybe Prelude.Text,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header confirming the
    -- encryption algorithm used.
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the uploaded object uses an S3 Bucket Key for
    -- server-side encryption with Amazon Web Services KMS (SSE-KMS).
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header to provide round-trip
    -- message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | If @x-amz-server-side-encryption@ is present and has the value of
    -- @aws:kms@, this header specifies the ID of the Amazon Web Services Key
    -- Management Service (Amazon Web Services KMS) symmetric customer managed
    -- key that was used for the object.
    sSEKMSKeyId :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | If present, specifies the Amazon Web Services KMS Encryption Context to
    -- use for object encryption. The value of this header is a base64-encoded
    -- UTF-8 string holding JSON with the encryption context key-value pairs.
    sSEKMSEncryptionContext :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | If you specified server-side encryption either with an Amazon Web
    -- Services KMS key or Amazon S3-managed encryption key in your PUT
    -- request, the response includes this header. It confirms the encryption
    -- algorithm that Amazon S3 used to encrypt the object.
    serverSideEncryption :: Prelude.Maybe ServerSideEncryption,
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
-- 'requestCharged', 'putObjectResponse_requestCharged' - Undocumented member.
--
-- 'eTag', 'putObjectResponse_eTag' - Entity tag for the uploaded object.
--
-- 'versionId', 'putObjectResponse_versionId' - Version of the object.
--
-- 'expiration', 'putObjectResponse_expiration' - If the expiration is configured for the object (see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>),
-- the response includes this header. It includes the expiry-date and
-- rule-id key-value pairs that provide information about object
-- expiration. The value of the rule-id is URL encoded.
--
-- 'sSECustomerAlgorithm', 'putObjectResponse_sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
--
-- 'bucketKeyEnabled', 'putObjectResponse_bucketKeyEnabled' - Indicates whether the uploaded object uses an S3 Bucket Key for
-- server-side encryption with Amazon Web Services KMS (SSE-KMS).
--
-- 'sSECustomerKeyMD5', 'putObjectResponse_sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
--
-- 'sSEKMSKeyId', 'putObjectResponse_sSEKMSKeyId' - If @x-amz-server-side-encryption@ is present and has the value of
-- @aws:kms@, this header specifies the ID of the Amazon Web Services Key
-- Management Service (Amazon Web Services KMS) symmetric customer managed
-- key that was used for the object.
--
-- 'sSEKMSEncryptionContext', 'putObjectResponse_sSEKMSEncryptionContext' - If present, specifies the Amazon Web Services KMS Encryption Context to
-- use for object encryption. The value of this header is a base64-encoded
-- UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- 'serverSideEncryption', 'putObjectResponse_serverSideEncryption' - If you specified server-side encryption either with an Amazon Web
-- Services KMS key or Amazon S3-managed encryption key in your PUT
-- request, the response includes this header. It confirms the encryption
-- algorithm that Amazon S3 used to encrypt the object.
--
-- 'httpStatus', 'putObjectResponse_httpStatus' - The response's http status code.
newPutObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutObjectResponse
newPutObjectResponse pHttpStatus_ =
  PutObjectResponse'
    { requestCharged =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      versionId = Prelude.Nothing,
      expiration = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      bucketKeyEnabled = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      sSEKMSKeyId = Prelude.Nothing,
      sSEKMSEncryptionContext = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
putObjectResponse_requestCharged :: Lens.Lens' PutObjectResponse (Prelude.Maybe RequestCharged)
putObjectResponse_requestCharged = Lens.lens (\PutObjectResponse' {requestCharged} -> requestCharged) (\s@PutObjectResponse' {} a -> s {requestCharged = a} :: PutObjectResponse)

-- | Entity tag for the uploaded object.
putObjectResponse_eTag :: Lens.Lens' PutObjectResponse (Prelude.Maybe ETag)
putObjectResponse_eTag = Lens.lens (\PutObjectResponse' {eTag} -> eTag) (\s@PutObjectResponse' {} a -> s {eTag = a} :: PutObjectResponse)

-- | Version of the object.
putObjectResponse_versionId :: Lens.Lens' PutObjectResponse (Prelude.Maybe ObjectVersionId)
putObjectResponse_versionId = Lens.lens (\PutObjectResponse' {versionId} -> versionId) (\s@PutObjectResponse' {} a -> s {versionId = a} :: PutObjectResponse)

-- | If the expiration is configured for the object (see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>),
-- the response includes this header. It includes the expiry-date and
-- rule-id key-value pairs that provide information about object
-- expiration. The value of the rule-id is URL encoded.
putObjectResponse_expiration :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_expiration = Lens.lens (\PutObjectResponse' {expiration} -> expiration) (\s@PutObjectResponse' {} a -> s {expiration = a} :: PutObjectResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
putObjectResponse_sSECustomerAlgorithm :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_sSECustomerAlgorithm = Lens.lens (\PutObjectResponse' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@PutObjectResponse' {} a -> s {sSECustomerAlgorithm = a} :: PutObjectResponse)

-- | Indicates whether the uploaded object uses an S3 Bucket Key for
-- server-side encryption with Amazon Web Services KMS (SSE-KMS).
putObjectResponse_bucketKeyEnabled :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Bool)
putObjectResponse_bucketKeyEnabled = Lens.lens (\PutObjectResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@PutObjectResponse' {} a -> s {bucketKeyEnabled = a} :: PutObjectResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
putObjectResponse_sSECustomerKeyMD5 :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_sSECustomerKeyMD5 = Lens.lens (\PutObjectResponse' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@PutObjectResponse' {} a -> s {sSECustomerKeyMD5 = a} :: PutObjectResponse)

-- | If @x-amz-server-side-encryption@ is present and has the value of
-- @aws:kms@, this header specifies the ID of the Amazon Web Services Key
-- Management Service (Amazon Web Services KMS) symmetric customer managed
-- key that was used for the object.
putObjectResponse_sSEKMSKeyId :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_sSEKMSKeyId = Lens.lens (\PutObjectResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@PutObjectResponse' {} a -> s {sSEKMSKeyId = a} :: PutObjectResponse) Prelude.. Lens.mapping Core._Sensitive

-- | If present, specifies the Amazon Web Services KMS Encryption Context to
-- use for object encryption. The value of this header is a base64-encoded
-- UTF-8 string holding JSON with the encryption context key-value pairs.
putObjectResponse_sSEKMSEncryptionContext :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_sSEKMSEncryptionContext = Lens.lens (\PutObjectResponse' {sSEKMSEncryptionContext} -> sSEKMSEncryptionContext) (\s@PutObjectResponse' {} a -> s {sSEKMSEncryptionContext = a} :: PutObjectResponse) Prelude.. Lens.mapping Core._Sensitive

-- | If you specified server-side encryption either with an Amazon Web
-- Services KMS key or Amazon S3-managed encryption key in your PUT
-- request, the response includes this header. It confirms the encryption
-- algorithm that Amazon S3 used to encrypt the object.
putObjectResponse_serverSideEncryption :: Lens.Lens' PutObjectResponse (Prelude.Maybe ServerSideEncryption)
putObjectResponse_serverSideEncryption = Lens.lens (\PutObjectResponse' {serverSideEncryption} -> serverSideEncryption) (\s@PutObjectResponse' {} a -> s {serverSideEncryption = a} :: PutObjectResponse)

-- | The response's http status code.
putObjectResponse_httpStatus :: Lens.Lens' PutObjectResponse Prelude.Int
putObjectResponse_httpStatus = Lens.lens (\PutObjectResponse' {httpStatus} -> httpStatus) (\s@PutObjectResponse' {} a -> s {httpStatus = a} :: PutObjectResponse)

instance Prelude.NFData PutObjectResponse where
  rnf PutObjectResponse' {..} =
    Prelude.rnf requestCharged
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf sSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf bucketKeyEnabled
      `Prelude.seq` Prelude.rnf sSECustomerKeyMD5
      `Prelude.seq` Prelude.rnf sSEKMSKeyId
      `Prelude.seq` Prelude.rnf sSEKMSEncryptionContext
      `Prelude.seq` Prelude.rnf serverSideEncryption
      `Prelude.seq` Prelude.rnf httpStatus
