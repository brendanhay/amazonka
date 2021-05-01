{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.S3.PutObject
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
-- The @Content-MD5@ header is required for any request to upload an object
-- with a retention period configured using Amazon S3 Object Lock. For more
-- information about Amazon S3 Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock-overview.html Amazon S3 Object Lock Overview>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- __Server-side Encryption__
--
-- You can optionally request server-side encryption. With server-side
-- encryption, Amazon S3 encrypts your data as it writes it to disks in its
-- data centers and decrypts the data when you access it. You have the
-- option to provide your own encryption key or use AWS managed encryption
-- keys (SSE-S3 or SSE-KMS). For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Using Server-Side Encryption>.
--
-- If you request server-side encryption using AWS Key Management Service
-- (SSE-KMS), you can enable an S3 Bucket Key at the object-level. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-key.html Amazon S3 Bucket Keys>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- __Access Control List (ACL)-Specific Request Headers__
--
-- You can use headers to grant ACL- based permissions. By default, all
-- objects are private. Only the owner has full access control. When adding
-- a new object, you can grant permissions to individual AWS accounts or to
-- predefined groups defined by Amazon S3. These permissions are then added
-- to the ACL on the object. For more information, see
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
-- in the /Amazon S3 Service Developer Guide/.
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
module Network.AWS.S3.PutObject
  ( -- * Creating a Request
    PutObject (..),
    newPutObject,

    -- * Request Lenses
    putObject_websiteRedirectLocation,
    putObject_grantRead,
    putObject_contentType,
    putObject_expectedBucketOwner,
    putObject_contentDisposition,
    putObject_contentLanguage,
    putObject_sSEKMSEncryptionContext,
    putObject_contentMD5,
    putObject_metadata,
    putObject_contentLength,
    putObject_contentEncoding,
    putObject_sSEKMSKeyId,
    putObject_sSECustomerKeyMD5,
    putObject_storageClass,
    putObject_bucketKeyEnabled,
    putObject_grantWriteACP,
    putObject_serverSideEncryption,
    putObject_objectLockLegalHoldStatus,
    putObject_grantReadACP,
    putObject_acl,
    putObject_sSECustomerAlgorithm,
    putObject_requestPayer,
    putObject_sSECustomerKey,
    putObject_cacheControl,
    putObject_expires,
    putObject_objectLockMode,
    putObject_objectLockRetainUntilDate,
    putObject_tagging,
    putObject_grantFullControl,
    putObject_bucket,
    putObject_key,
    putObject_body,

    -- * Destructuring the Response
    PutObjectResponse (..),
    newPutObjectResponse,

    -- * Response Lenses
    putObjectResponse_eTag,
    putObjectResponse_requestCharged,
    putObjectResponse_expiration,
    putObjectResponse_sSEKMSEncryptionContext,
    putObjectResponse_sSEKMSKeyId,
    putObjectResponse_sSECustomerKeyMD5,
    putObjectResponse_versionId,
    putObjectResponse_bucketKeyEnabled,
    putObjectResponse_serverSideEncryption,
    putObjectResponse_sSECustomerAlgorithm,
    putObjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newPutObject' smart constructor.
data PutObject = PutObject'
  { -- | If the bucket is configured as a website, redirects requests for this
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
    -- | A standard MIME type describing the format of the contents. For more
    -- information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17>.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Specifies presentational information for the object. For more
    -- information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1>.
    contentDisposition :: Prelude.Maybe Prelude.Text,
    -- | The language the content is in.
    contentLanguage :: Prelude.Maybe Prelude.Text,
    -- | Specifies the AWS KMS Encryption Context to use for object encryption.
    -- The value of this header is a base64-encoded UTF-8 string holding JSON
    -- with the encryption context key-value pairs.
    sSEKMSEncryptionContext :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The base64-encoded 128-bit MD5 digest of the message (without the
    -- headers) according to RFC 1864. This header can be used as a message
    -- integrity check to verify that the data is the same data that was
    -- originally sent. Although it is optional, we recommend using the
    -- Content-MD5 mechanism as an end-to-end integrity check. For more
    -- information about REST request authentication, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication>.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | A map of metadata to store with the object in S3.
    metadata :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | Size of the body in bytes. This parameter is useful when the size of the
    -- body cannot be determined automatically. For more information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13>.
    contentLength :: Prelude.Maybe Prelude.Integer,
    -- | Specifies what content encodings have been applied to the object and
    -- thus what decoding mechanisms must be applied to obtain the media-type
    -- referenced by the Content-Type header field. For more information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11>.
    contentEncoding :: Prelude.Maybe Prelude.Text,
    -- | If @x-amz-server-side-encryption@ is present and has the value of
    -- @aws:kms@, this header specifies the ID of the AWS Key Management
    -- Service (AWS KMS) symmetrical customer managed customer master key (CMK)
    -- that was used for the object.
    --
    -- If the value of @x-amz-server-side-encryption@ is @aws:kms@, this header
    -- specifies the ID of the symmetric customer managed AWS KMS CMK that will
    -- be used for the object. If you specify
    -- @x-amz-server-side-encryption:aws:kms@, but do not
    -- provide@ x-amz-server-side-encryption-aws-kms-key-id@, Amazon S3 uses
    -- the AWS managed CMK in AWS to protect the data.
    sSEKMSKeyId :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
    -- 1321. Amazon S3 uses this header for a message integrity check to ensure
    -- that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | By default, Amazon S3 uses the STANDARD Storage Class to store newly
    -- created objects. The STANDARD storage class provides high durability and
    -- high availability. Depending on performance needs, you can specify a
    -- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
    -- Storage Class. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
    -- in the /Amazon S3 Service Developer Guide/.
    storageClass :: Prelude.Maybe StorageClass,
    -- | Specifies whether Amazon S3 should use an S3 Bucket Key for object
    -- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
    -- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
    -- object encryption with SSE-KMS.
    --
    -- Specifying this header with a PUT operation doesn’t affect bucket-level
    -- settings for S3 Bucket Key.
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Allows grantee to write the ACL for the applicable object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantWriteACP :: Prelude.Maybe Prelude.Text,
    -- | The server-side encryption algorithm used when storing this object in
    -- Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Prelude.Maybe ServerSideEncryption,
    -- | Specifies whether a legal hold will be applied to this object. For more
    -- information about S3 Object Lock, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
    objectLockLegalHoldStatus :: Prelude.Maybe ObjectLockLegalHoldStatus,
    -- | Allows grantee to read the object ACL.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantReadACP :: Prelude.Maybe Prelude.Text,
    -- | The canned ACL to apply to the object. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    acl :: Prelude.Maybe ObjectCannedACL,
    -- | Specifies the algorithm to use to when encrypting the object (for
    -- example, AES256).
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use in
    -- encrypting data. This value is used to store the object and then it is
    -- discarded; Amazon S3 does not store the encryption key. The key must be
    -- appropriate for use with the algorithm specified in the
    -- @x-amz-server-side-encryption-customer-algorithm@ header.
    sSECustomerKey :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | Can be used to specify caching behavior along the request\/reply chain.
    -- For more information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
    cacheControl :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the object is no longer cacheable. For more
    -- information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21>.
    expires :: Prelude.Maybe Prelude.ISO8601,
    -- | The Object Lock mode that you want to apply to this object.
    objectLockMode :: Prelude.Maybe ObjectLockMode,
    -- | The date and time when you want this object\'s Object Lock to expire.
    objectLockRetainUntilDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The tag-set for the object. The tag-set must be encoded as URL Query
    -- parameters. (For example, \"Key1=Value1\")
    tagging :: Prelude.Maybe Prelude.Text,
    -- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
    -- object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantFullControl :: Prelude.Maybe Prelude.Text,
    -- | The bucket name to which the PUT operation was initiated.
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
    -- | Object key for which the PUT operation was initiated.
    key :: ObjectKey,
    -- | Object data.
    body :: Prelude.RqBody
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
-- 'contentType', 'putObject_contentType' - A standard MIME type describing the format of the contents. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17>.
--
-- 'expectedBucketOwner', 'putObject_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'contentDisposition', 'putObject_contentDisposition' - Specifies presentational information for the object. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1>.
--
-- 'contentLanguage', 'putObject_contentLanguage' - The language the content is in.
--
-- 'sSEKMSEncryptionContext', 'putObject_sSEKMSEncryptionContext' - Specifies the AWS KMS Encryption Context to use for object encryption.
-- The value of this header is a base64-encoded UTF-8 string holding JSON
-- with the encryption context key-value pairs.
--
-- 'contentMD5', 'putObject_contentMD5' - The base64-encoded 128-bit MD5 digest of the message (without the
-- headers) according to RFC 1864. This header can be used as a message
-- integrity check to verify that the data is the same data that was
-- originally sent. Although it is optional, we recommend using the
-- Content-MD5 mechanism as an end-to-end integrity check. For more
-- information about REST request authentication, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication>.
--
-- 'metadata', 'putObject_metadata' - A map of metadata to store with the object in S3.
--
-- 'contentLength', 'putObject_contentLength' - Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically. For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13>.
--
-- 'contentEncoding', 'putObject_contentEncoding' - Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field. For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11>.
--
-- 'sSEKMSKeyId', 'putObject_sSEKMSKeyId' - If @x-amz-server-side-encryption@ is present and has the value of
-- @aws:kms@, this header specifies the ID of the AWS Key Management
-- Service (AWS KMS) symmetrical customer managed customer master key (CMK)
-- that was used for the object.
--
-- If the value of @x-amz-server-side-encryption@ is @aws:kms@, this header
-- specifies the ID of the symmetric customer managed AWS KMS CMK that will
-- be used for the object. If you specify
-- @x-amz-server-side-encryption:aws:kms@, but do not
-- provide@ x-amz-server-side-encryption-aws-kms-key-id@, Amazon S3 uses
-- the AWS managed CMK in AWS to protect the data.
--
-- 'sSECustomerKeyMD5', 'putObject_sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'storageClass', 'putObject_storageClass' - By default, Amazon S3 uses the STANDARD Storage Class to store newly
-- created objects. The STANDARD storage class provides high durability and
-- high availability. Depending on performance needs, you can specify a
-- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
-- Storage Class. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 Service Developer Guide/.
--
-- 'bucketKeyEnabled', 'putObject_bucketKeyEnabled' - Specifies whether Amazon S3 should use an S3 Bucket Key for object
-- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
-- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
-- object encryption with SSE-KMS.
--
-- Specifying this header with a PUT operation doesn’t affect bucket-level
-- settings for S3 Bucket Key.
--
-- 'grantWriteACP', 'putObject_grantWriteACP' - Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'serverSideEncryption', 'putObject_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
--
-- 'objectLockLegalHoldStatus', 'putObject_objectLockLegalHoldStatus' - Specifies whether a legal hold will be applied to this object. For more
-- information about S3 Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
--
-- 'grantReadACP', 'putObject_grantReadACP' - Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'acl', 'putObject_acl' - The canned ACL to apply to the object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'sSECustomerAlgorithm', 'putObject_sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
--
-- 'requestPayer', 'putObject_requestPayer' - Undocumented member.
--
-- 'sSECustomerKey', 'putObject_sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- 'cacheControl', 'putObject_cacheControl' - Can be used to specify caching behavior along the request\/reply chain.
-- For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
--
-- 'expires', 'putObject_expires' - The date and time at which the object is no longer cacheable. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21>.
--
-- 'objectLockMode', 'putObject_objectLockMode' - The Object Lock mode that you want to apply to this object.
--
-- 'objectLockRetainUntilDate', 'putObject_objectLockRetainUntilDate' - The date and time when you want this object\'s Object Lock to expire.
--
-- 'tagging', 'putObject_tagging' - The tag-set for the object. The tag-set must be encoded as URL Query
-- parameters. (For example, \"Key1=Value1\")
--
-- 'grantFullControl', 'putObject_grantFullControl' - Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'bucket', 'putObject_bucket' - The bucket name to which the PUT operation was initiated.
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
-- 'key', 'putObject_key' - Object key for which the PUT operation was initiated.
--
-- 'body', 'putObject_body' - Object data.
newPutObject ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  -- | 'body'
  Prelude.RqBody ->
  PutObject
newPutObject pBucket_ pKey_ pBody_ =
  PutObject'
    { websiteRedirectLocation =
        Prelude.Nothing,
      grantRead = Prelude.Nothing,
      contentType = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      contentDisposition = Prelude.Nothing,
      contentLanguage = Prelude.Nothing,
      sSEKMSEncryptionContext = Prelude.Nothing,
      contentMD5 = Prelude.Nothing,
      metadata = Prelude.mempty,
      contentLength = Prelude.Nothing,
      contentEncoding = Prelude.Nothing,
      sSEKMSKeyId = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      bucketKeyEnabled = Prelude.Nothing,
      grantWriteACP = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      objectLockLegalHoldStatus = Prelude.Nothing,
      grantReadACP = Prelude.Nothing,
      acl = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      sSECustomerKey = Prelude.Nothing,
      cacheControl = Prelude.Nothing,
      expires = Prelude.Nothing,
      objectLockMode = Prelude.Nothing,
      objectLockRetainUntilDate = Prelude.Nothing,
      tagging = Prelude.Nothing,
      grantFullControl = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_,
      body = pBody_
    }

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

-- | A standard MIME type describing the format of the contents. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17>.
putObject_contentType :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_contentType = Lens.lens (\PutObject' {contentType} -> contentType) (\s@PutObject' {} a -> s {contentType = a} :: PutObject)

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putObject_expectedBucketOwner :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_expectedBucketOwner = Lens.lens (\PutObject' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutObject' {} a -> s {expectedBucketOwner = a} :: PutObject)

-- | Specifies presentational information for the object. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1>.
putObject_contentDisposition :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_contentDisposition = Lens.lens (\PutObject' {contentDisposition} -> contentDisposition) (\s@PutObject' {} a -> s {contentDisposition = a} :: PutObject)

-- | The language the content is in.
putObject_contentLanguage :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_contentLanguage = Lens.lens (\PutObject' {contentLanguage} -> contentLanguage) (\s@PutObject' {} a -> s {contentLanguage = a} :: PutObject)

-- | Specifies the AWS KMS Encryption Context to use for object encryption.
-- The value of this header is a base64-encoded UTF-8 string holding JSON
-- with the encryption context key-value pairs.
putObject_sSEKMSEncryptionContext :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_sSEKMSEncryptionContext = Lens.lens (\PutObject' {sSEKMSEncryptionContext} -> sSEKMSEncryptionContext) (\s@PutObject' {} a -> s {sSEKMSEncryptionContext = a} :: PutObject) Prelude.. Lens.mapping Prelude._Sensitive

-- | The base64-encoded 128-bit MD5 digest of the message (without the
-- headers) according to RFC 1864. This header can be used as a message
-- integrity check to verify that the data is the same data that was
-- originally sent. Although it is optional, we recommend using the
-- Content-MD5 mechanism as an end-to-end integrity check. For more
-- information about REST request authentication, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication>.
putObject_contentMD5 :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_contentMD5 = Lens.lens (\PutObject' {contentMD5} -> contentMD5) (\s@PutObject' {} a -> s {contentMD5 = a} :: PutObject)

-- | A map of metadata to store with the object in S3.
putObject_metadata :: Lens.Lens' PutObject (Prelude.HashMap Prelude.Text Prelude.Text)
putObject_metadata = Lens.lens (\PutObject' {metadata} -> metadata) (\s@PutObject' {} a -> s {metadata = a} :: PutObject) Prelude.. Prelude._Coerce

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically. For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13>.
putObject_contentLength :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Integer)
putObject_contentLength = Lens.lens (\PutObject' {contentLength} -> contentLength) (\s@PutObject' {} a -> s {contentLength = a} :: PutObject)

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field. For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11>.
putObject_contentEncoding :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_contentEncoding = Lens.lens (\PutObject' {contentEncoding} -> contentEncoding) (\s@PutObject' {} a -> s {contentEncoding = a} :: PutObject)

-- | If @x-amz-server-side-encryption@ is present and has the value of
-- @aws:kms@, this header specifies the ID of the AWS Key Management
-- Service (AWS KMS) symmetrical customer managed customer master key (CMK)
-- that was used for the object.
--
-- If the value of @x-amz-server-side-encryption@ is @aws:kms@, this header
-- specifies the ID of the symmetric customer managed AWS KMS CMK that will
-- be used for the object. If you specify
-- @x-amz-server-side-encryption:aws:kms@, but do not
-- provide@ x-amz-server-side-encryption-aws-kms-key-id@, Amazon S3 uses
-- the AWS managed CMK in AWS to protect the data.
putObject_sSEKMSKeyId :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_sSEKMSKeyId = Lens.lens (\PutObject' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@PutObject' {} a -> s {sSEKMSKeyId = a} :: PutObject) Prelude.. Lens.mapping Prelude._Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
putObject_sSECustomerKeyMD5 :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_sSECustomerKeyMD5 = Lens.lens (\PutObject' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@PutObject' {} a -> s {sSECustomerKeyMD5 = a} :: PutObject)

-- | By default, Amazon S3 uses the STANDARD Storage Class to store newly
-- created objects. The STANDARD storage class provides high durability and
-- high availability. Depending on performance needs, you can specify a
-- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
-- Storage Class. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 Service Developer Guide/.
putObject_storageClass :: Lens.Lens' PutObject (Prelude.Maybe StorageClass)
putObject_storageClass = Lens.lens (\PutObject' {storageClass} -> storageClass) (\s@PutObject' {} a -> s {storageClass = a} :: PutObject)

-- | Specifies whether Amazon S3 should use an S3 Bucket Key for object
-- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
-- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
-- object encryption with SSE-KMS.
--
-- Specifying this header with a PUT operation doesn’t affect bucket-level
-- settings for S3 Bucket Key.
putObject_bucketKeyEnabled :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Bool)
putObject_bucketKeyEnabled = Lens.lens (\PutObject' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@PutObject' {} a -> s {bucketKeyEnabled = a} :: PutObject)

-- | Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
putObject_grantWriteACP :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_grantWriteACP = Lens.lens (\PutObject' {grantWriteACP} -> grantWriteACP) (\s@PutObject' {} a -> s {grantWriteACP = a} :: PutObject)

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
putObject_serverSideEncryption :: Lens.Lens' PutObject (Prelude.Maybe ServerSideEncryption)
putObject_serverSideEncryption = Lens.lens (\PutObject' {serverSideEncryption} -> serverSideEncryption) (\s@PutObject' {} a -> s {serverSideEncryption = a} :: PutObject)

-- | Specifies whether a legal hold will be applied to this object. For more
-- information about S3 Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
putObject_objectLockLegalHoldStatus :: Lens.Lens' PutObject (Prelude.Maybe ObjectLockLegalHoldStatus)
putObject_objectLockLegalHoldStatus = Lens.lens (\PutObject' {objectLockLegalHoldStatus} -> objectLockLegalHoldStatus) (\s@PutObject' {} a -> s {objectLockLegalHoldStatus = a} :: PutObject)

-- | Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
putObject_grantReadACP :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_grantReadACP = Lens.lens (\PutObject' {grantReadACP} -> grantReadACP) (\s@PutObject' {} a -> s {grantReadACP = a} :: PutObject)

-- | The canned ACL to apply to the object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
--
-- This action is not supported by Amazon S3 on Outposts.
putObject_acl :: Lens.Lens' PutObject (Prelude.Maybe ObjectCannedACL)
putObject_acl = Lens.lens (\PutObject' {acl} -> acl) (\s@PutObject' {} a -> s {acl = a} :: PutObject)

-- | Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
putObject_sSECustomerAlgorithm :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_sSECustomerAlgorithm = Lens.lens (\PutObject' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@PutObject' {} a -> s {sSECustomerAlgorithm = a} :: PutObject)

-- | Undocumented member.
putObject_requestPayer :: Lens.Lens' PutObject (Prelude.Maybe RequestPayer)
putObject_requestPayer = Lens.lens (\PutObject' {requestPayer} -> requestPayer) (\s@PutObject' {} a -> s {requestPayer = a} :: PutObject)

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
putObject_sSECustomerKey :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_sSECustomerKey = Lens.lens (\PutObject' {sSECustomerKey} -> sSECustomerKey) (\s@PutObject' {} a -> s {sSECustomerKey = a} :: PutObject) Prelude.. Lens.mapping Prelude._Sensitive

-- | Can be used to specify caching behavior along the request\/reply chain.
-- For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
putObject_cacheControl :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_cacheControl = Lens.lens (\PutObject' {cacheControl} -> cacheControl) (\s@PutObject' {} a -> s {cacheControl = a} :: PutObject)

-- | The date and time at which the object is no longer cacheable. For more
-- information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21>.
putObject_expires :: Lens.Lens' PutObject (Prelude.Maybe Prelude.UTCTime)
putObject_expires = Lens.lens (\PutObject' {expires} -> expires) (\s@PutObject' {} a -> s {expires = a} :: PutObject) Prelude.. Lens.mapping Prelude._Time

-- | The Object Lock mode that you want to apply to this object.
putObject_objectLockMode :: Lens.Lens' PutObject (Prelude.Maybe ObjectLockMode)
putObject_objectLockMode = Lens.lens (\PutObject' {objectLockMode} -> objectLockMode) (\s@PutObject' {} a -> s {objectLockMode = a} :: PutObject)

-- | The date and time when you want this object\'s Object Lock to expire.
putObject_objectLockRetainUntilDate :: Lens.Lens' PutObject (Prelude.Maybe Prelude.UTCTime)
putObject_objectLockRetainUntilDate = Lens.lens (\PutObject' {objectLockRetainUntilDate} -> objectLockRetainUntilDate) (\s@PutObject' {} a -> s {objectLockRetainUntilDate = a} :: PutObject) Prelude.. Lens.mapping Prelude._Time

-- | The tag-set for the object. The tag-set must be encoded as URL Query
-- parameters. (For example, \"Key1=Value1\")
putObject_tagging :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_tagging = Lens.lens (\PutObject' {tagging} -> tagging) (\s@PutObject' {} a -> s {tagging = a} :: PutObject)

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
--
-- This action is not supported by Amazon S3 on Outposts.
putObject_grantFullControl :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_grantFullControl = Lens.lens (\PutObject' {grantFullControl} -> grantFullControl) (\s@PutObject' {} a -> s {grantFullControl = a} :: PutObject)

-- | The bucket name to which the PUT operation was initiated.
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
putObject_bucket :: Lens.Lens' PutObject BucketName
putObject_bucket = Lens.lens (\PutObject' {bucket} -> bucket) (\s@PutObject' {} a -> s {bucket = a} :: PutObject)

-- | Object key for which the PUT operation was initiated.
putObject_key :: Lens.Lens' PutObject ObjectKey
putObject_key = Lens.lens (\PutObject' {key} -> key) (\s@PutObject' {} a -> s {key = a} :: PutObject)

-- | Object data.
putObject_body :: Lens.Lens' PutObject Prelude.RqBody
putObject_body = Lens.lens (\PutObject' {body} -> body) (\s@PutObject' {} a -> s {body = a} :: PutObject)

instance Prelude.AWSRequest PutObject where
  type Rs PutObject = PutObjectResponse
  request =
    Request.expectHeader
      Prelude.. Request.putBody defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutObjectResponse'
            Prelude.<$> (h Prelude..#? "ETag")
            Prelude.<*> (h Prelude..#? "x-amz-request-charged")
            Prelude.<*> (h Prelude..#? "x-amz-expiration")
            Prelude.<*> ( h
                            Prelude..#? "x-amz-server-side-encryption-context"
                        )
            Prelude.<*> ( h
                            Prelude..#? "x-amz-server-side-encryption-aws-kms-key-id"
                        )
            Prelude.<*> ( h
                            Prelude..#? "x-amz-server-side-encryption-customer-key-MD5"
                        )
            Prelude.<*> (h Prelude..#? "x-amz-version-id")
            Prelude.<*> ( h
                            Prelude..#? "x-amz-server-side-encryption-bucket-key-enabled"
                        )
            Prelude.<*> (h Prelude..#? "x-amz-server-side-encryption")
            Prelude.<*> ( h
                            Prelude..#? "x-amz-server-side-encryption-customer-algorithm"
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.ToBody PutObject where
  toBody PutObject' {..} = Prelude.toBody body

instance Prelude.ToHeaders PutObject where
  toHeaders PutObject' {..} =
    Prelude.mconcat
      [ "x-amz-website-redirect-location"
          Prelude.=# websiteRedirectLocation,
        "x-amz-grant-read" Prelude.=# grantRead,
        "Content-Type" Prelude.=# contentType,
        "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner,
        "Content-Disposition" Prelude.=# contentDisposition,
        "Content-Language" Prelude.=# contentLanguage,
        "x-amz-server-side-encryption-context"
          Prelude.=# sSEKMSEncryptionContext,
        "Content-MD5" Prelude.=# contentMD5,
        "x-amz-meta-" Prelude.=# metadata,
        "Content-Length" Prelude.=# contentLength,
        "Content-Encoding" Prelude.=# contentEncoding,
        "x-amz-server-side-encryption-aws-kms-key-id"
          Prelude.=# sSEKMSKeyId,
        "x-amz-server-side-encryption-customer-key-MD5"
          Prelude.=# sSECustomerKeyMD5,
        "x-amz-storage-class" Prelude.=# storageClass,
        "x-amz-server-side-encryption-bucket-key-enabled"
          Prelude.=# bucketKeyEnabled,
        "x-amz-grant-write-acp" Prelude.=# grantWriteACP,
        "x-amz-server-side-encryption"
          Prelude.=# serverSideEncryption,
        "x-amz-object-lock-legal-hold"
          Prelude.=# objectLockLegalHoldStatus,
        "x-amz-grant-read-acp" Prelude.=# grantReadACP,
        "x-amz-acl" Prelude.=# acl,
        "x-amz-server-side-encryption-customer-algorithm"
          Prelude.=# sSECustomerAlgorithm,
        "x-amz-request-payer" Prelude.=# requestPayer,
        "x-amz-server-side-encryption-customer-key"
          Prelude.=# sSECustomerKey,
        "Cache-Control" Prelude.=# cacheControl,
        "Expires" Prelude.=# expires,
        "x-amz-object-lock-mode" Prelude.=# objectLockMode,
        "x-amz-object-lock-retain-until-date"
          Prelude.=# objectLockRetainUntilDate,
        "x-amz-tagging" Prelude.=# tagging,
        "x-amz-grant-full-control"
          Prelude.=# grantFullControl
      ]

instance Prelude.ToPath PutObject where
  toPath PutObject' {..} =
    Prelude.mconcat
      ["/", Prelude.toBS bucket, "/", Prelude.toBS key]

instance Prelude.ToQuery PutObject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutObjectResponse' smart constructor.
data PutObjectResponse = PutObjectResponse'
  { -- | Entity tag for the uploaded object.
    eTag :: Prelude.Maybe ETag,
    requestCharged :: Prelude.Maybe RequestCharged,
    -- | If the expiration is configured for the object (see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>),
    -- the response includes this header. It includes the expiry-date and
    -- rule-id key-value pairs that provide information about object
    -- expiration. The value of the rule-id is URL encoded.
    expiration :: Prelude.Maybe Prelude.Text,
    -- | If present, specifies the AWS KMS Encryption Context to use for object
    -- encryption. The value of this header is a base64-encoded UTF-8 string
    -- holding JSON with the encryption context key-value pairs.
    sSEKMSEncryptionContext :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | If @x-amz-server-side-encryption@ is present and has the value of
    -- @aws:kms@, this header specifies the ID of the AWS Key Management
    -- Service (AWS KMS) symmetric customer managed customer master key (CMK)
    -- that was used for the object.
    sSEKMSKeyId :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header to provide round-trip
    -- message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | Version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | Indicates whether the uploaded object uses an S3 Bucket Key for
    -- server-side encryption with AWS KMS (SSE-KMS).
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | If you specified server-side encryption either with an AWS KMS customer
    -- master key (CMK) or Amazon S3-managed encryption key in your PUT
    -- request, the response includes this header. It confirms the encryption
    -- algorithm that Amazon S3 used to encrypt the object.
    serverSideEncryption :: Prelude.Maybe ServerSideEncryption,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header confirming the
    -- encryption algorithm used.
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'putObjectResponse_eTag' - Entity tag for the uploaded object.
--
-- 'requestCharged', 'putObjectResponse_requestCharged' - Undocumented member.
--
-- 'expiration', 'putObjectResponse_expiration' - If the expiration is configured for the object (see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>),
-- the response includes this header. It includes the expiry-date and
-- rule-id key-value pairs that provide information about object
-- expiration. The value of the rule-id is URL encoded.
--
-- 'sSEKMSEncryptionContext', 'putObjectResponse_sSEKMSEncryptionContext' - If present, specifies the AWS KMS Encryption Context to use for object
-- encryption. The value of this header is a base64-encoded UTF-8 string
-- holding JSON with the encryption context key-value pairs.
--
-- 'sSEKMSKeyId', 'putObjectResponse_sSEKMSKeyId' - If @x-amz-server-side-encryption@ is present and has the value of
-- @aws:kms@, this header specifies the ID of the AWS Key Management
-- Service (AWS KMS) symmetric customer managed customer master key (CMK)
-- that was used for the object.
--
-- 'sSECustomerKeyMD5', 'putObjectResponse_sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
--
-- 'versionId', 'putObjectResponse_versionId' - Version of the object.
--
-- 'bucketKeyEnabled', 'putObjectResponse_bucketKeyEnabled' - Indicates whether the uploaded object uses an S3 Bucket Key for
-- server-side encryption with AWS KMS (SSE-KMS).
--
-- 'serverSideEncryption', 'putObjectResponse_serverSideEncryption' - If you specified server-side encryption either with an AWS KMS customer
-- master key (CMK) or Amazon S3-managed encryption key in your PUT
-- request, the response includes this header. It confirms the encryption
-- algorithm that Amazon S3 used to encrypt the object.
--
-- 'sSECustomerAlgorithm', 'putObjectResponse_sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
--
-- 'httpStatus', 'putObjectResponse_httpStatus' - The response's http status code.
newPutObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutObjectResponse
newPutObjectResponse pHttpStatus_ =
  PutObjectResponse'
    { eTag = Prelude.Nothing,
      requestCharged = Prelude.Nothing,
      expiration = Prelude.Nothing,
      sSEKMSEncryptionContext = Prelude.Nothing,
      sSEKMSKeyId = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      versionId = Prelude.Nothing,
      bucketKeyEnabled = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Entity tag for the uploaded object.
putObjectResponse_eTag :: Lens.Lens' PutObjectResponse (Prelude.Maybe ETag)
putObjectResponse_eTag = Lens.lens (\PutObjectResponse' {eTag} -> eTag) (\s@PutObjectResponse' {} a -> s {eTag = a} :: PutObjectResponse)

-- | Undocumented member.
putObjectResponse_requestCharged :: Lens.Lens' PutObjectResponse (Prelude.Maybe RequestCharged)
putObjectResponse_requestCharged = Lens.lens (\PutObjectResponse' {requestCharged} -> requestCharged) (\s@PutObjectResponse' {} a -> s {requestCharged = a} :: PutObjectResponse)

-- | If the expiration is configured for the object (see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>),
-- the response includes this header. It includes the expiry-date and
-- rule-id key-value pairs that provide information about object
-- expiration. The value of the rule-id is URL encoded.
putObjectResponse_expiration :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_expiration = Lens.lens (\PutObjectResponse' {expiration} -> expiration) (\s@PutObjectResponse' {} a -> s {expiration = a} :: PutObjectResponse)

-- | If present, specifies the AWS KMS Encryption Context to use for object
-- encryption. The value of this header is a base64-encoded UTF-8 string
-- holding JSON with the encryption context key-value pairs.
putObjectResponse_sSEKMSEncryptionContext :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_sSEKMSEncryptionContext = Lens.lens (\PutObjectResponse' {sSEKMSEncryptionContext} -> sSEKMSEncryptionContext) (\s@PutObjectResponse' {} a -> s {sSEKMSEncryptionContext = a} :: PutObjectResponse) Prelude.. Lens.mapping Prelude._Sensitive

-- | If @x-amz-server-side-encryption@ is present and has the value of
-- @aws:kms@, this header specifies the ID of the AWS Key Management
-- Service (AWS KMS) symmetric customer managed customer master key (CMK)
-- that was used for the object.
putObjectResponse_sSEKMSKeyId :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_sSEKMSKeyId = Lens.lens (\PutObjectResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@PutObjectResponse' {} a -> s {sSEKMSKeyId = a} :: PutObjectResponse) Prelude.. Lens.mapping Prelude._Sensitive

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
putObjectResponse_sSECustomerKeyMD5 :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_sSECustomerKeyMD5 = Lens.lens (\PutObjectResponse' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@PutObjectResponse' {} a -> s {sSECustomerKeyMD5 = a} :: PutObjectResponse)

-- | Version of the object.
putObjectResponse_versionId :: Lens.Lens' PutObjectResponse (Prelude.Maybe ObjectVersionId)
putObjectResponse_versionId = Lens.lens (\PutObjectResponse' {versionId} -> versionId) (\s@PutObjectResponse' {} a -> s {versionId = a} :: PutObjectResponse)

-- | Indicates whether the uploaded object uses an S3 Bucket Key for
-- server-side encryption with AWS KMS (SSE-KMS).
putObjectResponse_bucketKeyEnabled :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Bool)
putObjectResponse_bucketKeyEnabled = Lens.lens (\PutObjectResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@PutObjectResponse' {} a -> s {bucketKeyEnabled = a} :: PutObjectResponse)

-- | If you specified server-side encryption either with an AWS KMS customer
-- master key (CMK) or Amazon S3-managed encryption key in your PUT
-- request, the response includes this header. It confirms the encryption
-- algorithm that Amazon S3 used to encrypt the object.
putObjectResponse_serverSideEncryption :: Lens.Lens' PutObjectResponse (Prelude.Maybe ServerSideEncryption)
putObjectResponse_serverSideEncryption = Lens.lens (\PutObjectResponse' {serverSideEncryption} -> serverSideEncryption) (\s@PutObjectResponse' {} a -> s {serverSideEncryption = a} :: PutObjectResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
putObjectResponse_sSECustomerAlgorithm :: Lens.Lens' PutObjectResponse (Prelude.Maybe Prelude.Text)
putObjectResponse_sSECustomerAlgorithm = Lens.lens (\PutObjectResponse' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@PutObjectResponse' {} a -> s {sSECustomerAlgorithm = a} :: PutObjectResponse)

-- | The response's http status code.
putObjectResponse_httpStatus :: Lens.Lens' PutObjectResponse Prelude.Int
putObjectResponse_httpStatus = Lens.lens (\PutObjectResponse' {httpStatus} -> httpStatus) (\s@PutObjectResponse' {} a -> s {httpStatus = a} :: PutObjectResponse)

instance Prelude.NFData PutObjectResponse
