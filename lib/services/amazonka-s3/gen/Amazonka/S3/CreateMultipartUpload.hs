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
-- Module      : Amazonka.S3.CreateMultipartUpload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action initiates a multipart upload and returns an upload ID. This
-- upload ID is used to associate all of the parts in the specific
-- multipart upload. You specify this upload ID in each of your subsequent
-- upload part requests (see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>).
-- You also include this upload ID in the final request to either complete
-- or abort the multipart upload request.
--
-- For more information about multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html Multipart Upload Overview>.
--
-- If you have configured a lifecycle rule to abort incomplete multipart
-- uploads, the upload must complete within the number of days specified in
-- the bucket lifecycle configuration. Otherwise, the incomplete multipart
-- upload becomes eligible for an abort action and Amazon S3 aborts the
-- multipart upload. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy>.
--
-- For information about the permissions required to use the multipart
-- upload API, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload and Permissions>.
--
-- For request signing, multipart upload is just a series of regular
-- requests. You initiate a multipart upload, send one or more requests to
-- upload parts, and then complete the multipart upload process. You sign
-- each request individually. There is nothing special about signing
-- multipart upload requests. For more information about signing, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html Authenticating Requests (Amazon Web Services Signature Version 4)>.
--
-- After you initiate a multipart upload and upload one or more parts, to
-- stop being charged for storing the uploaded parts, you must either
-- complete or abort the multipart upload. Amazon S3 frees up the space
-- used to store the parts and stop charging you for storing them only
-- after you either complete or abort a multipart upload.
--
-- You can optionally request server-side encryption. For server-side
-- encryption, Amazon S3 encrypts your data as it writes it to disks in its
-- data centers and decrypts it when you access it. You can provide your
-- own encryption key, or use Amazon Web Services KMS keys or Amazon
-- S3-managed encryption keys. If you choose to provide your own encryption
-- key, the request headers you provide in
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPartCopy.html UploadPartCopy>
-- requests must match the headers you used in the request to initiate the
-- upload by using @CreateMultipartUpload@.
--
-- To perform a multipart upload with encryption using an Amazon Web
-- Services KMS key, the requester must have permission to the
-- @kms:Decrypt@ and @kms:GenerateDataKey*@ actions on the key. These
-- permissions are required because Amazon S3 must decrypt and read data
-- from the encrypted file parts before it completes the multipart upload.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/mpuoverview.html#mpuAndPermissions Multipart upload API and permissions>
-- in the /Amazon S3 User Guide/.
--
-- If your Identity and Access Management (IAM) user or role is in the same
-- Amazon Web Services account as the KMS key, then you must have these
-- permissions on the key policy. If your IAM user or role belongs to a
-- different account than the key, then you must have the permissions on
-- both the key policy and your IAM user or role.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html Protecting Data Using Server-Side Encryption>.
--
-- [Access Permissions]
--     When copying an object, you can optionally specify the accounts or
--     groups that should be granted specific permissions on the new
--     object. There are two ways to grant the permissions using the
--     request headers:
--
--     -   Specify a canned ACL with the @x-amz-acl@ request header. For
--         more information, see
--         <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
--
--     -   Specify access permissions explicitly with the
--         @x-amz-grant-read@, @x-amz-grant-read-acp@,
--         @x-amz-grant-write-acp@, and @x-amz-grant-full-control@ headers.
--         These parameters map to the set of permissions that Amazon S3
--         supports in an ACL. For more information, see
--         <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview>.
--
--     You can use either a canned ACL or specify access permissions
--     explicitly. You cannot do both.
--
-- [Server-Side- Encryption-Specific Request Headers]
--     You can optionally tell Amazon S3 to encrypt data at rest using
--     server-side encryption. Server-side encryption is for data
--     encryption at rest. Amazon S3 encrypts your data as it writes it to
--     disks in its data centers and decrypts it when you access it. The
--     option you use depends on whether you want to use Amazon Web
--     Services managed encryption keys or provide your own encryption key.
--
--     -   Use encryption keys managed by Amazon S3 or customer managed key
--         stored in Amazon Web Services Key Management Service (Amazon Web
--         Services KMS) – If you want Amazon Web Services to manage the
--         keys used to encrypt data, specify the following headers in the
--         request.
--
--         -   @x-amz-server-side-encryption@
--
--         -   @x-amz-server-side-encryption-aws-kms-key-id@
--
--         -   @x-amz-server-side-encryption-context@
--
--         If you specify @x-amz-server-side-encryption:aws:kms@, but
--         don\'t provide @x-amz-server-side-encryption-aws-kms-key-id@,
--         Amazon S3 uses the Amazon Web Services managed key in Amazon Web
--         Services KMS to protect the data.
--
--         All GET and PUT requests for an object protected by Amazon Web
--         Services KMS fail if you don\'t make them with SSL or by using
--         SigV4.
--
--         For more information about server-side encryption with KMS key
--         (SSE-KMS), see
--         <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting Data Using Server-Side Encryption with KMS keys>.
--
--     -   Use customer-provided encryption keys – If you want to manage
--         your own encryption keys, provide all the following headers in
--         the request.
--
--         -   @x-amz-server-side-encryption-customer-algorithm@
--
--         -   @x-amz-server-side-encryption-customer-key@
--
--         -   @x-amz-server-side-encryption-customer-key-MD5@
--
--         For more information about server-side encryption with KMS keys
--         (SSE-KMS), see
--         <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting Data Using Server-Side Encryption with KMS keys>.
--
-- [Access-Control-List (ACL)-Specific Request Headers]
--     You also can use the following access control–related headers with
--     this operation. By default, all objects are private. Only the owner
--     has full access control. When adding a new object, you can grant
--     permissions to individual Amazon Web Services accounts or to
--     predefined groups defined by Amazon S3. These permissions are then
--     added to the access control list (ACL) on the object. For more
--     information, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3_ACLs_UsingACLs.html Using ACLs>.
--     With this operation, you can grant access permissions using one of
--     the following two methods:
--
--     -   Specify a canned ACL (@x-amz-acl@) — Amazon S3 supports a set of
--         predefined ACLs, known as /canned ACLs/. Each canned ACL has a
--         predefined set of grantees and permissions. For more
--         information, see
--         <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
--
--     -   Specify access permissions explicitly — To explicitly grant
--         access permissions to specific Amazon Web Services accounts or
--         groups, use the following headers. Each header maps to specific
--         permissions that Amazon S3 supports in an ACL. For more
--         information, see
--         <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview>.
--         In the header, you specify a list of grantees who get the
--         specific permission. To grant permissions explicitly, use:
--
--         -   @x-amz-grant-read@
--
--         -   @x-amz-grant-write@
--
--         -   @x-amz-grant-read-acp@
--
--         -   @x-amz-grant-write-acp@
--
--         -   @x-amz-grant-full-control@
--
--         You specify each grantee as a type=value pair, where the type is
--         one of the following:
--
--         -   @id@ – if the value specified is the canonical user ID of an
--             Amazon Web Services account
--
--         -   @uri@ – if you are granting permissions to a predefined
--             group
--
--         -   @emailAddress@ – if the value specified is the email address
--             of an Amazon Web Services account
--
--             Using email addresses to specify a grantee is only supported
--             in the following Amazon Web Services Regions:
--
--             -   US East (N. Virginia)
--
--             -   US West (N. California)
--
--             -   US West (Oregon)
--
--             -   Asia Pacific (Singapore)
--
--             -   Asia Pacific (Sydney)
--
--             -   Asia Pacific (Tokyo)
--
--             -   Europe (Ireland)
--
--             -   South America (São Paulo)
--
--             For a list of all the Amazon S3 supported Regions and
--             endpoints, see
--             <https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Regions and Endpoints>
--             in the Amazon Web Services General Reference.
--
--         For example, the following @x-amz-grant-read@ header grants the
--         Amazon Web Services accounts identified by account IDs
--         permissions to read object data and its metadata:
--
--         @x-amz-grant-read: id=\"11112222333\", id=\"444455556666\" @
--
-- The following operations are related to @CreateMultipartUpload@:
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
module Amazonka.S3.CreateMultipartUpload
  ( -- * Creating a Request
    CreateMultipartUpload (..),
    newCreateMultipartUpload,

    -- * Request Lenses
    createMultipartUpload_acl,
    createMultipartUpload_bucketKeyEnabled,
    createMultipartUpload_cacheControl,
    createMultipartUpload_checksumAlgorithm,
    createMultipartUpload_contentDisposition,
    createMultipartUpload_contentEncoding,
    createMultipartUpload_contentLanguage,
    createMultipartUpload_contentType,
    createMultipartUpload_expectedBucketOwner,
    createMultipartUpload_expires,
    createMultipartUpload_grantFullControl,
    createMultipartUpload_grantRead,
    createMultipartUpload_grantReadACP,
    createMultipartUpload_grantWriteACP,
    createMultipartUpload_metadata,
    createMultipartUpload_objectLockLegalHoldStatus,
    createMultipartUpload_objectLockMode,
    createMultipartUpload_objectLockRetainUntilDate,
    createMultipartUpload_requestPayer,
    createMultipartUpload_sSECustomerAlgorithm,
    createMultipartUpload_sSECustomerKey,
    createMultipartUpload_sSECustomerKeyMD5,
    createMultipartUpload_sSEKMSEncryptionContext,
    createMultipartUpload_sSEKMSKeyId,
    createMultipartUpload_serverSideEncryption,
    createMultipartUpload_storageClass,
    createMultipartUpload_tagging,
    createMultipartUpload_websiteRedirectLocation,
    createMultipartUpload_bucket,
    createMultipartUpload_key,

    -- * Destructuring the Response
    CreateMultipartUploadResponse (..),
    newCreateMultipartUploadResponse,

    -- * Response Lenses
    createMultipartUploadResponse_abortDate,
    createMultipartUploadResponse_abortRuleId,
    createMultipartUploadResponse_bucket,
    createMultipartUploadResponse_bucketKeyEnabled,
    createMultipartUploadResponse_checksumAlgorithm,
    createMultipartUploadResponse_key,
    createMultipartUploadResponse_requestCharged,
    createMultipartUploadResponse_sSECustomerAlgorithm,
    createMultipartUploadResponse_sSECustomerKeyMD5,
    createMultipartUploadResponse_sSEKMSEncryptionContext,
    createMultipartUploadResponse_sSEKMSKeyId,
    createMultipartUploadResponse_serverSideEncryption,
    createMultipartUploadResponse_httpStatus,
    createMultipartUploadResponse_uploadId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newCreateMultipartUpload' smart constructor.
data CreateMultipartUpload = CreateMultipartUpload'
  { -- | The canned ACL to apply to the object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    acl :: Prelude.Maybe ObjectCannedACL,
    -- | Specifies whether Amazon S3 should use an S3 Bucket Key for object
    -- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
    -- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
    -- object encryption with SSE-KMS.
    --
    -- Specifying this header with an object action doesn’t affect bucket-level
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
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
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
    -- | Specifies whether you want to apply a legal hold to the uploaded object.
    objectLockLegalHoldStatus :: Prelude.Maybe ObjectLockLegalHoldStatus,
    -- | Specifies the Object Lock mode that you want to apply to the uploaded
    -- object.
    objectLockMode :: Prelude.Maybe ObjectLockMode,
    -- | Specifies the date and time when you want the Object Lock to expire.
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
    -- | Specifies the ID of the symmetric customer managed key to use for object
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
    -- | The tag-set for the object. The tag-set must be encoded as URL Query
    -- parameters.
    tagging :: Prelude.Maybe Prelude.Text,
    -- | If the bucket is configured as a website, redirects requests for this
    -- object to another object in the same bucket or to an external URL.
    -- Amazon S3 stores the value of this header in the object metadata.
    websiteRedirectLocation :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket to which to initiate the upload
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
    -- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
    -- When using this action with S3 on Outposts through the Amazon Web
    -- Services SDKs, you provide the Outposts bucket ARN in place of the
    -- bucket name. For more information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName,
    -- | Object key for which the multipart upload is to be initiated.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMultipartUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acl', 'createMultipartUpload_acl' - The canned ACL to apply to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'bucketKeyEnabled', 'createMultipartUpload_bucketKeyEnabled' - Specifies whether Amazon S3 should use an S3 Bucket Key for object
-- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
-- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
-- object encryption with SSE-KMS.
--
-- Specifying this header with an object action doesn’t affect bucket-level
-- settings for S3 Bucket Key.
--
-- 'cacheControl', 'createMultipartUpload_cacheControl' - Specifies caching behavior along the request\/reply chain.
--
-- 'checksumAlgorithm', 'createMultipartUpload_checksumAlgorithm' - Indicates the algorithm you want Amazon S3 to use to create the checksum
-- for the object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'contentDisposition', 'createMultipartUpload_contentDisposition' - Specifies presentational information for the object.
--
-- 'contentEncoding', 'createMultipartUpload_contentEncoding' - Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
--
-- 'contentLanguage', 'createMultipartUpload_contentLanguage' - The language the content is in.
--
-- 'contentType', 'createMultipartUpload_contentType' - A standard MIME type describing the format of the object data.
--
-- 'expectedBucketOwner', 'createMultipartUpload_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'expires', 'createMultipartUpload_expires' - The date and time at which the object is no longer cacheable.
--
-- 'grantFullControl', 'createMultipartUpload_grantFullControl' - Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'grantRead', 'createMultipartUpload_grantRead' - Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'grantReadACP', 'createMultipartUpload_grantReadACP' - Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'grantWriteACP', 'createMultipartUpload_grantWriteACP' - Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'metadata', 'createMultipartUpload_metadata' - A map of metadata to store with the object in S3.
--
-- 'objectLockLegalHoldStatus', 'createMultipartUpload_objectLockLegalHoldStatus' - Specifies whether you want to apply a legal hold to the uploaded object.
--
-- 'objectLockMode', 'createMultipartUpload_objectLockMode' - Specifies the Object Lock mode that you want to apply to the uploaded
-- object.
--
-- 'objectLockRetainUntilDate', 'createMultipartUpload_objectLockRetainUntilDate' - Specifies the date and time when you want the Object Lock to expire.
--
-- 'requestPayer', 'createMultipartUpload_requestPayer' - Undocumented member.
--
-- 'sSECustomerAlgorithm', 'createMultipartUpload_sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
--
-- 'sSECustomerKey', 'createMultipartUpload_sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- 'sSECustomerKeyMD5', 'createMultipartUpload_sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'sSEKMSEncryptionContext', 'createMultipartUpload_sSEKMSEncryptionContext' - Specifies the Amazon Web Services KMS Encryption Context to use for
-- object encryption. The value of this header is a base64-encoded UTF-8
-- string holding JSON with the encryption context key-value pairs.
--
-- 'sSEKMSKeyId', 'createMultipartUpload_sSEKMSKeyId' - Specifies the ID of the symmetric customer managed key to use for object
-- encryption. All GET and PUT requests for an object protected by Amazon
-- Web Services KMS will fail if not made via SSL or using SigV4. For
-- information about configuring using any of the officially supported
-- Amazon Web Services SDKs and Amazon Web Services CLI, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication>
-- in the /Amazon S3 User Guide/.
--
-- 'serverSideEncryption', 'createMultipartUpload_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
--
-- 'storageClass', 'createMultipartUpload_storageClass' - By default, Amazon S3 uses the STANDARD Storage Class to store newly
-- created objects. The STANDARD storage class provides high durability and
-- high availability. Depending on performance needs, you can specify a
-- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
-- Storage Class. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 User Guide/.
--
-- 'tagging', 'createMultipartUpload_tagging' - The tag-set for the object. The tag-set must be encoded as URL Query
-- parameters.
--
-- 'websiteRedirectLocation', 'createMultipartUpload_websiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
--
-- 'bucket', 'createMultipartUpload_bucket' - The name of the bucket to which to initiate the upload
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
-- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
-- When using this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
-- in the /Amazon S3 User Guide/.
--
-- 'key', 'createMultipartUpload_key' - Object key for which the multipart upload is to be initiated.
newCreateMultipartUpload ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  CreateMultipartUpload
newCreateMultipartUpload pBucket_ pKey_ =
  CreateMultipartUpload'
    { acl = Prelude.Nothing,
      bucketKeyEnabled = Prelude.Nothing,
      cacheControl = Prelude.Nothing,
      checksumAlgorithm = Prelude.Nothing,
      contentDisposition = Prelude.Nothing,
      contentEncoding = Prelude.Nothing,
      contentLanguage = Prelude.Nothing,
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
      key = pKey_
    }

-- | The canned ACL to apply to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
createMultipartUpload_acl :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe ObjectCannedACL)
createMultipartUpload_acl = Lens.lens (\CreateMultipartUpload' {acl} -> acl) (\s@CreateMultipartUpload' {} a -> s {acl = a} :: CreateMultipartUpload)

-- | Specifies whether Amazon S3 should use an S3 Bucket Key for object
-- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
-- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
-- object encryption with SSE-KMS.
--
-- Specifying this header with an object action doesn’t affect bucket-level
-- settings for S3 Bucket Key.
createMultipartUpload_bucketKeyEnabled :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Bool)
createMultipartUpload_bucketKeyEnabled = Lens.lens (\CreateMultipartUpload' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@CreateMultipartUpload' {} a -> s {bucketKeyEnabled = a} :: CreateMultipartUpload)

-- | Specifies caching behavior along the request\/reply chain.
createMultipartUpload_cacheControl :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_cacheControl = Lens.lens (\CreateMultipartUpload' {cacheControl} -> cacheControl) (\s@CreateMultipartUpload' {} a -> s {cacheControl = a} :: CreateMultipartUpload)

-- | Indicates the algorithm you want Amazon S3 to use to create the checksum
-- for the object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
createMultipartUpload_checksumAlgorithm :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe ChecksumAlgorithm)
createMultipartUpload_checksumAlgorithm = Lens.lens (\CreateMultipartUpload' {checksumAlgorithm} -> checksumAlgorithm) (\s@CreateMultipartUpload' {} a -> s {checksumAlgorithm = a} :: CreateMultipartUpload)

-- | Specifies presentational information for the object.
createMultipartUpload_contentDisposition :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_contentDisposition = Lens.lens (\CreateMultipartUpload' {contentDisposition} -> contentDisposition) (\s@CreateMultipartUpload' {} a -> s {contentDisposition = a} :: CreateMultipartUpload)

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
createMultipartUpload_contentEncoding :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_contentEncoding = Lens.lens (\CreateMultipartUpload' {contentEncoding} -> contentEncoding) (\s@CreateMultipartUpload' {} a -> s {contentEncoding = a} :: CreateMultipartUpload)

-- | The language the content is in.
createMultipartUpload_contentLanguage :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_contentLanguage = Lens.lens (\CreateMultipartUpload' {contentLanguage} -> contentLanguage) (\s@CreateMultipartUpload' {} a -> s {contentLanguage = a} :: CreateMultipartUpload)

-- | A standard MIME type describing the format of the object data.
createMultipartUpload_contentType :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_contentType = Lens.lens (\CreateMultipartUpload' {contentType} -> contentType) (\s@CreateMultipartUpload' {} a -> s {contentType = a} :: CreateMultipartUpload)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
createMultipartUpload_expectedBucketOwner :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_expectedBucketOwner = Lens.lens (\CreateMultipartUpload' {expectedBucketOwner} -> expectedBucketOwner) (\s@CreateMultipartUpload' {} a -> s {expectedBucketOwner = a} :: CreateMultipartUpload)

-- | The date and time at which the object is no longer cacheable.
createMultipartUpload_expires :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.UTCTime)
createMultipartUpload_expires = Lens.lens (\CreateMultipartUpload' {expires} -> expires) (\s@CreateMultipartUpload' {} a -> s {expires = a} :: CreateMultipartUpload) Prelude.. Lens.mapping Data._Time

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
--
-- This action is not supported by Amazon S3 on Outposts.
createMultipartUpload_grantFullControl :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_grantFullControl = Lens.lens (\CreateMultipartUpload' {grantFullControl} -> grantFullControl) (\s@CreateMultipartUpload' {} a -> s {grantFullControl = a} :: CreateMultipartUpload)

-- | Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
createMultipartUpload_grantRead :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_grantRead = Lens.lens (\CreateMultipartUpload' {grantRead} -> grantRead) (\s@CreateMultipartUpload' {} a -> s {grantRead = a} :: CreateMultipartUpload)

-- | Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
createMultipartUpload_grantReadACP :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_grantReadACP = Lens.lens (\CreateMultipartUpload' {grantReadACP} -> grantReadACP) (\s@CreateMultipartUpload' {} a -> s {grantReadACP = a} :: CreateMultipartUpload)

-- | Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
createMultipartUpload_grantWriteACP :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_grantWriteACP = Lens.lens (\CreateMultipartUpload' {grantWriteACP} -> grantWriteACP) (\s@CreateMultipartUpload' {} a -> s {grantWriteACP = a} :: CreateMultipartUpload)

-- | A map of metadata to store with the object in S3.
createMultipartUpload_metadata :: Lens.Lens' CreateMultipartUpload (Prelude.HashMap Prelude.Text Prelude.Text)
createMultipartUpload_metadata = Lens.lens (\CreateMultipartUpload' {metadata} -> metadata) (\s@CreateMultipartUpload' {} a -> s {metadata = a} :: CreateMultipartUpload) Prelude.. Lens.coerced

-- | Specifies whether you want to apply a legal hold to the uploaded object.
createMultipartUpload_objectLockLegalHoldStatus :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe ObjectLockLegalHoldStatus)
createMultipartUpload_objectLockLegalHoldStatus = Lens.lens (\CreateMultipartUpload' {objectLockLegalHoldStatus} -> objectLockLegalHoldStatus) (\s@CreateMultipartUpload' {} a -> s {objectLockLegalHoldStatus = a} :: CreateMultipartUpload)

-- | Specifies the Object Lock mode that you want to apply to the uploaded
-- object.
createMultipartUpload_objectLockMode :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe ObjectLockMode)
createMultipartUpload_objectLockMode = Lens.lens (\CreateMultipartUpload' {objectLockMode} -> objectLockMode) (\s@CreateMultipartUpload' {} a -> s {objectLockMode = a} :: CreateMultipartUpload)

-- | Specifies the date and time when you want the Object Lock to expire.
createMultipartUpload_objectLockRetainUntilDate :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.UTCTime)
createMultipartUpload_objectLockRetainUntilDate = Lens.lens (\CreateMultipartUpload' {objectLockRetainUntilDate} -> objectLockRetainUntilDate) (\s@CreateMultipartUpload' {} a -> s {objectLockRetainUntilDate = a} :: CreateMultipartUpload) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
createMultipartUpload_requestPayer :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe RequestPayer)
createMultipartUpload_requestPayer = Lens.lens (\CreateMultipartUpload' {requestPayer} -> requestPayer) (\s@CreateMultipartUpload' {} a -> s {requestPayer = a} :: CreateMultipartUpload)

-- | Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
createMultipartUpload_sSECustomerAlgorithm :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_sSECustomerAlgorithm = Lens.lens (\CreateMultipartUpload' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@CreateMultipartUpload' {} a -> s {sSECustomerAlgorithm = a} :: CreateMultipartUpload)

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
createMultipartUpload_sSECustomerKey :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_sSECustomerKey = Lens.lens (\CreateMultipartUpload' {sSECustomerKey} -> sSECustomerKey) (\s@CreateMultipartUpload' {} a -> s {sSECustomerKey = a} :: CreateMultipartUpload) Prelude.. Lens.mapping Data._Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
createMultipartUpload_sSECustomerKeyMD5 :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_sSECustomerKeyMD5 = Lens.lens (\CreateMultipartUpload' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@CreateMultipartUpload' {} a -> s {sSECustomerKeyMD5 = a} :: CreateMultipartUpload)

-- | Specifies the Amazon Web Services KMS Encryption Context to use for
-- object encryption. The value of this header is a base64-encoded UTF-8
-- string holding JSON with the encryption context key-value pairs.
createMultipartUpload_sSEKMSEncryptionContext :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_sSEKMSEncryptionContext = Lens.lens (\CreateMultipartUpload' {sSEKMSEncryptionContext} -> sSEKMSEncryptionContext) (\s@CreateMultipartUpload' {} a -> s {sSEKMSEncryptionContext = a} :: CreateMultipartUpload) Prelude.. Lens.mapping Data._Sensitive

-- | Specifies the ID of the symmetric customer managed key to use for object
-- encryption. All GET and PUT requests for an object protected by Amazon
-- Web Services KMS will fail if not made via SSL or using SigV4. For
-- information about configuring using any of the officially supported
-- Amazon Web Services SDKs and Amazon Web Services CLI, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication>
-- in the /Amazon S3 User Guide/.
createMultipartUpload_sSEKMSKeyId :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_sSEKMSKeyId = Lens.lens (\CreateMultipartUpload' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@CreateMultipartUpload' {} a -> s {sSEKMSKeyId = a} :: CreateMultipartUpload) Prelude.. Lens.mapping Data._Sensitive

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
createMultipartUpload_serverSideEncryption :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe ServerSideEncryption)
createMultipartUpload_serverSideEncryption = Lens.lens (\CreateMultipartUpload' {serverSideEncryption} -> serverSideEncryption) (\s@CreateMultipartUpload' {} a -> s {serverSideEncryption = a} :: CreateMultipartUpload)

-- | By default, Amazon S3 uses the STANDARD Storage Class to store newly
-- created objects. The STANDARD storage class provides high durability and
-- high availability. Depending on performance needs, you can specify a
-- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
-- Storage Class. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 User Guide/.
createMultipartUpload_storageClass :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe StorageClass)
createMultipartUpload_storageClass = Lens.lens (\CreateMultipartUpload' {storageClass} -> storageClass) (\s@CreateMultipartUpload' {} a -> s {storageClass = a} :: CreateMultipartUpload)

-- | The tag-set for the object. The tag-set must be encoded as URL Query
-- parameters.
createMultipartUpload_tagging :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_tagging = Lens.lens (\CreateMultipartUpload' {tagging} -> tagging) (\s@CreateMultipartUpload' {} a -> s {tagging = a} :: CreateMultipartUpload)

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
createMultipartUpload_websiteRedirectLocation :: Lens.Lens' CreateMultipartUpload (Prelude.Maybe Prelude.Text)
createMultipartUpload_websiteRedirectLocation = Lens.lens (\CreateMultipartUpload' {websiteRedirectLocation} -> websiteRedirectLocation) (\s@CreateMultipartUpload' {} a -> s {websiteRedirectLocation = a} :: CreateMultipartUpload)

-- | The name of the bucket to which to initiate the upload
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
-- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
-- When using this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
-- in the /Amazon S3 User Guide/.
createMultipartUpload_bucket :: Lens.Lens' CreateMultipartUpload BucketName
createMultipartUpload_bucket = Lens.lens (\CreateMultipartUpload' {bucket} -> bucket) (\s@CreateMultipartUpload' {} a -> s {bucket = a} :: CreateMultipartUpload)

-- | Object key for which the multipart upload is to be initiated.
createMultipartUpload_key :: Lens.Lens' CreateMultipartUpload ObjectKey
createMultipartUpload_key = Lens.lens (\CreateMultipartUpload' {key} -> key) (\s@CreateMultipartUpload' {} a -> s {key = a} :: CreateMultipartUpload)

instance Core.AWSRequest CreateMultipartUpload where
  type
    AWSResponse CreateMultipartUpload =
      CreateMultipartUploadResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.post (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateMultipartUploadResponse'
            Prelude.<$> (h Data..#? "x-amz-abort-date")
            Prelude.<*> (h Data..#? "x-amz-abort-rule-id")
            Prelude.<*> (x Data..@? "Bucket")
            Prelude.<*> ( h
                            Data..#? "x-amz-server-side-encryption-bucket-key-enabled"
                        )
            Prelude.<*> (h Data..#? "x-amz-checksum-algorithm")
            Prelude.<*> (x Data..@? "Key")
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
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "UploadId")
      )

instance Prelude.Hashable CreateMultipartUpload where
  hashWithSalt _salt CreateMultipartUpload' {..} =
    _salt `Prelude.hashWithSalt` acl
      `Prelude.hashWithSalt` bucketKeyEnabled
      `Prelude.hashWithSalt` cacheControl
      `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` contentDisposition
      `Prelude.hashWithSalt` contentEncoding
      `Prelude.hashWithSalt` contentLanguage
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` expires
      `Prelude.hashWithSalt` grantFullControl
      `Prelude.hashWithSalt` grantRead
      `Prelude.hashWithSalt` grantReadACP
      `Prelude.hashWithSalt` grantWriteACP
      `Prelude.hashWithSalt` metadata
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
      `Prelude.hashWithSalt` websiteRedirectLocation
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData CreateMultipartUpload where
  rnf CreateMultipartUpload' {..} =
    Prelude.rnf acl
      `Prelude.seq` Prelude.rnf bucketKeyEnabled
      `Prelude.seq` Prelude.rnf cacheControl
      `Prelude.seq` Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf contentDisposition
      `Prelude.seq` Prelude.rnf contentEncoding
      `Prelude.seq` Prelude.rnf contentLanguage
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf expires
      `Prelude.seq` Prelude.rnf grantFullControl
      `Prelude.seq` Prelude.rnf grantRead
      `Prelude.seq` Prelude.rnf grantReadACP
      `Prelude.seq` Prelude.rnf grantWriteACP
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf objectLockLegalHoldStatus
      `Prelude.seq` Prelude.rnf objectLockMode
      `Prelude.seq` Prelude.rnf
        objectLockRetainUntilDate
      `Prelude.seq` Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf
        sSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf sSECustomerKey
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
        websiteRedirectLocation
      `Prelude.seq` Prelude.rnf
        bucket
      `Prelude.seq` Prelude.rnf
        key

instance Data.ToHeaders CreateMultipartUpload where
  toHeaders CreateMultipartUpload' {..} =
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

instance Data.ToPath CreateMultipartUpload where
  toPath CreateMultipartUpload' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery CreateMultipartUpload where
  toQuery = Prelude.const (Prelude.mconcat ["uploads"])

-- | /See:/ 'newCreateMultipartUploadResponse' smart constructor.
data CreateMultipartUploadResponse = CreateMultipartUploadResponse'
  { -- | If the bucket has a lifecycle rule configured with an action to abort
    -- incomplete multipart uploads and the prefix in the lifecycle rule
    -- matches the object name in the request, the response includes this
    -- header. The header indicates when the initiated multipart upload becomes
    -- eligible for an abort operation. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy>.
    --
    -- The response also includes the @x-amz-abort-rule-id@ header that
    -- provides the ID of the lifecycle configuration rule that defines this
    -- action.
    abortDate :: Prelude.Maybe Data.RFC822,
    -- | This header is returned along with the @x-amz-abort-date@ header. It
    -- identifies the applicable lifecycle configuration rule that defines the
    -- action to abort incomplete multipart uploads.
    abortRuleId :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket to which the multipart upload was initiated. Does
    -- not return the access point ARN or access point alias if used.
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
    -- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
    -- When using this action with S3 on Outposts through the Amazon Web
    -- Services SDKs, you provide the Outposts bucket ARN in place of the
    -- bucket name. For more information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
    -- in the /Amazon S3 User Guide/.
    bucket :: Prelude.Maybe BucketName,
    -- | Indicates whether the multipart upload uses an S3 Bucket Key for
    -- server-side encryption with Amazon Web Services KMS (SSE-KMS).
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The algorithm that was used to create a checksum of the object.
    checksumAlgorithm :: Prelude.Maybe ChecksumAlgorithm,
    -- | Object key for which the multipart upload was initiated.
    key :: Prelude.Maybe ObjectKey,
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
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | ID for the initiated multipart upload.
    uploadId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMultipartUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'abortDate', 'createMultipartUploadResponse_abortDate' - If the bucket has a lifecycle rule configured with an action to abort
-- incomplete multipart uploads and the prefix in the lifecycle rule
-- matches the object name in the request, the response includes this
-- header. The header indicates when the initiated multipart upload becomes
-- eligible for an abort operation. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy>.
--
-- The response also includes the @x-amz-abort-rule-id@ header that
-- provides the ID of the lifecycle configuration rule that defines this
-- action.
--
-- 'abortRuleId', 'createMultipartUploadResponse_abortRuleId' - This header is returned along with the @x-amz-abort-date@ header. It
-- identifies the applicable lifecycle configuration rule that defines the
-- action to abort incomplete multipart uploads.
--
-- 'bucket', 'createMultipartUploadResponse_bucket' - The name of the bucket to which the multipart upload was initiated. Does
-- not return the access point ARN or access point alias if used.
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
-- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
-- When using this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
-- in the /Amazon S3 User Guide/.
--
-- 'bucketKeyEnabled', 'createMultipartUploadResponse_bucketKeyEnabled' - Indicates whether the multipart upload uses an S3 Bucket Key for
-- server-side encryption with Amazon Web Services KMS (SSE-KMS).
--
-- 'checksumAlgorithm', 'createMultipartUploadResponse_checksumAlgorithm' - The algorithm that was used to create a checksum of the object.
--
-- 'key', 'createMultipartUploadResponse_key' - Object key for which the multipart upload was initiated.
--
-- 'requestCharged', 'createMultipartUploadResponse_requestCharged' - Undocumented member.
--
-- 'sSECustomerAlgorithm', 'createMultipartUploadResponse_sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
--
-- 'sSECustomerKeyMD5', 'createMultipartUploadResponse_sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
--
-- 'sSEKMSEncryptionContext', 'createMultipartUploadResponse_sSEKMSEncryptionContext' - If present, specifies the Amazon Web Services KMS Encryption Context to
-- use for object encryption. The value of this header is a base64-encoded
-- UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- 'sSEKMSKeyId', 'createMultipartUploadResponse_sSEKMSKeyId' - If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key that
-- was used for the object.
--
-- 'serverSideEncryption', 'createMultipartUploadResponse_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
--
-- 'httpStatus', 'createMultipartUploadResponse_httpStatus' - The response's http status code.
--
-- 'uploadId', 'createMultipartUploadResponse_uploadId' - ID for the initiated multipart upload.
newCreateMultipartUploadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'uploadId'
  Prelude.Text ->
  CreateMultipartUploadResponse
newCreateMultipartUploadResponse
  pHttpStatus_
  pUploadId_ =
    CreateMultipartUploadResponse'
      { abortDate =
          Prelude.Nothing,
        abortRuleId = Prelude.Nothing,
        bucket = Prelude.Nothing,
        bucketKeyEnabled = Prelude.Nothing,
        checksumAlgorithm = Prelude.Nothing,
        key = Prelude.Nothing,
        requestCharged = Prelude.Nothing,
        sSECustomerAlgorithm = Prelude.Nothing,
        sSECustomerKeyMD5 = Prelude.Nothing,
        sSEKMSEncryptionContext = Prelude.Nothing,
        sSEKMSKeyId = Prelude.Nothing,
        serverSideEncryption = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        uploadId = pUploadId_
      }

-- | If the bucket has a lifecycle rule configured with an action to abort
-- incomplete multipart uploads and the prefix in the lifecycle rule
-- matches the object name in the request, the response includes this
-- header. The header indicates when the initiated multipart upload becomes
-- eligible for an abort operation. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy>.
--
-- The response also includes the @x-amz-abort-rule-id@ header that
-- provides the ID of the lifecycle configuration rule that defines this
-- action.
createMultipartUploadResponse_abortDate :: Lens.Lens' CreateMultipartUploadResponse (Prelude.Maybe Prelude.UTCTime)
createMultipartUploadResponse_abortDate = Lens.lens (\CreateMultipartUploadResponse' {abortDate} -> abortDate) (\s@CreateMultipartUploadResponse' {} a -> s {abortDate = a} :: CreateMultipartUploadResponse) Prelude.. Lens.mapping Data._Time

-- | This header is returned along with the @x-amz-abort-date@ header. It
-- identifies the applicable lifecycle configuration rule that defines the
-- action to abort incomplete multipart uploads.
createMultipartUploadResponse_abortRuleId :: Lens.Lens' CreateMultipartUploadResponse (Prelude.Maybe Prelude.Text)
createMultipartUploadResponse_abortRuleId = Lens.lens (\CreateMultipartUploadResponse' {abortRuleId} -> abortRuleId) (\s@CreateMultipartUploadResponse' {} a -> s {abortRuleId = a} :: CreateMultipartUploadResponse)

-- | The name of the bucket to which the multipart upload was initiated. Does
-- not return the access point ARN or access point alias if used.
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
-- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
-- When using this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
-- in the /Amazon S3 User Guide/.
createMultipartUploadResponse_bucket :: Lens.Lens' CreateMultipartUploadResponse (Prelude.Maybe BucketName)
createMultipartUploadResponse_bucket = Lens.lens (\CreateMultipartUploadResponse' {bucket} -> bucket) (\s@CreateMultipartUploadResponse' {} a -> s {bucket = a} :: CreateMultipartUploadResponse)

-- | Indicates whether the multipart upload uses an S3 Bucket Key for
-- server-side encryption with Amazon Web Services KMS (SSE-KMS).
createMultipartUploadResponse_bucketKeyEnabled :: Lens.Lens' CreateMultipartUploadResponse (Prelude.Maybe Prelude.Bool)
createMultipartUploadResponse_bucketKeyEnabled = Lens.lens (\CreateMultipartUploadResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@CreateMultipartUploadResponse' {} a -> s {bucketKeyEnabled = a} :: CreateMultipartUploadResponse)

-- | The algorithm that was used to create a checksum of the object.
createMultipartUploadResponse_checksumAlgorithm :: Lens.Lens' CreateMultipartUploadResponse (Prelude.Maybe ChecksumAlgorithm)
createMultipartUploadResponse_checksumAlgorithm = Lens.lens (\CreateMultipartUploadResponse' {checksumAlgorithm} -> checksumAlgorithm) (\s@CreateMultipartUploadResponse' {} a -> s {checksumAlgorithm = a} :: CreateMultipartUploadResponse)

-- | Object key for which the multipart upload was initiated.
createMultipartUploadResponse_key :: Lens.Lens' CreateMultipartUploadResponse (Prelude.Maybe ObjectKey)
createMultipartUploadResponse_key = Lens.lens (\CreateMultipartUploadResponse' {key} -> key) (\s@CreateMultipartUploadResponse' {} a -> s {key = a} :: CreateMultipartUploadResponse)

-- | Undocumented member.
createMultipartUploadResponse_requestCharged :: Lens.Lens' CreateMultipartUploadResponse (Prelude.Maybe RequestCharged)
createMultipartUploadResponse_requestCharged = Lens.lens (\CreateMultipartUploadResponse' {requestCharged} -> requestCharged) (\s@CreateMultipartUploadResponse' {} a -> s {requestCharged = a} :: CreateMultipartUploadResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
createMultipartUploadResponse_sSECustomerAlgorithm :: Lens.Lens' CreateMultipartUploadResponse (Prelude.Maybe Prelude.Text)
createMultipartUploadResponse_sSECustomerAlgorithm = Lens.lens (\CreateMultipartUploadResponse' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@CreateMultipartUploadResponse' {} a -> s {sSECustomerAlgorithm = a} :: CreateMultipartUploadResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
createMultipartUploadResponse_sSECustomerKeyMD5 :: Lens.Lens' CreateMultipartUploadResponse (Prelude.Maybe Prelude.Text)
createMultipartUploadResponse_sSECustomerKeyMD5 = Lens.lens (\CreateMultipartUploadResponse' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@CreateMultipartUploadResponse' {} a -> s {sSECustomerKeyMD5 = a} :: CreateMultipartUploadResponse)

-- | If present, specifies the Amazon Web Services KMS Encryption Context to
-- use for object encryption. The value of this header is a base64-encoded
-- UTF-8 string holding JSON with the encryption context key-value pairs.
createMultipartUploadResponse_sSEKMSEncryptionContext :: Lens.Lens' CreateMultipartUploadResponse (Prelude.Maybe Prelude.Text)
createMultipartUploadResponse_sSEKMSEncryptionContext = Lens.lens (\CreateMultipartUploadResponse' {sSEKMSEncryptionContext} -> sSEKMSEncryptionContext) (\s@CreateMultipartUploadResponse' {} a -> s {sSEKMSEncryptionContext = a} :: CreateMultipartUploadResponse) Prelude.. Lens.mapping Data._Sensitive

-- | If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key that
-- was used for the object.
createMultipartUploadResponse_sSEKMSKeyId :: Lens.Lens' CreateMultipartUploadResponse (Prelude.Maybe Prelude.Text)
createMultipartUploadResponse_sSEKMSKeyId = Lens.lens (\CreateMultipartUploadResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@CreateMultipartUploadResponse' {} a -> s {sSEKMSKeyId = a} :: CreateMultipartUploadResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
createMultipartUploadResponse_serverSideEncryption :: Lens.Lens' CreateMultipartUploadResponse (Prelude.Maybe ServerSideEncryption)
createMultipartUploadResponse_serverSideEncryption = Lens.lens (\CreateMultipartUploadResponse' {serverSideEncryption} -> serverSideEncryption) (\s@CreateMultipartUploadResponse' {} a -> s {serverSideEncryption = a} :: CreateMultipartUploadResponse)

-- | The response's http status code.
createMultipartUploadResponse_httpStatus :: Lens.Lens' CreateMultipartUploadResponse Prelude.Int
createMultipartUploadResponse_httpStatus = Lens.lens (\CreateMultipartUploadResponse' {httpStatus} -> httpStatus) (\s@CreateMultipartUploadResponse' {} a -> s {httpStatus = a} :: CreateMultipartUploadResponse)

-- | ID for the initiated multipart upload.
createMultipartUploadResponse_uploadId :: Lens.Lens' CreateMultipartUploadResponse Prelude.Text
createMultipartUploadResponse_uploadId = Lens.lens (\CreateMultipartUploadResponse' {uploadId} -> uploadId) (\s@CreateMultipartUploadResponse' {} a -> s {uploadId = a} :: CreateMultipartUploadResponse)

instance Prelude.NFData CreateMultipartUploadResponse where
  rnf CreateMultipartUploadResponse' {..} =
    Prelude.rnf abortDate
      `Prelude.seq` Prelude.rnf abortRuleId
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf bucketKeyEnabled
      `Prelude.seq` Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf requestCharged
      `Prelude.seq` Prelude.rnf sSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf sSECustomerKeyMD5
      `Prelude.seq` Prelude.rnf sSEKMSEncryptionContext
      `Prelude.seq` Prelude.rnf sSEKMSKeyId
      `Prelude.seq` Prelude.rnf serverSideEncryption
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf uploadId
