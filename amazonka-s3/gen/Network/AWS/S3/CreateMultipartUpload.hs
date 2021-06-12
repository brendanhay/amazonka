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
-- Module      : Network.AWS.S3.CreateMultipartUpload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates a multipart upload and returns an upload ID.
-- This upload ID is used to associate all of the parts in the specific
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
-- upload becomes eligible for an abort operation and Amazon S3 aborts the
-- multipart upload. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy>.
--
-- For information about the permissions required to use the multipart
-- upload API, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions>.
--
-- For request signing, multipart upload is just a series of regular
-- requests. You initiate a multipart upload, send one or more requests to
-- upload parts, and then complete the multipart upload process. You sign
-- each request individually. There is nothing special about signing
-- multipart upload requests. For more information about signing, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html Authenticating Requests (AWS Signature Version 4)>.
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
-- own encryption key, or use AWS Key Management Service (AWS KMS) customer
-- master keys (CMKs) or Amazon S3-managed encryption keys. If you choose
-- to provide your own encryption key, the request headers you provide in
-- <AmazonS3/latest/API/API_UploadPart.html UploadPart> and
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPartCopy.html UploadPartCopy>
-- requests must match the headers you used in the request to initiate the
-- upload by using @CreateMultipartUpload@.
--
-- To perform a multipart upload with encryption using an AWS KMS CMK, the
-- requester must have permission to the @kms:Encrypt@, @kms:Decrypt@,
-- @kms:ReEncrypt*@, @kms:GenerateDataKey*@, and @kms:DescribeKey@ actions
-- on the key. These permissions are required because Amazon S3 must
-- decrypt and read data from the encrypted file parts before it completes
-- the multipart upload.
--
-- If your AWS Identity and Access Management (IAM) user or role is in the
-- same AWS account as the AWS KMS CMK, then you must have these
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
--     option you use depends on whether you want to use AWS managed
--     encryption keys or provide your own encryption key.
--
--     -   Use encryption keys managed by Amazon S3 or customer master keys
--         (CMKs) stored in AWS Key Management Service (AWS KMS) – If you
--         want AWS to manage the keys used to encrypt data, specify the
--         following headers in the request.
--
--         -   x-amz-server-side-encryption
--
--         -   x-amz-server-side-encryption-aws-kms-key-id
--
--         -   x-amz-server-side-encryption-context
--
--         If you specify @x-amz-server-side-encryption:aws:kms@, but
--         don\'t provide @x-amz-server-side-encryption-aws-kms-key-id@,
--         Amazon S3 uses the AWS managed CMK in AWS KMS to protect the
--         data.
--
--         All GET and PUT requests for an object protected by AWS KMS fail
--         if you don\'t make them with SSL or by using SigV4.
--
--         For more information about server-side encryption with CMKs
--         stored in AWS KMS (SSE-KMS), see
--         <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting Data Using Server-Side Encryption with CMKs stored in AWS KMS>.
--
--     -   Use customer-provided encryption keys – If you want to manage
--         your own encryption keys, provide all the following headers in
--         the request.
--
--         -   x-amz-server-side-encryption-customer-algorithm
--
--         -   x-amz-server-side-encryption-customer-key
--
--         -   x-amz-server-side-encryption-customer-key-MD5
--
--         For more information about server-side encryption with CMKs
--         stored in AWS KMS (SSE-KMS), see
--         <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting Data Using Server-Side Encryption with CMKs stored in AWS KMS>.
--
-- [Access-Control-List (ACL)-Specific Request Headers]
--     You also can use the following access control–related headers with
--     this operation. By default, all objects are private. Only the owner
--     has full access control. When adding a new object, you can grant
--     permissions to individual AWS accounts or to predefined groups
--     defined by Amazon S3. These permissions are then added to the access
--     control list (ACL) on the object. For more information, see
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
--         access permissions to specific AWS accounts or groups, use the
--         following headers. Each header maps to specific permissions that
--         Amazon S3 supports in an ACL. For more information, see
--         <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview>.
--         In the header, you specify a list of grantees who get the
--         specific permission. To grant permissions explicitly, use:
--
--         -   x-amz-grant-read
--
--         -   x-amz-grant-write
--
--         -   x-amz-grant-read-acp
--
--         -   x-amz-grant-write-acp
--
--         -   x-amz-grant-full-control
--
--         You specify each grantee as a type=value pair, where the type is
--         one of the following:
--
--         -   @id@ – if the value specified is the canonical user ID of an
--             AWS account
--
--         -   @uri@ – if you are granting permissions to a predefined
--             group
--
--         -   @emailAddress@ – if the value specified is the email address
--             of an AWS account
--
--             Using email addresses to specify a grantee is only supported
--             in the following AWS Regions:
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
--             in the AWS General Reference.
--
--         For example, the following @x-amz-grant-read@ header grants the
--         AWS accounts identified by account IDs permissions to read
--         object data and its metadata:
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
module Network.AWS.S3.CreateMultipartUpload
  ( -- * Creating a Request
    CreateMultipartUpload (..),
    newCreateMultipartUpload,

    -- * Request Lenses
    createMultipartUpload_websiteRedirectLocation,
    createMultipartUpload_grantRead,
    createMultipartUpload_contentType,
    createMultipartUpload_expectedBucketOwner,
    createMultipartUpload_contentDisposition,
    createMultipartUpload_contentLanguage,
    createMultipartUpload_sSEKMSEncryptionContext,
    createMultipartUpload_metadata,
    createMultipartUpload_contentEncoding,
    createMultipartUpload_sSEKMSKeyId,
    createMultipartUpload_sSECustomerKeyMD5,
    createMultipartUpload_storageClass,
    createMultipartUpload_bucketKeyEnabled,
    createMultipartUpload_grantWriteACP,
    createMultipartUpload_serverSideEncryption,
    createMultipartUpload_objectLockLegalHoldStatus,
    createMultipartUpload_grantReadACP,
    createMultipartUpload_acl,
    createMultipartUpload_sSECustomerAlgorithm,
    createMultipartUpload_requestPayer,
    createMultipartUpload_sSECustomerKey,
    createMultipartUpload_cacheControl,
    createMultipartUpload_expires,
    createMultipartUpload_objectLockMode,
    createMultipartUpload_objectLockRetainUntilDate,
    createMultipartUpload_tagging,
    createMultipartUpload_grantFullControl,
    createMultipartUpload_bucket,
    createMultipartUpload_key,

    -- * Destructuring the Response
    CreateMultipartUploadResponse (..),
    newCreateMultipartUploadResponse,

    -- * Response Lenses
    createMultipartUploadResponse_requestCharged,
    createMultipartUploadResponse_key,
    createMultipartUploadResponse_uploadId,
    createMultipartUploadResponse_abortDate,
    createMultipartUploadResponse_sSEKMSEncryptionContext,
    createMultipartUploadResponse_sSEKMSKeyId,
    createMultipartUploadResponse_sSECustomerKeyMD5,
    createMultipartUploadResponse_bucketKeyEnabled,
    createMultipartUploadResponse_serverSideEncryption,
    createMultipartUploadResponse_abortRuleId,
    createMultipartUploadResponse_sSECustomerAlgorithm,
    createMultipartUploadResponse_bucket,
    createMultipartUploadResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newCreateMultipartUpload' smart constructor.
data CreateMultipartUpload = CreateMultipartUpload'
  { -- | If the bucket is configured as a website, redirects requests for this
    -- object to another object in the same bucket or to an external URL.
    -- Amazon S3 stores the value of this header in the object metadata.
    websiteRedirectLocation :: Core.Maybe Core.Text,
    -- | Allows grantee to read the object data and its metadata.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantRead :: Core.Maybe Core.Text,
    -- | A standard MIME type describing the format of the object data.
    contentType :: Core.Maybe Core.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | Specifies presentational information for the object.
    contentDisposition :: Core.Maybe Core.Text,
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
    -- | Specifies the ID of the symmetric customer managed AWS KMS CMK to use
    -- for object encryption. All GET and PUT requests for an object protected
    -- by AWS KMS will fail if not made via SSL or using SigV4. For information
    -- about configuring using any of the officially supported AWS SDKs and AWS
    -- CLI, see
    -- <https://docs.aws.amazon.com/http:/docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication>
    -- in the /Amazon S3 Developer Guide/.
    sSEKMSKeyId :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
    -- 1321. Amazon S3 uses this header for a message integrity check to ensure
    -- that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Core.Maybe Core.Text,
    -- | By default, Amazon S3 uses the STANDARD Storage Class to store newly
    -- created objects. The STANDARD storage class provides high durability and
    -- high availability. Depending on performance needs, you can specify a
    -- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
    -- Storage Class. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
    -- in the /Amazon S3 Service Developer Guide/.
    storageClass :: Core.Maybe StorageClass,
    -- | Specifies whether Amazon S3 should use an S3 Bucket Key for object
    -- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
    -- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
    -- object encryption with SSE-KMS.
    --
    -- Specifying this header with an object operation doesn’t affect
    -- bucket-level settings for S3 Bucket Key.
    bucketKeyEnabled :: Core.Maybe Core.Bool,
    -- | Allows grantee to write the ACL for the applicable object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantWriteACP :: Core.Maybe Core.Text,
    -- | The server-side encryption algorithm used when storing this object in
    -- Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Core.Maybe ServerSideEncryption,
    -- | Specifies whether you want to apply a Legal Hold to the uploaded object.
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
    -- | Specifies the Object Lock mode that you want to apply to the uploaded
    -- object.
    objectLockMode :: Core.Maybe ObjectLockMode,
    -- | Specifies the date and time when you want the Object Lock to expire.
    objectLockRetainUntilDate :: Core.Maybe Core.ISO8601,
    -- | The tag-set for the object. The tag-set must be encoded as URL Query
    -- parameters.
    tagging :: Core.Maybe Core.Text,
    -- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
    -- object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantFullControl :: Core.Maybe Core.Text,
    -- | The name of the bucket to which to initiate the upload
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
    -- | Object key for which the multipart upload is to be initiated.
    key :: ObjectKey
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateMultipartUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'websiteRedirectLocation', 'createMultipartUpload_websiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
--
-- 'grantRead', 'createMultipartUpload_grantRead' - Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'contentType', 'createMultipartUpload_contentType' - A standard MIME type describing the format of the object data.
--
-- 'expectedBucketOwner', 'createMultipartUpload_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'contentDisposition', 'createMultipartUpload_contentDisposition' - Specifies presentational information for the object.
--
-- 'contentLanguage', 'createMultipartUpload_contentLanguage' - The language the content is in.
--
-- 'sSEKMSEncryptionContext', 'createMultipartUpload_sSEKMSEncryptionContext' - Specifies the AWS KMS Encryption Context to use for object encryption.
-- The value of this header is a base64-encoded UTF-8 string holding JSON
-- with the encryption context key-value pairs.
--
-- 'metadata', 'createMultipartUpload_metadata' - A map of metadata to store with the object in S3.
--
-- 'contentEncoding', 'createMultipartUpload_contentEncoding' - Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
--
-- 'sSEKMSKeyId', 'createMultipartUpload_sSEKMSKeyId' - Specifies the ID of the symmetric customer managed AWS KMS CMK to use
-- for object encryption. All GET and PUT requests for an object protected
-- by AWS KMS will fail if not made via SSL or using SigV4. For information
-- about configuring using any of the officially supported AWS SDKs and AWS
-- CLI, see
-- <https://docs.aws.amazon.com/http:/docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication>
-- in the /Amazon S3 Developer Guide/.
--
-- 'sSECustomerKeyMD5', 'createMultipartUpload_sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'storageClass', 'createMultipartUpload_storageClass' - By default, Amazon S3 uses the STANDARD Storage Class to store newly
-- created objects. The STANDARD storage class provides high durability and
-- high availability. Depending on performance needs, you can specify a
-- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
-- Storage Class. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 Service Developer Guide/.
--
-- 'bucketKeyEnabled', 'createMultipartUpload_bucketKeyEnabled' - Specifies whether Amazon S3 should use an S3 Bucket Key for object
-- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
-- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
-- object encryption with SSE-KMS.
--
-- Specifying this header with an object operation doesn’t affect
-- bucket-level settings for S3 Bucket Key.
--
-- 'grantWriteACP', 'createMultipartUpload_grantWriteACP' - Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'serverSideEncryption', 'createMultipartUpload_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
--
-- 'objectLockLegalHoldStatus', 'createMultipartUpload_objectLockLegalHoldStatus' - Specifies whether you want to apply a Legal Hold to the uploaded object.
--
-- 'grantReadACP', 'createMultipartUpload_grantReadACP' - Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'acl', 'createMultipartUpload_acl' - The canned ACL to apply to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'sSECustomerAlgorithm', 'createMultipartUpload_sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
--
-- 'requestPayer', 'createMultipartUpload_requestPayer' - Undocumented member.
--
-- 'sSECustomerKey', 'createMultipartUpload_sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- 'cacheControl', 'createMultipartUpload_cacheControl' - Specifies caching behavior along the request\/reply chain.
--
-- 'expires', 'createMultipartUpload_expires' - The date and time at which the object is no longer cacheable.
--
-- 'objectLockMode', 'createMultipartUpload_objectLockMode' - Specifies the Object Lock mode that you want to apply to the uploaded
-- object.
--
-- 'objectLockRetainUntilDate', 'createMultipartUpload_objectLockRetainUntilDate' - Specifies the date and time when you want the Object Lock to expire.
--
-- 'tagging', 'createMultipartUpload_tagging' - The tag-set for the object. The tag-set must be encoded as URL Query
-- parameters.
--
-- 'grantFullControl', 'createMultipartUpload_grantFullControl' - Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'bucket', 'createMultipartUpload_bucket' - The name of the bucket to which to initiate the upload
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
-- 'key', 'createMultipartUpload_key' - Object key for which the multipart upload is to be initiated.
newCreateMultipartUpload ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  CreateMultipartUpload
newCreateMultipartUpload pBucket_ pKey_ =
  CreateMultipartUpload'
    { websiteRedirectLocation =
        Core.Nothing,
      grantRead = Core.Nothing,
      contentType = Core.Nothing,
      expectedBucketOwner = Core.Nothing,
      contentDisposition = Core.Nothing,
      contentLanguage = Core.Nothing,
      sSEKMSEncryptionContext = Core.Nothing,
      metadata = Core.mempty,
      contentEncoding = Core.Nothing,
      sSEKMSKeyId = Core.Nothing,
      sSECustomerKeyMD5 = Core.Nothing,
      storageClass = Core.Nothing,
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
      bucket = pBucket_,
      key = pKey_
    }

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
createMultipartUpload_websiteRedirectLocation :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_websiteRedirectLocation = Lens.lens (\CreateMultipartUpload' {websiteRedirectLocation} -> websiteRedirectLocation) (\s@CreateMultipartUpload' {} a -> s {websiteRedirectLocation = a} :: CreateMultipartUpload)

-- | Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
createMultipartUpload_grantRead :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_grantRead = Lens.lens (\CreateMultipartUpload' {grantRead} -> grantRead) (\s@CreateMultipartUpload' {} a -> s {grantRead = a} :: CreateMultipartUpload)

-- | A standard MIME type describing the format of the object data.
createMultipartUpload_contentType :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_contentType = Lens.lens (\CreateMultipartUpload' {contentType} -> contentType) (\s@CreateMultipartUpload' {} a -> s {contentType = a} :: CreateMultipartUpload)

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
createMultipartUpload_expectedBucketOwner :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_expectedBucketOwner = Lens.lens (\CreateMultipartUpload' {expectedBucketOwner} -> expectedBucketOwner) (\s@CreateMultipartUpload' {} a -> s {expectedBucketOwner = a} :: CreateMultipartUpload)

-- | Specifies presentational information for the object.
createMultipartUpload_contentDisposition :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_contentDisposition = Lens.lens (\CreateMultipartUpload' {contentDisposition} -> contentDisposition) (\s@CreateMultipartUpload' {} a -> s {contentDisposition = a} :: CreateMultipartUpload)

-- | The language the content is in.
createMultipartUpload_contentLanguage :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_contentLanguage = Lens.lens (\CreateMultipartUpload' {contentLanguage} -> contentLanguage) (\s@CreateMultipartUpload' {} a -> s {contentLanguage = a} :: CreateMultipartUpload)

-- | Specifies the AWS KMS Encryption Context to use for object encryption.
-- The value of this header is a base64-encoded UTF-8 string holding JSON
-- with the encryption context key-value pairs.
createMultipartUpload_sSEKMSEncryptionContext :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_sSEKMSEncryptionContext = Lens.lens (\CreateMultipartUpload' {sSEKMSEncryptionContext} -> sSEKMSEncryptionContext) (\s@CreateMultipartUpload' {} a -> s {sSEKMSEncryptionContext = a} :: CreateMultipartUpload) Core.. Lens.mapping Core._Sensitive

-- | A map of metadata to store with the object in S3.
createMultipartUpload_metadata :: Lens.Lens' CreateMultipartUpload (Core.HashMap Core.Text Core.Text)
createMultipartUpload_metadata = Lens.lens (\CreateMultipartUpload' {metadata} -> metadata) (\s@CreateMultipartUpload' {} a -> s {metadata = a} :: CreateMultipartUpload) Core.. Lens._Coerce

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
createMultipartUpload_contentEncoding :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_contentEncoding = Lens.lens (\CreateMultipartUpload' {contentEncoding} -> contentEncoding) (\s@CreateMultipartUpload' {} a -> s {contentEncoding = a} :: CreateMultipartUpload)

-- | Specifies the ID of the symmetric customer managed AWS KMS CMK to use
-- for object encryption. All GET and PUT requests for an object protected
-- by AWS KMS will fail if not made via SSL or using SigV4. For information
-- about configuring using any of the officially supported AWS SDKs and AWS
-- CLI, see
-- <https://docs.aws.amazon.com/http:/docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication>
-- in the /Amazon S3 Developer Guide/.
createMultipartUpload_sSEKMSKeyId :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_sSEKMSKeyId = Lens.lens (\CreateMultipartUpload' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@CreateMultipartUpload' {} a -> s {sSEKMSKeyId = a} :: CreateMultipartUpload) Core.. Lens.mapping Core._Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
createMultipartUpload_sSECustomerKeyMD5 :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_sSECustomerKeyMD5 = Lens.lens (\CreateMultipartUpload' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@CreateMultipartUpload' {} a -> s {sSECustomerKeyMD5 = a} :: CreateMultipartUpload)

-- | By default, Amazon S3 uses the STANDARD Storage Class to store newly
-- created objects. The STANDARD storage class provides high durability and
-- high availability. Depending on performance needs, you can specify a
-- different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS
-- Storage Class. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>
-- in the /Amazon S3 Service Developer Guide/.
createMultipartUpload_storageClass :: Lens.Lens' CreateMultipartUpload (Core.Maybe StorageClass)
createMultipartUpload_storageClass = Lens.lens (\CreateMultipartUpload' {storageClass} -> storageClass) (\s@CreateMultipartUpload' {} a -> s {storageClass = a} :: CreateMultipartUpload)

-- | Specifies whether Amazon S3 should use an S3 Bucket Key for object
-- encryption with server-side encryption using AWS KMS (SSE-KMS). Setting
-- this header to @true@ causes Amazon S3 to use an S3 Bucket Key for
-- object encryption with SSE-KMS.
--
-- Specifying this header with an object operation doesn’t affect
-- bucket-level settings for S3 Bucket Key.
createMultipartUpload_bucketKeyEnabled :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Bool)
createMultipartUpload_bucketKeyEnabled = Lens.lens (\CreateMultipartUpload' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@CreateMultipartUpload' {} a -> s {bucketKeyEnabled = a} :: CreateMultipartUpload)

-- | Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
createMultipartUpload_grantWriteACP :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_grantWriteACP = Lens.lens (\CreateMultipartUpload' {grantWriteACP} -> grantWriteACP) (\s@CreateMultipartUpload' {} a -> s {grantWriteACP = a} :: CreateMultipartUpload)

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
createMultipartUpload_serverSideEncryption :: Lens.Lens' CreateMultipartUpload (Core.Maybe ServerSideEncryption)
createMultipartUpload_serverSideEncryption = Lens.lens (\CreateMultipartUpload' {serverSideEncryption} -> serverSideEncryption) (\s@CreateMultipartUpload' {} a -> s {serverSideEncryption = a} :: CreateMultipartUpload)

-- | Specifies whether you want to apply a Legal Hold to the uploaded object.
createMultipartUpload_objectLockLegalHoldStatus :: Lens.Lens' CreateMultipartUpload (Core.Maybe ObjectLockLegalHoldStatus)
createMultipartUpload_objectLockLegalHoldStatus = Lens.lens (\CreateMultipartUpload' {objectLockLegalHoldStatus} -> objectLockLegalHoldStatus) (\s@CreateMultipartUpload' {} a -> s {objectLockLegalHoldStatus = a} :: CreateMultipartUpload)

-- | Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
createMultipartUpload_grantReadACP :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_grantReadACP = Lens.lens (\CreateMultipartUpload' {grantReadACP} -> grantReadACP) (\s@CreateMultipartUpload' {} a -> s {grantReadACP = a} :: CreateMultipartUpload)

-- | The canned ACL to apply to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
createMultipartUpload_acl :: Lens.Lens' CreateMultipartUpload (Core.Maybe ObjectCannedACL)
createMultipartUpload_acl = Lens.lens (\CreateMultipartUpload' {acl} -> acl) (\s@CreateMultipartUpload' {} a -> s {acl = a} :: CreateMultipartUpload)

-- | Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
createMultipartUpload_sSECustomerAlgorithm :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_sSECustomerAlgorithm = Lens.lens (\CreateMultipartUpload' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@CreateMultipartUpload' {} a -> s {sSECustomerAlgorithm = a} :: CreateMultipartUpload)

-- | Undocumented member.
createMultipartUpload_requestPayer :: Lens.Lens' CreateMultipartUpload (Core.Maybe RequestPayer)
createMultipartUpload_requestPayer = Lens.lens (\CreateMultipartUpload' {requestPayer} -> requestPayer) (\s@CreateMultipartUpload' {} a -> s {requestPayer = a} :: CreateMultipartUpload)

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
createMultipartUpload_sSECustomerKey :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_sSECustomerKey = Lens.lens (\CreateMultipartUpload' {sSECustomerKey} -> sSECustomerKey) (\s@CreateMultipartUpload' {} a -> s {sSECustomerKey = a} :: CreateMultipartUpload) Core.. Lens.mapping Core._Sensitive

-- | Specifies caching behavior along the request\/reply chain.
createMultipartUpload_cacheControl :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_cacheControl = Lens.lens (\CreateMultipartUpload' {cacheControl} -> cacheControl) (\s@CreateMultipartUpload' {} a -> s {cacheControl = a} :: CreateMultipartUpload)

-- | The date and time at which the object is no longer cacheable.
createMultipartUpload_expires :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.UTCTime)
createMultipartUpload_expires = Lens.lens (\CreateMultipartUpload' {expires} -> expires) (\s@CreateMultipartUpload' {} a -> s {expires = a} :: CreateMultipartUpload) Core.. Lens.mapping Core._Time

-- | Specifies the Object Lock mode that you want to apply to the uploaded
-- object.
createMultipartUpload_objectLockMode :: Lens.Lens' CreateMultipartUpload (Core.Maybe ObjectLockMode)
createMultipartUpload_objectLockMode = Lens.lens (\CreateMultipartUpload' {objectLockMode} -> objectLockMode) (\s@CreateMultipartUpload' {} a -> s {objectLockMode = a} :: CreateMultipartUpload)

-- | Specifies the date and time when you want the Object Lock to expire.
createMultipartUpload_objectLockRetainUntilDate :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.UTCTime)
createMultipartUpload_objectLockRetainUntilDate = Lens.lens (\CreateMultipartUpload' {objectLockRetainUntilDate} -> objectLockRetainUntilDate) (\s@CreateMultipartUpload' {} a -> s {objectLockRetainUntilDate = a} :: CreateMultipartUpload) Core.. Lens.mapping Core._Time

-- | The tag-set for the object. The tag-set must be encoded as URL Query
-- parameters.
createMultipartUpload_tagging :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_tagging = Lens.lens (\CreateMultipartUpload' {tagging} -> tagging) (\s@CreateMultipartUpload' {} a -> s {tagging = a} :: CreateMultipartUpload)

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
--
-- This action is not supported by Amazon S3 on Outposts.
createMultipartUpload_grantFullControl :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.Text)
createMultipartUpload_grantFullControl = Lens.lens (\CreateMultipartUpload' {grantFullControl} -> grantFullControl) (\s@CreateMultipartUpload' {} a -> s {grantFullControl = a} :: CreateMultipartUpload)

-- | The name of the bucket to which to initiate the upload
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
createMultipartUpload_bucket :: Lens.Lens' CreateMultipartUpload BucketName
createMultipartUpload_bucket = Lens.lens (\CreateMultipartUpload' {bucket} -> bucket) (\s@CreateMultipartUpload' {} a -> s {bucket = a} :: CreateMultipartUpload)

-- | Object key for which the multipart upload is to be initiated.
createMultipartUpload_key :: Lens.Lens' CreateMultipartUpload ObjectKey
createMultipartUpload_key = Lens.lens (\CreateMultipartUpload' {key} -> key) (\s@CreateMultipartUpload' {} a -> s {key = a} :: CreateMultipartUpload)

instance Core.AWSRequest CreateMultipartUpload where
  type
    AWSResponse CreateMultipartUpload =
      CreateMultipartUploadResponse
  request = Request.post defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateMultipartUploadResponse'
            Core.<$> (h Core..#? "x-amz-request-charged")
            Core.<*> (x Core..@? "Key")
            Core.<*> (x Core..@? "UploadId")
            Core.<*> (h Core..#? "x-amz-abort-date")
            Core.<*> (h Core..#? "x-amz-server-side-encryption-context")
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
            Core.<*> (h Core..#? "x-amz-abort-rule-id")
            Core.<*> ( h
                         Core..#? "x-amz-server-side-encryption-customer-algorithm"
                     )
            Core.<*> (x Core..@? "Bucket")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateMultipartUpload

instance Core.NFData CreateMultipartUpload

instance Core.ToHeaders CreateMultipartUpload where
  toHeaders CreateMultipartUpload' {..} =
    Core.mconcat
      [ "x-amz-website-redirect-location"
          Core.=# websiteRedirectLocation,
        "x-amz-grant-read" Core.=# grantRead,
        "Content-Type" Core.=# contentType,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "Content-Disposition" Core.=# contentDisposition,
        "Content-Language" Core.=# contentLanguage,
        "x-amz-server-side-encryption-context"
          Core.=# sSEKMSEncryptionContext,
        "x-amz-meta-" Core.=# metadata,
        "Content-Encoding" Core.=# contentEncoding,
        "x-amz-server-side-encryption-aws-kms-key-id"
          Core.=# sSEKMSKeyId,
        "x-amz-server-side-encryption-customer-key-MD5"
          Core.=# sSECustomerKeyMD5,
        "x-amz-storage-class" Core.=# storageClass,
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
        "x-amz-grant-full-control" Core.=# grantFullControl
      ]

instance Core.ToPath CreateMultipartUpload where
  toPath CreateMultipartUpload' {..} =
    Core.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery CreateMultipartUpload where
  toQuery = Core.const (Core.mconcat ["uploads"])

-- | /See:/ 'newCreateMultipartUploadResponse' smart constructor.
data CreateMultipartUploadResponse = CreateMultipartUploadResponse'
  { requestCharged :: Core.Maybe RequestCharged,
    -- | Object key for which the multipart upload was initiated.
    key :: Core.Maybe ObjectKey,
    -- | ID for the initiated multipart upload.
    uploadId :: Core.Maybe Core.Text,
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
    abortDate :: Core.Maybe Core.ISO8601,
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
    -- | Indicates whether the multipart upload uses an S3 Bucket Key for
    -- server-side encryption with AWS KMS (SSE-KMS).
    bucketKeyEnabled :: Core.Maybe Core.Bool,
    -- | The server-side encryption algorithm used when storing this object in
    -- Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Core.Maybe ServerSideEncryption,
    -- | This header is returned along with the @x-amz-abort-date@ header. It
    -- identifies the applicable lifecycle configuration rule that defines the
    -- action to abort incomplete multipart uploads.
    abortRuleId :: Core.Maybe Core.Text,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header confirming the
    -- encryption algorithm used.
    sSECustomerAlgorithm :: Core.Maybe Core.Text,
    -- | The name of the bucket to which the multipart upload was initiated.
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
    bucket :: Core.Maybe BucketName,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateMultipartUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'createMultipartUploadResponse_requestCharged' - Undocumented member.
--
-- 'key', 'createMultipartUploadResponse_key' - Object key for which the multipart upload was initiated.
--
-- 'uploadId', 'createMultipartUploadResponse_uploadId' - ID for the initiated multipart upload.
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
-- 'sSEKMSEncryptionContext', 'createMultipartUploadResponse_sSEKMSEncryptionContext' - If present, specifies the AWS KMS Encryption Context to use for object
-- encryption. The value of this header is a base64-encoded UTF-8 string
-- holding JSON with the encryption context key-value pairs.
--
-- 'sSEKMSKeyId', 'createMultipartUploadResponse_sSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS)
-- symmetric customer managed customer master key (CMK) that was used for
-- the object.
--
-- 'sSECustomerKeyMD5', 'createMultipartUploadResponse_sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
--
-- 'bucketKeyEnabled', 'createMultipartUploadResponse_bucketKeyEnabled' - Indicates whether the multipart upload uses an S3 Bucket Key for
-- server-side encryption with AWS KMS (SSE-KMS).
--
-- 'serverSideEncryption', 'createMultipartUploadResponse_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
--
-- 'abortRuleId', 'createMultipartUploadResponse_abortRuleId' - This header is returned along with the @x-amz-abort-date@ header. It
-- identifies the applicable lifecycle configuration rule that defines the
-- action to abort incomplete multipart uploads.
--
-- 'sSECustomerAlgorithm', 'createMultipartUploadResponse_sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
--
-- 'bucket', 'createMultipartUploadResponse_bucket' - The name of the bucket to which the multipart upload was initiated.
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
-- 'httpStatus', 'createMultipartUploadResponse_httpStatus' - The response's http status code.
newCreateMultipartUploadResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateMultipartUploadResponse
newCreateMultipartUploadResponse pHttpStatus_ =
  CreateMultipartUploadResponse'
    { requestCharged =
        Core.Nothing,
      key = Core.Nothing,
      uploadId = Core.Nothing,
      abortDate = Core.Nothing,
      sSEKMSEncryptionContext = Core.Nothing,
      sSEKMSKeyId = Core.Nothing,
      sSECustomerKeyMD5 = Core.Nothing,
      bucketKeyEnabled = Core.Nothing,
      serverSideEncryption = Core.Nothing,
      abortRuleId = Core.Nothing,
      sSECustomerAlgorithm = Core.Nothing,
      bucket = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createMultipartUploadResponse_requestCharged :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe RequestCharged)
createMultipartUploadResponse_requestCharged = Lens.lens (\CreateMultipartUploadResponse' {requestCharged} -> requestCharged) (\s@CreateMultipartUploadResponse' {} a -> s {requestCharged = a} :: CreateMultipartUploadResponse)

-- | Object key for which the multipart upload was initiated.
createMultipartUploadResponse_key :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe ObjectKey)
createMultipartUploadResponse_key = Lens.lens (\CreateMultipartUploadResponse' {key} -> key) (\s@CreateMultipartUploadResponse' {} a -> s {key = a} :: CreateMultipartUploadResponse)

-- | ID for the initiated multipart upload.
createMultipartUploadResponse_uploadId :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Core.Text)
createMultipartUploadResponse_uploadId = Lens.lens (\CreateMultipartUploadResponse' {uploadId} -> uploadId) (\s@CreateMultipartUploadResponse' {} a -> s {uploadId = a} :: CreateMultipartUploadResponse)

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
createMultipartUploadResponse_abortDate :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Core.UTCTime)
createMultipartUploadResponse_abortDate = Lens.lens (\CreateMultipartUploadResponse' {abortDate} -> abortDate) (\s@CreateMultipartUploadResponse' {} a -> s {abortDate = a} :: CreateMultipartUploadResponse) Core.. Lens.mapping Core._Time

-- | If present, specifies the AWS KMS Encryption Context to use for object
-- encryption. The value of this header is a base64-encoded UTF-8 string
-- holding JSON with the encryption context key-value pairs.
createMultipartUploadResponse_sSEKMSEncryptionContext :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Core.Text)
createMultipartUploadResponse_sSEKMSEncryptionContext = Lens.lens (\CreateMultipartUploadResponse' {sSEKMSEncryptionContext} -> sSEKMSEncryptionContext) (\s@CreateMultipartUploadResponse' {} a -> s {sSEKMSEncryptionContext = a} :: CreateMultipartUploadResponse) Core.. Lens.mapping Core._Sensitive

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS)
-- symmetric customer managed customer master key (CMK) that was used for
-- the object.
createMultipartUploadResponse_sSEKMSKeyId :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Core.Text)
createMultipartUploadResponse_sSEKMSKeyId = Lens.lens (\CreateMultipartUploadResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@CreateMultipartUploadResponse' {} a -> s {sSEKMSKeyId = a} :: CreateMultipartUploadResponse) Core.. Lens.mapping Core._Sensitive

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
createMultipartUploadResponse_sSECustomerKeyMD5 :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Core.Text)
createMultipartUploadResponse_sSECustomerKeyMD5 = Lens.lens (\CreateMultipartUploadResponse' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@CreateMultipartUploadResponse' {} a -> s {sSECustomerKeyMD5 = a} :: CreateMultipartUploadResponse)

-- | Indicates whether the multipart upload uses an S3 Bucket Key for
-- server-side encryption with AWS KMS (SSE-KMS).
createMultipartUploadResponse_bucketKeyEnabled :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Core.Bool)
createMultipartUploadResponse_bucketKeyEnabled = Lens.lens (\CreateMultipartUploadResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@CreateMultipartUploadResponse' {} a -> s {bucketKeyEnabled = a} :: CreateMultipartUploadResponse)

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
createMultipartUploadResponse_serverSideEncryption :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe ServerSideEncryption)
createMultipartUploadResponse_serverSideEncryption = Lens.lens (\CreateMultipartUploadResponse' {serverSideEncryption} -> serverSideEncryption) (\s@CreateMultipartUploadResponse' {} a -> s {serverSideEncryption = a} :: CreateMultipartUploadResponse)

-- | This header is returned along with the @x-amz-abort-date@ header. It
-- identifies the applicable lifecycle configuration rule that defines the
-- action to abort incomplete multipart uploads.
createMultipartUploadResponse_abortRuleId :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Core.Text)
createMultipartUploadResponse_abortRuleId = Lens.lens (\CreateMultipartUploadResponse' {abortRuleId} -> abortRuleId) (\s@CreateMultipartUploadResponse' {} a -> s {abortRuleId = a} :: CreateMultipartUploadResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
createMultipartUploadResponse_sSECustomerAlgorithm :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Core.Text)
createMultipartUploadResponse_sSECustomerAlgorithm = Lens.lens (\CreateMultipartUploadResponse' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@CreateMultipartUploadResponse' {} a -> s {sSECustomerAlgorithm = a} :: CreateMultipartUploadResponse)

-- | The name of the bucket to which the multipart upload was initiated.
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
createMultipartUploadResponse_bucket :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe BucketName)
createMultipartUploadResponse_bucket = Lens.lens (\CreateMultipartUploadResponse' {bucket} -> bucket) (\s@CreateMultipartUploadResponse' {} a -> s {bucket = a} :: CreateMultipartUploadResponse)

-- | The response's http status code.
createMultipartUploadResponse_httpStatus :: Lens.Lens' CreateMultipartUploadResponse Core.Int
createMultipartUploadResponse_httpStatus = Lens.lens (\CreateMultipartUploadResponse' {httpStatus} -> httpStatus) (\s@CreateMultipartUploadResponse' {} a -> s {httpStatus = a} :: CreateMultipartUploadResponse)

instance Core.NFData CreateMultipartUploadResponse
