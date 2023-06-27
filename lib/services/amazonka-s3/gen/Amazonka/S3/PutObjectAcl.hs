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
-- Module      : Amazonka.S3.PutObjectAcl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uses the @acl@ subresource to set the access control list (ACL)
-- permissions for a new or existing object in an S3 bucket. You must have
-- @WRITE_ACP@ permission to set the ACL of an object. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#permissions What permissions can I grant?>
-- in the /Amazon S3 User Guide/.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- Depending on your application needs, you can choose to set the ACL on an
-- object using either the request body or the headers. For example, if you
-- have an existing application that updates a bucket ACL using the request
-- body, you can continue to use that approach. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview>
-- in the /Amazon S3 User Guide/.
--
-- If your bucket uses the bucket owner enforced setting for S3 Object
-- Ownership, ACLs are disabled and no longer affect permissions. You must
-- use policies to grant access to your bucket and the objects in it.
-- Requests to set ACLs or update ACLs fail and return the
-- @AccessControlListNotSupported@ error code. Requests to read ACLs are
-- still supported. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/about-object-ownership.html Controlling object ownership>
-- in the /Amazon S3 User Guide/.
--
-- [Permissions]
--     You can set access permissions using one of the following methods:
--
--     -   Specify a canned ACL with the @x-amz-acl@ request header. Amazon
--         S3 supports a set of predefined ACLs, known as canned ACLs. Each
--         canned ACL has a predefined set of grantees and permissions.
--         Specify the canned ACL name as the value of @x-amz-ac@l. If you
--         use this header, you cannot use other access control-specific
--         headers in your request. For more information, see
--         <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
--
--     -   Specify access permissions explicitly with the
--         @x-amz-grant-read@, @x-amz-grant-read-acp@,
--         @x-amz-grant-write-acp@, and @x-amz-grant-full-control@ headers.
--         When using these headers, you specify explicit access
--         permissions and grantees (Amazon Web Services accounts or Amazon
--         S3 groups) who will receive the permission. If you use these
--         ACL-specific headers, you cannot use @x-amz-acl@ header to set a
--         canned ACL. These parameters map to the set of permissions that
--         Amazon S3 supports in an ACL. For more information, see
--         <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview>.
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
--         For example, the following @x-amz-grant-read@ header grants list
--         objects permission to the two Amazon Web Services accounts
--         identified by their email addresses.
--
--         @x-amz-grant-read: emailAddress=\"xyz\@amazon.com\", emailAddress=\"abc\@amazon.com\" @
--
--     You can use either a canned ACL or specify access permissions
--     explicitly. You cannot do both.
--
-- [Grantee Values]
--     You can specify the person (grantee) to whom you\'re assigning
--     access rights (using request elements) in the following ways:
--
--     -   By the person\'s ID:
--
--         @\<Grantee xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xsi:type=\"CanonicalUser\">\<ID>\<>ID\<>\<\/ID>\<DisplayName>\<>GranteesEmail\<>\<\/DisplayName> \<\/Grantee>@
--
--         DisplayName is optional and ignored in the request.
--
--     -   By URI:
--
--         @\<Grantee xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xsi:type=\"Group\">\<URI>\<>http:\/\/acs.amazonaws.com\/groups\/global\/AuthenticatedUsers\<>\<\/URI>\<\/Grantee>@
--
--     -   By Email address:
--
--         @\<Grantee xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xsi:type=\"AmazonCustomerByEmail\">\<EmailAddress>\<>Grantees\@email.com\<>\<\/EmailAddress>lt;\/Grantee>@
--
--         The grantee is resolved to the CanonicalUser and, in a response
--         to a GET Object acl request, appears as the CanonicalUser.
--
--         Using email addresses to specify a grantee is only supported in
--         the following Amazon Web Services Regions:
--
--         -   US East (N. Virginia)
--
--         -   US West (N. California)
--
--         -   US West (Oregon)
--
--         -   Asia Pacific (Singapore)
--
--         -   Asia Pacific (Sydney)
--
--         -   Asia Pacific (Tokyo)
--
--         -   Europe (Ireland)
--
--         -   South America (São Paulo)
--
--         For a list of all the Amazon S3 supported Regions and endpoints,
--         see
--         <https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Regions and Endpoints>
--         in the Amazon Web Services General Reference.
--
-- [Versioning]
--     The ACL of an object is set at the object version level. By default,
--     PUT sets the ACL of the current version of an object. To set the ACL
--     of a different version, use the @versionId@ subresource.
--
-- The following operations are related to @PutObjectAcl@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CopyObject.html CopyObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
module Amazonka.S3.PutObjectAcl
  ( -- * Creating a Request
    PutObjectAcl (..),
    newPutObjectAcl,

    -- * Request Lenses
    putObjectAcl_acl,
    putObjectAcl_accessControlPolicy,
    putObjectAcl_checksumAlgorithm,
    putObjectAcl_contentMD5,
    putObjectAcl_expectedBucketOwner,
    putObjectAcl_grantFullControl,
    putObjectAcl_grantRead,
    putObjectAcl_grantReadACP,
    putObjectAcl_grantWrite,
    putObjectAcl_grantWriteACP,
    putObjectAcl_requestPayer,
    putObjectAcl_versionId,
    putObjectAcl_bucket,
    putObjectAcl_key,

    -- * Destructuring the Response
    PutObjectAclResponse (..),
    newPutObjectAclResponse,

    -- * Response Lenses
    putObjectAclResponse_requestCharged,
    putObjectAclResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutObjectAcl' smart constructor.
data PutObjectAcl = PutObjectAcl'
  { -- | The canned ACL to apply to the object. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
    acl :: Prelude.Maybe ObjectCannedACL,
    -- | Contains the elements that set the ACL permissions for an object per
    -- grantee.
    accessControlPolicy :: Prelude.Maybe AccessControlPolicy,
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
    -- | The base64-encoded 128-bit MD5 digest of the data. This header must be
    -- used as a message integrity check to verify that the request body was
    -- not corrupted in transit. For more information, go to
    -- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>>
    --
    -- For requests made using the Amazon Web Services Command Line Interface
    -- (CLI) or Amazon Web Services SDKs, this field is calculated
    -- automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee the read, write, read ACP, and write ACP permissions on
    -- the bucket.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantFullControl :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee to list the objects in the bucket.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantRead :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee to read the bucket ACL.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantReadACP :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee to create new objects in the bucket.
    --
    -- For the bucket and object owners of existing objects, also allows
    -- deletions and overwrites of those objects.
    grantWrite :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee to write the ACL for the applicable bucket.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantWriteACP :: Prelude.Maybe Prelude.Text,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | VersionId used to reference a specific version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The bucket name that contains the object to which you want to attach the
    -- ACL.
    --
    -- When using this action with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this action with an access point through the Amazon Web
    -- Services SDKs, you provide the access point ARN in place of the bucket
    -- name. For more information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName,
    -- | Key for which the PUT action was initiated.
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
    -- When you use this action with Amazon S3 on Outposts, you must direct
    -- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
    -- takes the form
    -- @ @/@AccessPointName@/@-@/@AccountId@/@.@/@outpostID@/@.s3-outposts.@/@Region@/@.amazonaws.com@.
    -- When you use this action with S3 on Outposts through the Amazon Web
    -- Services SDKs, you provide the Outposts access point ARN in place of the
    -- bucket name. For more information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html What is S3 on Outposts>
    -- in the /Amazon S3 User Guide/.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutObjectAcl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acl', 'putObjectAcl_acl' - The canned ACL to apply to the object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
--
-- 'accessControlPolicy', 'putObjectAcl_accessControlPolicy' - Contains the elements that set the ACL permissions for an object per
-- grantee.
--
-- 'checksumAlgorithm', 'putObjectAcl_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
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
-- 'contentMD5', 'putObjectAcl_contentMD5' - The base64-encoded 128-bit MD5 digest of the data. This header must be
-- used as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, go to
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>>
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putObjectAcl_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'grantFullControl', 'putObjectAcl_grantFullControl' - Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'grantRead', 'putObjectAcl_grantRead' - Allows grantee to list the objects in the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'grantReadACP', 'putObjectAcl_grantReadACP' - Allows grantee to read the bucket ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'grantWrite', 'putObjectAcl_grantWrite' - Allows grantee to create new objects in the bucket.
--
-- For the bucket and object owners of existing objects, also allows
-- deletions and overwrites of those objects.
--
-- 'grantWriteACP', 'putObjectAcl_grantWriteACP' - Allows grantee to write the ACL for the applicable bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'requestPayer', 'putObjectAcl_requestPayer' - Undocumented member.
--
-- 'versionId', 'putObjectAcl_versionId' - VersionId used to reference a specific version of the object.
--
-- 'bucket', 'putObjectAcl_bucket' - The bucket name that contains the object to which you want to attach the
-- ACL.
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
-- 'key', 'putObjectAcl_key' - Key for which the PUT action was initiated.
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
-- When you use this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- @ @/@AccessPointName@/@-@/@AccountId@/@.@/@outpostID@/@.s3-outposts.@/@Region@/@.amazonaws.com@.
-- When you use this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts access point ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html What is S3 on Outposts>
-- in the /Amazon S3 User Guide/.
newPutObjectAcl ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  PutObjectAcl
newPutObjectAcl pBucket_ pKey_ =
  PutObjectAcl'
    { acl = Prelude.Nothing,
      accessControlPolicy = Prelude.Nothing,
      checksumAlgorithm = Prelude.Nothing,
      contentMD5 = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      grantFullControl = Prelude.Nothing,
      grantRead = Prelude.Nothing,
      grantReadACP = Prelude.Nothing,
      grantWrite = Prelude.Nothing,
      grantWriteACP = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      versionId = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The canned ACL to apply to the object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
putObjectAcl_acl :: Lens.Lens' PutObjectAcl (Prelude.Maybe ObjectCannedACL)
putObjectAcl_acl = Lens.lens (\PutObjectAcl' {acl} -> acl) (\s@PutObjectAcl' {} a -> s {acl = a} :: PutObjectAcl)

-- | Contains the elements that set the ACL permissions for an object per
-- grantee.
putObjectAcl_accessControlPolicy :: Lens.Lens' PutObjectAcl (Prelude.Maybe AccessControlPolicy)
putObjectAcl_accessControlPolicy = Lens.lens (\PutObjectAcl' {accessControlPolicy} -> accessControlPolicy) (\s@PutObjectAcl' {} a -> s {accessControlPolicy = a} :: PutObjectAcl)

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
putObjectAcl_checksumAlgorithm :: Lens.Lens' PutObjectAcl (Prelude.Maybe ChecksumAlgorithm)
putObjectAcl_checksumAlgorithm = Lens.lens (\PutObjectAcl' {checksumAlgorithm} -> checksumAlgorithm) (\s@PutObjectAcl' {} a -> s {checksumAlgorithm = a} :: PutObjectAcl)

-- | The base64-encoded 128-bit MD5 digest of the data. This header must be
-- used as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, go to
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>>
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putObjectAcl_contentMD5 :: Lens.Lens' PutObjectAcl (Prelude.Maybe Prelude.Text)
putObjectAcl_contentMD5 = Lens.lens (\PutObjectAcl' {contentMD5} -> contentMD5) (\s@PutObjectAcl' {} a -> s {contentMD5 = a} :: PutObjectAcl)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putObjectAcl_expectedBucketOwner :: Lens.Lens' PutObjectAcl (Prelude.Maybe Prelude.Text)
putObjectAcl_expectedBucketOwner = Lens.lens (\PutObjectAcl' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutObjectAcl' {} a -> s {expectedBucketOwner = a} :: PutObjectAcl)

-- | Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
putObjectAcl_grantFullControl :: Lens.Lens' PutObjectAcl (Prelude.Maybe Prelude.Text)
putObjectAcl_grantFullControl = Lens.lens (\PutObjectAcl' {grantFullControl} -> grantFullControl) (\s@PutObjectAcl' {} a -> s {grantFullControl = a} :: PutObjectAcl)

-- | Allows grantee to list the objects in the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
putObjectAcl_grantRead :: Lens.Lens' PutObjectAcl (Prelude.Maybe Prelude.Text)
putObjectAcl_grantRead = Lens.lens (\PutObjectAcl' {grantRead} -> grantRead) (\s@PutObjectAcl' {} a -> s {grantRead = a} :: PutObjectAcl)

-- | Allows grantee to read the bucket ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
putObjectAcl_grantReadACP :: Lens.Lens' PutObjectAcl (Prelude.Maybe Prelude.Text)
putObjectAcl_grantReadACP = Lens.lens (\PutObjectAcl' {grantReadACP} -> grantReadACP) (\s@PutObjectAcl' {} a -> s {grantReadACP = a} :: PutObjectAcl)

-- | Allows grantee to create new objects in the bucket.
--
-- For the bucket and object owners of existing objects, also allows
-- deletions and overwrites of those objects.
putObjectAcl_grantWrite :: Lens.Lens' PutObjectAcl (Prelude.Maybe Prelude.Text)
putObjectAcl_grantWrite = Lens.lens (\PutObjectAcl' {grantWrite} -> grantWrite) (\s@PutObjectAcl' {} a -> s {grantWrite = a} :: PutObjectAcl)

-- | Allows grantee to write the ACL for the applicable bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
putObjectAcl_grantWriteACP :: Lens.Lens' PutObjectAcl (Prelude.Maybe Prelude.Text)
putObjectAcl_grantWriteACP = Lens.lens (\PutObjectAcl' {grantWriteACP} -> grantWriteACP) (\s@PutObjectAcl' {} a -> s {grantWriteACP = a} :: PutObjectAcl)

-- | Undocumented member.
putObjectAcl_requestPayer :: Lens.Lens' PutObjectAcl (Prelude.Maybe RequestPayer)
putObjectAcl_requestPayer = Lens.lens (\PutObjectAcl' {requestPayer} -> requestPayer) (\s@PutObjectAcl' {} a -> s {requestPayer = a} :: PutObjectAcl)

-- | VersionId used to reference a specific version of the object.
putObjectAcl_versionId :: Lens.Lens' PutObjectAcl (Prelude.Maybe ObjectVersionId)
putObjectAcl_versionId = Lens.lens (\PutObjectAcl' {versionId} -> versionId) (\s@PutObjectAcl' {} a -> s {versionId = a} :: PutObjectAcl)

-- | The bucket name that contains the object to which you want to attach the
-- ACL.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
putObjectAcl_bucket :: Lens.Lens' PutObjectAcl BucketName
putObjectAcl_bucket = Lens.lens (\PutObjectAcl' {bucket} -> bucket) (\s@PutObjectAcl' {} a -> s {bucket = a} :: PutObjectAcl)

-- | Key for which the PUT action was initiated.
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
-- When you use this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- @ @/@AccessPointName@/@-@/@AccountId@/@.@/@outpostID@/@.s3-outposts.@/@Region@/@.amazonaws.com@.
-- When you use this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts access point ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html What is S3 on Outposts>
-- in the /Amazon S3 User Guide/.
putObjectAcl_key :: Lens.Lens' PutObjectAcl ObjectKey
putObjectAcl_key = Lens.lens (\PutObjectAcl' {key} -> key) (\s@PutObjectAcl' {} a -> s {key = a} :: PutObjectAcl)

instance Core.AWSRequest PutObjectAcl where
  type AWSResponse PutObjectAcl = PutObjectAclResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutObjectAclResponse'
            Prelude.<$> (h Data..#? "x-amz-request-charged")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutObjectAcl where
  hashWithSalt _salt PutObjectAcl' {..} =
    _salt
      `Prelude.hashWithSalt` acl
      `Prelude.hashWithSalt` accessControlPolicy
      `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` grantFullControl
      `Prelude.hashWithSalt` grantRead
      `Prelude.hashWithSalt` grantReadACP
      `Prelude.hashWithSalt` grantWrite
      `Prelude.hashWithSalt` grantWriteACP
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData PutObjectAcl where
  rnf PutObjectAcl' {..} =
    Prelude.rnf acl
      `Prelude.seq` Prelude.rnf accessControlPolicy
      `Prelude.seq` Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf grantFullControl
      `Prelude.seq` Prelude.rnf grantRead
      `Prelude.seq` Prelude.rnf grantReadACP
      `Prelude.seq` Prelude.rnf grantWrite
      `Prelude.seq` Prelude.rnf grantWriteACP
      `Prelude.seq` Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key

instance Data.ToElement PutObjectAcl where
  toElement PutObjectAcl' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}AccessControlPolicy"
      accessControlPolicy

instance Data.ToHeaders PutObjectAcl where
  toHeaders PutObjectAcl' {..} =
    Prelude.mconcat
      [ "x-amz-acl" Data.=# acl,
        "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "Content-MD5" Data.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-grant-full-control" Data.=# grantFullControl,
        "x-amz-grant-read" Data.=# grantRead,
        "x-amz-grant-read-acp" Data.=# grantReadACP,
        "x-amz-grant-write" Data.=# grantWrite,
        "x-amz-grant-write-acp" Data.=# grantWriteACP,
        "x-amz-request-payer" Data.=# requestPayer
      ]

instance Data.ToPath PutObjectAcl where
  toPath PutObjectAcl' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery PutObjectAcl where
  toQuery PutObjectAcl' {..} =
    Prelude.mconcat
      ["versionId" Data.=: versionId, "acl"]

-- | /See:/ 'newPutObjectAclResponse' smart constructor.
data PutObjectAclResponse = PutObjectAclResponse'
  { requestCharged :: Prelude.Maybe RequestCharged,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutObjectAclResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'putObjectAclResponse_requestCharged' - Undocumented member.
--
-- 'httpStatus', 'putObjectAclResponse_httpStatus' - The response's http status code.
newPutObjectAclResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutObjectAclResponse
newPutObjectAclResponse pHttpStatus_ =
  PutObjectAclResponse'
    { requestCharged =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
putObjectAclResponse_requestCharged :: Lens.Lens' PutObjectAclResponse (Prelude.Maybe RequestCharged)
putObjectAclResponse_requestCharged = Lens.lens (\PutObjectAclResponse' {requestCharged} -> requestCharged) (\s@PutObjectAclResponse' {} a -> s {requestCharged = a} :: PutObjectAclResponse)

-- | The response's http status code.
putObjectAclResponse_httpStatus :: Lens.Lens' PutObjectAclResponse Prelude.Int
putObjectAclResponse_httpStatus = Lens.lens (\PutObjectAclResponse' {httpStatus} -> httpStatus) (\s@PutObjectAclResponse' {} a -> s {httpStatus = a} :: PutObjectAclResponse)

instance Prelude.NFData PutObjectAclResponse where
  rnf PutObjectAclResponse' {..} =
    Prelude.rnf requestCharged
      `Prelude.seq` Prelude.rnf httpStatus
