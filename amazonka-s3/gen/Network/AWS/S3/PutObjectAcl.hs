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
-- Module      : Network.AWS.S3.PutObjectAcl
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- Depending on your application needs, you can choose to set the ACL on an
-- object using either the request body or the headers. For example, if you
-- have an existing application that updates a bucket ACL using the request
-- body, you can continue to use that approach. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview>
-- in the /Amazon S3 Developer Guide/.
--
-- __Access Permissions__
--
-- You can set access permissions using one of the following methods:
--
-- -   Specify a canned ACL with the @x-amz-acl@ request header. Amazon S3
--     supports a set of predefined ACLs, known as canned ACLs. Each canned
--     ACL has a predefined set of grantees and permissions. Specify the
--     canned ACL name as the value of @x-amz-ac@l. If you use this header,
--     you cannot use other access control-specific headers in your
--     request. For more information, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
--
-- -   Specify access permissions explicitly with the @x-amz-grant-read@,
--     @x-amz-grant-read-acp@, @x-amz-grant-write-acp@, and
--     @x-amz-grant-full-control@ headers. When using these headers, you
--     specify explicit access permissions and grantees (AWS accounts or
--     Amazon S3 groups) who will receive the permission. If you use these
--     ACL-specific headers, you cannot use @x-amz-acl@ header to set a
--     canned ACL. These parameters map to the set of permissions that
--     Amazon S3 supports in an ACL. For more information, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview>.
--
--     You specify each grantee as a type=value pair, where the type is one
--     of the following:
--
--     -   @id@ – if the value specified is the canonical user ID of an AWS
--         account
--
--     -   @uri@ – if you are granting permissions to a predefined group
--
--     -   @emailAddress@ – if the value specified is the email address of
--         an AWS account
--
--         Using email addresses to specify a grantee is only supported in
--         the following AWS Regions:
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
--         in the AWS General Reference.
--
--     For example, the following @x-amz-grant-read@ header grants list
--     objects permission to the two AWS accounts identified by their email
--     addresses.
--
--     @x-amz-grant-read: emailAddress=\"xyz\@amazon.com\", emailAddress=\"abc\@amazon.com\" @
--
-- You can use either a canned ACL or specify access permissions
-- explicitly. You cannot do both.
--
-- __Grantee Values__
--
-- You can specify the person (grantee) to whom you\'re assigning access
-- rights (using request elements) in the following ways:
--
-- -   By the person\'s ID:
--
--     @\<Grantee xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xsi:type=\"CanonicalUser\">\<ID>\<>ID\<>\<\/ID>\<DisplayName>\<>GranteesEmail\<>\<\/DisplayName> \<\/Grantee>@
--
--     DisplayName is optional and ignored in the request.
--
-- -   By URI:
--
--     @\<Grantee xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xsi:type=\"Group\">\<URI>\<>http:\/\/acs.amazonaws.com\/groups\/global\/AuthenticatedUsers\<>\<\/URI>\<\/Grantee>@
--
-- -   By Email address:
--
--     @\<Grantee xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xsi:type=\"AmazonCustomerByEmail\">\<EmailAddress>\<>Grantees\@email.com\<>\<\/EmailAddress>lt;\/Grantee>@
--
--     The grantee is resolved to the CanonicalUser and, in a response to a
--     GET Object acl request, appears as the CanonicalUser.
--
--     Using email addresses to specify a grantee is only supported in the
--     following AWS Regions:
--
--     -   US East (N. Virginia)
--
--     -   US West (N. California)
--
--     -   US West (Oregon)
--
--     -   Asia Pacific (Singapore)
--
--     -   Asia Pacific (Sydney)
--
--     -   Asia Pacific (Tokyo)
--
--     -   Europe (Ireland)
--
--     -   South America (São Paulo)
--
--     For a list of all the Amazon S3 supported Regions and endpoints, see
--     <https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Regions and Endpoints>
--     in the AWS General Reference.
--
-- __Versioning__
--
-- The ACL of an object is set at the object version level. By default, PUT
-- sets the ACL of the current version of an object. To set the ACL of a
-- different version, use the @versionId@ subresource.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CopyObject.html CopyObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
module Network.AWS.S3.PutObjectAcl
  ( -- * Creating a Request
    PutObjectAcl (..),
    newPutObjectAcl,

    -- * Request Lenses
    putObjectAcl_grantRead,
    putObjectAcl_expectedBucketOwner,
    putObjectAcl_accessControlPolicy,
    putObjectAcl_contentMD5,
    putObjectAcl_versionId,
    putObjectAcl_grantWriteACP,
    putObjectAcl_grantReadACP,
    putObjectAcl_acl,
    putObjectAcl_requestPayer,
    putObjectAcl_grantWrite,
    putObjectAcl_grantFullControl,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newPutObjectAcl' smart constructor.
data PutObjectAcl = PutObjectAcl'
  { -- | Allows grantee to list the objects in the bucket.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantRead :: Prelude.Maybe Prelude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Contains the elements that set the ACL permissions for an object per
    -- grantee.
    accessControlPolicy :: Prelude.Maybe AccessControlPolicy,
    -- | The base64-encoded 128-bit MD5 digest of the data. This header must be
    -- used as a message integrity check to verify that the request body was
    -- not corrupted in transit. For more information, go to
    -- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>>
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS
    -- SDKs, this field is calculated automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | VersionId used to reference a specific version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | Allows grantee to write the ACL for the applicable bucket.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantWriteACP :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee to read the bucket ACL.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantReadACP :: Prelude.Maybe Prelude.Text,
    -- | The canned ACL to apply to the object. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
    acl :: Prelude.Maybe ObjectCannedACL,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | Allows grantee to create, overwrite, and delete any object in the
    -- bucket.
    grantWrite :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee the read, write, read ACP, and write ACP permissions on
    -- the bucket.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantFullControl :: Prelude.Maybe Prelude.Text,
    -- | The bucket name that contains the object to which you want to attach the
    -- ACL.
    --
    -- When using this API with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this operation with an access point through the AWS SDKs, you
    -- provide the access point ARN in place of the bucket name. For more
    -- information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    bucket :: BucketName,
    -- | Key for which the PUT operation was initiated.
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
-- 'grantRead', 'putObjectAcl_grantRead' - Allows grantee to list the objects in the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'expectedBucketOwner', 'putObjectAcl_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'accessControlPolicy', 'putObjectAcl_accessControlPolicy' - Contains the elements that set the ACL permissions for an object per
-- grantee.
--
-- 'contentMD5', 'putObjectAcl_contentMD5' - The base64-encoded 128-bit MD5 digest of the data. This header must be
-- used as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, go to
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>>
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
--
-- 'versionId', 'putObjectAcl_versionId' - VersionId used to reference a specific version of the object.
--
-- 'grantWriteACP', 'putObjectAcl_grantWriteACP' - Allows grantee to write the ACL for the applicable bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'grantReadACP', 'putObjectAcl_grantReadACP' - Allows grantee to read the bucket ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'acl', 'putObjectAcl_acl' - The canned ACL to apply to the object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
--
-- 'requestPayer', 'putObjectAcl_requestPayer' - Undocumented member.
--
-- 'grantWrite', 'putObjectAcl_grantWrite' - Allows grantee to create, overwrite, and delete any object in the
-- bucket.
--
-- 'grantFullControl', 'putObjectAcl_grantFullControl' - Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- 'bucket', 'putObjectAcl_bucket' - The bucket name that contains the object to which you want to attach the
-- ACL.
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
-- 'key', 'putObjectAcl_key' - Key for which the PUT operation was initiated.
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
newPutObjectAcl ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  PutObjectAcl
newPutObjectAcl pBucket_ pKey_ =
  PutObjectAcl'
    { grantRead = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      accessControlPolicy = Prelude.Nothing,
      contentMD5 = Prelude.Nothing,
      versionId = Prelude.Nothing,
      grantWriteACP = Prelude.Nothing,
      grantReadACP = Prelude.Nothing,
      acl = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      grantWrite = Prelude.Nothing,
      grantFullControl = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | Allows grantee to list the objects in the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
putObjectAcl_grantRead :: Lens.Lens' PutObjectAcl (Prelude.Maybe Prelude.Text)
putObjectAcl_grantRead = Lens.lens (\PutObjectAcl' {grantRead} -> grantRead) (\s@PutObjectAcl' {} a -> s {grantRead = a} :: PutObjectAcl)

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putObjectAcl_expectedBucketOwner :: Lens.Lens' PutObjectAcl (Prelude.Maybe Prelude.Text)
putObjectAcl_expectedBucketOwner = Lens.lens (\PutObjectAcl' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutObjectAcl' {} a -> s {expectedBucketOwner = a} :: PutObjectAcl)

-- | Contains the elements that set the ACL permissions for an object per
-- grantee.
putObjectAcl_accessControlPolicy :: Lens.Lens' PutObjectAcl (Prelude.Maybe AccessControlPolicy)
putObjectAcl_accessControlPolicy = Lens.lens (\PutObjectAcl' {accessControlPolicy} -> accessControlPolicy) (\s@PutObjectAcl' {} a -> s {accessControlPolicy = a} :: PutObjectAcl)

-- | The base64-encoded 128-bit MD5 digest of the data. This header must be
-- used as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, go to
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>>
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
putObjectAcl_contentMD5 :: Lens.Lens' PutObjectAcl (Prelude.Maybe Prelude.Text)
putObjectAcl_contentMD5 = Lens.lens (\PutObjectAcl' {contentMD5} -> contentMD5) (\s@PutObjectAcl' {} a -> s {contentMD5 = a} :: PutObjectAcl)

-- | VersionId used to reference a specific version of the object.
putObjectAcl_versionId :: Lens.Lens' PutObjectAcl (Prelude.Maybe ObjectVersionId)
putObjectAcl_versionId = Lens.lens (\PutObjectAcl' {versionId} -> versionId) (\s@PutObjectAcl' {} a -> s {versionId = a} :: PutObjectAcl)

-- | Allows grantee to write the ACL for the applicable bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
putObjectAcl_grantWriteACP :: Lens.Lens' PutObjectAcl (Prelude.Maybe Prelude.Text)
putObjectAcl_grantWriteACP = Lens.lens (\PutObjectAcl' {grantWriteACP} -> grantWriteACP) (\s@PutObjectAcl' {} a -> s {grantWriteACP = a} :: PutObjectAcl)

-- | Allows grantee to read the bucket ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
putObjectAcl_grantReadACP :: Lens.Lens' PutObjectAcl (Prelude.Maybe Prelude.Text)
putObjectAcl_grantReadACP = Lens.lens (\PutObjectAcl' {grantReadACP} -> grantReadACP) (\s@PutObjectAcl' {} a -> s {grantReadACP = a} :: PutObjectAcl)

-- | The canned ACL to apply to the object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
putObjectAcl_acl :: Lens.Lens' PutObjectAcl (Prelude.Maybe ObjectCannedACL)
putObjectAcl_acl = Lens.lens (\PutObjectAcl' {acl} -> acl) (\s@PutObjectAcl' {} a -> s {acl = a} :: PutObjectAcl)

-- | Undocumented member.
putObjectAcl_requestPayer :: Lens.Lens' PutObjectAcl (Prelude.Maybe RequestPayer)
putObjectAcl_requestPayer = Lens.lens (\PutObjectAcl' {requestPayer} -> requestPayer) (\s@PutObjectAcl' {} a -> s {requestPayer = a} :: PutObjectAcl)

-- | Allows grantee to create, overwrite, and delete any object in the
-- bucket.
putObjectAcl_grantWrite :: Lens.Lens' PutObjectAcl (Prelude.Maybe Prelude.Text)
putObjectAcl_grantWrite = Lens.lens (\PutObjectAcl' {grantWrite} -> grantWrite) (\s@PutObjectAcl' {} a -> s {grantWrite = a} :: PutObjectAcl)

-- | Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
putObjectAcl_grantFullControl :: Lens.Lens' PutObjectAcl (Prelude.Maybe Prelude.Text)
putObjectAcl_grantFullControl = Lens.lens (\PutObjectAcl' {grantFullControl} -> grantFullControl) (\s@PutObjectAcl' {} a -> s {grantFullControl = a} :: PutObjectAcl)

-- | The bucket name that contains the object to which you want to attach the
-- ACL.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
putObjectAcl_bucket :: Lens.Lens' PutObjectAcl BucketName
putObjectAcl_bucket = Lens.lens (\PutObjectAcl' {bucket} -> bucket) (\s@PutObjectAcl' {} a -> s {bucket = a} :: PutObjectAcl)

-- | Key for which the PUT operation was initiated.
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
putObjectAcl_key :: Lens.Lens' PutObjectAcl ObjectKey
putObjectAcl_key = Lens.lens (\PutObjectAcl' {key} -> key) (\s@PutObjectAcl' {} a -> s {key = a} :: PutObjectAcl)

instance Core.AWSRequest PutObjectAcl where
  type AWSResponse PutObjectAcl = PutObjectAclResponse
  request = Request.putXML defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutObjectAclResponse'
            Prelude.<$> (h Core..#? "x-amz-request-charged")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutObjectAcl

instance Prelude.NFData PutObjectAcl

instance Core.ToElement PutObjectAcl where
  toElement PutObjectAcl' {..} =
    Core.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}AccessControlPolicy"
      accessControlPolicy

instance Core.ToHeaders PutObjectAcl where
  toHeaders PutObjectAcl' {..} =
    Prelude.mconcat
      [ "x-amz-grant-read" Core.=# grantRead,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "Content-MD5" Core.=# contentMD5,
        "x-amz-grant-write-acp" Core.=# grantWriteACP,
        "x-amz-grant-read-acp" Core.=# grantReadACP,
        "x-amz-acl" Core.=# acl,
        "x-amz-request-payer" Core.=# requestPayer,
        "x-amz-grant-write" Core.=# grantWrite,
        "x-amz-grant-full-control" Core.=# grantFullControl
      ]

instance Core.ToPath PutObjectAcl where
  toPath PutObjectAcl' {..} =
    Prelude.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery PutObjectAcl where
  toQuery PutObjectAcl' {..} =
    Prelude.mconcat
      ["versionId" Core.=: versionId, "acl"]

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

instance Prelude.NFData PutObjectAclResponse
