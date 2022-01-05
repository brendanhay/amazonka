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
-- Module      : Amazonka.S3.PutBucketAcl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the permissions on an existing bucket using access control lists
-- (ACL). For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3_ACLs_UsingACLs.html Using ACLs>.
-- To set the ACL of a bucket, you must have @WRITE_ACP@ permission.
--
-- You can use one of the following two ways to set a bucket\'s
-- permissions:
--
-- -   Specify the ACL in the request body
--
-- -   Specify permissions using request headers
--
-- You cannot specify access permission using both the body and the request
-- headers.
--
-- Depending on your application needs, you may choose to set the ACL on a
-- bucket using either the request body or the headers. For example, if you
-- have an existing application that updates a bucket ACL using the request
-- body, then you can continue to use that approach.
--
-- __Access Permissions__
--
-- You can set access permissions using one of the following methods:
--
-- -   Specify a canned ACL with the @x-amz-acl@ request header. Amazon S3
--     supports a set of predefined ACLs, known as /canned ACLs/. Each
--     canned ACL has a predefined set of grantees and permissions. Specify
--     the canned ACL name as the value of @x-amz-acl@. If you use this
--     header, you cannot use other access control-specific headers in your
--     request. For more information, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
--
-- -   Specify access permissions explicitly with the @x-amz-grant-read@,
--     @x-amz-grant-read-acp@, @x-amz-grant-write-acp@, and
--     @x-amz-grant-full-control@ headers. When using these headers, you
--     specify explicit access permissions and grantees (Amazon Web
--     Services accounts or Amazon S3 groups) who will receive the
--     permission. If you use these ACL-specific headers, you cannot use
--     the @x-amz-acl@ header to set a canned ACL. These parameters map to
--     the set of permissions that Amazon S3 supports in an ACL. For more
--     information, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview>.
--
--     You specify each grantee as a type=value pair, where the type is one
--     of the following:
--
--     -   @id@ – if the value specified is the canonical user ID of an
--         Amazon Web Services account
--
--     -   @uri@ – if you are granting permissions to a predefined group
--
--     -   @emailAddress@ – if the value specified is the email address of
--         an Amazon Web Services account
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
--     For example, the following @x-amz-grant-write@ header grants create,
--     overwrite, and delete objects permission to LogDelivery group
--     predefined by Amazon S3 and two Amazon Web Services accounts
--     identified by their email addresses.
--
--     @x-amz-grant-write: uri=\"http:\/\/acs.amazonaws.com\/groups\/s3\/LogDelivery\", id=\"111122223333\", id=\"555566667777\" @
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
--     DisplayName is optional and ignored in the request
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
--     following Amazon Web Services Regions:
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
--     in the Amazon Web Services General Reference.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAcl.html GetObjectAcl>
module Amazonka.S3.PutBucketAcl
  ( -- * Creating a Request
    PutBucketAcl (..),
    newPutBucketAcl,

    -- * Request Lenses
    putBucketAcl_grantReadACP,
    putBucketAcl_grantWriteACP,
    putBucketAcl_grantRead,
    putBucketAcl_grantFullControl,
    putBucketAcl_contentMD5,
    putBucketAcl_accessControlPolicy,
    putBucketAcl_grantWrite,
    putBucketAcl_acl,
    putBucketAcl_expectedBucketOwner,
    putBucketAcl_bucket,

    -- * Destructuring the Response
    PutBucketAclResponse (..),
    newPutBucketAclResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketAcl' smart constructor.
data PutBucketAcl = PutBucketAcl'
  { -- | Allows grantee to read the bucket ACL.
    grantReadACP :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee to write the ACL for the applicable bucket.
    grantWriteACP :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee to list the objects in the bucket.
    grantRead :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee the read, write, read ACP, and write ACP permissions on
    -- the bucket.
    grantFullControl :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded 128-bit MD5 digest of the data. This header must be
    -- used as a message integrity check to verify that the request body was
    -- not corrupted in transit. For more information, go to
    -- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>
    --
    -- For requests made using the Amazon Web Services Command Line Interface
    -- (CLI) or Amazon Web Services SDKs, this field is calculated
    -- automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | Contains the elements that set the ACL permissions for an object per
    -- grantee.
    accessControlPolicy :: Prelude.Maybe AccessControlPolicy,
    -- | Allows grantee to create new objects in the bucket.
    --
    -- For the bucket and object owners of existing objects, also allows
    -- deletions and overwrites of those objects.
    grantWrite :: Prelude.Maybe Prelude.Text,
    -- | The canned ACL to apply to the bucket.
    acl :: Prelude.Maybe BucketCannedACL,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket to which to apply the ACL.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketAcl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantReadACP', 'putBucketAcl_grantReadACP' - Allows grantee to read the bucket ACL.
--
-- 'grantWriteACP', 'putBucketAcl_grantWriteACP' - Allows grantee to write the ACL for the applicable bucket.
--
-- 'grantRead', 'putBucketAcl_grantRead' - Allows grantee to list the objects in the bucket.
--
-- 'grantFullControl', 'putBucketAcl_grantFullControl' - Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
--
-- 'contentMD5', 'putBucketAcl_contentMD5' - The base64-encoded 128-bit MD5 digest of the data. This header must be
-- used as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, go to
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'accessControlPolicy', 'putBucketAcl_accessControlPolicy' - Contains the elements that set the ACL permissions for an object per
-- grantee.
--
-- 'grantWrite', 'putBucketAcl_grantWrite' - Allows grantee to create new objects in the bucket.
--
-- For the bucket and object owners of existing objects, also allows
-- deletions and overwrites of those objects.
--
-- 'acl', 'putBucketAcl_acl' - The canned ACL to apply to the bucket.
--
-- 'expectedBucketOwner', 'putBucketAcl_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'putBucketAcl_bucket' - The bucket to which to apply the ACL.
newPutBucketAcl ::
  -- | 'bucket'
  BucketName ->
  PutBucketAcl
newPutBucketAcl pBucket_ =
  PutBucketAcl'
    { grantReadACP = Prelude.Nothing,
      grantWriteACP = Prelude.Nothing,
      grantRead = Prelude.Nothing,
      grantFullControl = Prelude.Nothing,
      contentMD5 = Prelude.Nothing,
      accessControlPolicy = Prelude.Nothing,
      grantWrite = Prelude.Nothing,
      acl = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      bucket = pBucket_
    }

-- | Allows grantee to read the bucket ACL.
putBucketAcl_grantReadACP :: Lens.Lens' PutBucketAcl (Prelude.Maybe Prelude.Text)
putBucketAcl_grantReadACP = Lens.lens (\PutBucketAcl' {grantReadACP} -> grantReadACP) (\s@PutBucketAcl' {} a -> s {grantReadACP = a} :: PutBucketAcl)

-- | Allows grantee to write the ACL for the applicable bucket.
putBucketAcl_grantWriteACP :: Lens.Lens' PutBucketAcl (Prelude.Maybe Prelude.Text)
putBucketAcl_grantWriteACP = Lens.lens (\PutBucketAcl' {grantWriteACP} -> grantWriteACP) (\s@PutBucketAcl' {} a -> s {grantWriteACP = a} :: PutBucketAcl)

-- | Allows grantee to list the objects in the bucket.
putBucketAcl_grantRead :: Lens.Lens' PutBucketAcl (Prelude.Maybe Prelude.Text)
putBucketAcl_grantRead = Lens.lens (\PutBucketAcl' {grantRead} -> grantRead) (\s@PutBucketAcl' {} a -> s {grantRead = a} :: PutBucketAcl)

-- | Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
putBucketAcl_grantFullControl :: Lens.Lens' PutBucketAcl (Prelude.Maybe Prelude.Text)
putBucketAcl_grantFullControl = Lens.lens (\PutBucketAcl' {grantFullControl} -> grantFullControl) (\s@PutBucketAcl' {} a -> s {grantFullControl = a} :: PutBucketAcl)

-- | The base64-encoded 128-bit MD5 digest of the data. This header must be
-- used as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, go to
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putBucketAcl_contentMD5 :: Lens.Lens' PutBucketAcl (Prelude.Maybe Prelude.Text)
putBucketAcl_contentMD5 = Lens.lens (\PutBucketAcl' {contentMD5} -> contentMD5) (\s@PutBucketAcl' {} a -> s {contentMD5 = a} :: PutBucketAcl)

-- | Contains the elements that set the ACL permissions for an object per
-- grantee.
putBucketAcl_accessControlPolicy :: Lens.Lens' PutBucketAcl (Prelude.Maybe AccessControlPolicy)
putBucketAcl_accessControlPolicy = Lens.lens (\PutBucketAcl' {accessControlPolicy} -> accessControlPolicy) (\s@PutBucketAcl' {} a -> s {accessControlPolicy = a} :: PutBucketAcl)

-- | Allows grantee to create new objects in the bucket.
--
-- For the bucket and object owners of existing objects, also allows
-- deletions and overwrites of those objects.
putBucketAcl_grantWrite :: Lens.Lens' PutBucketAcl (Prelude.Maybe Prelude.Text)
putBucketAcl_grantWrite = Lens.lens (\PutBucketAcl' {grantWrite} -> grantWrite) (\s@PutBucketAcl' {} a -> s {grantWrite = a} :: PutBucketAcl)

-- | The canned ACL to apply to the bucket.
putBucketAcl_acl :: Lens.Lens' PutBucketAcl (Prelude.Maybe BucketCannedACL)
putBucketAcl_acl = Lens.lens (\PutBucketAcl' {acl} -> acl) (\s@PutBucketAcl' {} a -> s {acl = a} :: PutBucketAcl)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putBucketAcl_expectedBucketOwner :: Lens.Lens' PutBucketAcl (Prelude.Maybe Prelude.Text)
putBucketAcl_expectedBucketOwner = Lens.lens (\PutBucketAcl' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketAcl' {} a -> s {expectedBucketOwner = a} :: PutBucketAcl)

-- | The bucket to which to apply the ACL.
putBucketAcl_bucket :: Lens.Lens' PutBucketAcl BucketName
putBucketAcl_bucket = Lens.lens (\PutBucketAcl' {bucket} -> bucket) (\s@PutBucketAcl' {} a -> s {bucket = a} :: PutBucketAcl)

instance Core.AWSRequest PutBucketAcl where
  type AWSResponse PutBucketAcl = PutBucketAclResponse
  request =
    Request.s3vhost
      Prelude.. Request.putXML defaultService
  response = Response.receiveNull PutBucketAclResponse'

instance Prelude.Hashable PutBucketAcl where
  hashWithSalt _salt PutBucketAcl' {..} =
    _salt `Prelude.hashWithSalt` grantReadACP
      `Prelude.hashWithSalt` grantWriteACP
      `Prelude.hashWithSalt` grantRead
      `Prelude.hashWithSalt` grantFullControl
      `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` accessControlPolicy
      `Prelude.hashWithSalt` grantWrite
      `Prelude.hashWithSalt` acl
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData PutBucketAcl where
  rnf PutBucketAcl' {..} =
    Prelude.rnf grantReadACP
      `Prelude.seq` Prelude.rnf grantWriteACP
      `Prelude.seq` Prelude.rnf grantRead
      `Prelude.seq` Prelude.rnf grantFullControl
      `Prelude.seq` Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf accessControlPolicy
      `Prelude.seq` Prelude.rnf grantWrite
      `Prelude.seq` Prelude.rnf acl
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Core.ToElement PutBucketAcl where
  toElement PutBucketAcl' {..} =
    Core.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}AccessControlPolicy"
      accessControlPolicy

instance Core.ToHeaders PutBucketAcl where
  toHeaders PutBucketAcl' {..} =
    Prelude.mconcat
      [ "x-amz-grant-read-acp" Core.=# grantReadACP,
        "x-amz-grant-write-acp" Core.=# grantWriteACP,
        "x-amz-grant-read" Core.=# grantRead,
        "x-amz-grant-full-control" Core.=# grantFullControl,
        "Content-MD5" Core.=# contentMD5,
        "x-amz-grant-write" Core.=# grantWrite,
        "x-amz-acl" Core.=# acl,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath PutBucketAcl where
  toPath PutBucketAcl' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery PutBucketAcl where
  toQuery = Prelude.const (Prelude.mconcat ["acl"])

-- | /See:/ 'newPutBucketAclResponse' smart constructor.
data PutBucketAclResponse = PutBucketAclResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketAclResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketAclResponse ::
  PutBucketAclResponse
newPutBucketAclResponse = PutBucketAclResponse'

instance Prelude.NFData PutBucketAclResponse where
  rnf _ = ()
