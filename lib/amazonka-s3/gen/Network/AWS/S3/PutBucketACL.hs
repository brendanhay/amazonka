{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the permissions on an existing bucket using access control lists (ACL). For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3_ACLs_UsingACLs.html Using ACLs> . To set the ACL of a bucket, you must have @WRITE_ACP@ permission.
--
-- You can use one of the following two ways to set a bucket's permissions:
--
--     * Specify the ACL in the request body
--
--
--     * Specify permissions using request headers
--
--
-- Depending on your application needs, you may choose to set the ACL on a bucket using either the request body or the headers. For example, if you have an existing application that updates a bucket ACL using the request body, then you can continue to use that approach.
-- __Access Permissions__
-- You can set access permissions using one of the following methods:
--
--     * Specify a canned ACL with the @x-amz-acl@ request header. Amazon S3 supports a set of predefined ACLs, known as /canned ACLs/ . Each canned ACL has a predefined set of grantees and permissions. Specify the canned ACL name as the value of @x-amz-acl@ . If you use this header, you cannot use other access control-specific headers in your request. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
--
--     * Specify access permissions explicitly with the @x-amz-grant-read@ , @x-amz-grant-read-acp@ , @x-amz-grant-write-acp@ , and @x-amz-grant-full-control@ headers. When using these headers, you specify explicit access permissions and grantees (AWS accounts or Amazon S3 groups) who will receive the permission. If you use these ACL-specific headers, you cannot use the @x-amz-acl@ header to set a canned ACL. These parameters map to the set of permissions that Amazon S3 supports in an ACL. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> .
-- You specify each grantee as a type=value pair, where the type is one of the following:
--
--     * @id@ – if the value specified is the canonical user ID of an AWS account
--
--
--     * @uri@ – if you are granting permissions to a predefined group
--
--
--     * @emailAddress@ – if the value specified is the email address of an AWS account
--
--
-- For example, the following @x-amz-grant-write@ header grants create, overwrite, and delete objects permission to LogDelivery group predefined by Amazon S3 and two AWS accounts identified by their email addresses.
-- @x-amz-grant-write: uri="http://acs.amazonaws.com/groups/s3/LogDelivery", id="111122223333", id="555566667777" @
--
--
-- You can use either a canned ACL or specify access permissions explicitly. You cannot do both.
-- __Grantee Values__
-- You can specify the person (grantee) to whom you're assigning access rights (using request elements) in the following ways:
--
--     * By the person's ID:
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser"><ID><>ID<></ID><DisplayName><>GranteesEmail<></DisplayName> </Grantee>@
-- DisplayName is optional and ignored in the request
--
--
--     * By URI:
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="Group"><URI><>http://acs.amazonaws.com/groups/global/AuthenticatedUsers<></URI></Grantee>@
--
--
--     * By Email address:
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="AmazonCustomerByEmail"><EmailAddress><>Grantees@email.com<></EmailAddress>lt;/Grantee>@
-- The grantee is resolved to the CanonicalUser and, in a response to a GET Object acl request, appears as the CanonicalUser.
--
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAcl.html GetObjectAcl>
module Network.AWS.S3.PutBucketACL
  ( -- * Creating a request
    PutBucketACL (..),
    mkPutBucketACL,

    -- ** Request lenses
    pbaGrantReadACP,
    pbaBucket,
    pbaGrantWriteACP,
    pbaGrantRead,
    pbaGrantFullControl,
    pbaContentMD5,
    pbaAccessControlPolicy,
    pbaGrantWrite,
    pbaACL,
    pbaExpectedBucketOwner,

    -- * Destructuring the response
    PutBucketACLResponse (..),
    mkPutBucketACLResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutBucketACL' smart constructor.
data PutBucketACL = PutBucketACL'
  { -- | Allows grantee to read the bucket ACL.
    grantReadACP :: Lude.Maybe Lude.Text,
    -- | The bucket to which to apply the ACL.
    bucket :: BucketName,
    -- | Allows grantee to write the ACL for the applicable bucket.
    grantWriteACP :: Lude.Maybe Lude.Text,
    -- | Allows grantee to list the objects in the bucket.
    grantRead :: Lude.Maybe Lude.Text,
    -- | Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
    grantFullControl :: Lude.Maybe Lude.Text,
    -- | The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Lude.Maybe Lude.Text,
    -- | Contains the elements that set the ACL permissions for an object per grantee.
    accessControlPolicy :: Lude.Maybe AccessControlPolicy,
    -- | Allows grantee to create, overwrite, and delete any object in the bucket.
    grantWrite :: Lude.Maybe Lude.Text,
    -- | The canned ACL to apply to the bucket.
    acl :: Lude.Maybe BucketCannedACL,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketACL' with the minimum fields required to make a request.
--
-- * 'grantReadACP' - Allows grantee to read the bucket ACL.
-- * 'bucket' - The bucket to which to apply the ACL.
-- * 'grantWriteACP' - Allows grantee to write the ACL for the applicable bucket.
-- * 'grantRead' - Allows grantee to list the objects in the bucket.
-- * 'grantFullControl' - Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
-- * 'contentMD5' - The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
-- * 'accessControlPolicy' - Contains the elements that set the ACL permissions for an object per grantee.
-- * 'grantWrite' - Allows grantee to create, overwrite, and delete any object in the bucket.
-- * 'acl' - The canned ACL to apply to the bucket.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkPutBucketACL ::
  -- | 'bucket'
  BucketName ->
  PutBucketACL
mkPutBucketACL pBucket_ =
  PutBucketACL'
    { grantReadACP = Lude.Nothing,
      bucket = pBucket_,
      grantWriteACP = Lude.Nothing,
      grantRead = Lude.Nothing,
      grantFullControl = Lude.Nothing,
      contentMD5 = Lude.Nothing,
      accessControlPolicy = Lude.Nothing,
      grantWrite = Lude.Nothing,
      acl = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing
    }

-- | Allows grantee to read the bucket ACL.
--
-- /Note:/ Consider using 'grantReadACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaGrantReadACP :: Lens.Lens' PutBucketACL (Lude.Maybe Lude.Text)
pbaGrantReadACP = Lens.lens (grantReadACP :: PutBucketACL -> Lude.Maybe Lude.Text) (\s a -> s {grantReadACP = a} :: PutBucketACL)
{-# DEPRECATED pbaGrantReadACP "Use generic-lens or generic-optics with 'grantReadACP' instead." #-}

-- | The bucket to which to apply the ACL.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaBucket :: Lens.Lens' PutBucketACL BucketName
pbaBucket = Lens.lens (bucket :: PutBucketACL -> BucketName) (\s a -> s {bucket = a} :: PutBucketACL)
{-# DEPRECATED pbaBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Allows grantee to write the ACL for the applicable bucket.
--
-- /Note:/ Consider using 'grantWriteACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaGrantWriteACP :: Lens.Lens' PutBucketACL (Lude.Maybe Lude.Text)
pbaGrantWriteACP = Lens.lens (grantWriteACP :: PutBucketACL -> Lude.Maybe Lude.Text) (\s a -> s {grantWriteACP = a} :: PutBucketACL)
{-# DEPRECATED pbaGrantWriteACP "Use generic-lens or generic-optics with 'grantWriteACP' instead." #-}

-- | Allows grantee to list the objects in the bucket.
--
-- /Note:/ Consider using 'grantRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaGrantRead :: Lens.Lens' PutBucketACL (Lude.Maybe Lude.Text)
pbaGrantRead = Lens.lens (grantRead :: PutBucketACL -> Lude.Maybe Lude.Text) (\s a -> s {grantRead = a} :: PutBucketACL)
{-# DEPRECATED pbaGrantRead "Use generic-lens or generic-optics with 'grantRead' instead." #-}

-- | Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
--
-- /Note:/ Consider using 'grantFullControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaGrantFullControl :: Lens.Lens' PutBucketACL (Lude.Maybe Lude.Text)
pbaGrantFullControl = Lens.lens (grantFullControl :: PutBucketACL -> Lude.Maybe Lude.Text) (\s a -> s {grantFullControl = a} :: PutBucketACL)
{-# DEPRECATED pbaGrantFullControl "Use generic-lens or generic-optics with 'grantFullControl' instead." #-}

-- | The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaContentMD5 :: Lens.Lens' PutBucketACL (Lude.Maybe Lude.Text)
pbaContentMD5 = Lens.lens (contentMD5 :: PutBucketACL -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutBucketACL)
{-# DEPRECATED pbaContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | Contains the elements that set the ACL permissions for an object per grantee.
--
-- /Note:/ Consider using 'accessControlPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaAccessControlPolicy :: Lens.Lens' PutBucketACL (Lude.Maybe AccessControlPolicy)
pbaAccessControlPolicy = Lens.lens (accessControlPolicy :: PutBucketACL -> Lude.Maybe AccessControlPolicy) (\s a -> s {accessControlPolicy = a} :: PutBucketACL)
{-# DEPRECATED pbaAccessControlPolicy "Use generic-lens or generic-optics with 'accessControlPolicy' instead." #-}

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
--
-- /Note:/ Consider using 'grantWrite' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaGrantWrite :: Lens.Lens' PutBucketACL (Lude.Maybe Lude.Text)
pbaGrantWrite = Lens.lens (grantWrite :: PutBucketACL -> Lude.Maybe Lude.Text) (\s a -> s {grantWrite = a} :: PutBucketACL)
{-# DEPRECATED pbaGrantWrite "Use generic-lens or generic-optics with 'grantWrite' instead." #-}

-- | The canned ACL to apply to the bucket.
--
-- /Note:/ Consider using 'acl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaACL :: Lens.Lens' PutBucketACL (Lude.Maybe BucketCannedACL)
pbaACL = Lens.lens (acl :: PutBucketACL -> Lude.Maybe BucketCannedACL) (\s a -> s {acl = a} :: PutBucketACL)
{-# DEPRECATED pbaACL "Use generic-lens or generic-optics with 'acl' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaExpectedBucketOwner :: Lens.Lens' PutBucketACL (Lude.Maybe Lude.Text)
pbaExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutBucketACL -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutBucketACL)
{-# DEPRECATED pbaExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest PutBucketACL where
  type Rs PutBucketACL = PutBucketACLResponse
  request = Req.putXML s3Service
  response = Res.receiveNull PutBucketACLResponse'

instance Lude.ToElement PutBucketACL where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}AccessControlPolicy"
      Lude.. accessControlPolicy

instance Lude.ToHeaders PutBucketACL where
  toHeaders PutBucketACL' {..} =
    Lude.mconcat
      [ "x-amz-grant-read-acp" Lude.=# grantReadACP,
        "x-amz-grant-write-acp" Lude.=# grantWriteACP,
        "x-amz-grant-read" Lude.=# grantRead,
        "x-amz-grant-full-control" Lude.=# grantFullControl,
        "Content-MD5" Lude.=# contentMD5,
        "x-amz-grant-write" Lude.=# grantWrite,
        "x-amz-acl" Lude.=# acl,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath PutBucketACL where
  toPath PutBucketACL' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutBucketACL where
  toQuery = Lude.const (Lude.mconcat ["acl"])

-- | /See:/ 'mkPutBucketACLResponse' smart constructor.
data PutBucketACLResponse = PutBucketACLResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketACLResponse' with the minimum fields required to make a request.
mkPutBucketACLResponse ::
  PutBucketACLResponse
mkPutBucketACLResponse = PutBucketACLResponse'
