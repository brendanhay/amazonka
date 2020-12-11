{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutObjectACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uses the @acl@ subresource to set the access control list (ACL) permissions for a new or existing object in an S3 bucket. You must have @WRITE_ACP@ permission to set the ACL of an object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#permissions What permissions can I grant?> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- This action is not supported by Amazon S3 on Outposts.
-- Depending on your application needs, you can choose to set the ACL on an object using either the request body or the headers. For example, if you have an existing application that updates a bucket ACL using the request body, you can continue to use that approach. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> in the /Amazon S3 Developer Guide/ .
-- __Access Permissions__
-- You can set access permissions using one of the following methods:
--
--     * Specify a canned ACL with the @x-amz-acl@ request header. Amazon S3 supports a set of predefined ACLs, known as canned ACLs. Each canned ACL has a predefined set of grantees and permissions. Specify the canned ACL name as the value of @x-amz-ac@ l. If you use this header, you cannot use other access control-specific headers in your request. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
--
--     * Specify access permissions explicitly with the @x-amz-grant-read@ , @x-amz-grant-read-acp@ , @x-amz-grant-write-acp@ , and @x-amz-grant-full-control@ headers. When using these headers, you specify explicit access permissions and grantees (AWS accounts or Amazon S3 groups) who will receive the permission. If you use these ACL-specific headers, you cannot use @x-amz-acl@ header to set a canned ACL. These parameters map to the set of permissions that Amazon S3 supports in an ACL. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> .
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
-- For example, the following @x-amz-grant-read@ header grants list objects permission to the two AWS accounts identified by their email addresses.
-- @x-amz-grant-read: emailAddress="xyz@amazon.com", emailAddress="abc@amazon.com" @
--
--
-- You can use either a canned ACL or specify access permissions explicitly. You cannot do both.
-- __Grantee Values__
-- You can specify the person (grantee) to whom you're assigning access rights (using request elements) in the following ways:
--
--     * By the person's ID:
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser"><ID><>ID<></ID><DisplayName><>GranteesEmail<></DisplayName> </Grantee>@
-- DisplayName is optional and ignored in the request.
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
-- __Versioning__
-- The ACL of an object is set at the object version level. By default, PUT sets the ACL of the current version of an object. To set the ACL of a different version, use the @versionId@ subresource.
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CopyObject.html CopyObject>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
module Network.AWS.S3.PutObjectACL
  ( -- * Creating a request
    PutObjectACL (..),
    mkPutObjectACL,

    -- ** Request lenses
    poaVersionId,
    poaGrantReadACP,
    poaRequestPayer,
    poaGrantWriteACP,
    poaGrantRead,
    poaGrantFullControl,
    poaContentMD5,
    poaAccessControlPolicy,
    poaGrantWrite,
    poaACL,
    poaExpectedBucketOwner,
    poaBucket,
    poaKey,

    -- * Destructuring the response
    PutObjectACLResponse (..),
    mkPutObjectACLResponse,

    -- ** Response lenses
    poarsRequestCharged,
    poarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutObjectACL' smart constructor.
data PutObjectACL = PutObjectACL'
  { versionId ::
      Lude.Maybe ObjectVersionId,
    grantReadACP :: Lude.Maybe Lude.Text,
    requestPayer :: Lude.Maybe RequestPayer,
    grantWriteACP :: Lude.Maybe Lude.Text,
    grantRead :: Lude.Maybe Lude.Text,
    grantFullControl :: Lude.Maybe Lude.Text,
    contentMD5 :: Lude.Maybe Lude.Text,
    accessControlPolicy :: Lude.Maybe AccessControlPolicy,
    grantWrite :: Lude.Maybe Lude.Text,
    acl :: Lude.Maybe ObjectCannedACL,
    expectedBucketOwner :: Lude.Maybe Lude.Text,
    bucket :: BucketName,
    key :: ObjectKey
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutObjectACL' with the minimum fields required to make a request.
--
-- * 'accessControlPolicy' - Contains the elements that set the ACL permissions for an object per grantee.
-- * 'acl' - The canned ACL to apply to the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
-- * 'bucket' - The bucket name that contains the object to which you want to attach the ACL.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'contentMD5' - The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>>
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'grantFullControl' - Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
-- * 'grantRead' - Allows grantee to list the objects in the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
-- * 'grantReadACP' - Allows grantee to read the bucket ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
-- * 'grantWrite' - Allows grantee to create, overwrite, and delete any object in the bucket.
-- * 'grantWriteACP' - Allows grantee to write the ACL for the applicable bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
-- * 'key' - Key for which the PUT operation was initiated.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'requestPayer' - Undocumented field.
-- * 'versionId' - VersionId used to reference a specific version of the object.
mkPutObjectACL ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  PutObjectACL
mkPutObjectACL pBucket_ pKey_ =
  PutObjectACL'
    { versionId = Lude.Nothing,
      grantReadACP = Lude.Nothing,
      requestPayer = Lude.Nothing,
      grantWriteACP = Lude.Nothing,
      grantRead = Lude.Nothing,
      grantFullControl = Lude.Nothing,
      contentMD5 = Lude.Nothing,
      accessControlPolicy = Lude.Nothing,
      grantWrite = Lude.Nothing,
      acl = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | VersionId used to reference a specific version of the object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaVersionId :: Lens.Lens' PutObjectACL (Lude.Maybe ObjectVersionId)
poaVersionId = Lens.lens (versionId :: PutObjectACL -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: PutObjectACL)
{-# DEPRECATED poaVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Allows grantee to read the bucket ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantReadACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaGrantReadACP :: Lens.Lens' PutObjectACL (Lude.Maybe Lude.Text)
poaGrantReadACP = Lens.lens (grantReadACP :: PutObjectACL -> Lude.Maybe Lude.Text) (\s a -> s {grantReadACP = a} :: PutObjectACL)
{-# DEPRECATED poaGrantReadACP "Use generic-lens or generic-optics with 'grantReadACP' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaRequestPayer :: Lens.Lens' PutObjectACL (Lude.Maybe RequestPayer)
poaRequestPayer = Lens.lens (requestPayer :: PutObjectACL -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: PutObjectACL)
{-# DEPRECATED poaRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Allows grantee to write the ACL for the applicable bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantWriteACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaGrantWriteACP :: Lens.Lens' PutObjectACL (Lude.Maybe Lude.Text)
poaGrantWriteACP = Lens.lens (grantWriteACP :: PutObjectACL -> Lude.Maybe Lude.Text) (\s a -> s {grantWriteACP = a} :: PutObjectACL)
{-# DEPRECATED poaGrantWriteACP "Use generic-lens or generic-optics with 'grantWriteACP' instead." #-}

-- | Allows grantee to list the objects in the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaGrantRead :: Lens.Lens' PutObjectACL (Lude.Maybe Lude.Text)
poaGrantRead = Lens.lens (grantRead :: PutObjectACL -> Lude.Maybe Lude.Text) (\s a -> s {grantRead = a} :: PutObjectACL)
{-# DEPRECATED poaGrantRead "Use generic-lens or generic-optics with 'grantRead' instead." #-}

-- | Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantFullControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaGrantFullControl :: Lens.Lens' PutObjectACL (Lude.Maybe Lude.Text)
poaGrantFullControl = Lens.lens (grantFullControl :: PutObjectACL -> Lude.Maybe Lude.Text) (\s a -> s {grantFullControl = a} :: PutObjectACL)
{-# DEPRECATED poaGrantFullControl "Use generic-lens or generic-optics with 'grantFullControl' instead." #-}

-- | The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>>
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaContentMD5 :: Lens.Lens' PutObjectACL (Lude.Maybe Lude.Text)
poaContentMD5 = Lens.lens (contentMD5 :: PutObjectACL -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutObjectACL)
{-# DEPRECATED poaContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | Contains the elements that set the ACL permissions for an object per grantee.
--
-- /Note:/ Consider using 'accessControlPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaAccessControlPolicy :: Lens.Lens' PutObjectACL (Lude.Maybe AccessControlPolicy)
poaAccessControlPolicy = Lens.lens (accessControlPolicy :: PutObjectACL -> Lude.Maybe AccessControlPolicy) (\s a -> s {accessControlPolicy = a} :: PutObjectACL)
{-# DEPRECATED poaAccessControlPolicy "Use generic-lens or generic-optics with 'accessControlPolicy' instead." #-}

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
--
-- /Note:/ Consider using 'grantWrite' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaGrantWrite :: Lens.Lens' PutObjectACL (Lude.Maybe Lude.Text)
poaGrantWrite = Lens.lens (grantWrite :: PutObjectACL -> Lude.Maybe Lude.Text) (\s a -> s {grantWrite = a} :: PutObjectACL)
{-# DEPRECATED poaGrantWrite "Use generic-lens or generic-optics with 'grantWrite' instead." #-}

-- | The canned ACL to apply to the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
-- /Note:/ Consider using 'acl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaACL :: Lens.Lens' PutObjectACL (Lude.Maybe ObjectCannedACL)
poaACL = Lens.lens (acl :: PutObjectACL -> Lude.Maybe ObjectCannedACL) (\s a -> s {acl = a} :: PutObjectACL)
{-# DEPRECATED poaACL "Use generic-lens or generic-optics with 'acl' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaExpectedBucketOwner :: Lens.Lens' PutObjectACL (Lude.Maybe Lude.Text)
poaExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutObjectACL -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutObjectACL)
{-# DEPRECATED poaExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name that contains the object to which you want to attach the ACL.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaBucket :: Lens.Lens' PutObjectACL BucketName
poaBucket = Lens.lens (bucket :: PutObjectACL -> BucketName) (\s a -> s {bucket = a} :: PutObjectACL)
{-# DEPRECATED poaBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Key for which the PUT operation was initiated.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaKey :: Lens.Lens' PutObjectACL ObjectKey
poaKey = Lens.lens (key :: PutObjectACL -> ObjectKey) (\s a -> s {key = a} :: PutObjectACL)
{-# DEPRECATED poaKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.AWSRequest PutObjectACL where
  type Rs PutObjectACL = PutObjectACLResponse
  request = Req.putXML s3Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutObjectACLResponse'
            Lude.<$> (h Lude..#? "x-amz-request-charged")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement PutObjectACL where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}AccessControlPolicy"
      Lude.. accessControlPolicy

instance Lude.ToHeaders PutObjectACL where
  toHeaders PutObjectACL' {..} =
    Lude.mconcat
      [ "x-amz-grant-read-acp" Lude.=# grantReadACP,
        "x-amz-request-payer" Lude.=# requestPayer,
        "x-amz-grant-write-acp" Lude.=# grantWriteACP,
        "x-amz-grant-read" Lude.=# grantRead,
        "x-amz-grant-full-control" Lude.=# grantFullControl,
        "Content-MD5" Lude.=# contentMD5,
        "x-amz-grant-write" Lude.=# grantWrite,
        "x-amz-acl" Lude.=# acl,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath PutObjectACL where
  toPath PutObjectACL' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery PutObjectACL where
  toQuery PutObjectACL' {..} =
    Lude.mconcat ["versionId" Lude.=: versionId, "acl"]

-- | /See:/ 'mkPutObjectACLResponse' smart constructor.
data PutObjectACLResponse = PutObjectACLResponse'
  { requestCharged ::
      Lude.Maybe RequestCharged,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutObjectACLResponse' with the minimum fields required to make a request.
--
-- * 'requestCharged' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkPutObjectACLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutObjectACLResponse
mkPutObjectACLResponse pResponseStatus_ =
  PutObjectACLResponse'
    { requestCharged = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poarsRequestCharged :: Lens.Lens' PutObjectACLResponse (Lude.Maybe RequestCharged)
poarsRequestCharged = Lens.lens (requestCharged :: PutObjectACLResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: PutObjectACLResponse)
{-# DEPRECATED poarsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poarsResponseStatus :: Lens.Lens' PutObjectACLResponse Lude.Int
poarsResponseStatus = Lens.lens (responseStatus :: PutObjectACLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutObjectACLResponse)
{-# DEPRECATED poarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
