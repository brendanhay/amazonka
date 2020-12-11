{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObjectACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the access control list (ACL) of an object. To use this operation, you must have @READ_ACP@ access to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
-- __Versioning__
-- By default, GET returns ACL information about the current version of an object. To return ACL information about a different version, use the versionId subresource.
-- The following operations are related to @GetObjectAcl@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
module Network.AWS.S3.GetObjectACL
  ( -- * Creating a request
    GetObjectACL (..),
    mkGetObjectACL,

    -- ** Request lenses
    goaVersionId,
    goaRequestPayer,
    goaExpectedBucketOwner,
    goaBucket,
    goaKey,

    -- * Destructuring the response
    GetObjectACLResponse (..),
    mkGetObjectACLResponse,

    -- ** Response lenses
    goarsRequestCharged,
    goarsGrants,
    goarsOwner,
    goarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetObjectACL' smart constructor.
data GetObjectACL = GetObjectACL'
  { versionId ::
      Lude.Maybe ObjectVersionId,
    requestPayer :: Lude.Maybe RequestPayer,
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

-- | Creates a value of 'GetObjectACL' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name that contains the object for which to get the ACL information.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'key' - The key of the object for which to get the ACL information.
-- * 'requestPayer' - Undocumented field.
-- * 'versionId' - VersionId used to reference a specific version of the object.
mkGetObjectACL ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  GetObjectACL
mkGetObjectACL pBucket_ pKey_ =
  GetObjectACL'
    { versionId = Lude.Nothing,
      requestPayer = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | VersionId used to reference a specific version of the object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaVersionId :: Lens.Lens' GetObjectACL (Lude.Maybe ObjectVersionId)
goaVersionId = Lens.lens (versionId :: GetObjectACL -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: GetObjectACL)
{-# DEPRECATED goaVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaRequestPayer :: Lens.Lens' GetObjectACL (Lude.Maybe RequestPayer)
goaRequestPayer = Lens.lens (requestPayer :: GetObjectACL -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: GetObjectACL)
{-# DEPRECATED goaRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaExpectedBucketOwner :: Lens.Lens' GetObjectACL (Lude.Maybe Lude.Text)
goaExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetObjectACL -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetObjectACL)
{-# DEPRECATED goaExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name that contains the object for which to get the ACL information.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaBucket :: Lens.Lens' GetObjectACL BucketName
goaBucket = Lens.lens (bucket :: GetObjectACL -> BucketName) (\s a -> s {bucket = a} :: GetObjectACL)
{-# DEPRECATED goaBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The key of the object for which to get the ACL information.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaKey :: Lens.Lens' GetObjectACL ObjectKey
goaKey = Lens.lens (key :: GetObjectACL -> ObjectKey) (\s a -> s {key = a} :: GetObjectACL)
{-# DEPRECATED goaKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.AWSRequest GetObjectACL where
  type Rs GetObjectACL = GetObjectACLResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetObjectACLResponse'
            Lude.<$> (h Lude..#? "x-amz-request-charged")
            Lude.<*> ( x Lude..@? "AccessControlList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "Grant")
                     )
            Lude.<*> (x Lude..@? "Owner")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetObjectACL where
  toHeaders GetObjectACL' {..} =
    Lude.mconcat
      [ "x-amz-request-payer" Lude.=# requestPayer,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath GetObjectACL where
  toPath GetObjectACL' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery GetObjectACL where
  toQuery GetObjectACL' {..} =
    Lude.mconcat ["versionId" Lude.=: versionId, "acl"]

-- | /See:/ 'mkGetObjectACLResponse' smart constructor.
data GetObjectACLResponse = GetObjectACLResponse'
  { requestCharged ::
      Lude.Maybe RequestCharged,
    grants :: Lude.Maybe [Grant],
    owner :: Lude.Maybe Owner,
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

-- | Creates a value of 'GetObjectACLResponse' with the minimum fields required to make a request.
--
-- * 'grants' - A list of grants.
-- * 'owner' - Container for the bucket owner's display name and ID.
-- * 'requestCharged' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetObjectACLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetObjectACLResponse
mkGetObjectACLResponse pResponseStatus_ =
  GetObjectACLResponse'
    { requestCharged = Lude.Nothing,
      grants = Lude.Nothing,
      owner = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goarsRequestCharged :: Lens.Lens' GetObjectACLResponse (Lude.Maybe RequestCharged)
goarsRequestCharged = Lens.lens (requestCharged :: GetObjectACLResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: GetObjectACLResponse)
{-# DEPRECATED goarsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | A list of grants.
--
-- /Note:/ Consider using 'grants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goarsGrants :: Lens.Lens' GetObjectACLResponse (Lude.Maybe [Grant])
goarsGrants = Lens.lens (grants :: GetObjectACLResponse -> Lude.Maybe [Grant]) (\s a -> s {grants = a} :: GetObjectACLResponse)
{-# DEPRECATED goarsGrants "Use generic-lens or generic-optics with 'grants' instead." #-}

-- | Container for the bucket owner's display name and ID.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goarsOwner :: Lens.Lens' GetObjectACLResponse (Lude.Maybe Owner)
goarsOwner = Lens.lens (owner :: GetObjectACLResponse -> Lude.Maybe Owner) (\s a -> s {owner = a} :: GetObjectACLResponse)
{-# DEPRECATED goarsOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goarsResponseStatus :: Lens.Lens' GetObjectACLResponse Lude.Int
goarsResponseStatus = Lens.lens (responseStatus :: GetObjectACLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetObjectACLResponse)
{-# DEPRECATED goarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
