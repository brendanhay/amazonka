{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObjectLegalHold
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an object's current Legal Hold status. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects> .
--
-- This action is not supported by Amazon S3 on Outposts.
module Network.AWS.S3.GetObjectLegalHold
  ( -- * Creating a request
    GetObjectLegalHold (..),
    mkGetObjectLegalHold,

    -- ** Request lenses
    golhVersionId,
    golhBucket,
    golhRequestPayer,
    golhKey,
    golhExpectedBucketOwner,

    -- * Destructuring the response
    GetObjectLegalHoldResponse (..),
    mkGetObjectLegalHoldResponse,

    -- ** Response lenses
    golhrsLegalHold,
    golhrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetObjectLegalHold' smart constructor.
data GetObjectLegalHold = GetObjectLegalHold'
  { -- | The version ID of the object whose Legal Hold status you want to retrieve.
    versionId :: Lude.Maybe ObjectVersionId,
    -- | The bucket name containing the object whose Legal Hold status you want to retrieve.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: BucketName,
    requestPayer :: Lude.Maybe RequestPayer,
    -- | The key name for the object whose Legal Hold status you want to retrieve.
    key :: ObjectKey,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetObjectLegalHold' with the minimum fields required to make a request.
--
-- * 'versionId' - The version ID of the object whose Legal Hold status you want to retrieve.
-- * 'bucket' - The bucket name containing the object whose Legal Hold status you want to retrieve.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'requestPayer' -
-- * 'key' - The key name for the object whose Legal Hold status you want to retrieve.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetObjectLegalHold ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  GetObjectLegalHold
mkGetObjectLegalHold pBucket_ pKey_ =
  GetObjectLegalHold'
    { versionId = Lude.Nothing,
      bucket = pBucket_,
      requestPayer = Lude.Nothing,
      key = pKey_,
      expectedBucketOwner = Lude.Nothing
    }

-- | The version ID of the object whose Legal Hold status you want to retrieve.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golhVersionId :: Lens.Lens' GetObjectLegalHold (Lude.Maybe ObjectVersionId)
golhVersionId = Lens.lens (versionId :: GetObjectLegalHold -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: GetObjectLegalHold)
{-# DEPRECATED golhVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The bucket name containing the object whose Legal Hold status you want to retrieve.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golhBucket :: Lens.Lens' GetObjectLegalHold BucketName
golhBucket = Lens.lens (bucket :: GetObjectLegalHold -> BucketName) (\s a -> s {bucket = a} :: GetObjectLegalHold)
{-# DEPRECATED golhBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golhRequestPayer :: Lens.Lens' GetObjectLegalHold (Lude.Maybe RequestPayer)
golhRequestPayer = Lens.lens (requestPayer :: GetObjectLegalHold -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: GetObjectLegalHold)
{-# DEPRECATED golhRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | The key name for the object whose Legal Hold status you want to retrieve.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golhKey :: Lens.Lens' GetObjectLegalHold ObjectKey
golhKey = Lens.lens (key :: GetObjectLegalHold -> ObjectKey) (\s a -> s {key = a} :: GetObjectLegalHold)
{-# DEPRECATED golhKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golhExpectedBucketOwner :: Lens.Lens' GetObjectLegalHold (Lude.Maybe Lude.Text)
golhExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetObjectLegalHold -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetObjectLegalHold)
{-# DEPRECATED golhExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest GetObjectLegalHold where
  type Rs GetObjectLegalHold = GetObjectLegalHoldResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetObjectLegalHoldResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetObjectLegalHold where
  toHeaders GetObjectLegalHold' {..} =
    Lude.mconcat
      [ "x-amz-request-payer" Lude.=# requestPayer,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath GetObjectLegalHold where
  toPath GetObjectLegalHold' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery GetObjectLegalHold where
  toQuery GetObjectLegalHold' {..} =
    Lude.mconcat ["versionId" Lude.=: versionId, "legal-hold"]

-- | /See:/ 'mkGetObjectLegalHoldResponse' smart constructor.
data GetObjectLegalHoldResponse = GetObjectLegalHoldResponse'
  { -- | The current Legal Hold status for the specified object.
    legalHold :: Lude.Maybe ObjectLockLegalHold,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetObjectLegalHoldResponse' with the minimum fields required to make a request.
--
-- * 'legalHold' - The current Legal Hold status for the specified object.
-- * 'responseStatus' - The response status code.
mkGetObjectLegalHoldResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetObjectLegalHoldResponse
mkGetObjectLegalHoldResponse pResponseStatus_ =
  GetObjectLegalHoldResponse'
    { legalHold = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current Legal Hold status for the specified object.
--
-- /Note:/ Consider using 'legalHold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golhrsLegalHold :: Lens.Lens' GetObjectLegalHoldResponse (Lude.Maybe ObjectLockLegalHold)
golhrsLegalHold = Lens.lens (legalHold :: GetObjectLegalHoldResponse -> Lude.Maybe ObjectLockLegalHold) (\s a -> s {legalHold = a} :: GetObjectLegalHoldResponse)
{-# DEPRECATED golhrsLegalHold "Use generic-lens or generic-optics with 'legalHold' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golhrsResponseStatus :: Lens.Lens' GetObjectLegalHoldResponse Lude.Int
golhrsResponseStatus = Lens.lens (responseStatus :: GetObjectLegalHoldResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetObjectLegalHoldResponse)
{-# DEPRECATED golhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
