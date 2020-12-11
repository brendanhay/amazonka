{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObjectRetention
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an object's retention settings. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects> .
--
-- This action is not supported by Amazon S3 on Outposts.
module Network.AWS.S3.GetObjectRetention
  ( -- * Creating a request
    GetObjectRetention (..),
    mkGetObjectRetention,

    -- ** Request lenses
    gorVersionId,
    gorRequestPayer,
    gorExpectedBucketOwner,
    gorBucket,
    gorKey,

    -- * Destructuring the response
    GetObjectRetentionResponse (..),
    mkGetObjectRetentionResponse,

    -- ** Response lenses
    gorrsRetention,
    gorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetObjectRetention' smart constructor.
data GetObjectRetention = GetObjectRetention'
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

-- | Creates a value of 'GetObjectRetention' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name containing the object whose retention settings you want to retrieve.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'key' - The key name for the object whose retention settings you want to retrieve.
-- * 'requestPayer' - Undocumented field.
-- * 'versionId' - The version ID for the object whose retention settings you want to retrieve.
mkGetObjectRetention ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  GetObjectRetention
mkGetObjectRetention pBucket_ pKey_ =
  GetObjectRetention'
    { versionId = Lude.Nothing,
      requestPayer = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The version ID for the object whose retention settings you want to retrieve.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorVersionId :: Lens.Lens' GetObjectRetention (Lude.Maybe ObjectVersionId)
gorVersionId = Lens.lens (versionId :: GetObjectRetention -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: GetObjectRetention)
{-# DEPRECATED gorVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorRequestPayer :: Lens.Lens' GetObjectRetention (Lude.Maybe RequestPayer)
gorRequestPayer = Lens.lens (requestPayer :: GetObjectRetention -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: GetObjectRetention)
{-# DEPRECATED gorRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorExpectedBucketOwner :: Lens.Lens' GetObjectRetention (Lude.Maybe Lude.Text)
gorExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetObjectRetention -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetObjectRetention)
{-# DEPRECATED gorExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name containing the object whose retention settings you want to retrieve.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorBucket :: Lens.Lens' GetObjectRetention BucketName
gorBucket = Lens.lens (bucket :: GetObjectRetention -> BucketName) (\s a -> s {bucket = a} :: GetObjectRetention)
{-# DEPRECATED gorBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The key name for the object whose retention settings you want to retrieve.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorKey :: Lens.Lens' GetObjectRetention ObjectKey
gorKey = Lens.lens (key :: GetObjectRetention -> ObjectKey) (\s a -> s {key = a} :: GetObjectRetention)
{-# DEPRECATED gorKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.AWSRequest GetObjectRetention where
  type Rs GetObjectRetention = GetObjectRetentionResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetObjectRetentionResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetObjectRetention where
  toHeaders GetObjectRetention' {..} =
    Lude.mconcat
      [ "x-amz-request-payer" Lude.=# requestPayer,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath GetObjectRetention where
  toPath GetObjectRetention' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery GetObjectRetention where
  toQuery GetObjectRetention' {..} =
    Lude.mconcat ["versionId" Lude.=: versionId, "retention"]

-- | /See:/ 'mkGetObjectRetentionResponse' smart constructor.
data GetObjectRetentionResponse = GetObjectRetentionResponse'
  { retention ::
      Lude.Maybe ObjectLockRetention,
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

-- | Creates a value of 'GetObjectRetentionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'retention' - The container element for an object's retention settings.
mkGetObjectRetentionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetObjectRetentionResponse
mkGetObjectRetentionResponse pResponseStatus_ =
  GetObjectRetentionResponse'
    { retention = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The container element for an object's retention settings.
--
-- /Note:/ Consider using 'retention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsRetention :: Lens.Lens' GetObjectRetentionResponse (Lude.Maybe ObjectLockRetention)
gorrsRetention = Lens.lens (retention :: GetObjectRetentionResponse -> Lude.Maybe ObjectLockRetention) (\s a -> s {retention = a} :: GetObjectRetentionResponse)
{-# DEPRECATED gorrsRetention "Use generic-lens or generic-optics with 'retention' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsResponseStatus :: Lens.Lens' GetObjectRetentionResponse Lude.Int
gorrsResponseStatus = Lens.lens (responseStatus :: GetObjectRetentionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetObjectRetentionResponse)
{-# DEPRECATED gorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
