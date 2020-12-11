{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutObjectRetention
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Places an Object Retention configuration on an object.
--
-- This action is not supported by Amazon S3 on Outposts.
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects>
module Network.AWS.S3.PutObjectRetention
  ( -- * Creating a request
    PutObjectRetention (..),
    mkPutObjectRetention,

    -- ** Request lenses
    porRetention,
    porVersionId,
    porRequestPayer,
    porContentMD5,
    porBypassGovernanceRetention,
    porExpectedBucketOwner,
    porBucket,
    porKey,

    -- * Destructuring the response
    PutObjectRetentionResponse (..),
    mkPutObjectRetentionResponse,

    -- ** Response lenses
    porrsRequestCharged,
    porrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutObjectRetention' smart constructor.
data PutObjectRetention = PutObjectRetention'
  { retention ::
      Lude.Maybe ObjectLockRetention,
    versionId :: Lude.Maybe ObjectVersionId,
    requestPayer :: Lude.Maybe RequestPayer,
    contentMD5 :: Lude.Maybe Lude.Text,
    bypassGovernanceRetention :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'PutObjectRetention' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name that contains the object you want to apply this Object Retention configuration to.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'bypassGovernanceRetention' - Indicates whether this operation should bypass Governance-mode restrictions.
-- * 'contentMD5' - The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'key' - The key name for the object that you want to apply this Object Retention configuration to.
-- * 'requestPayer' - Undocumented field.
-- * 'retention' - The container element for the Object Retention configuration.
-- * 'versionId' - The version ID for the object that you want to apply this Object Retention configuration to.
mkPutObjectRetention ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  PutObjectRetention
mkPutObjectRetention pBucket_ pKey_ =
  PutObjectRetention'
    { retention = Lude.Nothing,
      versionId = Lude.Nothing,
      requestPayer = Lude.Nothing,
      contentMD5 = Lude.Nothing,
      bypassGovernanceRetention = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The container element for the Object Retention configuration.
--
-- /Note:/ Consider using 'retention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porRetention :: Lens.Lens' PutObjectRetention (Lude.Maybe ObjectLockRetention)
porRetention = Lens.lens (retention :: PutObjectRetention -> Lude.Maybe ObjectLockRetention) (\s a -> s {retention = a} :: PutObjectRetention)
{-# DEPRECATED porRetention "Use generic-lens or generic-optics with 'retention' instead." #-}

-- | The version ID for the object that you want to apply this Object Retention configuration to.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porVersionId :: Lens.Lens' PutObjectRetention (Lude.Maybe ObjectVersionId)
porVersionId = Lens.lens (versionId :: PutObjectRetention -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: PutObjectRetention)
{-# DEPRECATED porVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porRequestPayer :: Lens.Lens' PutObjectRetention (Lude.Maybe RequestPayer)
porRequestPayer = Lens.lens (requestPayer :: PutObjectRetention -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: PutObjectRetention)
{-# DEPRECATED porRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porContentMD5 :: Lens.Lens' PutObjectRetention (Lude.Maybe Lude.Text)
porContentMD5 = Lens.lens (contentMD5 :: PutObjectRetention -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutObjectRetention)
{-# DEPRECATED porContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | Indicates whether this operation should bypass Governance-mode restrictions.
--
-- /Note:/ Consider using 'bypassGovernanceRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porBypassGovernanceRetention :: Lens.Lens' PutObjectRetention (Lude.Maybe Lude.Bool)
porBypassGovernanceRetention = Lens.lens (bypassGovernanceRetention :: PutObjectRetention -> Lude.Maybe Lude.Bool) (\s a -> s {bypassGovernanceRetention = a} :: PutObjectRetention)
{-# DEPRECATED porBypassGovernanceRetention "Use generic-lens or generic-optics with 'bypassGovernanceRetention' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porExpectedBucketOwner :: Lens.Lens' PutObjectRetention (Lude.Maybe Lude.Text)
porExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutObjectRetention -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutObjectRetention)
{-# DEPRECATED porExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name that contains the object you want to apply this Object Retention configuration to.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porBucket :: Lens.Lens' PutObjectRetention BucketName
porBucket = Lens.lens (bucket :: PutObjectRetention -> BucketName) (\s a -> s {bucket = a} :: PutObjectRetention)
{-# DEPRECATED porBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The key name for the object that you want to apply this Object Retention configuration to.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porKey :: Lens.Lens' PutObjectRetention ObjectKey
porKey = Lens.lens (key :: PutObjectRetention -> ObjectKey) (\s a -> s {key = a} :: PutObjectRetention)
{-# DEPRECATED porKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.AWSRequest PutObjectRetention where
  type Rs PutObjectRetention = PutObjectRetentionResponse
  request = Req.putXML s3Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutObjectRetentionResponse'
            Lude.<$> (h Lude..#? "x-amz-request-charged")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement PutObjectRetention where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}Retention"
      Lude.. retention

instance Lude.ToHeaders PutObjectRetention where
  toHeaders PutObjectRetention' {..} =
    Lude.mconcat
      [ "x-amz-request-payer" Lude.=# requestPayer,
        "Content-MD5" Lude.=# contentMD5,
        "x-amz-bypass-governance-retention"
          Lude.=# bypassGovernanceRetention,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath PutObjectRetention where
  toPath PutObjectRetention' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery PutObjectRetention where
  toQuery PutObjectRetention' {..} =
    Lude.mconcat ["versionId" Lude.=: versionId, "retention"]

-- | /See:/ 'mkPutObjectRetentionResponse' smart constructor.
data PutObjectRetentionResponse = PutObjectRetentionResponse'
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

-- | Creates a value of 'PutObjectRetentionResponse' with the minimum fields required to make a request.
--
-- * 'requestCharged' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkPutObjectRetentionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutObjectRetentionResponse
mkPutObjectRetentionResponse pResponseStatus_ =
  PutObjectRetentionResponse'
    { requestCharged = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsRequestCharged :: Lens.Lens' PutObjectRetentionResponse (Lude.Maybe RequestCharged)
porrsRequestCharged = Lens.lens (requestCharged :: PutObjectRetentionResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: PutObjectRetentionResponse)
{-# DEPRECATED porrsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsResponseStatus :: Lens.Lens' PutObjectRetentionResponse Lude.Int
porrsResponseStatus = Lens.lens (responseStatus :: PutObjectRetentionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutObjectRetentionResponse)
{-# DEPRECATED porrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
