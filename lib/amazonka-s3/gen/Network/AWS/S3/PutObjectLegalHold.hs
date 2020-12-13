{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutObjectLegalHold
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a Legal Hold configuration to the specified object.
--
-- This action is not supported by Amazon S3 on Outposts.
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects>
module Network.AWS.S3.PutObjectLegalHold
  ( -- * Creating a request
    PutObjectLegalHold (..),
    mkPutObjectLegalHold,

    -- ** Request lenses
    polhLegalHold,
    polhVersionId,
    polhBucket,
    polhRequestPayer,
    polhKey,
    polhContentMD5,
    polhExpectedBucketOwner,

    -- * Destructuring the response
    PutObjectLegalHoldResponse (..),
    mkPutObjectLegalHoldResponse,

    -- ** Response lenses
    polhrsRequestCharged,
    polhrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutObjectLegalHold' smart constructor.
data PutObjectLegalHold = PutObjectLegalHold'
  { -- | Container element for the Legal Hold configuration you want to apply to the specified object.
    legalHold :: Lude.Maybe ObjectLockLegalHold,
    -- | The version ID of the object that you want to place a Legal Hold on.
    versionId :: Lude.Maybe ObjectVersionId,
    -- | The bucket name containing the object that you want to place a Legal Hold on.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: BucketName,
    requestPayer :: Lude.Maybe RequestPayer,
    -- | The key name for the object that you want to place a Legal Hold on.
    key :: ObjectKey,
    -- | The MD5 hash for the request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Lude.Maybe Lude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutObjectLegalHold' with the minimum fields required to make a request.
--
-- * 'legalHold' - Container element for the Legal Hold configuration you want to apply to the specified object.
-- * 'versionId' - The version ID of the object that you want to place a Legal Hold on.
-- * 'bucket' - The bucket name containing the object that you want to place a Legal Hold on.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'requestPayer' -
-- * 'key' - The key name for the object that you want to place a Legal Hold on.
-- * 'contentMD5' - The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkPutObjectLegalHold ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  PutObjectLegalHold
mkPutObjectLegalHold pBucket_ pKey_ =
  PutObjectLegalHold'
    { legalHold = Lude.Nothing,
      versionId = Lude.Nothing,
      bucket = pBucket_,
      requestPayer = Lude.Nothing,
      key = pKey_,
      contentMD5 = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing
    }

-- | Container element for the Legal Hold configuration you want to apply to the specified object.
--
-- /Note:/ Consider using 'legalHold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhLegalHold :: Lens.Lens' PutObjectLegalHold (Lude.Maybe ObjectLockLegalHold)
polhLegalHold = Lens.lens (legalHold :: PutObjectLegalHold -> Lude.Maybe ObjectLockLegalHold) (\s a -> s {legalHold = a} :: PutObjectLegalHold)
{-# DEPRECATED polhLegalHold "Use generic-lens or generic-optics with 'legalHold' instead." #-}

-- | The version ID of the object that you want to place a Legal Hold on.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhVersionId :: Lens.Lens' PutObjectLegalHold (Lude.Maybe ObjectVersionId)
polhVersionId = Lens.lens (versionId :: PutObjectLegalHold -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: PutObjectLegalHold)
{-# DEPRECATED polhVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The bucket name containing the object that you want to place a Legal Hold on.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhBucket :: Lens.Lens' PutObjectLegalHold BucketName
polhBucket = Lens.lens (bucket :: PutObjectLegalHold -> BucketName) (\s a -> s {bucket = a} :: PutObjectLegalHold)
{-# DEPRECATED polhBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhRequestPayer :: Lens.Lens' PutObjectLegalHold (Lude.Maybe RequestPayer)
polhRequestPayer = Lens.lens (requestPayer :: PutObjectLegalHold -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: PutObjectLegalHold)
{-# DEPRECATED polhRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | The key name for the object that you want to place a Legal Hold on.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhKey :: Lens.Lens' PutObjectLegalHold ObjectKey
polhKey = Lens.lens (key :: PutObjectLegalHold -> ObjectKey) (\s a -> s {key = a} :: PutObjectLegalHold)
{-# DEPRECATED polhKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhContentMD5 :: Lens.Lens' PutObjectLegalHold (Lude.Maybe Lude.Text)
polhContentMD5 = Lens.lens (contentMD5 :: PutObjectLegalHold -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutObjectLegalHold)
{-# DEPRECATED polhContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhExpectedBucketOwner :: Lens.Lens' PutObjectLegalHold (Lude.Maybe Lude.Text)
polhExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutObjectLegalHold -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutObjectLegalHold)
{-# DEPRECATED polhExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest PutObjectLegalHold where
  type Rs PutObjectLegalHold = PutObjectLegalHoldResponse
  request = Req.putXML s3Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutObjectLegalHoldResponse'
            Lude.<$> (h Lude..#? "x-amz-request-charged")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement PutObjectLegalHold where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}LegalHold"
      Lude.. legalHold

instance Lude.ToHeaders PutObjectLegalHold where
  toHeaders PutObjectLegalHold' {..} =
    Lude.mconcat
      [ "x-amz-request-payer" Lude.=# requestPayer,
        "Content-MD5" Lude.=# contentMD5,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath PutObjectLegalHold where
  toPath PutObjectLegalHold' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery PutObjectLegalHold where
  toQuery PutObjectLegalHold' {..} =
    Lude.mconcat ["versionId" Lude.=: versionId, "legal-hold"]

-- | /See:/ 'mkPutObjectLegalHoldResponse' smart constructor.
data PutObjectLegalHoldResponse = PutObjectLegalHoldResponse'
  { requestCharged :: Lude.Maybe RequestCharged,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutObjectLegalHoldResponse' with the minimum fields required to make a request.
--
-- * 'requestCharged' -
-- * 'responseStatus' - The response status code.
mkPutObjectLegalHoldResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutObjectLegalHoldResponse
mkPutObjectLegalHoldResponse pResponseStatus_ =
  PutObjectLegalHoldResponse'
    { requestCharged = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhrsRequestCharged :: Lens.Lens' PutObjectLegalHoldResponse (Lude.Maybe RequestCharged)
polhrsRequestCharged = Lens.lens (requestCharged :: PutObjectLegalHoldResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: PutObjectLegalHoldResponse)
{-# DEPRECATED polhrsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhrsResponseStatus :: Lens.Lens' PutObjectLegalHoldResponse Lude.Int
polhrsResponseStatus = Lens.lens (responseStatus :: PutObjectLegalHoldResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutObjectLegalHoldResponse)
{-# DEPRECATED polhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
