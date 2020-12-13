{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutObjectLockConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Places an Object Lock configuration on the specified bucket. The rule specified in the Object Lock configuration will be applied by default to every new object placed in the specified bucket.
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects>
module Network.AWS.S3.PutObjectLockConfiguration
  ( -- * Creating a request
    PutObjectLockConfiguration (..),
    mkPutObjectLockConfiguration,

    -- ** Request lenses
    polcToken,
    polcBucket,
    polcObjectLockConfiguration,
    polcRequestPayer,
    polcContentMD5,
    polcExpectedBucketOwner,

    -- * Destructuring the response
    PutObjectLockConfigurationResponse (..),
    mkPutObjectLockConfigurationResponse,

    -- ** Response lenses
    polcrsRequestCharged,
    polcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutObjectLockConfiguration' smart constructor.
data PutObjectLockConfiguration = PutObjectLockConfiguration'
  { -- | A token to allow Object Lock to be enabled for an existing bucket.
    token :: Lude.Maybe Lude.Text,
    -- | The bucket whose Object Lock configuration you want to create or replace.
    bucket :: BucketName,
    -- | The Object Lock configuration that you want to apply to the specified bucket.
    objectLockConfiguration :: Lude.Maybe ObjectLockConfiguration,
    requestPayer :: Lude.Maybe RequestPayer,
    -- | The MD5 hash for the request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Lude.Maybe Lude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutObjectLockConfiguration' with the minimum fields required to make a request.
--
-- * 'token' - A token to allow Object Lock to be enabled for an existing bucket.
-- * 'bucket' - The bucket whose Object Lock configuration you want to create or replace.
-- * 'objectLockConfiguration' - The Object Lock configuration that you want to apply to the specified bucket.
-- * 'requestPayer' -
-- * 'contentMD5' - The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkPutObjectLockConfiguration ::
  -- | 'bucket'
  BucketName ->
  PutObjectLockConfiguration
mkPutObjectLockConfiguration pBucket_ =
  PutObjectLockConfiguration'
    { token = Lude.Nothing,
      bucket = pBucket_,
      objectLockConfiguration = Lude.Nothing,
      requestPayer = Lude.Nothing,
      contentMD5 = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing
    }

-- | A token to allow Object Lock to be enabled for an existing bucket.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polcToken :: Lens.Lens' PutObjectLockConfiguration (Lude.Maybe Lude.Text)
polcToken = Lens.lens (token :: PutObjectLockConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: PutObjectLockConfiguration)
{-# DEPRECATED polcToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The bucket whose Object Lock configuration you want to create or replace.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polcBucket :: Lens.Lens' PutObjectLockConfiguration BucketName
polcBucket = Lens.lens (bucket :: PutObjectLockConfiguration -> BucketName) (\s a -> s {bucket = a} :: PutObjectLockConfiguration)
{-# DEPRECATED polcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The Object Lock configuration that you want to apply to the specified bucket.
--
-- /Note:/ Consider using 'objectLockConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polcObjectLockConfiguration :: Lens.Lens' PutObjectLockConfiguration (Lude.Maybe ObjectLockConfiguration)
polcObjectLockConfiguration = Lens.lens (objectLockConfiguration :: PutObjectLockConfiguration -> Lude.Maybe ObjectLockConfiguration) (\s a -> s {objectLockConfiguration = a} :: PutObjectLockConfiguration)
{-# DEPRECATED polcObjectLockConfiguration "Use generic-lens or generic-optics with 'objectLockConfiguration' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polcRequestPayer :: Lens.Lens' PutObjectLockConfiguration (Lude.Maybe RequestPayer)
polcRequestPayer = Lens.lens (requestPayer :: PutObjectLockConfiguration -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: PutObjectLockConfiguration)
{-# DEPRECATED polcRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polcContentMD5 :: Lens.Lens' PutObjectLockConfiguration (Lude.Maybe Lude.Text)
polcContentMD5 = Lens.lens (contentMD5 :: PutObjectLockConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutObjectLockConfiguration)
{-# DEPRECATED polcContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polcExpectedBucketOwner :: Lens.Lens' PutObjectLockConfiguration (Lude.Maybe Lude.Text)
polcExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutObjectLockConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutObjectLockConfiguration)
{-# DEPRECATED polcExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest PutObjectLockConfiguration where
  type
    Rs PutObjectLockConfiguration =
      PutObjectLockConfigurationResponse
  request = Req.putXML s3Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutObjectLockConfigurationResponse'
            Lude.<$> (h Lude..#? "x-amz-request-charged")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement PutObjectLockConfiguration where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}ObjectLockConfiguration"
      Lude.. objectLockConfiguration

instance Lude.ToHeaders PutObjectLockConfiguration where
  toHeaders PutObjectLockConfiguration' {..} =
    Lude.mconcat
      [ "x-amz-bucket-object-lock-token" Lude.=# token,
        "x-amz-request-payer" Lude.=# requestPayer,
        "Content-MD5" Lude.=# contentMD5,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath PutObjectLockConfiguration where
  toPath PutObjectLockConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutObjectLockConfiguration where
  toQuery = Lude.const (Lude.mconcat ["object-lock"])

-- | /See:/ 'mkPutObjectLockConfigurationResponse' smart constructor.
data PutObjectLockConfigurationResponse = PutObjectLockConfigurationResponse'
  { requestCharged :: Lude.Maybe RequestCharged,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutObjectLockConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'requestCharged' -
-- * 'responseStatus' - The response status code.
mkPutObjectLockConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutObjectLockConfigurationResponse
mkPutObjectLockConfigurationResponse pResponseStatus_ =
  PutObjectLockConfigurationResponse'
    { requestCharged =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polcrsRequestCharged :: Lens.Lens' PutObjectLockConfigurationResponse (Lude.Maybe RequestCharged)
polcrsRequestCharged = Lens.lens (requestCharged :: PutObjectLockConfigurationResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: PutObjectLockConfigurationResponse)
{-# DEPRECATED polcrsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polcrsResponseStatus :: Lens.Lens' PutObjectLockConfigurationResponse Lude.Int
polcrsResponseStatus = Lens.lens (responseStatus :: PutObjectLockConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutObjectLockConfigurationResponse)
{-# DEPRECATED polcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
