{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetPublicAccessBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the @PublicAccessBlock@ configuration for an Amazon S3 bucket. To use this operation, you must have the @s3:GetBucketPublicAccessBlock@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
-- /Important:/ When Amazon S3 evaluates the @PublicAccessBlock@ configuration for a bucket or an object, it checks the @PublicAccessBlock@ configuration for both the bucket (or the bucket that contains the object) and the bucket owner's account. If the @PublicAccessBlock@ settings are different between the bucket and the account, Amazon S3 uses the most restrictive combination of the bucket-level and account-level settings.
-- For more information about when Amazon S3 considers a bucket or an object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> .
-- The following operations are related to @GetPublicAccessBlock@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html Using Amazon S3 Block Public Access>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutPublicAccessBlock.html PutPublicAccessBlock>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html GetPublicAccessBlock>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeletePublicAccessBlock.html DeletePublicAccessBlock>
module Network.AWS.S3.GetPublicAccessBlock
  ( -- * Creating a request
    GetPublicAccessBlock (..),
    mkGetPublicAccessBlock,

    -- ** Request lenses
    gpabExpectedBucketOwner,
    gpabBucket,

    -- * Destructuring the response
    GetPublicAccessBlockResponse (..),
    mkGetPublicAccessBlockResponse,

    -- ** Response lenses
    gpabrsPublicAccessBlockConfiguration,
    gpabrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetPublicAccessBlock' smart constructor.
data GetPublicAccessBlock = GetPublicAccessBlock'
  { expectedBucketOwner ::
      Lude.Maybe Lude.Text,
    bucket :: BucketName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPublicAccessBlock' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to retrieve.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetPublicAccessBlock ::
  -- | 'bucket'
  BucketName ->
  GetPublicAccessBlock
mkGetPublicAccessBlock pBucket_ =
  GetPublicAccessBlock'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpabExpectedBucketOwner :: Lens.Lens' GetPublicAccessBlock (Lude.Maybe Lude.Text)
gpabExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetPublicAccessBlock -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetPublicAccessBlock)
{-# DEPRECATED gpabExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpabBucket :: Lens.Lens' GetPublicAccessBlock BucketName
gpabBucket = Lens.lens (bucket :: GetPublicAccessBlock -> BucketName) (\s a -> s {bucket = a} :: GetPublicAccessBlock)
{-# DEPRECATED gpabBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest GetPublicAccessBlock where
  type Rs GetPublicAccessBlock = GetPublicAccessBlockResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetPublicAccessBlockResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPublicAccessBlock where
  toHeaders GetPublicAccessBlock' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetPublicAccessBlock where
  toPath GetPublicAccessBlock' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetPublicAccessBlock where
  toQuery = Lude.const (Lude.mconcat ["publicAccessBlock"])

-- | /See:/ 'mkGetPublicAccessBlockResponse' smart constructor.
data GetPublicAccessBlockResponse = GetPublicAccessBlockResponse'
  { publicAccessBlockConfiguration ::
      Lude.Maybe
        PublicAccessBlockConfiguration,
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

-- | Creates a value of 'GetPublicAccessBlockResponse' with the minimum fields required to make a request.
--
-- * 'publicAccessBlockConfiguration' - The @PublicAccessBlock@ configuration currently in effect for this Amazon S3 bucket.
-- * 'responseStatus' - The response status code.
mkGetPublicAccessBlockResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPublicAccessBlockResponse
mkGetPublicAccessBlockResponse pResponseStatus_ =
  GetPublicAccessBlockResponse'
    { publicAccessBlockConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @PublicAccessBlock@ configuration currently in effect for this Amazon S3 bucket.
--
-- /Note:/ Consider using 'publicAccessBlockConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpabrsPublicAccessBlockConfiguration :: Lens.Lens' GetPublicAccessBlockResponse (Lude.Maybe PublicAccessBlockConfiguration)
gpabrsPublicAccessBlockConfiguration = Lens.lens (publicAccessBlockConfiguration :: GetPublicAccessBlockResponse -> Lude.Maybe PublicAccessBlockConfiguration) (\s a -> s {publicAccessBlockConfiguration = a} :: GetPublicAccessBlockResponse)
{-# DEPRECATED gpabrsPublicAccessBlockConfiguration "Use generic-lens or generic-optics with 'publicAccessBlockConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpabrsResponseStatus :: Lens.Lens' GetPublicAccessBlockResponse Lude.Int
gpabrsResponseStatus = Lens.lens (responseStatus :: GetPublicAccessBlockResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPublicAccessBlockResponse)
{-# DEPRECATED gpabrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
