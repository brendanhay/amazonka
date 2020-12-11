{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeletePublicAccessBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the @PublicAccessBlock@ configuration for an Amazon S3 bucket. To use this operation, you must have the @s3:PutBucketPublicAccessBlock@ permission. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- The following operations are related to @DeletePublicAccessBlock@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html Using Amazon S3 Block Public Access>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html GetPublicAccessBlock>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutPublicAccessBlock.html PutPublicAccessBlock>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketPolicyStatus.html GetBucketPolicyStatus>
module Network.AWS.S3.DeletePublicAccessBlock
  ( -- * Creating a request
    DeletePublicAccessBlock (..),
    mkDeletePublicAccessBlock,

    -- ** Request lenses
    dpabExpectedBucketOwner,
    dpabBucket,

    -- * Destructuring the response
    DeletePublicAccessBlockResponse (..),
    mkDeletePublicAccessBlockResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeletePublicAccessBlock' smart constructor.
data DeletePublicAccessBlock = DeletePublicAccessBlock'
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

-- | Creates a value of 'DeletePublicAccessBlock' with the minimum fields required to make a request.
--
-- * 'bucket' - The Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to delete.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkDeletePublicAccessBlock ::
  -- | 'bucket'
  BucketName ->
  DeletePublicAccessBlock
mkDeletePublicAccessBlock pBucket_ =
  DeletePublicAccessBlock'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpabExpectedBucketOwner :: Lens.Lens' DeletePublicAccessBlock (Lude.Maybe Lude.Text)
dpabExpectedBucketOwner = Lens.lens (expectedBucketOwner :: DeletePublicAccessBlock -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: DeletePublicAccessBlock)
{-# DEPRECATED dpabExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to delete.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpabBucket :: Lens.Lens' DeletePublicAccessBlock BucketName
dpabBucket = Lens.lens (bucket :: DeletePublicAccessBlock -> BucketName) (\s a -> s {bucket = a} :: DeletePublicAccessBlock)
{-# DEPRECATED dpabBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest DeletePublicAccessBlock where
  type Rs DeletePublicAccessBlock = DeletePublicAccessBlockResponse
  request = Req.delete s3Service
  response = Res.receiveNull DeletePublicAccessBlockResponse'

instance Lude.ToHeaders DeletePublicAccessBlock where
  toHeaders DeletePublicAccessBlock' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath DeletePublicAccessBlock where
  toPath DeletePublicAccessBlock' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery DeletePublicAccessBlock where
  toQuery = Lude.const (Lude.mconcat ["publicAccessBlock"])

-- | /See:/ 'mkDeletePublicAccessBlockResponse' smart constructor.
data DeletePublicAccessBlockResponse = DeletePublicAccessBlockResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePublicAccessBlockResponse' with the minimum fields required to make a request.
mkDeletePublicAccessBlockResponse ::
  DeletePublicAccessBlockResponse
mkDeletePublicAccessBlockResponse =
  DeletePublicAccessBlockResponse'
