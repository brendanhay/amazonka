{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketLifecycle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the lifecycle configuration from the specified bucket. Amazon S3 removes all the lifecycle configuration rules in the lifecycle subresource associated with the bucket. Your objects never expire, and Amazon S3 no longer automatically deletes any objects on the basis of rules contained in the deleted lifecycle configuration.
--
-- To use this operation, you must have permission to perform the @s3:PutLifecycleConfiguration@ action. By default, the bucket owner has this permission and the bucket owner can grant this permission to others.
-- There is usually some time lag before lifecycle configuration deletion is fully propagated to all the Amazon S3 systems.
-- For more information about the object expiration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#intro-lifecycle-rules-actions Elements to Describe Lifecycle Actions> .
-- Related actions include:
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLifecycleConfiguration.html GetBucketLifecycleConfiguration>
module Network.AWS.S3.DeleteBucketLifecycle
  ( -- * Creating a request
    DeleteBucketLifecycle (..),
    mkDeleteBucketLifecycle,

    -- ** Request lenses
    dblExpectedBucketOwner,
    dblBucket,

    -- * Destructuring the response
    DeleteBucketLifecycleResponse (..),
    mkDeleteBucketLifecycleResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeleteBucketLifecycle' smart constructor.
data DeleteBucketLifecycle = DeleteBucketLifecycle'
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

-- | Creates a value of 'DeleteBucketLifecycle' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name of the lifecycle to delete.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkDeleteBucketLifecycle ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketLifecycle
mkDeleteBucketLifecycle pBucket_ =
  DeleteBucketLifecycle'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dblExpectedBucketOwner :: Lens.Lens' DeleteBucketLifecycle (Lude.Maybe Lude.Text)
dblExpectedBucketOwner = Lens.lens (expectedBucketOwner :: DeleteBucketLifecycle -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: DeleteBucketLifecycle)
{-# DEPRECATED dblExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name of the lifecycle to delete.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dblBucket :: Lens.Lens' DeleteBucketLifecycle BucketName
dblBucket = Lens.lens (bucket :: DeleteBucketLifecycle -> BucketName) (\s a -> s {bucket = a} :: DeleteBucketLifecycle)
{-# DEPRECATED dblBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest DeleteBucketLifecycle where
  type Rs DeleteBucketLifecycle = DeleteBucketLifecycleResponse
  request = Req.delete s3Service
  response = Res.receiveNull DeleteBucketLifecycleResponse'

instance Lude.ToHeaders DeleteBucketLifecycle where
  toHeaders DeleteBucketLifecycle' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath DeleteBucketLifecycle where
  toPath DeleteBucketLifecycle' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery DeleteBucketLifecycle where
  toQuery = Lude.const (Lude.mconcat ["lifecycle"])

-- | /See:/ 'mkDeleteBucketLifecycleResponse' smart constructor.
data DeleteBucketLifecycleResponse = DeleteBucketLifecycleResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketLifecycleResponse' with the minimum fields required to make a request.
mkDeleteBucketLifecycleResponse ::
  DeleteBucketLifecycleResponse
mkDeleteBucketLifecycleResponse = DeleteBucketLifecycleResponse'
