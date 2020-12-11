{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the S3 bucket. All objects (including all object versions and delete markers) in the bucket must be deleted before the bucket itself can be deleted.
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
module Network.AWS.S3.DeleteBucket
  ( -- * Creating a request
    DeleteBucket (..),
    mkDeleteBucket,

    -- ** Request lenses
    dbExpectedBucketOwner,
    dbBucket,

    -- * Destructuring the response
    DeleteBucketResponse (..),
    mkDeleteBucketResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeleteBucket' smart constructor.
data DeleteBucket = DeleteBucket'
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

-- | Creates a value of 'DeleteBucket' with the minimum fields required to make a request.
--
-- * 'bucket' - Specifies the bucket being deleted.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkDeleteBucket ::
  -- | 'bucket'
  BucketName ->
  DeleteBucket
mkDeleteBucket pBucket_ =
  DeleteBucket'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbExpectedBucketOwner :: Lens.Lens' DeleteBucket (Lude.Maybe Lude.Text)
dbExpectedBucketOwner = Lens.lens (expectedBucketOwner :: DeleteBucket -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: DeleteBucket)
{-# DEPRECATED dbExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | Specifies the bucket being deleted.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBucket :: Lens.Lens' DeleteBucket BucketName
dbBucket = Lens.lens (bucket :: DeleteBucket -> BucketName) (\s a -> s {bucket = a} :: DeleteBucket)
{-# DEPRECATED dbBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest DeleteBucket where
  type Rs DeleteBucket = DeleteBucketResponse
  request = Req.delete s3Service
  response = Res.receiveNull DeleteBucketResponse'

instance Lude.ToHeaders DeleteBucket where
  toHeaders DeleteBucket' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath DeleteBucket where
  toPath DeleteBucket' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery DeleteBucket where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteBucketResponse' smart constructor.
data DeleteBucketResponse = DeleteBucketResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketResponse' with the minimum fields required to make a request.
mkDeleteBucketResponse ::
  DeleteBucketResponse
mkDeleteBucketResponse = DeleteBucketResponse'
