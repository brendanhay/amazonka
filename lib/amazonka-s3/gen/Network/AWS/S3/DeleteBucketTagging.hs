{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the tags from the bucket.
--
-- To use this operation, you must have permission to perform the @s3:PutBucketTagging@ action. By default, the bucket owner has this permission and can grant this permission to others.
-- The following operations are related to @DeleteBucketTagging@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketTagging.html GetBucketTagging>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketTagging.html PutBucketTagging>
module Network.AWS.S3.DeleteBucketTagging
  ( -- * Creating a request
    DeleteBucketTagging (..),
    mkDeleteBucketTagging,

    -- ** Request lenses
    dbtBucket,
    dbtExpectedBucketOwner,

    -- * Destructuring the response
    DeleteBucketTaggingResponse (..),
    mkDeleteBucketTaggingResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeleteBucketTagging' smart constructor.
data DeleteBucketTagging = DeleteBucketTagging'
  { -- | The bucket that has the tag set to be removed.
    bucket :: BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketTagging' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket that has the tag set to be removed.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkDeleteBucketTagging ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketTagging
mkDeleteBucketTagging pBucket_ =
  DeleteBucketTagging'
    { bucket = pBucket_,
      expectedBucketOwner = Lude.Nothing
    }

-- | The bucket that has the tag set to be removed.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbtBucket :: Lens.Lens' DeleteBucketTagging BucketName
dbtBucket = Lens.lens (bucket :: DeleteBucketTagging -> BucketName) (\s a -> s {bucket = a} :: DeleteBucketTagging)
{-# DEPRECATED dbtBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbtExpectedBucketOwner :: Lens.Lens' DeleteBucketTagging (Lude.Maybe Lude.Text)
dbtExpectedBucketOwner = Lens.lens (expectedBucketOwner :: DeleteBucketTagging -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: DeleteBucketTagging)
{-# DEPRECATED dbtExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest DeleteBucketTagging where
  type Rs DeleteBucketTagging = DeleteBucketTaggingResponse
  request = Req.delete s3Service
  response = Res.receiveNull DeleteBucketTaggingResponse'

instance Lude.ToHeaders DeleteBucketTagging where
  toHeaders DeleteBucketTagging' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath DeleteBucketTagging where
  toPath DeleteBucketTagging' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery DeleteBucketTagging where
  toQuery = Lude.const (Lude.mconcat ["tagging"])

-- | /See:/ 'mkDeleteBucketTaggingResponse' smart constructor.
data DeleteBucketTaggingResponse = DeleteBucketTaggingResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketTaggingResponse' with the minimum fields required to make a request.
mkDeleteBucketTaggingResponse ::
  DeleteBucketTaggingResponse
mkDeleteBucketTaggingResponse = DeleteBucketTaggingResponse'
