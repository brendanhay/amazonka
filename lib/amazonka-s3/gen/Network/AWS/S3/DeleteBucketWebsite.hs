{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketWebsite
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation removes the website configuration for a bucket. Amazon S3 returns a @200 OK@ response upon successfully deleting a website configuration on the specified bucket. You will get a @200 OK@ response if the website configuration you are trying to delete does not exist on the bucket. Amazon S3 returns a @404@ response if the bucket specified in the request does not exist.
--
-- This DELETE operation requires the @S3:DeleteBucketWebsite@ permission. By default, only the bucket owner can delete the website configuration attached to a bucket. However, bucket owners can grant other users permission to delete the website configuration by writing a bucket policy granting them the @S3:DeleteBucketWebsite@ permission.
-- For more information about hosting websites, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3> .
-- The following operations are related to @DeleteBucketWebsite@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketWebsite.html GetBucketWebsite>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketWebsite.html PutBucketWebsite>
module Network.AWS.S3.DeleteBucketWebsite
  ( -- * Creating a request
    DeleteBucketWebsite (..),
    mkDeleteBucketWebsite,

    -- ** Request lenses
    dbwExpectedBucketOwner,
    dbwBucket,

    -- * Destructuring the response
    DeleteBucketWebsiteResponse (..),
    mkDeleteBucketWebsiteResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeleteBucketWebsite' smart constructor.
data DeleteBucketWebsite = DeleteBucketWebsite'
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

-- | Creates a value of 'DeleteBucketWebsite' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name for which you want to remove the website configuration.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkDeleteBucketWebsite ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketWebsite
mkDeleteBucketWebsite pBucket_ =
  DeleteBucketWebsite'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbwExpectedBucketOwner :: Lens.Lens' DeleteBucketWebsite (Lude.Maybe Lude.Text)
dbwExpectedBucketOwner = Lens.lens (expectedBucketOwner :: DeleteBucketWebsite -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: DeleteBucketWebsite)
{-# DEPRECATED dbwExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name for which you want to remove the website configuration.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbwBucket :: Lens.Lens' DeleteBucketWebsite BucketName
dbwBucket = Lens.lens (bucket :: DeleteBucketWebsite -> BucketName) (\s a -> s {bucket = a} :: DeleteBucketWebsite)
{-# DEPRECATED dbwBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest DeleteBucketWebsite where
  type Rs DeleteBucketWebsite = DeleteBucketWebsiteResponse
  request = Req.delete s3Service
  response = Res.receiveNull DeleteBucketWebsiteResponse'

instance Lude.ToHeaders DeleteBucketWebsite where
  toHeaders DeleteBucketWebsite' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath DeleteBucketWebsite where
  toPath DeleteBucketWebsite' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery DeleteBucketWebsite where
  toQuery = Lude.const (Lude.mconcat ["website"])

-- | /See:/ 'mkDeleteBucketWebsiteResponse' smart constructor.
data DeleteBucketWebsiteResponse = DeleteBucketWebsiteResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketWebsiteResponse' with the minimum fields required to make a request.
mkDeleteBucketWebsiteResponse ::
  DeleteBucketWebsiteResponse
mkDeleteBucketWebsiteResponse = DeleteBucketWebsiteResponse'
