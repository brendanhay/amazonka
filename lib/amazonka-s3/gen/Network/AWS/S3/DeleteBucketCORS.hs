{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketCORS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the @cors@ configuration information set for the bucket.
--
-- To use this operation, you must have permission to perform the @s3:PutBucketCORS@ action. The bucket owner has this permission by default and can grant this permission to others.
-- For information about @cors@ , see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> in the /Amazon Simple Storage Service Developer Guide/ .
-- __Related Resources:__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketCors.html PutBucketCors>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTOPTIONSobject.html RESTOPTIONSobject>
module Network.AWS.S3.DeleteBucketCORS
  ( -- * Creating a request
    DeleteBucketCORS (..),
    mkDeleteBucketCORS,

    -- ** Request lenses
    dbcBucket,
    dbcExpectedBucketOwner,

    -- * Destructuring the response
    DeleteBucketCORSResponse (..),
    mkDeleteBucketCORSResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeleteBucketCORS' smart constructor.
data DeleteBucketCORS = DeleteBucketCORS'
  { -- | Specifies the bucket whose @cors@ configuration is being deleted.
    bucket :: BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketCORS' with the minimum fields required to make a request.
--
-- * 'bucket' - Specifies the bucket whose @cors@ configuration is being deleted.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkDeleteBucketCORS ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketCORS
mkDeleteBucketCORS pBucket_ =
  DeleteBucketCORS'
    { bucket = pBucket_,
      expectedBucketOwner = Lude.Nothing
    }

-- | Specifies the bucket whose @cors@ configuration is being deleted.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcBucket :: Lens.Lens' DeleteBucketCORS BucketName
dbcBucket = Lens.lens (bucket :: DeleteBucketCORS -> BucketName) (\s a -> s {bucket = a} :: DeleteBucketCORS)
{-# DEPRECATED dbcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcExpectedBucketOwner :: Lens.Lens' DeleteBucketCORS (Lude.Maybe Lude.Text)
dbcExpectedBucketOwner = Lens.lens (expectedBucketOwner :: DeleteBucketCORS -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: DeleteBucketCORS)
{-# DEPRECATED dbcExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest DeleteBucketCORS where
  type Rs DeleteBucketCORS = DeleteBucketCORSResponse
  request = Req.delete s3Service
  response = Res.receiveNull DeleteBucketCORSResponse'

instance Lude.ToHeaders DeleteBucketCORS where
  toHeaders DeleteBucketCORS' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath DeleteBucketCORS where
  toPath DeleteBucketCORS' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery DeleteBucketCORS where
  toQuery = Lude.const (Lude.mconcat ["cors"])

-- | /See:/ 'mkDeleteBucketCORSResponse' smart constructor.
data DeleteBucketCORSResponse = DeleteBucketCORSResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketCORSResponse' with the minimum fields required to make a request.
mkDeleteBucketCORSResponse ::
  DeleteBucketCORSResponse
mkDeleteBucketCORSResponse = DeleteBucketCORSResponse'
