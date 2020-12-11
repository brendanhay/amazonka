{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketOwnershipControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes @OwnershipControls@ for an Amazon S3 bucket. To use this operation, you must have the @s3:PutBucketOwnershipControls@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
-- For information about Amazon S3 Object Ownership, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/about-object-ownership.html Using Object Ownership> .
-- The following operations are related to @DeleteBucketOwnershipControls@ :
--
--     * 'GetBucketOwnershipControls'
--
--
--     * 'PutBucketOwnershipControls'
module Network.AWS.S3.DeleteBucketOwnershipControls
  ( -- * Creating a request
    DeleteBucketOwnershipControls (..),
    mkDeleteBucketOwnershipControls,

    -- ** Request lenses
    dbocExpectedBucketOwner,
    dbocBucket,

    -- * Destructuring the response
    DeleteBucketOwnershipControlsResponse (..),
    mkDeleteBucketOwnershipControlsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeleteBucketOwnershipControls' smart constructor.
data DeleteBucketOwnershipControls = DeleteBucketOwnershipControls'
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

-- | Creates a value of 'DeleteBucketOwnershipControls' with the minimum fields required to make a request.
--
-- * 'bucket' - The Amazon S3 bucket whose @OwnershipControls@ you want to delete.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkDeleteBucketOwnershipControls ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketOwnershipControls
mkDeleteBucketOwnershipControls pBucket_ =
  DeleteBucketOwnershipControls'
    { expectedBucketOwner =
        Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbocExpectedBucketOwner :: Lens.Lens' DeleteBucketOwnershipControls (Lude.Maybe Lude.Text)
dbocExpectedBucketOwner = Lens.lens (expectedBucketOwner :: DeleteBucketOwnershipControls -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: DeleteBucketOwnershipControls)
{-# DEPRECATED dbocExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The Amazon S3 bucket whose @OwnershipControls@ you want to delete.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbocBucket :: Lens.Lens' DeleteBucketOwnershipControls BucketName
dbocBucket = Lens.lens (bucket :: DeleteBucketOwnershipControls -> BucketName) (\s a -> s {bucket = a} :: DeleteBucketOwnershipControls)
{-# DEPRECATED dbocBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest DeleteBucketOwnershipControls where
  type
    Rs DeleteBucketOwnershipControls =
      DeleteBucketOwnershipControlsResponse
  request = Req.delete s3Service
  response = Res.receiveNull DeleteBucketOwnershipControlsResponse'

instance Lude.ToHeaders DeleteBucketOwnershipControls where
  toHeaders DeleteBucketOwnershipControls' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath DeleteBucketOwnershipControls where
  toPath DeleteBucketOwnershipControls' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery DeleteBucketOwnershipControls where
  toQuery = Lude.const (Lude.mconcat ["ownershipControls"])

-- | /See:/ 'mkDeleteBucketOwnershipControlsResponse' smart constructor.
data DeleteBucketOwnershipControlsResponse = DeleteBucketOwnershipControlsResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketOwnershipControlsResponse' with the minimum fields required to make a request.
mkDeleteBucketOwnershipControlsResponse ::
  DeleteBucketOwnershipControlsResponse
mkDeleteBucketOwnershipControlsResponse =
  DeleteBucketOwnershipControlsResponse'
