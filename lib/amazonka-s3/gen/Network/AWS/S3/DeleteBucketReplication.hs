{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the replication configuration from the bucket.
--
-- To use this operation, you must have permissions to perform the @s3:PutReplicationConfiguration@ action. The bucket owner has these permissions by default and can grant it to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- For information about replication configuration, see < https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication> in the /Amazon S3 Developer Guide/ .
-- The following operations are related to @DeleteBucketReplication@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketReplication.html PutBucketReplication>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketReplication.html GetBucketReplication>
module Network.AWS.S3.DeleteBucketReplication
  ( -- * Creating a request
    DeleteBucketReplication (..),
    mkDeleteBucketReplication,

    -- ** Request lenses
    dbrExpectedBucketOwner,
    dbrBucket,

    -- * Destructuring the response
    DeleteBucketReplicationResponse (..),
    mkDeleteBucketReplicationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeleteBucketReplication' smart constructor.
data DeleteBucketReplication = DeleteBucketReplication'
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

-- | Creates a value of 'DeleteBucketReplication' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkDeleteBucketReplication ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketReplication
mkDeleteBucketReplication pBucket_ =
  DeleteBucketReplication'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrExpectedBucketOwner :: Lens.Lens' DeleteBucketReplication (Lude.Maybe Lude.Text)
dbrExpectedBucketOwner = Lens.lens (expectedBucketOwner :: DeleteBucketReplication -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: DeleteBucketReplication)
{-# DEPRECATED dbrExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrBucket :: Lens.Lens' DeleteBucketReplication BucketName
dbrBucket = Lens.lens (bucket :: DeleteBucketReplication -> BucketName) (\s a -> s {bucket = a} :: DeleteBucketReplication)
{-# DEPRECATED dbrBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest DeleteBucketReplication where
  type Rs DeleteBucketReplication = DeleteBucketReplicationResponse
  request = Req.delete s3Service
  response = Res.receiveNull DeleteBucketReplicationResponse'

instance Lude.ToHeaders DeleteBucketReplication where
  toHeaders DeleteBucketReplication' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath DeleteBucketReplication where
  toPath DeleteBucketReplication' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery DeleteBucketReplication where
  toQuery = Lude.const (Lude.mconcat ["replication"])

-- | /See:/ 'mkDeleteBucketReplicationResponse' smart constructor.
data DeleteBucketReplicationResponse = DeleteBucketReplicationResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketReplicationResponse' with the minimum fields required to make a request.
mkDeleteBucketReplicationResponse ::
  DeleteBucketReplicationResponse
mkDeleteBucketReplicationResponse =
  DeleteBucketReplicationResponse'
