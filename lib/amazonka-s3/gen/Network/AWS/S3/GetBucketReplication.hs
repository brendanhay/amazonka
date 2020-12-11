{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the replication configuration of a bucket.
--
-- For information about replication configuration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication> in the /Amazon Simple Storage Service Developer Guide/ .
-- This operation requires permissions for the @s3:GetReplicationConfiguration@ action. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-iam-policies.html Using Bucket Policies and User Policies> .
-- If you include the @Filter@ element in a replication configuration, you must also include the @DeleteMarkerReplication@ and @Priority@ elements. The response also returns those elements.
-- For information about @GetBucketReplication@ errors, see <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html#ReplicationErrorCodeList List of replication-related error codes>
-- The following operations are related to @GetBucketReplication@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketReplication.html PutBucketReplication>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketReplication.html DeleteBucketReplication>
module Network.AWS.S3.GetBucketReplication
  ( -- * Creating a request
    GetBucketReplication (..),
    mkGetBucketReplication,

    -- ** Request lenses
    gbrExpectedBucketOwner,
    gbrBucket,

    -- * Destructuring the response
    GetBucketReplicationResponse (..),
    mkGetBucketReplicationResponse,

    -- ** Response lenses
    gbrrsReplicationConfiguration,
    gbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketReplication' smart constructor.
data GetBucketReplication = GetBucketReplication'
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

-- | Creates a value of 'GetBucketReplication' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name for which to get the replication information.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetBucketReplication ::
  -- | 'bucket'
  BucketName ->
  GetBucketReplication
mkGetBucketReplication pBucket_ =
  GetBucketReplication'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrExpectedBucketOwner :: Lens.Lens' GetBucketReplication (Lude.Maybe Lude.Text)
gbrExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketReplication -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketReplication)
{-# DEPRECATED gbrExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name for which to get the replication information.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrBucket :: Lens.Lens' GetBucketReplication BucketName
gbrBucket = Lens.lens (bucket :: GetBucketReplication -> BucketName) (\s a -> s {bucket = a} :: GetBucketReplication)
{-# DEPRECATED gbrBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest GetBucketReplication where
  type Rs GetBucketReplication = GetBucketReplicationResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketReplicationResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketReplication where
  toHeaders GetBucketReplication' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketReplication where
  toPath GetBucketReplication' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketReplication where
  toQuery = Lude.const (Lude.mconcat ["replication"])

-- | /See:/ 'mkGetBucketReplicationResponse' smart constructor.
data GetBucketReplicationResponse = GetBucketReplicationResponse'
  { replicationConfiguration ::
      Lude.Maybe
        ReplicationConfiguration,
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

-- | Creates a value of 'GetBucketReplicationResponse' with the minimum fields required to make a request.
--
-- * 'replicationConfiguration' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetBucketReplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketReplicationResponse
mkGetBucketReplicationResponse pResponseStatus_ =
  GetBucketReplicationResponse'
    { replicationConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsReplicationConfiguration :: Lens.Lens' GetBucketReplicationResponse (Lude.Maybe ReplicationConfiguration)
gbrrsReplicationConfiguration = Lens.lens (replicationConfiguration :: GetBucketReplicationResponse -> Lude.Maybe ReplicationConfiguration) (\s a -> s {replicationConfiguration = a} :: GetBucketReplicationResponse)
{-# DEPRECATED gbrrsReplicationConfiguration "Use generic-lens or generic-optics with 'replicationConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsResponseStatus :: Lens.Lens' GetBucketReplicationResponse Lude.Int
gbrrsResponseStatus = Lens.lens (responseStatus :: GetBucketReplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketReplicationResponse)
{-# DEPRECATED gbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
