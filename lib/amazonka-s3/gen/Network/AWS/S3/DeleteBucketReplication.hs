{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dbrBucket,
    dbrExpectedBucketOwner,

    -- * Destructuring the response
    DeleteBucketReplicationResponse (..),
    mkDeleteBucketReplicationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkDeleteBucketReplication' smart constructor.
data DeleteBucketReplication = DeleteBucketReplication'
  { -- | The bucket name.
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketReplication' value with any optional fields omitted.
mkDeleteBucketReplication ::
  -- | 'bucket'
  Types.BucketName ->
  DeleteBucketReplication
mkDeleteBucketReplication bucket =
  DeleteBucketReplication'
    { bucket,
      expectedBucketOwner = Core.Nothing
    }

-- | The bucket name.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrBucket :: Lens.Lens' DeleteBucketReplication Types.BucketName
dbrBucket = Lens.field @"bucket"
{-# DEPRECATED dbrBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrExpectedBucketOwner :: Lens.Lens' DeleteBucketReplication (Core.Maybe Types.ExpectedBucketOwner)
dbrExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED dbrExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest DeleteBucketReplication where
  type Rs DeleteBucketReplication = DeleteBucketReplicationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("replication", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response = Response.receiveNull DeleteBucketReplicationResponse'

-- | /See:/ 'mkDeleteBucketReplicationResponse' smart constructor.
data DeleteBucketReplicationResponse = DeleteBucketReplicationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketReplicationResponse' value with any optional fields omitted.
mkDeleteBucketReplicationResponse ::
  DeleteBucketReplicationResponse
mkDeleteBucketReplicationResponse =
  DeleteBucketReplicationResponse'
