{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gbrBucket,
    gbrExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketReplicationResponse (..),
    mkGetBucketReplicationResponse,

    -- ** Response lenses
    gbrrrsReplicationConfiguration,
    gbrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketReplication' smart constructor.
data GetBucketReplication = GetBucketReplication'
  { -- | The bucket name for which to get the replication information.
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketReplication' value with any optional fields omitted.
mkGetBucketReplication ::
  -- | 'bucket'
  Types.BucketName ->
  GetBucketReplication
mkGetBucketReplication bucket =
  GetBucketReplication' {bucket, expectedBucketOwner = Core.Nothing}

-- | The bucket name for which to get the replication information.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrBucket :: Lens.Lens' GetBucketReplication Types.BucketName
gbrBucket = Lens.field @"bucket"
{-# DEPRECATED gbrBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrExpectedBucketOwner :: Lens.Lens' GetBucketReplication (Core.Maybe Types.ExpectedBucketOwner)
gbrExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED gbrExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest GetBucketReplication where
  type Rs GetBucketReplication = GetBucketReplicationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("replication", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketReplicationResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBucketReplicationResponse' smart constructor.
data GetBucketReplicationResponse = GetBucketReplicationResponse'
  { replicationConfiguration :: Core.Maybe Types.ReplicationConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketReplicationResponse' value with any optional fields omitted.
mkGetBucketReplicationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBucketReplicationResponse
mkGetBucketReplicationResponse responseStatus =
  GetBucketReplicationResponse'
    { replicationConfiguration =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrrsReplicationConfiguration :: Lens.Lens' GetBucketReplicationResponse (Core.Maybe Types.ReplicationConfiguration)
gbrrrsReplicationConfiguration = Lens.field @"replicationConfiguration"
{-# DEPRECATED gbrrrsReplicationConfiguration "Use generic-lens or generic-optics with 'replicationConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrrsResponseStatus :: Lens.Lens' GetBucketReplicationResponse Core.Int
gbrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
