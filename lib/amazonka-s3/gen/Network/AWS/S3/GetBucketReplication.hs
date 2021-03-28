{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.S3.GetBucketReplication
    (
    -- * Creating a request
      GetBucketReplication (..)
    , mkGetBucketReplication
    -- ** Request lenses
    , gbrBucket
    , gbrExpectedBucketOwner

    -- * Destructuring the response
    , GetBucketReplicationResponse (..)
    , mkGetBucketReplicationResponse
    -- ** Response lenses
    , gbrrrsReplicationConfiguration
    , gbrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketReplication' smart constructor.
data GetBucketReplication = GetBucketReplication'
  { bucket :: Types.BucketName
    -- ^ The bucket name for which to get the replication information.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketReplication' value with any optional fields omitted.
mkGetBucketReplication
    :: Types.BucketName -- ^ 'bucket'
    -> GetBucketReplication
mkGetBucketReplication bucket
  = GetBucketReplication'{bucket, expectedBucketOwner = Core.Nothing}

-- | The bucket name for which to get the replication information.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrBucket :: Lens.Lens' GetBucketReplication Types.BucketName
gbrBucket = Lens.field @"bucket"
{-# INLINEABLE gbrBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrExpectedBucketOwner :: Lens.Lens' GetBucketReplication (Core.Maybe Types.ExpectedBucketOwner)
gbrExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE gbrExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery GetBucketReplication where
        toQuery GetBucketReplication{..}
          = Core.toQueryPair "replication" ("" :: Core.Text)

instance Core.ToHeaders GetBucketReplication where
        toHeaders GetBucketReplication{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest GetBucketReplication where
        type Rs GetBucketReplication = GetBucketReplicationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetBucketReplicationResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetBucketReplicationResponse' smart constructor.
data GetBucketReplicationResponse = GetBucketReplicationResponse'
  { replicationConfiguration :: Core.Maybe Types.ReplicationConfiguration
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketReplicationResponse' value with any optional fields omitted.
mkGetBucketReplicationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBucketReplicationResponse
mkGetBucketReplicationResponse responseStatus
  = GetBucketReplicationResponse'{replicationConfiguration =
                                    Core.Nothing,
                                  responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrrsReplicationConfiguration :: Lens.Lens' GetBucketReplicationResponse (Core.Maybe Types.ReplicationConfiguration)
gbrrrsReplicationConfiguration = Lens.field @"replicationConfiguration"
{-# INLINEABLE gbrrrsReplicationConfiguration #-}
{-# DEPRECATED replicationConfiguration "Use generic-lens or generic-optics with 'replicationConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrrsResponseStatus :: Lens.Lens' GetBucketReplicationResponse Core.Int
gbrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
