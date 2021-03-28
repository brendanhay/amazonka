{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.UpdateShardCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the shard count of the specified stream to the specified number of shards.
--
-- Updating the shard count is an asynchronous operation. Upon receiving the request, Kinesis Data Streams returns immediately and sets the status of the stream to @UPDATING@ . After the update is complete, Kinesis Data Streams sets the status of the stream back to @ACTIVE@ . Depending on the size of the stream, the scaling action could take a few minutes to complete. You can continue to read and write data to your stream while its status is @UPDATING@ .
-- To update the shard count, Kinesis Data Streams performs splits or merges on individual shards. This can cause short-lived shards to be created, in addition to the final shards. These short-lived shards count towards your total shard limit for your account in the Region.
-- When using this operation, we recommend that you specify a target shard count that is a multiple of 25% (25%, 50%, 75%, 100%). You can specify any target value within your shard limit. However, if you specify a target that isn't a multiple of 25%, the scaling action might take longer to complete. 
-- This operation has the following default limits. By default, you cannot do the following:
--
--     * Scale more than ten times per rolling 24-hour period per stream
--
--
--     * Scale up to more than double your current shard count for a stream
--
--
--     * Scale down below half your current shard count for a stream
--
--
--     * Scale up to more than 500 shards in a stream
--
--
--     * Scale a stream with more than 500 shards down unless the result is less than 500 shards
--
--
--     * Scale up to more than the shard limit for your account
--
--
-- For the default limits for an AWS account, see <https://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Streams Limits> in the /Amazon Kinesis Data Streams Developer Guide/ . To request an increase in the call rate limit, the shard limit for this API, or your overall shard limit, use the <https://console.aws.amazon.com/support/v1#/case/create?issueType=service-limit-increase&limitType=service-code-kinesis limits form> .
module Network.AWS.Kinesis.UpdateShardCount
    (
    -- * Creating a request
      UpdateShardCount (..)
    , mkUpdateShardCount
    -- ** Request lenses
    , uscStreamName
    , uscTargetShardCount
    , uscScalingType

    -- * Destructuring the response
    , UpdateShardCountResponse (..)
    , mkUpdateShardCountResponse
    -- ** Response lenses
    , uscrrsCurrentShardCount
    , uscrrsStreamName
    , uscrrsTargetShardCount
    , uscrrsResponseStatus
    ) where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateShardCount' smart constructor.
data UpdateShardCount = UpdateShardCount'
  { streamName :: Types.StreamName
    -- ^ The name of the stream.
  , targetShardCount :: Core.Natural
    -- ^ The new number of shards. This value has the following default limits. By default, you cannot do the following: 
--
--
--     * Set this value to more than double your current shard count for a stream.
--
--
--     * Set this value below half your current shard count for a stream.
--
--
--     * Set this value to more than 500 shards in a stream (the default limit for shard count per stream is 500 per account per region), unless you request a limit increase.
--
--
--     * Scale a stream with more than 500 shards down unless you set this value to less than 500 shards.
--
--
  , scalingType :: Types.ScalingType
    -- ^ The scaling type. Uniform scaling creates shards of equal size.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateShardCount' value with any optional fields omitted.
mkUpdateShardCount
    :: Types.StreamName -- ^ 'streamName'
    -> Core.Natural -- ^ 'targetShardCount'
    -> Types.ScalingType -- ^ 'scalingType'
    -> UpdateShardCount
mkUpdateShardCount streamName targetShardCount scalingType
  = UpdateShardCount'{streamName, targetShardCount, scalingType}

-- | The name of the stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscStreamName :: Lens.Lens' UpdateShardCount Types.StreamName
uscStreamName = Lens.field @"streamName"
{-# INLINEABLE uscStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

-- | The new number of shards. This value has the following default limits. By default, you cannot do the following: 
--
--
--     * Set this value to more than double your current shard count for a stream.
--
--
--     * Set this value below half your current shard count for a stream.
--
--
--     * Set this value to more than 500 shards in a stream (the default limit for shard count per stream is 500 per account per region), unless you request a limit increase.
--
--
--     * Scale a stream with more than 500 shards down unless you set this value to less than 500 shards.
--
--
--
-- /Note:/ Consider using 'targetShardCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscTargetShardCount :: Lens.Lens' UpdateShardCount Core.Natural
uscTargetShardCount = Lens.field @"targetShardCount"
{-# INLINEABLE uscTargetShardCount #-}
{-# DEPRECATED targetShardCount "Use generic-lens or generic-optics with 'targetShardCount' instead"  #-}

-- | The scaling type. Uniform scaling creates shards of equal size.
--
-- /Note:/ Consider using 'scalingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscScalingType :: Lens.Lens' UpdateShardCount Types.ScalingType
uscScalingType = Lens.field @"scalingType"
{-# INLINEABLE uscScalingType #-}
{-# DEPRECATED scalingType "Use generic-lens or generic-optics with 'scalingType' instead"  #-}

instance Core.ToQuery UpdateShardCount where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateShardCount where
        toHeaders UpdateShardCount{..}
          = Core.pure ("X-Amz-Target", "Kinesis_20131202.UpdateShardCount")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateShardCount where
        toJSON UpdateShardCount{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StreamName" Core..= streamName),
                  Core.Just ("TargetShardCount" Core..= targetShardCount),
                  Core.Just ("ScalingType" Core..= scalingType)])

instance Core.AWSRequest UpdateShardCount where
        type Rs UpdateShardCount = UpdateShardCountResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateShardCountResponse' Core.<$>
                   (x Core..:? "CurrentShardCount") Core.<*> x Core..:? "StreamName"
                     Core.<*> x Core..:? "TargetShardCount"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateShardCountResponse' smart constructor.
data UpdateShardCountResponse = UpdateShardCountResponse'
  { currentShardCount :: Core.Maybe Core.Natural
    -- ^ The current number of shards.
  , streamName :: Core.Maybe Types.StreamName
    -- ^ The name of the stream.
  , targetShardCount :: Core.Maybe Core.Natural
    -- ^ The updated number of shards.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateShardCountResponse' value with any optional fields omitted.
mkUpdateShardCountResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateShardCountResponse
mkUpdateShardCountResponse responseStatus
  = UpdateShardCountResponse'{currentShardCount = Core.Nothing,
                              streamName = Core.Nothing, targetShardCount = Core.Nothing,
                              responseStatus}

-- | The current number of shards.
--
-- /Note:/ Consider using 'currentShardCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrrsCurrentShardCount :: Lens.Lens' UpdateShardCountResponse (Core.Maybe Core.Natural)
uscrrsCurrentShardCount = Lens.field @"currentShardCount"
{-# INLINEABLE uscrrsCurrentShardCount #-}
{-# DEPRECATED currentShardCount "Use generic-lens or generic-optics with 'currentShardCount' instead"  #-}

-- | The name of the stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrrsStreamName :: Lens.Lens' UpdateShardCountResponse (Core.Maybe Types.StreamName)
uscrrsStreamName = Lens.field @"streamName"
{-# INLINEABLE uscrrsStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

-- | The updated number of shards.
--
-- /Note:/ Consider using 'targetShardCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrrsTargetShardCount :: Lens.Lens' UpdateShardCountResponse (Core.Maybe Core.Natural)
uscrrsTargetShardCount = Lens.field @"targetShardCount"
{-# INLINEABLE uscrrsTargetShardCount #-}
{-# DEPRECATED targetShardCount "Use generic-lens or generic-optics with 'targetShardCount' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrrsResponseStatus :: Lens.Lens' UpdateShardCountResponse Core.Int
uscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
