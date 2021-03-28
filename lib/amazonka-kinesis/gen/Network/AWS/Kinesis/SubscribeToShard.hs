{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.SubscribeToShard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation establishes an HTTP/2 connection between the consumer you specify in the @ConsumerARN@ parameter and the shard you specify in the @ShardId@ parameter. After the connection is successfully established, Kinesis Data Streams pushes records from the shard to the consumer over this connection. Before you call this operation, call 'RegisterStreamConsumer' to register the consumer with Kinesis Data Streams.
--
-- When the @SubscribeToShard@ call succeeds, your consumer starts receiving events of type 'SubscribeToShardEvent' over the HTTP/2 connection for up to 5 minutes, after which time you need to call @SubscribeToShard@ again to renew the subscription if you want to continue to receive records.
-- You can make one call to @SubscribeToShard@ per second per registered consumer per shard. For example, if you have a 4000 shard stream and two registered stream consumers, you can make one @SubscribeToShard@ request per second for each combination of shard and registered consumer, allowing you to subscribe both consumers to all 4000 shards in one second. 
-- If you call @SubscribeToShard@ again with the same @ConsumerARN@ and @ShardId@ within 5 seconds of a successful call, you'll get a @ResourceInUseException@ . If you call @SubscribeToShard@ 5 seconds or more after a successful call, the first connection will expire and the second call will take over the subscription.
-- For an example of how to use this operations, see </streams/latest/dev/building-enhanced-consumers-api.html Enhanced Fan-Out Using the Kinesis Data Streams API> .
module Network.AWS.Kinesis.SubscribeToShard
    (
    -- * Creating a request
      SubscribeToShard (..)
    , mkSubscribeToShard
    -- ** Request lenses
    , stsConsumerARN
    , stsShardId
    , stsStartingPosition

    -- * Destructuring the response
    , SubscribeToShardResponse (..)
    , mkSubscribeToShardResponse
    -- ** Response lenses
    , stsrrsEventStream
    , stsrrsResponseStatus
    ) where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSubscribeToShard' smart constructor.
data SubscribeToShard = SubscribeToShard'
  { consumerARN :: Types.ConsumerARN
    -- ^ For this parameter, use the value you obtained when you called 'RegisterStreamConsumer' .
  , shardId :: Types.ShardId
    -- ^ The ID of the shard you want to subscribe to. To see a list of all the shards for a given stream, use 'ListShards' .
  , startingPosition :: Types.StartingPosition
    -- ^ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SubscribeToShard' value with any optional fields omitted.
mkSubscribeToShard
    :: Types.ConsumerARN -- ^ 'consumerARN'
    -> Types.ShardId -- ^ 'shardId'
    -> Types.StartingPosition -- ^ 'startingPosition'
    -> SubscribeToShard
mkSubscribeToShard consumerARN shardId startingPosition
  = SubscribeToShard'{consumerARN, shardId, startingPosition}

-- | For this parameter, use the value you obtained when you called 'RegisterStreamConsumer' .
--
-- /Note:/ Consider using 'consumerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsConsumerARN :: Lens.Lens' SubscribeToShard Types.ConsumerARN
stsConsumerARN = Lens.field @"consumerARN"
{-# INLINEABLE stsConsumerARN #-}
{-# DEPRECATED consumerARN "Use generic-lens or generic-optics with 'consumerARN' instead"  #-}

-- | The ID of the shard you want to subscribe to. To see a list of all the shards for a given stream, use 'ListShards' .
--
-- /Note:/ Consider using 'shardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsShardId :: Lens.Lens' SubscribeToShard Types.ShardId
stsShardId = Lens.field @"shardId"
{-# INLINEABLE stsShardId #-}
{-# DEPRECATED shardId "Use generic-lens or generic-optics with 'shardId' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'startingPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsStartingPosition :: Lens.Lens' SubscribeToShard Types.StartingPosition
stsStartingPosition = Lens.field @"startingPosition"
{-# INLINEABLE stsStartingPosition #-}
{-# DEPRECATED startingPosition "Use generic-lens or generic-optics with 'startingPosition' instead"  #-}

instance Core.ToQuery SubscribeToShard where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SubscribeToShard where
        toHeaders SubscribeToShard{..}
          = Core.pure ("X-Amz-Target", "Kinesis_20131202.SubscribeToShard")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SubscribeToShard where
        toJSON SubscribeToShard{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConsumerARN" Core..= consumerARN),
                  Core.Just ("ShardId" Core..= shardId),
                  Core.Just ("StartingPosition" Core..= startingPosition)])

instance Core.AWSRequest SubscribeToShard where
        type Rs SubscribeToShard = SubscribeToShardResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SubscribeToShardResponse' Core.<$>
                   (x Core..: "EventStream") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSubscribeToShardResponse' smart constructor.
data SubscribeToShardResponse = SubscribeToShardResponse'
  { eventStream :: Types.SubscribeToShardEventStream
    -- ^ The event stream that your consumer can use to read records from the shard.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SubscribeToShardResponse' value with any optional fields omitted.
mkSubscribeToShardResponse
    :: Types.SubscribeToShardEventStream -- ^ 'eventStream'
    -> Core.Int -- ^ 'responseStatus'
    -> SubscribeToShardResponse
mkSubscribeToShardResponse eventStream responseStatus
  = SubscribeToShardResponse'{eventStream, responseStatus}

-- | The event stream that your consumer can use to read records from the shard.
--
-- /Note:/ Consider using 'eventStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsrrsEventStream :: Lens.Lens' SubscribeToShardResponse Types.SubscribeToShardEventStream
stsrrsEventStream = Lens.field @"eventStream"
{-# INLINEABLE stsrrsEventStream #-}
{-# DEPRECATED eventStream "Use generic-lens or generic-optics with 'eventStream' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsrrsResponseStatus :: Lens.Lens' SubscribeToShardResponse Core.Int
stsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE stsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
