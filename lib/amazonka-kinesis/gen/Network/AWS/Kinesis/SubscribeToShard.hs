{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SubscribeToShard (..),
    mkSubscribeToShard,

    -- ** Request lenses
    stsConsumerARN,
    stsStartingPosition,
    stsShardId,

    -- * Destructuring the response
    SubscribeToShardResponse (..),
    mkSubscribeToShardResponse,

    -- ** Response lenses
    stsrsEventStream,
    stsrsResponseStatus,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSubscribeToShard' smart constructor.
data SubscribeToShard = SubscribeToShard'
  { -- | For this parameter, use the value you obtained when you called 'RegisterStreamConsumer' .
    consumerARN :: Lude.Text,
    -- |
    startingPosition :: StartingPosition,
    -- | The ID of the shard you want to subscribe to. To see a list of all the shards for a given stream, use 'ListShards' .
    shardId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubscribeToShard' with the minimum fields required to make a request.
--
-- * 'consumerARN' - For this parameter, use the value you obtained when you called 'RegisterStreamConsumer' .
-- * 'startingPosition' -
-- * 'shardId' - The ID of the shard you want to subscribe to. To see a list of all the shards for a given stream, use 'ListShards' .
mkSubscribeToShard ::
  -- | 'consumerARN'
  Lude.Text ->
  -- | 'startingPosition'
  StartingPosition ->
  -- | 'shardId'
  Lude.Text ->
  SubscribeToShard
mkSubscribeToShard pConsumerARN_ pStartingPosition_ pShardId_ =
  SubscribeToShard'
    { consumerARN = pConsumerARN_,
      startingPosition = pStartingPosition_,
      shardId = pShardId_
    }

-- | For this parameter, use the value you obtained when you called 'RegisterStreamConsumer' .
--
-- /Note:/ Consider using 'consumerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsConsumerARN :: Lens.Lens' SubscribeToShard Lude.Text
stsConsumerARN = Lens.lens (consumerARN :: SubscribeToShard -> Lude.Text) (\s a -> s {consumerARN = a} :: SubscribeToShard)
{-# DEPRECATED stsConsumerARN "Use generic-lens or generic-optics with 'consumerARN' instead." #-}

-- |
--
-- /Note:/ Consider using 'startingPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsStartingPosition :: Lens.Lens' SubscribeToShard StartingPosition
stsStartingPosition = Lens.lens (startingPosition :: SubscribeToShard -> StartingPosition) (\s a -> s {startingPosition = a} :: SubscribeToShard)
{-# DEPRECATED stsStartingPosition "Use generic-lens or generic-optics with 'startingPosition' instead." #-}

-- | The ID of the shard you want to subscribe to. To see a list of all the shards for a given stream, use 'ListShards' .
--
-- /Note:/ Consider using 'shardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsShardId :: Lens.Lens' SubscribeToShard Lude.Text
stsShardId = Lens.lens (shardId :: SubscribeToShard -> Lude.Text) (\s a -> s {shardId = a} :: SubscribeToShard)
{-# DEPRECATED stsShardId "Use generic-lens or generic-optics with 'shardId' instead." #-}

instance Lude.AWSRequest SubscribeToShard where
  type Rs SubscribeToShard = SubscribeToShardResponse
  request = Req.postJSON kinesisService
  response =
    Res.receiveJSON
      ( \s h x ->
          SubscribeToShardResponse'
            Lude.<$> (x Lude..:> "EventStream") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SubscribeToShard where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.SubscribeToShard" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SubscribeToShard where
  toJSON SubscribeToShard' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ConsumerARN" Lude..= consumerARN),
            Lude.Just ("StartingPosition" Lude..= startingPosition),
            Lude.Just ("ShardId" Lude..= shardId)
          ]
      )

instance Lude.ToPath SubscribeToShard where
  toPath = Lude.const "/"

instance Lude.ToQuery SubscribeToShard where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSubscribeToShardResponse' smart constructor.
data SubscribeToShardResponse = SubscribeToShardResponse'
  { -- | The event stream that your consumer can use to read records from the shard.
    eventStream :: SubscribeToShardEventStream,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubscribeToShardResponse' with the minimum fields required to make a request.
--
-- * 'eventStream' - The event stream that your consumer can use to read records from the shard.
-- * 'responseStatus' - The response status code.
mkSubscribeToShardResponse ::
  -- | 'eventStream'
  SubscribeToShardEventStream ->
  -- | 'responseStatus'
  Lude.Int ->
  SubscribeToShardResponse
mkSubscribeToShardResponse pEventStream_ pResponseStatus_ =
  SubscribeToShardResponse'
    { eventStream = pEventStream_,
      responseStatus = pResponseStatus_
    }

-- | The event stream that your consumer can use to read records from the shard.
--
-- /Note:/ Consider using 'eventStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsrsEventStream :: Lens.Lens' SubscribeToShardResponse SubscribeToShardEventStream
stsrsEventStream = Lens.lens (eventStream :: SubscribeToShardResponse -> SubscribeToShardEventStream) (\s a -> s {eventStream = a} :: SubscribeToShardResponse)
{-# DEPRECATED stsrsEventStream "Use generic-lens or generic-optics with 'eventStream' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsrsResponseStatus :: Lens.Lens' SubscribeToShardResponse Lude.Int
stsrsResponseStatus = Lens.lens (responseStatus :: SubscribeToShardResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SubscribeToShardResponse)
{-# DEPRECATED stsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
