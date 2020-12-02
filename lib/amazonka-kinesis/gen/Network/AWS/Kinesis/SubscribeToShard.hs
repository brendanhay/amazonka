{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- When the @SubscribeToShard@ call succeeds, your consumer starts receiving events of type 'SubscribeToShardEvent' over the HTTP/2 connection for up to 5 minutes, after which time you need to call @SubscribeToShard@ again to renew the subscription if you want to continue to receive records.
--
-- You can make one call to @SubscribeToShard@ per second per registered consumer per shard. For example, if you have a 4000 shard stream and two registered stream consumers, you can make one @SubscribeToShard@ request per second for each combination of shard and registered consumer, allowing you to subscribe both consumers to all 4000 shards in one second.
--
-- If you call @SubscribeToShard@ again with the same @ConsumerARN@ and @ShardId@ within 5 seconds of a successful call, you'll get a @ResourceInUseException@ . If you call @SubscribeToShard@ 5 seconds or more after a successful call, the first connection will expire and the second call will take over the subscription.
--
-- For an example of how to use this operations, see </streams/latest/dev/building-enhanced-consumers-api.html Enhanced Fan-Out Using the Kinesis Data Streams API> .
module Network.AWS.Kinesis.SubscribeToShard
  ( -- * Creating a Request
    subscribeToShard,
    SubscribeToShard,

    -- * Request Lenses
    stsConsumerARN,
    stsShardId,
    stsStartingPosition,

    -- * Destructuring the Response
    subscribeToShardResponse,
    SubscribeToShardResponse,

    -- * Response Lenses
    stsrsResponseStatus,
    stsrsEventStream,
  )
where

import Network.AWS.Kinesis.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'subscribeToShard' smart constructor.
data SubscribeToShard = SubscribeToShard'
  { _stsConsumerARN :: !Text,
    _stsShardId :: !Text,
    _stsStartingPosition :: !StartingPosition
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubscribeToShard' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stsConsumerARN' - For this parameter, use the value you obtained when you called 'RegisterStreamConsumer' .
--
-- * 'stsShardId' - The ID of the shard you want to subscribe to. To see a list of all the shards for a given stream, use 'ListShards' .
--
-- * 'stsStartingPosition' -
subscribeToShard ::
  -- | 'stsConsumerARN'
  Text ->
  -- | 'stsShardId'
  Text ->
  -- | 'stsStartingPosition'
  StartingPosition ->
  SubscribeToShard
subscribeToShard pConsumerARN_ pShardId_ pStartingPosition_ =
  SubscribeToShard'
    { _stsConsumerARN = pConsumerARN_,
      _stsShardId = pShardId_,
      _stsStartingPosition = pStartingPosition_
    }

-- | For this parameter, use the value you obtained when you called 'RegisterStreamConsumer' .
stsConsumerARN :: Lens' SubscribeToShard Text
stsConsumerARN = lens _stsConsumerARN (\s a -> s {_stsConsumerARN = a})

-- | The ID of the shard you want to subscribe to. To see a list of all the shards for a given stream, use 'ListShards' .
stsShardId :: Lens' SubscribeToShard Text
stsShardId = lens _stsShardId (\s a -> s {_stsShardId = a})

-- |
stsStartingPosition :: Lens' SubscribeToShard StartingPosition
stsStartingPosition = lens _stsStartingPosition (\s a -> s {_stsStartingPosition = a})

instance AWSRequest SubscribeToShard where
  type Rs SubscribeToShard = SubscribeToShardResponse
  request = postJSON kinesis
  response =
    receiveJSON
      ( \s h x ->
          SubscribeToShardResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "EventStream")
      )

instance Hashable SubscribeToShard

instance NFData SubscribeToShard

instance ToHeaders SubscribeToShard where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Kinesis_20131202.SubscribeToShard" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON SubscribeToShard where
  toJSON SubscribeToShard' {..} =
    object
      ( catMaybes
          [ Just ("ConsumerARN" .= _stsConsumerARN),
            Just ("ShardId" .= _stsShardId),
            Just ("StartingPosition" .= _stsStartingPosition)
          ]
      )

instance ToPath SubscribeToShard where
  toPath = const "/"

instance ToQuery SubscribeToShard where
  toQuery = const mempty

-- | /See:/ 'subscribeToShardResponse' smart constructor.
data SubscribeToShardResponse = SubscribeToShardResponse'
  { _stsrsResponseStatus ::
      !Int,
    _stsrsEventStream ::
      !SubscribeToShardEventStream
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubscribeToShardResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stsrsResponseStatus' - -- | The response status code.
--
-- * 'stsrsEventStream' - The event stream that your consumer can use to read records from the shard.
subscribeToShardResponse ::
  -- | 'stsrsResponseStatus'
  Int ->
  -- | 'stsrsEventStream'
  SubscribeToShardEventStream ->
  SubscribeToShardResponse
subscribeToShardResponse pResponseStatus_ pEventStream_ =
  SubscribeToShardResponse'
    { _stsrsResponseStatus =
        pResponseStatus_,
      _stsrsEventStream = pEventStream_
    }

-- | -- | The response status code.
stsrsResponseStatus :: Lens' SubscribeToShardResponse Int
stsrsResponseStatus = lens _stsrsResponseStatus (\s a -> s {_stsrsResponseStatus = a})

-- | The event stream that your consumer can use to read records from the shard.
stsrsEventStream :: Lens' SubscribeToShardResponse SubscribeToShardEventStream
stsrsEventStream = lens _stsrsEventStream (\s a -> s {_stsrsEventStream = a})

instance NFData SubscribeToShardResponse
