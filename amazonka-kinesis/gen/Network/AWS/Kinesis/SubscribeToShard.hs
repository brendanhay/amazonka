{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.SubscribeToShard
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Call this operation from your consumer after you call 'RegisterStreamConsumer' to register the consumer with Kinesis Data Streams. If the call succeeds, your consumer starts receiving events of type 'SubscribeToShardEvent' for up to 5 minutes, after which time you need to call @SubscribeToShard@ again to renew the subscription if you want to continue to receive records.
--
--
-- You can make one call to @SubscribeToShard@ per second per @ConsumerARN@ . If your call succeeds, and then you call the operation again less than 5 seconds later, the second call generates a 'ResourceInUseException' . If you call the operation a second time more than 5 seconds after the first call succeeds, the second call succeeds and the first connection gets shut down.
--
module Network.AWS.Kinesis.SubscribeToShard
    (
    -- * Creating a Request
      subscribeToShard
    , SubscribeToShard
    -- * Request Lenses
    , stsConsumerARN
    , stsShardId
    , stsStartingPosition

    -- * Destructuring the Response
    , subscribeToShardResponse
    , SubscribeToShardResponse
    -- * Response Lenses
    , stsrsResponseStatus
    , stsrsEventStream
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'subscribeToShard' smart constructor.
data SubscribeToShard = SubscribeToShard'
  { _stsConsumerARN      :: !Text
  , _stsShardId          :: !Text
  , _stsStartingPosition :: !StartingPosition
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubscribeToShard' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stsConsumerARN' - For this parameter, use the value you obtained when you called 'RegisterStreamConsumer' .
--
-- * 'stsShardId' - The ID of the shard you want to subscribe to. To see a list of all the shards for a given stream, use 'ListShards' .
--
-- * 'stsStartingPosition' - Undocumented member.
subscribeToShard
    :: Text -- ^ 'stsConsumerARN'
    -> Text -- ^ 'stsShardId'
    -> StartingPosition -- ^ 'stsStartingPosition'
    -> SubscribeToShard
subscribeToShard pConsumerARN_ pShardId_ pStartingPosition_ =
  SubscribeToShard'
    { _stsConsumerARN = pConsumerARN_
    , _stsShardId = pShardId_
    , _stsStartingPosition = pStartingPosition_
    }


-- | For this parameter, use the value you obtained when you called 'RegisterStreamConsumer' .
stsConsumerARN :: Lens' SubscribeToShard Text
stsConsumerARN = lens _stsConsumerARN (\ s a -> s{_stsConsumerARN = a})

-- | The ID of the shard you want to subscribe to. To see a list of all the shards for a given stream, use 'ListShards' .
stsShardId :: Lens' SubscribeToShard Text
stsShardId = lens _stsShardId (\ s a -> s{_stsShardId = a})

-- | Undocumented member.
stsStartingPosition :: Lens' SubscribeToShard StartingPosition
stsStartingPosition = lens _stsStartingPosition (\ s a -> s{_stsStartingPosition = a})

instance AWSRequest SubscribeToShard where
        type Rs SubscribeToShard = SubscribeToShardResponse
        request = postJSON kinesis
        response
          = receiveJSON
              (\ s h x ->
                 SubscribeToShardResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "EventStream"))

instance Hashable SubscribeToShard where

instance NFData SubscribeToShard where

instance ToHeaders SubscribeToShard where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.SubscribeToShard" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SubscribeToShard where
        toJSON SubscribeToShard'{..}
          = object
              (catMaybes
                 [Just ("ConsumerARN" .= _stsConsumerARN),
                  Just ("ShardId" .= _stsShardId),
                  Just ("StartingPosition" .= _stsStartingPosition)])

instance ToPath SubscribeToShard where
        toPath = const "/"

instance ToQuery SubscribeToShard where
        toQuery = const mempty

-- | /See:/ 'subscribeToShardResponse' smart constructor.
data SubscribeToShardResponse = SubscribeToShardResponse'
  { _stsrsResponseStatus :: !Int
  , _stsrsEventStream    :: !SubscribeToShardEventStream
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubscribeToShardResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stsrsResponseStatus' - -- | The response status code.
--
-- * 'stsrsEventStream' - The event stream that your consumer can use to read records from the shard.
subscribeToShardResponse
    :: Int -- ^ 'stsrsResponseStatus'
    -> SubscribeToShardEventStream -- ^ 'stsrsEventStream'
    -> SubscribeToShardResponse
subscribeToShardResponse pResponseStatus_ pEventStream_ =
  SubscribeToShardResponse'
    {_stsrsResponseStatus = pResponseStatus_, _stsrsEventStream = pEventStream_}


-- | -- | The response status code.
stsrsResponseStatus :: Lens' SubscribeToShardResponse Int
stsrsResponseStatus = lens _stsrsResponseStatus (\ s a -> s{_stsrsResponseStatus = a})

-- | The event stream that your consumer can use to read records from the shard.
stsrsEventStream :: Lens' SubscribeToShardResponse SubscribeToShardEventStream
stsrsEventStream = lens _stsrsEventStream (\ s a -> s{_stsrsEventStream = a})

instance NFData SubscribeToShardResponse where
