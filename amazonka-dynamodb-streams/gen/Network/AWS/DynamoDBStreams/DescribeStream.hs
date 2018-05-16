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
-- Module      : Network.AWS.DynamoDBStreams.DescribeStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a stream, including the current status of the stream, its Amazon Resource Name (ARN), the composition of its shards, and its corresponding DynamoDB table.
--
--
-- Each shard in the stream has a @SequenceNumberRange@ associated with it. If the @SequenceNumberRange@ has a @StartingSequenceNumber@ but no @EndingSequenceNumber@ , then the shard is still open (able to receive more stream records). If both @StartingSequenceNumber@ and @EndingSequenceNumber@ are present, then that shard is closed and can no longer receive more data.
--
module Network.AWS.DynamoDBStreams.DescribeStream
    (
    -- * Creating a Request
      describeStream
    , DescribeStream
    -- * Request Lenses
    , dsExclusiveStartShardId
    , dsLimit
    , dsStreamARN

    -- * Destructuring the Response
    , describeStreamResponse
    , DescribeStreamResponse
    -- * Response Lenses
    , dsrsStreamDescription
    , dsrsResponseStatus
    ) where

import Network.AWS.DynamoDBStreams.Types
import Network.AWS.DynamoDBStreams.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DescribeStream@ operation.
--
--
--
-- /See:/ 'describeStream' smart constructor.
data DescribeStream = DescribeStream'
  { _dsExclusiveStartShardId :: !(Maybe Text)
  , _dsLimit                 :: !(Maybe Nat)
  , _dsStreamARN             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsExclusiveStartShardId' - The shard ID of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedShardId@ in the previous operation.
--
-- * 'dsLimit' - The maximum number of shard objects to return. The upper limit is 100.
--
-- * 'dsStreamARN' - The Amazon Resource Name (ARN) for the stream.
describeStream
    :: Text -- ^ 'dsStreamARN'
    -> DescribeStream
describeStream pStreamARN_ =
  DescribeStream'
    { _dsExclusiveStartShardId = Nothing
    , _dsLimit = Nothing
    , _dsStreamARN = pStreamARN_
    }


-- | The shard ID of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedShardId@ in the previous operation.
dsExclusiveStartShardId :: Lens' DescribeStream (Maybe Text)
dsExclusiveStartShardId = lens _dsExclusiveStartShardId (\ s a -> s{_dsExclusiveStartShardId = a})

-- | The maximum number of shard objects to return. The upper limit is 100.
dsLimit :: Lens' DescribeStream (Maybe Natural)
dsLimit = lens _dsLimit (\ s a -> s{_dsLimit = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) for the stream.
dsStreamARN :: Lens' DescribeStream Text
dsStreamARN = lens _dsStreamARN (\ s a -> s{_dsStreamARN = a})

instance AWSRequest DescribeStream where
        type Rs DescribeStream = DescribeStreamResponse
        request = postJSON dynamoDBStreams
        response
          = receiveJSON
              (\ s h x ->
                 DescribeStreamResponse' <$>
                   (x .?> "StreamDescription") <*> (pure (fromEnum s)))

instance Hashable DescribeStream where

instance NFData DescribeStream where

instance ToHeaders DescribeStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDBStreams_20120810.DescribeStream" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DescribeStream where
        toJSON DescribeStream'{..}
          = object
              (catMaybes
                 [("ExclusiveStartShardId" .=) <$>
                    _dsExclusiveStartShardId,
                  ("Limit" .=) <$> _dsLimit,
                  Just ("StreamArn" .= _dsStreamARN)])

instance ToPath DescribeStream where
        toPath = const "/"

instance ToQuery DescribeStream where
        toQuery = const mempty

-- | Represents the output of a @DescribeStream@ operation.
--
--
--
-- /See:/ 'describeStreamResponse' smart constructor.
data DescribeStreamResponse = DescribeStreamResponse'
  { _dsrsStreamDescription :: !(Maybe StreamDescription)
  , _dsrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsStreamDescription' - A complete description of the stream, including its creation date and time, the DynamoDB table associated with the stream, the shard IDs within the stream, and the beginning and ending sequence numbers of stream records within the shards.
--
-- * 'dsrsResponseStatus' - -- | The response status code.
describeStreamResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> DescribeStreamResponse
describeStreamResponse pResponseStatus_ =
  DescribeStreamResponse'
    {_dsrsStreamDescription = Nothing, _dsrsResponseStatus = pResponseStatus_}


-- | A complete description of the stream, including its creation date and time, the DynamoDB table associated with the stream, the shard IDs within the stream, and the beginning and ending sequence numbers of stream records within the shards.
dsrsStreamDescription :: Lens' DescribeStreamResponse (Maybe StreamDescription)
dsrsStreamDescription = lens _dsrsStreamDescription (\ s a -> s{_dsrsStreamDescription = a})

-- | -- | The response status code.
dsrsResponseStatus :: Lens' DescribeStreamResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a})

instance NFData DescribeStreamResponse where
