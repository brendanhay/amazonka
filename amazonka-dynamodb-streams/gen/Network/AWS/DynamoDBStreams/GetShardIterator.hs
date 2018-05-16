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
-- Module      : Network.AWS.DynamoDBStreams.GetShardIterator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a shard iterator. A shard iterator provides information about how to retrieve the stream records from within a shard. Use the shard iterator in a subsequent @GetRecords@ request to read the stream records from the shard.
--
--
module Network.AWS.DynamoDBStreams.GetShardIterator
    (
    -- * Creating a Request
      getShardIterator
    , GetShardIterator
    -- * Request Lenses
    , gsiSequenceNumber
    , gsiStreamARN
    , gsiShardId
    , gsiShardIteratorType

    -- * Destructuring the Response
    , getShardIteratorResponse
    , GetShardIteratorResponse
    -- * Response Lenses
    , gsirsShardIterator
    , gsirsResponseStatus
    ) where

import Network.AWS.DynamoDBStreams.Types
import Network.AWS.DynamoDBStreams.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @GetShardIterator@ operation.
--
--
--
-- /See:/ 'getShardIterator' smart constructor.
data GetShardIterator = GetShardIterator'
  { _gsiSequenceNumber    :: !(Maybe Text)
  , _gsiStreamARN         :: !Text
  , _gsiShardId           :: !Text
  , _gsiShardIteratorType :: !ShardIteratorType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetShardIterator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsiSequenceNumber' - The sequence number of a stream record in the shard from which to start reading.
--
-- * 'gsiStreamARN' - The Amazon Resource Name (ARN) for the stream.
--
-- * 'gsiShardId' - The identifier of the shard. The iterator will be returned for this shard ID.
--
-- * 'gsiShardIteratorType' - Determines how the shard iterator is used to start reading stream records from the shard:     * @AT_SEQUENCE_NUMBER@ - Start reading exactly from the position denoted by a specific sequence number.     * @AFTER_SEQUENCE_NUMBER@ - Start reading right after the position denoted by a specific sequence number.     * @TRIM_HORIZON@ - Start reading at the last (untrimmed) stream record, which is the oldest record in the shard. In DynamoDB Streams, there is a 24 hour limit on data retention. Stream records whose age exceeds this limit are subject to removal (trimming) from the stream.     * @LATEST@ - Start reading just after the most recent stream record in the shard, so that you always read the most recent data in the shard.
getShardIterator
    :: Text -- ^ 'gsiStreamARN'
    -> Text -- ^ 'gsiShardId'
    -> ShardIteratorType -- ^ 'gsiShardIteratorType'
    -> GetShardIterator
getShardIterator pStreamARN_ pShardId_ pShardIteratorType_ =
  GetShardIterator'
    { _gsiSequenceNumber = Nothing
    , _gsiStreamARN = pStreamARN_
    , _gsiShardId = pShardId_
    , _gsiShardIteratorType = pShardIteratorType_
    }


-- | The sequence number of a stream record in the shard from which to start reading.
gsiSequenceNumber :: Lens' GetShardIterator (Maybe Text)
gsiSequenceNumber = lens _gsiSequenceNumber (\ s a -> s{_gsiSequenceNumber = a})

-- | The Amazon Resource Name (ARN) for the stream.
gsiStreamARN :: Lens' GetShardIterator Text
gsiStreamARN = lens _gsiStreamARN (\ s a -> s{_gsiStreamARN = a})

-- | The identifier of the shard. The iterator will be returned for this shard ID.
gsiShardId :: Lens' GetShardIterator Text
gsiShardId = lens _gsiShardId (\ s a -> s{_gsiShardId = a})

-- | Determines how the shard iterator is used to start reading stream records from the shard:     * @AT_SEQUENCE_NUMBER@ - Start reading exactly from the position denoted by a specific sequence number.     * @AFTER_SEQUENCE_NUMBER@ - Start reading right after the position denoted by a specific sequence number.     * @TRIM_HORIZON@ - Start reading at the last (untrimmed) stream record, which is the oldest record in the shard. In DynamoDB Streams, there is a 24 hour limit on data retention. Stream records whose age exceeds this limit are subject to removal (trimming) from the stream.     * @LATEST@ - Start reading just after the most recent stream record in the shard, so that you always read the most recent data in the shard.
gsiShardIteratorType :: Lens' GetShardIterator ShardIteratorType
gsiShardIteratorType = lens _gsiShardIteratorType (\ s a -> s{_gsiShardIteratorType = a})

instance AWSRequest GetShardIterator where
        type Rs GetShardIterator = GetShardIteratorResponse
        request = postJSON dynamoDBStreams
        response
          = receiveJSON
              (\ s h x ->
                 GetShardIteratorResponse' <$>
                   (x .?> "ShardIterator") <*> (pure (fromEnum s)))

instance Hashable GetShardIterator where

instance NFData GetShardIterator where

instance ToHeaders GetShardIterator where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDBStreams_20120810.GetShardIterator" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON GetShardIterator where
        toJSON GetShardIterator'{..}
          = object
              (catMaybes
                 [("SequenceNumber" .=) <$> _gsiSequenceNumber,
                  Just ("StreamArn" .= _gsiStreamARN),
                  Just ("ShardId" .= _gsiShardId),
                  Just ("ShardIteratorType" .= _gsiShardIteratorType)])

instance ToPath GetShardIterator where
        toPath = const "/"

instance ToQuery GetShardIterator where
        toQuery = const mempty

-- | Represents the output of a @GetShardIterator@ operation.
--
--
--
-- /See:/ 'getShardIteratorResponse' smart constructor.
data GetShardIteratorResponse = GetShardIteratorResponse'
  { _gsirsShardIterator  :: !(Maybe Text)
  , _gsirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetShardIteratorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsirsShardIterator' - The position in the shard from which to start reading stream records sequentially. A shard iterator specifies this position using the sequence number of a stream record in a shard.
--
-- * 'gsirsResponseStatus' - -- | The response status code.
getShardIteratorResponse
    :: Int -- ^ 'gsirsResponseStatus'
    -> GetShardIteratorResponse
getShardIteratorResponse pResponseStatus_ =
  GetShardIteratorResponse'
    {_gsirsShardIterator = Nothing, _gsirsResponseStatus = pResponseStatus_}


-- | The position in the shard from which to start reading stream records sequentially. A shard iterator specifies this position using the sequence number of a stream record in a shard.
gsirsShardIterator :: Lens' GetShardIteratorResponse (Maybe Text)
gsirsShardIterator = lens _gsirsShardIterator (\ s a -> s{_gsirsShardIterator = a})

-- | -- | The response status code.
gsirsResponseStatus :: Lens' GetShardIteratorResponse Int
gsirsResponseStatus = lens _gsirsResponseStatus (\ s a -> s{_gsirsResponseStatus = a})

instance NFData GetShardIteratorResponse where
