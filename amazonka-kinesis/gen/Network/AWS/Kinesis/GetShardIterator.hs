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
-- Module      : Network.AWS.Kinesis.GetShardIterator
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an Amazon Kinesis shard iterator. A shard iterator expires five minutes after it is returned to the requester.
--
-- A shard iterator specifies the shard position from which to start reading data records sequentially. The position is specified using the sequence number of a data record in a shard. A sequence number is the identifier associated with every record ingested in the stream, and is assigned when a record is put into the stream. Each stream has one or more shards.
--
-- You must specify the shard iterator type. For example, you can set the 'ShardIteratorType' parameter to read exactly from the position denoted by a specific sequence number by using the 'AT_SEQUENCE_NUMBER' shard iterator type, or right after the sequence number by using the 'AFTER_SEQUENCE_NUMBER' shard iterator type, using sequence numbers returned by earlier calls to < PutRecord>, < PutRecords>, < GetRecords>, or < DescribeStream>. In the request, you can specify the shard iterator type 'AT_TIMESTAMP' to read records from an arbitrary point in time, 'TRIM_HORIZON' to cause 'ShardIterator' to point to the last untrimmed record in the shard in the system (the oldest data record in the shard), or 'LATEST' so that you always read the most recent data in the shard.
--
-- When you read repeatedly from a stream, use a < GetShardIterator> request to get the first shard iterator for use in your first < GetRecords> request and for subsequent reads use the shard iterator returned by the < GetRecords> request in 'NextShardIterator'. A new shard iterator is returned by every < GetRecords> request in 'NextShardIterator', which you use in the 'ShardIterator' parameter of the next < GetRecords> request.
--
-- If a < GetShardIterator> request is made too often, you receive a 'ProvisionedThroughputExceededException'. For more information about throughput limits, see < GetRecords>, and <http://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Streams Limits> in the /Amazon Kinesis Streams Developer Guide/.
--
-- If the shard is closed, < GetShardIterator> returns a valid iterator for the last sequence number of the shard. Note that a shard can be closed as a result of using < SplitShard> or < MergeShards>.
--
-- < GetShardIterator> has a limit of 5 transactions per second per account per open shard.
module Network.AWS.Kinesis.GetShardIterator
    (
    -- * Creating a Request
      getShardIterator
    , GetShardIterator
    -- * Request Lenses
    , gsiStartingSequenceNumber
    , gsiTimestamp
    , gsiStreamName
    , gsiShardId
    , gsiShardIteratorType

    -- * Destructuring the Response
    , getShardIteratorResponse
    , GetShardIteratorResponse
    -- * Response Lenses
    , gsirsShardIterator
    , gsirsResponseStatus
    ) where

import           Network.AWS.Kinesis.Types
import           Network.AWS.Kinesis.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for 'GetShardIterator'.
--
-- /See:/ 'getShardIterator' smart constructor.
data GetShardIterator = GetShardIterator'
    { _gsiStartingSequenceNumber :: !(Maybe Text)
    , _gsiTimestamp              :: !(Maybe POSIX)
    , _gsiStreamName             :: !Text
    , _gsiShardId                :: !Text
    , _gsiShardIteratorType      :: !ShardIteratorType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetShardIterator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsiStartingSequenceNumber'
--
-- * 'gsiTimestamp'
--
-- * 'gsiStreamName'
--
-- * 'gsiShardId'
--
-- * 'gsiShardIteratorType'
getShardIterator
    :: Text -- ^ 'gsiStreamName'
    -> Text -- ^ 'gsiShardId'
    -> ShardIteratorType -- ^ 'gsiShardIteratorType'
    -> GetShardIterator
getShardIterator pStreamName_ pShardId_ pShardIteratorType_ =
    GetShardIterator'
    { _gsiStartingSequenceNumber = Nothing
    , _gsiTimestamp = Nothing
    , _gsiStreamName = pStreamName_
    , _gsiShardId = pShardId_
    , _gsiShardIteratorType = pShardIteratorType_
    }

-- | The sequence number of the data record in the shard from which to start reading. Used with shard iterator type AT_SEQUENCE_NUMBER and AFTER_SEQUENCE_NUMBER.
gsiStartingSequenceNumber :: Lens' GetShardIterator (Maybe Text)
gsiStartingSequenceNumber = lens _gsiStartingSequenceNumber (\ s a -> s{_gsiStartingSequenceNumber = a});

-- | The timestamp of the data record from which to start reading. Used with shard iterator type AT_TIMESTAMP. A timestamp is the Unix epoch date with precision in milliseconds. For example, '2016-04-04T19:58:46.480-00:00' or '1459799926.480'. If a record with this exact timestamp does not exist, the iterator returned is for the next (later) record. If the timestamp is older than the current trim horizon, the iterator returned is for the oldest untrimmed data record (TRIM_HORIZON).
gsiTimestamp :: Lens' GetShardIterator (Maybe UTCTime)
gsiTimestamp = lens _gsiTimestamp (\ s a -> s{_gsiTimestamp = a}) . mapping _Time;

-- | The name of the Amazon Kinesis stream.
gsiStreamName :: Lens' GetShardIterator Text
gsiStreamName = lens _gsiStreamName (\ s a -> s{_gsiStreamName = a});

-- | The shard ID of the Amazon Kinesis shard to get the iterator for.
gsiShardId :: Lens' GetShardIterator Text
gsiShardId = lens _gsiShardId (\ s a -> s{_gsiShardId = a});

-- | Determines how the shard iterator is used to start reading data records from the shard.
--
-- The following are the valid Amazon Kinesis shard iterator types:
--
-- -   AT_SEQUENCE_NUMBER - Start reading from the position denoted by a specific sequence number, provided in the value 'StartingSequenceNumber'.
-- -   AFTER_SEQUENCE_NUMBER - Start reading right after the position denoted by a specific sequence number, provided in the value 'StartingSequenceNumber'.
-- -   AT_TIMESTAMP - Start reading from the position denoted by a specific timestamp, provided in the value 'Timestamp'.
-- -   TRIM_HORIZON - Start reading at the last untrimmed record in the shard in the system, which is the oldest data record in the shard.
-- -   LATEST - Start reading just after the most recent record in the shard, so that you always read the most recent data in the shard.
gsiShardIteratorType :: Lens' GetShardIterator ShardIteratorType
gsiShardIteratorType = lens _gsiShardIteratorType (\ s a -> s{_gsiShardIteratorType = a});

instance AWSRequest GetShardIterator where
        type Rs GetShardIterator = GetShardIteratorResponse
        request = postJSON kinesis
        response
          = receiveJSON
              (\ s h x ->
                 GetShardIteratorResponse' <$>
                   (x .?> "ShardIterator") <*> (pure (fromEnum s)))

instance Hashable GetShardIterator

instance NFData GetShardIterator

instance ToHeaders GetShardIterator where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.GetShardIterator" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetShardIterator where
        toJSON GetShardIterator'{..}
          = object
              (catMaybes
                 [("StartingSequenceNumber" .=) <$>
                    _gsiStartingSequenceNumber,
                  ("Timestamp" .=) <$> _gsiTimestamp,
                  Just ("StreamName" .= _gsiStreamName),
                  Just ("ShardId" .= _gsiShardId),
                  Just ("ShardIteratorType" .= _gsiShardIteratorType)])

instance ToPath GetShardIterator where
        toPath = const "/"

instance ToQuery GetShardIterator where
        toQuery = const mempty

-- | Represents the output for 'GetShardIterator'.
--
-- /See:/ 'getShardIteratorResponse' smart constructor.
data GetShardIteratorResponse = GetShardIteratorResponse'
    { _gsirsShardIterator  :: !(Maybe Text)
    , _gsirsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetShardIteratorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsirsShardIterator'
--
-- * 'gsirsResponseStatus'
getShardIteratorResponse
    :: Int -- ^ 'gsirsResponseStatus'
    -> GetShardIteratorResponse
getShardIteratorResponse pResponseStatus_ =
    GetShardIteratorResponse'
    { _gsirsShardIterator = Nothing
    , _gsirsResponseStatus = pResponseStatus_
    }

-- | The position in the shard from which to start reading data records sequentially. A shard iterator specifies this position using the sequence number of a data record in a shard.
gsirsShardIterator :: Lens' GetShardIteratorResponse (Maybe Text)
gsirsShardIterator = lens _gsirsShardIterator (\ s a -> s{_gsirsShardIterator = a});

-- | The response status code.
gsirsResponseStatus :: Lens' GetShardIteratorResponse Int
gsirsResponseStatus = lens _gsirsResponseStatus (\ s a -> s{_gsirsResponseStatus = a});

instance NFData GetShardIteratorResponse
