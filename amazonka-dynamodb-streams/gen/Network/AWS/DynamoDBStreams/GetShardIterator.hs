{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.GetShardIterator
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a shard iterator. A shard iterator provides information about
-- how to retrieve the stream records from within a shard. Use the shard
-- iterator in a subsequent @GetRecords@ request to read the stream records
-- from the shard.
--
-- A shard iterator expires 15 minutes after it is returned to the
-- requester.
--
-- <http://dynamodb-preview.s3-website-us-west-2.amazonaws.com/docs/streams-api/API_GetShardIterator.html>
module Network.AWS.DynamoDBStreams.GetShardIterator
    (
    -- * Request
      GetShardIterator
    -- ** Request constructor
    , getShardIterator
    -- ** Request lenses
    , gsiSequenceNumber
    , gsiStreamARN
    , gsiShardId
    , gsiShardIteratorType

    -- * Response
    , GetShardIteratorResponse
    -- ** Response constructor
    , getShardIteratorResponse
    -- ** Response lenses
    , gsirsShardIterator
    , gsirsStatus
    ) where

import           Network.AWS.DynamoDBStreams.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /GetShardIterator/ operation.
--
-- /See:/ 'getShardIterator' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsiSequenceNumber'
--
-- * 'gsiStreamARN'
--
-- * 'gsiShardId'
--
-- * 'gsiShardIteratorType'
data GetShardIterator = GetShardIterator'
    { _gsiSequenceNumber    :: !(Maybe Text)
    , _gsiStreamARN         :: !Text
    , _gsiShardId           :: !Text
    , _gsiShardIteratorType :: !ShardIteratorType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetShardIterator' smart constructor.
getShardIterator :: Text -> Text -> ShardIteratorType -> GetShardIterator
getShardIterator pStreamARN_ pShardId_ pShardIteratorType_ =
    GetShardIterator'
    { _gsiSequenceNumber = Nothing
    , _gsiStreamARN = pStreamARN_
    , _gsiShardId = pShardId_
    , _gsiShardIteratorType = pShardIteratorType_
    }

-- | The sequence number of a stream record in the shard from which to start
-- reading.
gsiSequenceNumber :: Lens' GetShardIterator (Maybe Text)
gsiSequenceNumber = lens _gsiSequenceNumber (\ s a -> s{_gsiSequenceNumber = a});

-- | The Amazon Resource Name (ARN) for the stream.
gsiStreamARN :: Lens' GetShardIterator Text
gsiStreamARN = lens _gsiStreamARN (\ s a -> s{_gsiStreamARN = a});

-- | The identifier of the shard. The iterator will be returned for this
-- shard ID.
gsiShardId :: Lens' GetShardIterator Text
gsiShardId = lens _gsiShardId (\ s a -> s{_gsiShardId = a});

-- | Determines how the shard iterator is used to start reading stream
-- records from the shard:
--
-- -   @AT_SEQUENCE_NUMBER@ - Start reading exactly from the position
--     denoted by a specific sequence number.
--
-- -   @AFTER_SEQUENCE_NUMBER@ - Start reading right after the position
--     denoted by a specific sequence number.
--
-- -   @TRIM_HORIZON@ - Start reading at the last (untrimmed) stream
--     record, which is the oldest record in the shard. In DynamoDB
--     Streams, there is a 24 hour limit on data retention. Stream records
--     whose age exceeds this limit are subject to removal (trimming) from
--     the stream.
--
-- -   @LATEST@ - Start reading just after the most recent stream record in
--     the shard, so that you always read the most recent data in the
--     shard.
--
gsiShardIteratorType :: Lens' GetShardIterator ShardIteratorType
gsiShardIteratorType = lens _gsiShardIteratorType (\ s a -> s{_gsiShardIteratorType = a});

instance AWSRequest GetShardIterator where
        type Sv GetShardIterator = DynamoDBStreams
        type Rs GetShardIterator = GetShardIteratorResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetShardIteratorResponse' <$>
                   (x .?> "ShardIterator") <*> (pure (fromEnum s)))

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
              ["SequenceNumber" .= _gsiSequenceNumber,
               "StreamArn" .= _gsiStreamARN,
               "ShardId" .= _gsiShardId,
               "ShardIteratorType" .= _gsiShardIteratorType]

instance ToPath GetShardIterator where
        toPath = const mempty

instance ToQuery GetShardIterator where
        toQuery = const mempty

-- | Represents the output of a /GetShardIterator/ operation.
--
-- /See:/ 'getShardIteratorResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsirsShardIterator'
--
-- * 'gsirsStatus'
data GetShardIteratorResponse = GetShardIteratorResponse'
    { _gsirsShardIterator :: !(Maybe Text)
    , _gsirsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetShardIteratorResponse' smart constructor.
getShardIteratorResponse :: Int -> GetShardIteratorResponse
getShardIteratorResponse pStatus_ =
    GetShardIteratorResponse'
    { _gsirsShardIterator = Nothing
    , _gsirsStatus = pStatus_
    }

-- | The position in the shard from which to start reading stream records
-- sequentially. A shard iterator specifies this position using the
-- sequence number of a stream record in a shard.
gsirsShardIterator :: Lens' GetShardIteratorResponse (Maybe Text)
gsirsShardIterator = lens _gsirsShardIterator (\ s a -> s{_gsirsShardIterator = a});

-- | FIXME: Undocumented member.
gsirsStatus :: Lens' GetShardIteratorResponse Int
gsirsStatus = lens _gsirsStatus (\ s a -> s{_gsirsStatus = a});
