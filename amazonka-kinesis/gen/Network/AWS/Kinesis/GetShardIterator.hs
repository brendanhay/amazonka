{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.GetShardIterator
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets a shard iterator. A shard iterator expires five minutes after it is
-- returned to the requester.
--
-- A shard iterator specifies the position in the shard from which to start
-- reading data records sequentially. A shard iterator specifies this
-- position using the sequence number of a data record in a shard. A
-- sequence number is the identifier associated with every record ingested
-- in the Amazon Kinesis stream. The sequence number is assigned when a
-- record is put into the stream.
--
-- You must specify the shard iterator type. For example, you can set the
-- @ShardIteratorType@ parameter to read exactly from the position denoted
-- by a specific sequence number by using the @AT_SEQUENCE_NUMBER@ shard
-- iterator type, or right after the sequence number by using the
-- @AFTER_SEQUENCE_NUMBER@ shard iterator type, using sequence numbers
-- returned by earlier calls to PutRecord, PutRecords, GetRecords, or
-- DescribeStream. You can specify the shard iterator type @TRIM_HORIZON@
-- in the request to cause @ShardIterator@ to point to the last untrimmed
-- record in the shard in the system, which is the oldest data record in
-- the shard. Or you can point to just after the most recent record in the
-- shard, by using the shard iterator type @LATEST@, so that you always
-- read the most recent data in the shard.
--
-- When you repeatedly read from an Amazon Kinesis stream use a
-- GetShardIterator request to get the first shard iterator for use in your
-- first GetRecords request and then use the shard iterator returned by the
-- GetRecords request in @NextShardIterator@ for subsequent reads. A new
-- shard iterator is returned by every GetRecords request in
-- @NextShardIterator@, which you use in the @ShardIterator@ parameter of
-- the next GetRecords request.
--
-- If a GetShardIterator request is made too often, you receive a
-- @ProvisionedThroughputExceededException@. For more information about
-- throughput limits, see GetRecords.
--
-- If the shard is closed, the iterator can\'t return more data, and
-- GetShardIterator returns @null@ for its @ShardIterator@. A shard can be
-- closed using SplitShard or MergeShards.
--
-- GetShardIterator has a limit of 5 transactions per second per account
-- per open shard.
--
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetShardIterator.html>
module Network.AWS.Kinesis.GetShardIterator
    (
    -- * Request
      GetShardIterator
    -- ** Request constructor
    , getShardIterator
    -- ** Request lenses
    , gsiStartingSequenceNumber
    , gsiStreamName
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

import           Network.AWS.Kinesis.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for @GetShardIterator@.
--
-- /See:/ 'getShardIterator' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsiStartingSequenceNumber'
--
-- * 'gsiStreamName'
--
-- * 'gsiShardId'
--
-- * 'gsiShardIteratorType'
data GetShardIterator = GetShardIterator'
    { _gsiStartingSequenceNumber :: !(Maybe Text)
    , _gsiStreamName             :: !Text
    , _gsiShardId                :: !Text
    , _gsiShardIteratorType      :: !ShardIteratorType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetShardIterator' smart constructor.
getShardIterator :: Text -> Text -> ShardIteratorType -> GetShardIterator
getShardIterator pStreamName_ pShardId_ pShardIteratorType_ =
    GetShardIterator'
    { _gsiStartingSequenceNumber = Nothing
    , _gsiStreamName = pStreamName_
    , _gsiShardId = pShardId_
    , _gsiShardIteratorType = pShardIteratorType_
    }

-- | The sequence number of the data record in the shard from which to start
-- reading from.
gsiStartingSequenceNumber :: Lens' GetShardIterator (Maybe Text)
gsiStartingSequenceNumber = lens _gsiStartingSequenceNumber (\ s a -> s{_gsiStartingSequenceNumber = a});

-- | The name of the stream.
gsiStreamName :: Lens' GetShardIterator Text
gsiStreamName = lens _gsiStreamName (\ s a -> s{_gsiStreamName = a});

-- | The shard ID of the shard to get the iterator for.
gsiShardId :: Lens' GetShardIterator Text
gsiShardId = lens _gsiShardId (\ s a -> s{_gsiShardId = a});

-- | Determines how the shard iterator is used to start reading data records
-- from the shard.
--
-- The following are the valid shard iterator types:
--
-- -   AT_SEQUENCE_NUMBER - Start reading exactly from the position denoted
--     by a specific sequence number.
-- -   AFTER_SEQUENCE_NUMBER - Start reading right after the position
--     denoted by a specific sequence number.
-- -   TRIM_HORIZON - Start reading at the last untrimmed record in the
--     shard in the system, which is the oldest data record in the shard.
-- -   LATEST - Start reading just after the most recent record in the
--     shard, so that you always read the most recent data in the shard.
gsiShardIteratorType :: Lens' GetShardIterator ShardIteratorType
gsiShardIteratorType = lens _gsiShardIteratorType (\ s a -> s{_gsiShardIteratorType = a});

instance AWSRequest GetShardIterator where
        type Sv GetShardIterator = Kinesis
        type Rs GetShardIterator = GetShardIteratorResponse
        request = postJSON "GetShardIterator"
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
                    ("Kinesis_20131202.GetShardIterator" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetShardIterator where
        toJSON GetShardIterator'{..}
          = object
              ["StartingSequenceNumber" .=
                 _gsiStartingSequenceNumber,
               "StreamName" .= _gsiStreamName,
               "ShardId" .= _gsiShardId,
               "ShardIteratorType" .= _gsiShardIteratorType]

instance ToPath GetShardIterator where
        toPath = const "/"

instance ToQuery GetShardIterator where
        toQuery = const mempty

-- | Represents the output for @GetShardIterator@.
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

-- | The position in the shard from which to start reading data records
-- sequentially. A shard iterator specifies this position using the
-- sequence number of a data record in a shard.
gsirsShardIterator :: Lens' GetShardIteratorResponse (Maybe Text)
gsirsShardIterator = lens _gsirsShardIterator (\ s a -> s{_gsirsShardIterator = a});

-- | FIXME: Undocumented member.
gsirsStatus :: Lens' GetShardIteratorResponse Int
gsirsStatus = lens _gsirsStatus (\ s a -> s{_gsirsStatus = a});
