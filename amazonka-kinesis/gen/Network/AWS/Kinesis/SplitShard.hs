{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Kinesis.SplitShard
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Splits a shard into two new shards in the stream, to increase the
-- stream\'s capacity to ingest and transport data. @SplitShard@ is called
-- when there is a need to increase the overall capacity of stream because
-- of an expected increase in the volume of data records being ingested.
--
-- You can also use @SplitShard@ when a shard appears to be approaching its
-- maximum utilization, for example, when the set of producers sending data
-- into the specific shard are suddenly sending more than previously
-- anticipated. You can also call @SplitShard@ to increase stream capacity,
-- so that more Amazon Kinesis applications can simultaneously read data
-- from the stream for real-time processing.
--
-- You must specify the shard to be split and the new hash key, which is
-- the position in the shard where the shard gets split in two. In many
-- cases, the new hash key might simply be the average of the beginning and
-- ending hash key, but it can be any hash key value in the range being
-- mapped into the shard. For more information about splitting shards, see
-- <http://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-resharding-split.html Split a Shard>
-- in the /Amazon Kinesis Developer Guide/.
--
-- You can use DescribeStream to determine the shard ID and hash key values
-- for the @ShardToSplit@ and @NewStartingHashKey@ parameters that are
-- specified in the @SplitShard@ request.
--
-- @SplitShard@ is an asynchronous operation. Upon receiving a @SplitShard@
-- request, Amazon Kinesis immediately returns a response and sets the
-- stream status to @UPDATING@. After the operation is completed, Amazon
-- Kinesis sets the stream status to @ACTIVE@. Read and write operations
-- continue to work while the stream is in the @UPDATING@ state.
--
-- You can use @DescribeStream@ to check the status of the stream, which is
-- returned in @StreamStatus@. If the stream is in the @ACTIVE@ state, you
-- can call @SplitShard@. If a stream is in @CREATING@ or @UPDATING@ or
-- @DELETING@ states, @DescribeStream@ returns a @ResourceInUseException@.
--
-- If the specified stream does not exist, @DescribeStream@ returns a
-- @ResourceNotFoundException@. If you try to create more shards than are
-- authorized for your account, you receive a @LimitExceededException@.
--
-- For the default shard limit for an AWS account, see
-- <http://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Amazon Kinesis Limits>.
-- If you need to increase this limit,
-- <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html contact AWS Support>
--
-- If you try to operate on too many streams in parallel using
-- CreateStream, DeleteStream, MergeShards or SplitShard, you receive a
-- @LimitExceededException@.
--
-- @SplitShard@ has limit of 5 transactions per second per account.
--
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_SplitShard.html>
module Network.AWS.Kinesis.SplitShard
    (
    -- * Request
      SplitShard
    -- ** Request constructor
    , splitShard
    -- ** Request lenses
    , ssStreamName
    , ssShardToSplit
    , ssNewStartingHashKey

    -- * Response
    , SplitShardResponse
    -- ** Response constructor
    , splitShardResponse
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for @SplitShard@.
--
-- /See:/ 'splitShard' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssStreamName'
--
-- * 'ssShardToSplit'
--
-- * 'ssNewStartingHashKey'
data SplitShard = SplitShard'{_ssStreamName :: Text, _ssShardToSplit :: Text, _ssNewStartingHashKey :: Text} deriving (Eq, Read, Show)

-- | 'SplitShard' smart constructor.
splitShard :: Text -> Text -> Text -> SplitShard
splitShard pStreamName pShardToSplit pNewStartingHashKey = SplitShard'{_ssStreamName = pStreamName, _ssShardToSplit = pShardToSplit, _ssNewStartingHashKey = pNewStartingHashKey};

-- | The name of the stream for the shard split.
ssStreamName :: Lens' SplitShard Text
ssStreamName = lens _ssStreamName (\ s a -> s{_ssStreamName = a});

-- | The shard ID of the shard to split.
ssShardToSplit :: Lens' SplitShard Text
ssShardToSplit = lens _ssShardToSplit (\ s a -> s{_ssShardToSplit = a});

-- | A hash key value for the starting hash key of one of the child shards
-- created by the split. The hash key range for a given shard constitutes a
-- set of ordered contiguous positive integers. The value for
-- @NewStartingHashKey@ must be in the range of hash keys being mapped into
-- the shard. The @NewStartingHashKey@ hash key value and all higher hash
-- key values in hash key range are distributed to one of the child shards.
-- All the lower hash key values in the range are distributed to the other
-- child shard.
ssNewStartingHashKey :: Lens' SplitShard Text
ssNewStartingHashKey = lens _ssNewStartingHashKey (\ s a -> s{_ssNewStartingHashKey = a});

instance AWSRequest SplitShard where
        type Sv SplitShard = Kinesis
        type Rs SplitShard = SplitShardResponse
        request = postJSON
        response = receiveNull SplitShardResponse'

instance ToHeaders SplitShard where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.SplitShard" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SplitShard where
        toJSON SplitShard'{..}
          = object
              ["StreamName" .= _ssStreamName,
               "ShardToSplit" .= _ssShardToSplit,
               "NewStartingHashKey" .= _ssNewStartingHashKey]

instance ToPath SplitShard where
        toPath = const "/"

instance ToQuery SplitShard where
        toQuery = const mempty

-- | /See:/ 'splitShardResponse' smart constructor.
data SplitShardResponse = SplitShardResponse' deriving (Eq, Read, Show)

-- | 'SplitShardResponse' smart constructor.
splitShardResponse :: SplitShardResponse
splitShardResponse = SplitShardResponse';
