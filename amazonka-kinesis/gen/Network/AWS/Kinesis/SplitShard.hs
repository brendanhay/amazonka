{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.Kinesis.SplitShard
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Splits a shard into two new shards in the stream, to increase the stream's
-- capacity to ingest and transport data. SplitShard is called when there is a
-- need to increase the overall capacity of stream because of an expected
-- increase in the volume of data records being ingested. You can also use
-- SplitShard when a shard appears to be approaching its maximum utilization,
-- for example, when the set of producers sending data into the specific shard
-- are suddenly sending more than previously anticipated. You can also call
-- SplitShard to increase stream capacity, so that more Amazon Kinesis
-- applications can simultaneously read data from the stream for real-time
-- processing. You must specify the shard to be split and the new hash key,
-- which is the position in the shard where the shard gets split in two. In
-- many cases, the new hash key might simply be the average of the beginning
-- and ending hash key, but it can be any hash key value in the range being
-- mapped into the shard. For more information about splitting shards, see
-- Split a Shard in the Amazon Kinesis Developer Guide. You can use
-- DescribeStream to determine the shard ID and hash key values for the
-- ShardToSplit and NewStartingHashKey parameters that are specified in the
-- SplitShard request. SplitShard is an asynchronous operation. Upon receiving
-- a SplitShard request, Amazon Kinesis immediately returns a response and
-- sets the stream status to UPDATING. After the operation is completed,
-- Amazon Kinesis sets the stream status to ACTIVE. Read and write operations
-- continue to work while the stream is in the UPDATING state. You can use
-- DescribeStream to check the status of the stream, which is returned in
-- StreamStatus. If the stream is in the ACTIVE state, you can call
-- SplitShard. If a stream is in CREATING or UPDATING or DELETING states,
-- DescribeStream returns a ResourceInUseException. If the specified stream
-- does not exist, DescribeStream returns a ResourceNotFoundException. If you
-- try to create more shards than are authorized for your account, you receive
-- a LimitExceededException. The default limit for an AWS account is 10 shards
-- per stream. If you need to create a stream with more than 10 shards,
-- contact AWS Support to increase the limit on your account. If you try to
-- operate on too many streams in parallel using CreateStream, DeleteStream,
-- MergeShards or SplitShard, you receive a LimitExceededException. SplitShard
-- has limit of 5 transactions per second per account.
module Network.AWS.Kinesis.SplitShard
    (
    -- * Request
      SplitShard
    -- ** Request constructor
    , splitShard
    -- ** Request lenses
    , ssNewStartingHashKey
    , ssShardToSplit
    , ssStreamName

    -- * Response
    , SplitShardResponse
    -- ** Response constructor
    , splitShardResponse
    ) where

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Kinesis.Types

data SplitShard = SplitShard
    { _ssNewStartingHashKey :: Text
    , _ssShardToSplit       :: Text
    , _ssStreamName         :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SplitShard' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssNewStartingHashKey' @::@ 'Text'
--
-- * 'ssShardToSplit' @::@ 'Text'
--
-- * 'ssStreamName' @::@ 'Text'
--
splitShard :: Text -- ^ 'ssStreamName'
           -> Text -- ^ 'ssShardToSplit'
           -> Text -- ^ 'ssNewStartingHashKey'
           -> SplitShard
splitShard p1 p2 p3 = SplitShard
    { _ssStreamName         = p1
    , _ssShardToSplit       = p2
    , _ssNewStartingHashKey = p3
    }

-- | A hash key value for the starting hash key of one of the child shards
-- created by the split. The hash key range for a given shard constitutes a
-- set of ordered contiguous positive integers. The value for
-- NewStartingHashKey must be in the range of hash keys being mapped into
-- the shard. The NewStartingHashKey hash key value and all higher hash key
-- values in hash key range are distributed to one of the child shards. All
-- the lower hash key values in the range are distributed to the other child
-- shard.
ssNewStartingHashKey :: Lens' SplitShard Text
ssNewStartingHashKey =
    lens _ssNewStartingHashKey (\s a -> s { _ssNewStartingHashKey = a })

-- | The shard ID of the shard to split.
ssShardToSplit :: Lens' SplitShard Text
ssShardToSplit = lens _ssShardToSplit (\s a -> s { _ssShardToSplit = a })

-- | The name of the stream for the shard split.
ssStreamName :: Lens' SplitShard Text
ssStreamName = lens _ssStreamName (\s a -> s { _ssStreamName = a })

instance ToPath SplitShard where
    toPath = const "/"

instance ToQuery SplitShard where
    toQuery = const mempty

instance ToHeaders SplitShard

instance ToBody SplitShard where
    toBody = toBody . encode . _ssStreamName

data SplitShardResponse = SplitShardResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SplitShardResponse' constructor.
splitShardResponse :: SplitShardResponse
splitShardResponse = SplitShardResponse

-- FromJSON

instance AWSRequest SplitShard where
    type Sv SplitShard = Kinesis
    type Rs SplitShard = SplitShardResponse

    request  = post'
    response = nullaryResponse SplitShardResponse
