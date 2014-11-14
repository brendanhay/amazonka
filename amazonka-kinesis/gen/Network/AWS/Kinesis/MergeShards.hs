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

-- Module      : Network.AWS.Kinesis.MergeShards
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Merges two adjacent shards in a stream and combines them into a single
-- shard to reduce the stream's capacity to ingest and transport data. Two
-- shards are considered adjacent if the union of the hash key ranges for the
-- two shards form a contiguous set with no gaps. For example, if you have two
-- shards, one with a hash key range of 276...381 and the other with a hash
-- key range of 382...454, then you could merge these two shards into a single
-- shard that would have a hash key range of 276...454. After the merge, the
-- single child shard receives data for all hash key values covered by the two
-- parent shards. MergeShards is called when there is a need to reduce the
-- overall capacity of a stream because of excess capacity that is not being
-- used. You must specify the shard to be merged and the adjacent shard for a
-- stream. For more information about merging shards, see Merge Two Shards in
-- the Amazon Kinesis Developer Guide. If the stream is in the ACTIVE state,
-- you can call MergeShards. If a stream is in the CREATING, UPDATING, or
-- DELETING state, MergeShards returns a ResourceInUseException. If the
-- specified stream does not exist, MergeShards returns a
-- ResourceNotFoundException. You can use DescribeStream to check the state of
-- the stream, which is returned in StreamStatus. MergeShards is an
-- asynchronous operation. Upon receiving a MergeShards request, Amazon
-- Kinesis immediately returns a response and sets the StreamStatus to
-- UPDATING. After the operation is completed, Amazon Kinesis sets the
-- StreamStatus to ACTIVE. Read and write operations continue to work while
-- the stream is in the UPDATING state. You use DescribeStream to determine
-- the shard IDs that are specified in the MergeShards request. If you try to
-- operate on too many streams in parallel using CreateStream, DeleteStream,
-- MergeShards or SplitShard, you will receive a LimitExceededException.
-- MergeShards has limit of 5 transactions per second per account.
module Network.AWS.Kinesis.MergeShards
    (
    -- * Request
      MergeShards
    -- ** Request constructor
    , mergeShards
    -- ** Request lenses
    , msAdjacentShardToMerge
    , msShardToMerge
    , msStreamName

    -- * Response
    , MergeShardsResponse
    -- ** Response constructor
    , mergeShardsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Kinesis.Types

data MergeShards = MergeShards
    { _msAdjacentShardToMerge :: Text
    , _msShardToMerge         :: Text
    , _msStreamName           :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'MergeShards' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'msAdjacentShardToMerge' @::@ 'Text'
--
-- * 'msShardToMerge' @::@ 'Text'
--
-- * 'msStreamName' @::@ 'Text'
--
mergeShards :: Text -- ^ 'msStreamName'
            -> Text -- ^ 'msShardToMerge'
            -> Text -- ^ 'msAdjacentShardToMerge'
            -> MergeShards
mergeShards p1 p2 p3 = MergeShards
    { _msStreamName           = p1
    , _msShardToMerge         = p2
    , _msAdjacentShardToMerge = p3
    }

-- | The shard ID of the adjacent shard for the merge.
msAdjacentShardToMerge :: Lens' MergeShards Text
msAdjacentShardToMerge =
    lens _msAdjacentShardToMerge (\s a -> s { _msAdjacentShardToMerge = a })

-- | The shard ID of the shard to combine with the adjacent shard for the
-- merge.
msShardToMerge :: Lens' MergeShards Text
msShardToMerge = lens _msShardToMerge (\s a -> s { _msShardToMerge = a })

-- | The name of the stream for the merge.
msStreamName :: Lens' MergeShards Text
msStreamName = lens _msStreamName (\s a -> s { _msStreamName = a })

instance ToPath MergeShards where
    toPath = const "/"

instance ToQuery MergeShards where
    toQuery = const mempty

instance ToHeaders MergeShards

instance ToBody MergeShards where
    toBody = toBody . encode . _msStreamName

data MergeShardsResponse = MergeShardsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'MergeShardsResponse' constructor.
mergeShardsResponse :: MergeShardsResponse
mergeShardsResponse = MergeShardsResponse

instance AWSRequest MergeShards where
    type Sv MergeShards = Kinesis
    type Rs MergeShards = MergeShardsResponse

    request  = post
    response = nullaryResponse MergeShardsResponse
