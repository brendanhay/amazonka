{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.V2013_12_02.MergeShards
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation merges two adjacent shards in a stream and combines them
-- into a single shard to reduce the stream's capacity to ingest and transport
-- data. Two shards are considered adjacent if the union of the hash key
-- ranges for the two shards form a contiguous set with no gaps. For example,
-- if you have two shards, one with a hash key range of 276...381 and the
-- other with a hash key range of 382...454, then you could merge these two
-- shards into a single shard that would have a hash key range of 276...454.
-- After the merge, the single child shard receives data for all hash key
-- values covered by the two parent shards. MergeShards is called when there
-- is a need to reduce the overall capacity of a stream because of excess
-- capacity that is not being used. The operation requires that you specify
-- the shard to be merged and the adjacent shard for a given stream. For more
-- information about merging shards, see the Amazon Kinesis Developer Guide.
-- If the stream is in the ACTIVE state, you can call MergeShards. If a stream
-- is in CREATING or UPDATING or DELETING states, then Amazon Kinesis returns
-- a ResourceInUseException. If the specified stream does not exist, Amazon
-- Kinesis returns a ResourceNotFoundException. You can use the DescribeStream
-- operation to check the state of the stream, which is returned in
-- StreamStatus. MergeShards is an asynchronous operation. Upon receiving a
-- MergeShards request, Amazon Kinesis immediately returns a response and sets
-- the StreamStatus to UPDATING. After the operation is completed, Amazon
-- Kinesis sets the StreamStatus to ACTIVE. Read and write operations continue
-- to work while the stream is in the UPDATING state. You use the
-- DescribeStream operation to determine the shard IDs that are specified in
-- the MergeShards request. If you try to operate on too many streams in
-- parallel using CreateStream, DeleteStream, MergeShards or SplitShard, you
-- will receive a LimitExceededException. MergeShards has limit of 5
-- transactions per second per account. Merge Two Adjacent Shards The
-- following is an example of an Amazon Kinesis MergeShards request and
-- response. POST / HTTP/1.1 Host: kinesis.. x-amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.MergeShards { "StreamName": "exampleStreamName",
-- "ShardToMerge": "shardId-000000000000", "AdjacentShardToMerge":
-- "shardId-000000000001" } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]>.
module Network.AWS.Kinesis.V2013_12_02.MergeShards where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.Kinesis.V2013_12_02.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

data MergeShards = MergeShards
    { _msiShardToMerge :: Text
      -- ^ The shard ID of the shard to combine with the adjacent shard for
      -- the merge.
    , _msiAdjacentShardToMerge :: Text
      -- ^ The shard ID of the adjacent shard for the merge.
    , _msiStreamName :: Text
      -- ^ The name of the stream for the merge.
    } deriving (Show, Generic)

makeLenses ''MergeShards

instance ToPath MergeShards

instance ToQuery MergeShards

instance ToHeaders MergeShards

instance ToJSON MergeShards

data MergeShardsResponse = MergeShardsResponse
    deriving (Eq, Show, Generic)

makeLenses ''MergeShardsResponse

instance AWSRequest MergeShards where
    type Sv MergeShards = Kinesis
    type Rs MergeShards = MergeShardsResponse

    request = get
    response _ = nullaryResponse MergeShardsResponse
