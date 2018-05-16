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
-- Module      : Network.AWS.Kinesis.MergeShards
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two adjacent shards in a Kinesis data stream and combines them into a single shard to reduce the stream's capacity to ingest and transport data. Two shards are considered adjacent if the union of the hash key ranges for the two shards form a contiguous set with no gaps. For example, if you have two shards, one with a hash key range of 276...381 and the other with a hash key range of 382...454, then you could merge these two shards into a single shard that would have a hash key range of 276...454. After the merge, the single child shard receives data for all hash key values covered by the two parent shards.
--
--
-- @MergeShards@ is called when there is a need to reduce the overall capacity of a stream because of excess capacity that is not being used. You must specify the shard to be merged and the adjacent shard for a stream. For more information about merging shards, see <http://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-resharding-merge.html Merge Two Shards> in the /Amazon Kinesis Data Streams Developer Guide/ .
--
-- If the stream is in the @ACTIVE@ state, you can call @MergeShards@ . If a stream is in the @CREATING@ , @UPDATING@ , or @DELETING@ state, @MergeShards@ returns a @ResourceInUseException@ . If the specified stream does not exist, @MergeShards@ returns a @ResourceNotFoundException@ .
--
-- You can use 'DescribeStream' to check the state of the stream, which is returned in @StreamStatus@ .
--
-- @MergeShards@ is an asynchronous operation. Upon receiving a @MergeShards@ request, Amazon Kinesis Data Streams immediately returns a response and sets the @StreamStatus@ to @UPDATING@ . After the operation is completed, Kinesis Data Streams sets the @StreamStatus@ to @ACTIVE@ . Read and write operations continue to work while the stream is in the @UPDATING@ state.
--
-- You use 'DescribeStream' to determine the shard IDs that are specified in the @MergeShards@ request.
--
-- If you try to operate on too many streams in parallel using 'CreateStream' , 'DeleteStream' , @MergeShards@ , or 'SplitShard' , you receive a @LimitExceededException@ .
--
-- @MergeShards@ has a limit of five transactions per second per account.
--
module Network.AWS.Kinesis.MergeShards
    (
    -- * Creating a Request
      mergeShards
    , MergeShards
    -- * Request Lenses
    , msStreamName
    , msShardToMerge
    , msAdjacentShardToMerge

    -- * Destructuring the Response
    , mergeShardsResponse
    , MergeShardsResponse
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for @MergeShards@ .
--
--
--
-- /See:/ 'mergeShards' smart constructor.
data MergeShards = MergeShards'
  { _msStreamName           :: !Text
  , _msShardToMerge         :: !Text
  , _msAdjacentShardToMerge :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MergeShards' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msStreamName' - The name of the stream for the merge.
--
-- * 'msShardToMerge' - The shard ID of the shard to combine with the adjacent shard for the merge.
--
-- * 'msAdjacentShardToMerge' - The shard ID of the adjacent shard for the merge.
mergeShards
    :: Text -- ^ 'msStreamName'
    -> Text -- ^ 'msShardToMerge'
    -> Text -- ^ 'msAdjacentShardToMerge'
    -> MergeShards
mergeShards pStreamName_ pShardToMerge_ pAdjacentShardToMerge_ =
  MergeShards'
    { _msStreamName = pStreamName_
    , _msShardToMerge = pShardToMerge_
    , _msAdjacentShardToMerge = pAdjacentShardToMerge_
    }


-- | The name of the stream for the merge.
msStreamName :: Lens' MergeShards Text
msStreamName = lens _msStreamName (\ s a -> s{_msStreamName = a})

-- | The shard ID of the shard to combine with the adjacent shard for the merge.
msShardToMerge :: Lens' MergeShards Text
msShardToMerge = lens _msShardToMerge (\ s a -> s{_msShardToMerge = a})

-- | The shard ID of the adjacent shard for the merge.
msAdjacentShardToMerge :: Lens' MergeShards Text
msAdjacentShardToMerge = lens _msAdjacentShardToMerge (\ s a -> s{_msAdjacentShardToMerge = a})

instance AWSRequest MergeShards where
        type Rs MergeShards = MergeShardsResponse
        request = postJSON kinesis
        response = receiveNull MergeShardsResponse'

instance Hashable MergeShards where

instance NFData MergeShards where

instance ToHeaders MergeShards where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.MergeShards" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON MergeShards where
        toJSON MergeShards'{..}
          = object
              (catMaybes
                 [Just ("StreamName" .= _msStreamName),
                  Just ("ShardToMerge" .= _msShardToMerge),
                  Just
                    ("AdjacentShardToMerge" .= _msAdjacentShardToMerge)])

instance ToPath MergeShards where
        toPath = const "/"

instance ToQuery MergeShards where
        toQuery = const mempty

-- | /See:/ 'mergeShardsResponse' smart constructor.
data MergeShardsResponse =
  MergeShardsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MergeShardsResponse' with the minimum fields required to make a request.
--
mergeShardsResponse
    :: MergeShardsResponse
mergeShardsResponse = MergeShardsResponse'


instance NFData MergeShardsResponse where
