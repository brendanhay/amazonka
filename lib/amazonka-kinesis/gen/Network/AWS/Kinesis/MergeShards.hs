{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.MergeShards
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two adjacent shards in a Kinesis data stream and combines them into a single shard to reduce the stream's capacity to ingest and transport data. Two shards are considered adjacent if the union of the hash key ranges for the two shards form a contiguous set with no gaps. For example, if you have two shards, one with a hash key range of 276...381 and the other with a hash key range of 382...454, then you could merge these two shards into a single shard that would have a hash key range of 276...454. After the merge, the single child shard receives data for all hash key values covered by the two parent shards.
--
-- @MergeShards@ is called when there is a need to reduce the overall capacity of a stream because of excess capacity that is not being used. You must specify the shard to be merged and the adjacent shard for a stream. For more information about merging shards, see <https://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-resharding-merge.html Merge Two Shards> in the /Amazon Kinesis Data Streams Developer Guide/ .
-- If the stream is in the @ACTIVE@ state, you can call @MergeShards@ . If a stream is in the @CREATING@ , @UPDATING@ , or @DELETING@ state, @MergeShards@ returns a @ResourceInUseException@ . If the specified stream does not exist, @MergeShards@ returns a @ResourceNotFoundException@ . 
-- You can use 'DescribeStream' to check the state of the stream, which is returned in @StreamStatus@ .
-- @MergeShards@ is an asynchronous operation. Upon receiving a @MergeShards@ request, Amazon Kinesis Data Streams immediately returns a response and sets the @StreamStatus@ to @UPDATING@ . After the operation is completed, Kinesis Data Streams sets the @StreamStatus@ to @ACTIVE@ . Read and write operations continue to work while the stream is in the @UPDATING@ state. 
-- You use 'DescribeStream' to determine the shard IDs that are specified in the @MergeShards@ request. 
-- If you try to operate on too many streams in parallel using 'CreateStream' , 'DeleteStream' , @MergeShards@ , or 'SplitShard' , you receive a @LimitExceededException@ . 
-- @MergeShards@ has a limit of five transactions per second per account.
module Network.AWS.Kinesis.MergeShards
    (
    -- * Creating a request
      MergeShards (..)
    , mkMergeShards
    -- ** Request lenses
    , msStreamName
    , msShardToMerge
    , msAdjacentShardToMerge

    -- * Destructuring the response
    , MergeShardsResponse (..)
    , mkMergeShardsResponse
    ) where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @MergeShards@ .
--
-- /See:/ 'mkMergeShards' smart constructor.
data MergeShards = MergeShards'
  { streamName :: Types.StreamName
    -- ^ The name of the stream for the merge.
  , shardToMerge :: Types.ShardToMerge
    -- ^ The shard ID of the shard to combine with the adjacent shard for the merge.
  , adjacentShardToMerge :: Types.AdjacentShardToMerge
    -- ^ The shard ID of the adjacent shard for the merge.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergeShards' value with any optional fields omitted.
mkMergeShards
    :: Types.StreamName -- ^ 'streamName'
    -> Types.ShardToMerge -- ^ 'shardToMerge'
    -> Types.AdjacentShardToMerge -- ^ 'adjacentShardToMerge'
    -> MergeShards
mkMergeShards streamName shardToMerge adjacentShardToMerge
  = MergeShards'{streamName, shardToMerge, adjacentShardToMerge}

-- | The name of the stream for the merge.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msStreamName :: Lens.Lens' MergeShards Types.StreamName
msStreamName = Lens.field @"streamName"
{-# INLINEABLE msStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

-- | The shard ID of the shard to combine with the adjacent shard for the merge.
--
-- /Note:/ Consider using 'shardToMerge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msShardToMerge :: Lens.Lens' MergeShards Types.ShardToMerge
msShardToMerge = Lens.field @"shardToMerge"
{-# INLINEABLE msShardToMerge #-}
{-# DEPRECATED shardToMerge "Use generic-lens or generic-optics with 'shardToMerge' instead"  #-}

-- | The shard ID of the adjacent shard for the merge.
--
-- /Note:/ Consider using 'adjacentShardToMerge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAdjacentShardToMerge :: Lens.Lens' MergeShards Types.AdjacentShardToMerge
msAdjacentShardToMerge = Lens.field @"adjacentShardToMerge"
{-# INLINEABLE msAdjacentShardToMerge #-}
{-# DEPRECATED adjacentShardToMerge "Use generic-lens or generic-optics with 'adjacentShardToMerge' instead"  #-}

instance Core.ToQuery MergeShards where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders MergeShards where
        toHeaders MergeShards{..}
          = Core.pure ("X-Amz-Target", "Kinesis_20131202.MergeShards")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON MergeShards where
        toJSON MergeShards{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StreamName" Core..= streamName),
                  Core.Just ("ShardToMerge" Core..= shardToMerge),
                  Core.Just ("AdjacentShardToMerge" Core..= adjacentShardToMerge)])

instance Core.AWSRequest MergeShards where
        type Rs MergeShards = MergeShardsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull MergeShardsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkMergeShardsResponse' smart constructor.
data MergeShardsResponse = MergeShardsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergeShardsResponse' value with any optional fields omitted.
mkMergeShardsResponse
    :: MergeShardsResponse
mkMergeShardsResponse = MergeShardsResponse'
