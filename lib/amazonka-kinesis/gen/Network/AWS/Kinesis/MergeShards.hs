{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    MergeShards (..),
    mkMergeShards,

    -- ** Request lenses
    msShardToMerge,
    msAdjacentShardToMerge,
    msStreamName,

    -- * Destructuring the response
    MergeShardsResponse (..),
    mkMergeShardsResponse,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for @MergeShards@ .
--
-- /See:/ 'mkMergeShards' smart constructor.
data MergeShards = MergeShards'
  { -- | The shard ID of the shard to combine with the adjacent shard for the merge.
    shardToMerge :: Lude.Text,
    -- | The shard ID of the adjacent shard for the merge.
    adjacentShardToMerge :: Lude.Text,
    -- | The name of the stream for the merge.
    streamName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergeShards' with the minimum fields required to make a request.
--
-- * 'shardToMerge' - The shard ID of the shard to combine with the adjacent shard for the merge.
-- * 'adjacentShardToMerge' - The shard ID of the adjacent shard for the merge.
-- * 'streamName' - The name of the stream for the merge.
mkMergeShards ::
  -- | 'shardToMerge'
  Lude.Text ->
  -- | 'adjacentShardToMerge'
  Lude.Text ->
  -- | 'streamName'
  Lude.Text ->
  MergeShards
mkMergeShards pShardToMerge_ pAdjacentShardToMerge_ pStreamName_ =
  MergeShards'
    { shardToMerge = pShardToMerge_,
      adjacentShardToMerge = pAdjacentShardToMerge_,
      streamName = pStreamName_
    }

-- | The shard ID of the shard to combine with the adjacent shard for the merge.
--
-- /Note:/ Consider using 'shardToMerge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msShardToMerge :: Lens.Lens' MergeShards Lude.Text
msShardToMerge = Lens.lens (shardToMerge :: MergeShards -> Lude.Text) (\s a -> s {shardToMerge = a} :: MergeShards)
{-# DEPRECATED msShardToMerge "Use generic-lens or generic-optics with 'shardToMerge' instead." #-}

-- | The shard ID of the adjacent shard for the merge.
--
-- /Note:/ Consider using 'adjacentShardToMerge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAdjacentShardToMerge :: Lens.Lens' MergeShards Lude.Text
msAdjacentShardToMerge = Lens.lens (adjacentShardToMerge :: MergeShards -> Lude.Text) (\s a -> s {adjacentShardToMerge = a} :: MergeShards)
{-# DEPRECATED msAdjacentShardToMerge "Use generic-lens or generic-optics with 'adjacentShardToMerge' instead." #-}

-- | The name of the stream for the merge.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msStreamName :: Lens.Lens' MergeShards Lude.Text
msStreamName = Lens.lens (streamName :: MergeShards -> Lude.Text) (\s a -> s {streamName = a} :: MergeShards)
{-# DEPRECATED msStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest MergeShards where
  type Rs MergeShards = MergeShardsResponse
  request = Req.postJSON kinesisService
  response = Res.receiveNull MergeShardsResponse'

instance Lude.ToHeaders MergeShards where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.MergeShards" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON MergeShards where
  toJSON MergeShards' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ShardToMerge" Lude..= shardToMerge),
            Lude.Just ("AdjacentShardToMerge" Lude..= adjacentShardToMerge),
            Lude.Just ("StreamName" Lude..= streamName)
          ]
      )

instance Lude.ToPath MergeShards where
  toPath = Lude.const "/"

instance Lude.ToQuery MergeShards where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkMergeShardsResponse' smart constructor.
data MergeShardsResponse = MergeShardsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergeShardsResponse' with the minimum fields required to make a request.
mkMergeShardsResponse ::
  MergeShardsResponse
mkMergeShardsResponse = MergeShardsResponse'
