{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.MergeShards
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two adjacent shards in a Kinesis data stream and combines them
-- into a single shard to reduce the stream\'s capacity to ingest and
-- transport data. Two shards are considered adjacent if the union of the
-- hash key ranges for the two shards form a contiguous set with no gaps.
-- For example, if you have two shards, one with a hash key range of
-- 276...381 and the other with a hash key range of 382...454, then you
-- could merge these two shards into a single shard that would have a hash
-- key range of 276...454. After the merge, the single child shard receives
-- data for all hash key values covered by the two parent shards.
--
-- @MergeShards@ is called when there is a need to reduce the overall
-- capacity of a stream because of excess capacity that is not being used.
-- You must specify the shard to be merged and the adjacent shard for a
-- stream. For more information about merging shards, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-resharding-merge.html Merge Two Shards>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
--
-- If the stream is in the @ACTIVE@ state, you can call @MergeShards@. If a
-- stream is in the @CREATING@, @UPDATING@, or @DELETING@ state,
-- @MergeShards@ returns a @ResourceInUseException@. If the specified
-- stream does not exist, @MergeShards@ returns a
-- @ResourceNotFoundException@.
--
-- You can use DescribeStream to check the state of the stream, which is
-- returned in @StreamStatus@.
--
-- @MergeShards@ is an asynchronous operation. Upon receiving a
-- @MergeShards@ request, Amazon Kinesis Data Streams immediately returns a
-- response and sets the @StreamStatus@ to @UPDATING@. After the operation
-- is completed, Kinesis Data Streams sets the @StreamStatus@ to @ACTIVE@.
-- Read and write operations continue to work while the stream is in the
-- @UPDATING@ state.
--
-- You use DescribeStream to determine the shard IDs that are specified in
-- the @MergeShards@ request.
--
-- If you try to operate on too many streams in parallel using
-- CreateStream, DeleteStream, @MergeShards@, or SplitShard, you receive a
-- @LimitExceededException@.
--
-- @MergeShards@ has a limit of five transactions per second per account.
module Network.AWS.Kinesis.MergeShards
  ( -- * Creating a Request
    MergeShards (..),
    newMergeShards,

    -- * Request Lenses
    mergeShards_streamName,
    mergeShards_shardToMerge,
    mergeShards_adjacentShardToMerge,

    -- * Destructuring the Response
    MergeShardsResponse (..),
    newMergeShardsResponse,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @MergeShards@.
--
-- /See:/ 'newMergeShards' smart constructor.
data MergeShards = MergeShards'
  { -- | The name of the stream for the merge.
    streamName :: Prelude.Text,
    -- | The shard ID of the shard to combine with the adjacent shard for the
    -- merge.
    shardToMerge :: Prelude.Text,
    -- | The shard ID of the adjacent shard for the merge.
    adjacentShardToMerge :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MergeShards' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamName', 'mergeShards_streamName' - The name of the stream for the merge.
--
-- 'shardToMerge', 'mergeShards_shardToMerge' - The shard ID of the shard to combine with the adjacent shard for the
-- merge.
--
-- 'adjacentShardToMerge', 'mergeShards_adjacentShardToMerge' - The shard ID of the adjacent shard for the merge.
newMergeShards ::
  -- | 'streamName'
  Prelude.Text ->
  -- | 'shardToMerge'
  Prelude.Text ->
  -- | 'adjacentShardToMerge'
  Prelude.Text ->
  MergeShards
newMergeShards
  pStreamName_
  pShardToMerge_
  pAdjacentShardToMerge_ =
    MergeShards'
      { streamName = pStreamName_,
        shardToMerge = pShardToMerge_,
        adjacentShardToMerge = pAdjacentShardToMerge_
      }

-- | The name of the stream for the merge.
mergeShards_streamName :: Lens.Lens' MergeShards Prelude.Text
mergeShards_streamName = Lens.lens (\MergeShards' {streamName} -> streamName) (\s@MergeShards' {} a -> s {streamName = a} :: MergeShards)

-- | The shard ID of the shard to combine with the adjacent shard for the
-- merge.
mergeShards_shardToMerge :: Lens.Lens' MergeShards Prelude.Text
mergeShards_shardToMerge = Lens.lens (\MergeShards' {shardToMerge} -> shardToMerge) (\s@MergeShards' {} a -> s {shardToMerge = a} :: MergeShards)

-- | The shard ID of the adjacent shard for the merge.
mergeShards_adjacentShardToMerge :: Lens.Lens' MergeShards Prelude.Text
mergeShards_adjacentShardToMerge = Lens.lens (\MergeShards' {adjacentShardToMerge} -> adjacentShardToMerge) (\s@MergeShards' {} a -> s {adjacentShardToMerge = a} :: MergeShards)

instance Prelude.AWSRequest MergeShards where
  type Rs MergeShards = MergeShardsResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull MergeShardsResponse'

instance Prelude.Hashable MergeShards

instance Prelude.NFData MergeShards

instance Prelude.ToHeaders MergeShards where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Kinesis_20131202.MergeShards" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON MergeShards where
  toJSON MergeShards' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("StreamName" Prelude..= streamName),
            Prelude.Just
              ("ShardToMerge" Prelude..= shardToMerge),
            Prelude.Just
              ( "AdjacentShardToMerge"
                  Prelude..= adjacentShardToMerge
              )
          ]
      )

instance Prelude.ToPath MergeShards where
  toPath = Prelude.const "/"

instance Prelude.ToQuery MergeShards where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newMergeShardsResponse' smart constructor.
data MergeShardsResponse = MergeShardsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MergeShardsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newMergeShardsResponse ::
  MergeShardsResponse
newMergeShardsResponse = MergeShardsResponse'

instance Prelude.NFData MergeShardsResponse
