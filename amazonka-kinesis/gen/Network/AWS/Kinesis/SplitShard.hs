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
-- Module      : Network.AWS.Kinesis.SplitShard
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Splits a shard into two new shards in the Kinesis data stream, to
-- increase the stream\'s capacity to ingest and transport data.
-- @SplitShard@ is called when there is a need to increase the overall
-- capacity of a stream because of an expected increase in the volume of
-- data records being ingested.
--
-- You can also use @SplitShard@ when a shard appears to be approaching its
-- maximum utilization; for example, the producers sending data into the
-- specific shard are suddenly sending more than previously anticipated.
-- You can also call @SplitShard@ to increase stream capacity, so that more
-- Kinesis Data Streams applications can simultaneously read data from the
-- stream for real-time processing.
--
-- You must specify the shard to be split and the new hash key, which is
-- the position in the shard where the shard gets split in two. In many
-- cases, the new hash key might be the average of the beginning and ending
-- hash key, but it can be any hash key value in the range being mapped
-- into the shard. For more information, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-resharding-split.html Split a Shard>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
--
-- You can use DescribeStream to determine the shard ID and hash key values
-- for the @ShardToSplit@ and @NewStartingHashKey@ parameters that are
-- specified in the @SplitShard@ request.
--
-- @SplitShard@ is an asynchronous operation. Upon receiving a @SplitShard@
-- request, Kinesis Data Streams immediately returns a response and sets
-- the stream status to @UPDATING@. After the operation is completed,
-- Kinesis Data Streams sets the stream status to @ACTIVE@. Read and write
-- operations continue to work while the stream is in the @UPDATING@ state.
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
-- <https://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Kinesis Data Streams Limits>
-- in the /Amazon Kinesis Data Streams Developer Guide/. To increase this
-- limit,
-- <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html contact AWS Support>.
--
-- If you try to operate on too many streams simultaneously using
-- CreateStream, DeleteStream, MergeShards, and\/or SplitShard, you receive
-- a @LimitExceededException@.
--
-- @SplitShard@ has a limit of five transactions per second per account.
module Network.AWS.Kinesis.SplitShard
  ( -- * Creating a Request
    SplitShard (..),
    newSplitShard,

    -- * Request Lenses
    splitShard_streamName,
    splitShard_shardToSplit,
    splitShard_newStartingHashKey,

    -- * Destructuring the Response
    SplitShardResponse (..),
    newSplitShardResponse,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @SplitShard@.
--
-- /See:/ 'newSplitShard' smart constructor.
data SplitShard = SplitShard'
  { -- | The name of the stream for the shard split.
    streamName :: Prelude.Text,
    -- | The shard ID of the shard to split.
    shardToSplit :: Prelude.Text,
    -- | A hash key value for the starting hash key of one of the child shards
    -- created by the split. The hash key range for a given shard constitutes a
    -- set of ordered contiguous positive integers. The value for
    -- @NewStartingHashKey@ must be in the range of hash keys being mapped into
    -- the shard. The @NewStartingHashKey@ hash key value and all higher hash
    -- key values in hash key range are distributed to one of the child shards.
    -- All the lower hash key values in the range are distributed to the other
    -- child shard.
    newStartingHashKey' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SplitShard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamName', 'splitShard_streamName' - The name of the stream for the shard split.
--
-- 'shardToSplit', 'splitShard_shardToSplit' - The shard ID of the shard to split.
--
-- 'newStartingHashKey'', 'splitShard_newStartingHashKey' - A hash key value for the starting hash key of one of the child shards
-- created by the split. The hash key range for a given shard constitutes a
-- set of ordered contiguous positive integers. The value for
-- @NewStartingHashKey@ must be in the range of hash keys being mapped into
-- the shard. The @NewStartingHashKey@ hash key value and all higher hash
-- key values in hash key range are distributed to one of the child shards.
-- All the lower hash key values in the range are distributed to the other
-- child shard.
newSplitShard ::
  -- | 'streamName'
  Prelude.Text ->
  -- | 'shardToSplit'
  Prelude.Text ->
  -- | 'newStartingHashKey''
  Prelude.Text ->
  SplitShard
newSplitShard
  pStreamName_
  pShardToSplit_
  pNewStartingHashKey_ =
    SplitShard'
      { streamName = pStreamName_,
        shardToSplit = pShardToSplit_,
        newStartingHashKey' = pNewStartingHashKey_
      }

-- | The name of the stream for the shard split.
splitShard_streamName :: Lens.Lens' SplitShard Prelude.Text
splitShard_streamName = Lens.lens (\SplitShard' {streamName} -> streamName) (\s@SplitShard' {} a -> s {streamName = a} :: SplitShard)

-- | The shard ID of the shard to split.
splitShard_shardToSplit :: Lens.Lens' SplitShard Prelude.Text
splitShard_shardToSplit = Lens.lens (\SplitShard' {shardToSplit} -> shardToSplit) (\s@SplitShard' {} a -> s {shardToSplit = a} :: SplitShard)

-- | A hash key value for the starting hash key of one of the child shards
-- created by the split. The hash key range for a given shard constitutes a
-- set of ordered contiguous positive integers. The value for
-- @NewStartingHashKey@ must be in the range of hash keys being mapped into
-- the shard. The @NewStartingHashKey@ hash key value and all higher hash
-- key values in hash key range are distributed to one of the child shards.
-- All the lower hash key values in the range are distributed to the other
-- child shard.
splitShard_newStartingHashKey :: Lens.Lens' SplitShard Prelude.Text
splitShard_newStartingHashKey = Lens.lens (\SplitShard' {newStartingHashKey'} -> newStartingHashKey') (\s@SplitShard' {} a -> s {newStartingHashKey' = a} :: SplitShard)

instance Prelude.AWSRequest SplitShard where
  type Rs SplitShard = SplitShardResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull SplitShardResponse'

instance Prelude.Hashable SplitShard

instance Prelude.NFData SplitShard

instance Prelude.ToHeaders SplitShard where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Kinesis_20131202.SplitShard" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SplitShard where
  toJSON SplitShard' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("StreamName" Prelude..= streamName),
            Prelude.Just
              ("ShardToSplit" Prelude..= shardToSplit),
            Prelude.Just
              ( "NewStartingHashKey"
                  Prelude..= newStartingHashKey'
              )
          ]
      )

instance Prelude.ToPath SplitShard where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SplitShard where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSplitShardResponse' smart constructor.
data SplitShardResponse = SplitShardResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SplitShardResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSplitShardResponse ::
  SplitShardResponse
newSplitShardResponse = SplitShardResponse'

instance Prelude.NFData SplitShardResponse
