{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.SplitShard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Splits a shard into two new shards in the Kinesis data stream, to increase the stream's capacity to ingest and transport data. @SplitShard@ is called when there is a need to increase the overall capacity of a stream because of an expected increase in the volume of data records being ingested.
--
-- You can also use @SplitShard@ when a shard appears to be approaching its maximum utilization; for example, the producers sending data into the specific shard are suddenly sending more than previously anticipated. You can also call @SplitShard@ to increase stream capacity, so that more Kinesis Data Streams applications can simultaneously read data from the stream for real-time processing.
-- You must specify the shard to be split and the new hash key, which is the position in the shard where the shard gets split in two. In many cases, the new hash key might be the average of the beginning and ending hash key, but it can be any hash key value in the range being mapped into the shard. For more information, see <https://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-resharding-split.html Split a Shard> in the /Amazon Kinesis Data Streams Developer Guide/ .
-- You can use 'DescribeStream' to determine the shard ID and hash key values for the @ShardToSplit@ and @NewStartingHashKey@ parameters that are specified in the @SplitShard@ request.
-- @SplitShard@ is an asynchronous operation. Upon receiving a @SplitShard@ request, Kinesis Data Streams immediately returns a response and sets the stream status to @UPDATING@ . After the operation is completed, Kinesis Data Streams sets the stream status to @ACTIVE@ . Read and write operations continue to work while the stream is in the @UPDATING@ state.
-- You can use @DescribeStream@ to check the status of the stream, which is returned in @StreamStatus@ . If the stream is in the @ACTIVE@ state, you can call @SplitShard@ . If a stream is in @CREATING@ or @UPDATING@ or @DELETING@ states, @DescribeStream@ returns a @ResourceInUseException@ .
-- If the specified stream does not exist, @DescribeStream@ returns a @ResourceNotFoundException@ . If you try to create more shards than are authorized for your account, you receive a @LimitExceededException@ .
-- For the default shard limit for an AWS account, see <https://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Kinesis Data Streams Limits> in the /Amazon Kinesis Data Streams Developer Guide/ . To increase this limit, <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html contact AWS Support> .
-- If you try to operate on too many streams simultaneously using 'CreateStream' , 'DeleteStream' , 'MergeShards' , and/or 'SplitShard' , you receive a @LimitExceededException@ .
-- @SplitShard@ has a limit of five transactions per second per account.
module Network.AWS.Kinesis.SplitShard
  ( -- * Creating a request
    SplitShard (..),
    mkSplitShard,

    -- ** Request lenses
    ssStreamName,
    ssShardToSplit,
    ssNewStartingHashKey,

    -- * Destructuring the response
    SplitShardResponse (..),
    mkSplitShardResponse,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for @SplitShard@ .
--
-- /See:/ 'mkSplitShard' smart constructor.
data SplitShard = SplitShard'
  { streamName :: Lude.Text,
    shardToSplit :: Lude.Text,
    newStartingHashKey :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SplitShard' with the minimum fields required to make a request.
--
-- * 'newStartingHashKey' - A hash key value for the starting hash key of one of the child shards created by the split. The hash key range for a given shard constitutes a set of ordered contiguous positive integers. The value for @NewStartingHashKey@ must be in the range of hash keys being mapped into the shard. The @NewStartingHashKey@ hash key value and all higher hash key values in hash key range are distributed to one of the child shards. All the lower hash key values in the range are distributed to the other child shard.
-- * 'shardToSplit' - The shard ID of the shard to split.
-- * 'streamName' - The name of the stream for the shard split.
mkSplitShard ::
  -- | 'streamName'
  Lude.Text ->
  -- | 'shardToSplit'
  Lude.Text ->
  -- | 'newStartingHashKey'
  Lude.Text ->
  SplitShard
mkSplitShard pStreamName_ pShardToSplit_ pNewStartingHashKey_ =
  SplitShard'
    { streamName = pStreamName_,
      shardToSplit = pShardToSplit_,
      newStartingHashKey = pNewStartingHashKey_
    }

-- | The name of the stream for the shard split.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStreamName :: Lens.Lens' SplitShard Lude.Text
ssStreamName = Lens.lens (streamName :: SplitShard -> Lude.Text) (\s a -> s {streamName = a} :: SplitShard)
{-# DEPRECATED ssStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The shard ID of the shard to split.
--
-- /Note:/ Consider using 'shardToSplit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssShardToSplit :: Lens.Lens' SplitShard Lude.Text
ssShardToSplit = Lens.lens (shardToSplit :: SplitShard -> Lude.Text) (\s a -> s {shardToSplit = a} :: SplitShard)
{-# DEPRECATED ssShardToSplit "Use generic-lens or generic-optics with 'shardToSplit' instead." #-}

-- | A hash key value for the starting hash key of one of the child shards created by the split. The hash key range for a given shard constitutes a set of ordered contiguous positive integers. The value for @NewStartingHashKey@ must be in the range of hash keys being mapped into the shard. The @NewStartingHashKey@ hash key value and all higher hash key values in hash key range are distributed to one of the child shards. All the lower hash key values in the range are distributed to the other child shard.
--
-- /Note:/ Consider using 'newStartingHashKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssNewStartingHashKey :: Lens.Lens' SplitShard Lude.Text
ssNewStartingHashKey = Lens.lens (newStartingHashKey :: SplitShard -> Lude.Text) (\s a -> s {newStartingHashKey = a} :: SplitShard)
{-# DEPRECATED ssNewStartingHashKey "Use generic-lens or generic-optics with 'newStartingHashKey' instead." #-}

instance Lude.AWSRequest SplitShard where
  type Rs SplitShard = SplitShardResponse
  request = Req.postJSON kinesisService
  response = Res.receiveNull SplitShardResponse'

instance Lude.ToHeaders SplitShard where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.SplitShard" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SplitShard where
  toJSON SplitShard' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StreamName" Lude..= streamName),
            Lude.Just ("ShardToSplit" Lude..= shardToSplit),
            Lude.Just ("NewStartingHashKey" Lude..= newStartingHashKey)
          ]
      )

instance Lude.ToPath SplitShard where
  toPath = Lude.const "/"

instance Lude.ToQuery SplitShard where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSplitShardResponse' smart constructor.
data SplitShardResponse = SplitShardResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SplitShardResponse' with the minimum fields required to make a request.
mkSplitShardResponse ::
  SplitShardResponse
mkSplitShardResponse = SplitShardResponse'
