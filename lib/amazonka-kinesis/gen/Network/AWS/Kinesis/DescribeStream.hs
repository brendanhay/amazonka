{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.DescribeStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Kinesis data stream.
--
-- The information returned includes the stream name, Amazon Resource Name (ARN), creation time, enhanced metric configuration, and shard map. The shard map is an array of shard objects. For each shard object, there is the hash key and sequence number ranges that the shard spans, and the IDs of any earlier shards that played in a role in creating the shard. Every record ingested in the stream is identified by a sequence number, which is assigned when the record is put into the stream.
-- You can limit the number of shards returned by each call. For more information, see <https://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-retrieve-shards.html Retrieving Shards from a Stream> in the /Amazon Kinesis Data Streams Developer Guide/ .
-- There are no guarantees about the chronological order shards returned. To process shards in chronological order, use the ID of the parent shard to track the lineage to the oldest shard.
-- This operation has a limit of 10 transactions per second per account.
--
-- This operation returns paginated results.
module Network.AWS.Kinesis.DescribeStream
  ( -- * Creating a request
    DescribeStream (..),
    mkDescribeStream,

    -- ** Request lenses
    dsExclusiveStartShardId,
    dsLimit,
    dsStreamName,

    -- * Destructuring the response
    DescribeStreamResponse (..),
    mkDescribeStreamResponse,

    -- ** Response lenses
    dsrsStreamDescription,
    dsrsResponseStatus,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for @DescribeStream@ .
--
-- /See:/ 'mkDescribeStream' smart constructor.
data DescribeStream = DescribeStream'
  { -- | The shard ID of the shard to start with.
    exclusiveStartShardId :: Lude.Maybe Lude.Text,
    -- | The maximum number of shards to return in a single call. The default value is 100. If you specify a value greater than 100, at most 100 shards are returned.
    limit :: Lude.Maybe Lude.Natural,
    -- | The name of the stream to describe.
    streamName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStream' with the minimum fields required to make a request.
--
-- * 'exclusiveStartShardId' - The shard ID of the shard to start with.
-- * 'limit' - The maximum number of shards to return in a single call. The default value is 100. If you specify a value greater than 100, at most 100 shards are returned.
-- * 'streamName' - The name of the stream to describe.
mkDescribeStream ::
  -- | 'streamName'
  Lude.Text ->
  DescribeStream
mkDescribeStream pStreamName_ =
  DescribeStream'
    { exclusiveStartShardId = Lude.Nothing,
      limit = Lude.Nothing,
      streamName = pStreamName_
    }

-- | The shard ID of the shard to start with.
--
-- /Note:/ Consider using 'exclusiveStartShardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsExclusiveStartShardId :: Lens.Lens' DescribeStream (Lude.Maybe Lude.Text)
dsExclusiveStartShardId = Lens.lens (exclusiveStartShardId :: DescribeStream -> Lude.Maybe Lude.Text) (\s a -> s {exclusiveStartShardId = a} :: DescribeStream)
{-# DEPRECATED dsExclusiveStartShardId "Use generic-lens or generic-optics with 'exclusiveStartShardId' instead." #-}

-- | The maximum number of shards to return in a single call. The default value is 100. If you specify a value greater than 100, at most 100 shards are returned.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLimit :: Lens.Lens' DescribeStream (Lude.Maybe Lude.Natural)
dsLimit = Lens.lens (limit :: DescribeStream -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeStream)
{-# DEPRECATED dsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The name of the stream to describe.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStreamName :: Lens.Lens' DescribeStream Lude.Text
dsStreamName = Lens.lens (streamName :: DescribeStream -> Lude.Text) (\s a -> s {streamName = a} :: DescribeStream)
{-# DEPRECATED dsStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Page.AWSPager DescribeStream where
  page rq rs
    | Page.stop
        (rs Lens.^. dsrsStreamDescription Lude.. sdHasMoreShards) =
      Lude.Nothing
    | Lude.isNothing
        ( rs
            Lens.^? dsrsStreamDescription Lude.. sdShards Lude.. Lens._last
              Lude.. sShardId
        ) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsExclusiveStartShardId
          Lens..~ rs
          Lens.^? dsrsStreamDescription Lude.. sdShards Lude.. Lens._last
            Lude.. sShardId

instance Lude.AWSRequest DescribeStream where
  type Rs DescribeStream = DescribeStreamResponse
  request = Req.postJSON kinesisService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeStreamResponse'
            Lude.<$> (x Lude..:> "StreamDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.DescribeStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeStream where
  toJSON DescribeStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExclusiveStartShardId" Lude..=) Lude.<$> exclusiveStartShardId,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("StreamName" Lude..= streamName)
          ]
      )

instance Lude.ToPath DescribeStream where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStream where
  toQuery = Lude.const Lude.mempty

-- | Represents the output for @DescribeStream@ .
--
-- /See:/ 'mkDescribeStreamResponse' smart constructor.
data DescribeStreamResponse = DescribeStreamResponse'
  { -- | The current status of the stream, the stream Amazon Resource Name (ARN), an array of shard objects that comprise the stream, and whether there are more shards available.
    streamDescription :: StreamDescription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStreamResponse' with the minimum fields required to make a request.
--
-- * 'streamDescription' - The current status of the stream, the stream Amazon Resource Name (ARN), an array of shard objects that comprise the stream, and whether there are more shards available.
-- * 'responseStatus' - The response status code.
mkDescribeStreamResponse ::
  -- | 'streamDescription'
  StreamDescription ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStreamResponse
mkDescribeStreamResponse pStreamDescription_ pResponseStatus_ =
  DescribeStreamResponse'
    { streamDescription = pStreamDescription_,
      responseStatus = pResponseStatus_
    }

-- | The current status of the stream, the stream Amazon Resource Name (ARN), an array of shard objects that comprise the stream, and whether there are more shards available.
--
-- /Note:/ Consider using 'streamDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsStreamDescription :: Lens.Lens' DescribeStreamResponse StreamDescription
dsrsStreamDescription = Lens.lens (streamDescription :: DescribeStreamResponse -> StreamDescription) (\s a -> s {streamDescription = a} :: DescribeStreamResponse)
{-# DEPRECATED dsrsStreamDescription "Use generic-lens or generic-optics with 'streamDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DescribeStreamResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DescribeStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStreamResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
