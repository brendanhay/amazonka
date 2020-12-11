{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.DescribeStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a stream, including the current status of the stream, its Amazon Resource Name (ARN), the composition of its shards, and its corresponding DynamoDB table.
--
-- Each shard in the stream has a @SequenceNumberRange@ associated with it. If the @SequenceNumberRange@ has a @StartingSequenceNumber@ but no @EndingSequenceNumber@ , then the shard is still open (able to receive more stream records). If both @StartingSequenceNumber@ and @EndingSequenceNumber@ are present, then that shard is closed and can no longer receive more data.
module Network.AWS.DynamoDBStreams.DescribeStream
  ( -- * Creating a request
    DescribeStream (..),
    mkDescribeStream,

    -- ** Request lenses
    dsExclusiveStartShardId,
    dsLimit,
    dsStreamARN,

    -- * Destructuring the response
    DescribeStreamResponse (..),
    mkDescribeStreamResponse,

    -- ** Response lenses
    dsrsStreamDescription,
    dsrsResponseStatus,
  )
where

import Network.AWS.DynamoDBStreams.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DescribeStream@ operation.
--
-- /See:/ 'mkDescribeStream' smart constructor.
data DescribeStream = DescribeStream'
  { exclusiveStartShardId ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    streamARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStream' with the minimum fields required to make a request.
--
-- * 'exclusiveStartShardId' - The shard ID of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedShardId@ in the previous operation.
-- * 'limit' - The maximum number of shard objects to return. The upper limit is 100.
-- * 'streamARN' - The Amazon Resource Name (ARN) for the stream.
mkDescribeStream ::
  -- | 'streamARN'
  Lude.Text ->
  DescribeStream
mkDescribeStream pStreamARN_ =
  DescribeStream'
    { exclusiveStartShardId = Lude.Nothing,
      limit = Lude.Nothing,
      streamARN = pStreamARN_
    }

-- | The shard ID of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedShardId@ in the previous operation.
--
-- /Note:/ Consider using 'exclusiveStartShardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsExclusiveStartShardId :: Lens.Lens' DescribeStream (Lude.Maybe Lude.Text)
dsExclusiveStartShardId = Lens.lens (exclusiveStartShardId :: DescribeStream -> Lude.Maybe Lude.Text) (\s a -> s {exclusiveStartShardId = a} :: DescribeStream)
{-# DEPRECATED dsExclusiveStartShardId "Use generic-lens or generic-optics with 'exclusiveStartShardId' instead." #-}

-- | The maximum number of shard objects to return. The upper limit is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLimit :: Lens.Lens' DescribeStream (Lude.Maybe Lude.Natural)
dsLimit = Lens.lens (limit :: DescribeStream -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeStream)
{-# DEPRECATED dsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The Amazon Resource Name (ARN) for the stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStreamARN :: Lens.Lens' DescribeStream Lude.Text
dsStreamARN = Lens.lens (streamARN :: DescribeStream -> Lude.Text) (\s a -> s {streamARN = a} :: DescribeStream)
{-# DEPRECATED dsStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

instance Lude.AWSRequest DescribeStream where
  type Rs DescribeStream = DescribeStreamResponse
  request = Req.postJSON dynamoDBStreamsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeStreamResponse'
            Lude.<$> (x Lude..?> "StreamDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDBStreams_20120810.DescribeStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeStream where
  toJSON DescribeStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExclusiveStartShardId" Lude..=) Lude.<$> exclusiveStartShardId,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("StreamArn" Lude..= streamARN)
          ]
      )

instance Lude.ToPath DescribeStream where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStream where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @DescribeStream@ operation.
--
-- /See:/ 'mkDescribeStreamResponse' smart constructor.
data DescribeStreamResponse = DescribeStreamResponse'
  { streamDescription ::
      Lude.Maybe StreamDescription,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStreamResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'streamDescription' - A complete description of the stream, including its creation date and time, the DynamoDB table associated with the stream, the shard IDs within the stream, and the beginning and ending sequence numbers of stream records within the shards.
mkDescribeStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStreamResponse
mkDescribeStreamResponse pResponseStatus_ =
  DescribeStreamResponse'
    { streamDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A complete description of the stream, including its creation date and time, the DynamoDB table associated with the stream, the shard IDs within the stream, and the beginning and ending sequence numbers of stream records within the shards.
--
-- /Note:/ Consider using 'streamDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsStreamDescription :: Lens.Lens' DescribeStreamResponse (Lude.Maybe StreamDescription)
dsrsStreamDescription = Lens.lens (streamDescription :: DescribeStreamResponse -> Lude.Maybe StreamDescription) (\s a -> s {streamDescription = a} :: DescribeStreamResponse)
{-# DEPRECATED dsrsStreamDescription "Use generic-lens or generic-optics with 'streamDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DescribeStreamResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DescribeStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStreamResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
