{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dsStreamArn,
    dsExclusiveStartShardId,
    dsLimit,

    -- * Destructuring the response
    DescribeStreamResponse (..),
    mkDescribeStreamResponse,

    -- ** Response lenses
    dsrrsStreamDescription,
    dsrrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDBStreams.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeStream@ operation.
--
-- /See:/ 'mkDescribeStream' smart constructor.
data DescribeStream = DescribeStream'
  { -- | The Amazon Resource Name (ARN) for the stream.
    streamArn :: Types.StreamArn,
    -- | The shard ID of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedShardId@ in the previous operation.
    exclusiveStartShardId :: Core.Maybe Types.ExclusiveStartShardId,
    -- | The maximum number of shard objects to return. The upper limit is 100.
    limit :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStream' value with any optional fields omitted.
mkDescribeStream ::
  -- | 'streamArn'
  Types.StreamArn ->
  DescribeStream
mkDescribeStream streamArn =
  DescribeStream'
    { streamArn,
      exclusiveStartShardId = Core.Nothing,
      limit = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) for the stream.
--
-- /Note:/ Consider using 'streamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStreamArn :: Lens.Lens' DescribeStream Types.StreamArn
dsStreamArn = Lens.field @"streamArn"
{-# DEPRECATED dsStreamArn "Use generic-lens or generic-optics with 'streamArn' instead." #-}

-- | The shard ID of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedShardId@ in the previous operation.
--
-- /Note:/ Consider using 'exclusiveStartShardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsExclusiveStartShardId :: Lens.Lens' DescribeStream (Core.Maybe Types.ExclusiveStartShardId)
dsExclusiveStartShardId = Lens.field @"exclusiveStartShardId"
{-# DEPRECATED dsExclusiveStartShardId "Use generic-lens or generic-optics with 'exclusiveStartShardId' instead." #-}

-- | The maximum number of shard objects to return. The upper limit is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLimit :: Lens.Lens' DescribeStream (Core.Maybe Core.Natural)
dsLimit = Lens.field @"limit"
{-# DEPRECATED dsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Core.FromJSON DescribeStream where
  toJSON DescribeStream {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamArn" Core..= streamArn),
            ("ExclusiveStartShardId" Core..=) Core.<$> exclusiveStartShardId,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.AWSRequest DescribeStream where
  type Rs DescribeStream = DescribeStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DynamoDBStreams_20120810.DescribeStream")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStreamResponse'
            Core.<$> (x Core..:? "StreamDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @DescribeStream@ operation.
--
-- /See:/ 'mkDescribeStreamResponse' smart constructor.
data DescribeStreamResponse = DescribeStreamResponse'
  { -- | A complete description of the stream, including its creation date and time, the DynamoDB table associated with the stream, the shard IDs within the stream, and the beginning and ending sequence numbers of stream records within the shards.
    streamDescription :: Core.Maybe Types.StreamDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeStreamResponse' value with any optional fields omitted.
mkDescribeStreamResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeStreamResponse
mkDescribeStreamResponse responseStatus =
  DescribeStreamResponse'
    { streamDescription = Core.Nothing,
      responseStatus
    }

-- | A complete description of the stream, including its creation date and time, the DynamoDB table associated with the stream, the shard IDs within the stream, and the beginning and ending sequence numbers of stream records within the shards.
--
-- /Note:/ Consider using 'streamDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsStreamDescription :: Lens.Lens' DescribeStreamResponse (Core.Maybe Types.StreamDescription)
dsrrsStreamDescription = Lens.field @"streamDescription"
{-# DEPRECATED dsrrsStreamDescription "Use generic-lens or generic-optics with 'streamDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeStreamResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
