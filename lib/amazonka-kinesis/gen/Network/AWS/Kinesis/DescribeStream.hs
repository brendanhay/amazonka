{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeStream (..)
    , mkDescribeStream
    -- ** Request lenses
    , dStreamName
    , dExclusiveStartShardId
    , dLimit

    -- * Destructuring the response
    , DescribeStreamResponse (..)
    , mkDescribeStreamResponse
    -- ** Response lenses
    , dsrrsStreamDescription
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @DescribeStream@ .
--
-- /See:/ 'mkDescribeStream' smart constructor.
data DescribeStream = DescribeStream'
  { streamName :: Types.StreamName
    -- ^ The name of the stream to describe.
  , exclusiveStartShardId :: Core.Maybe Types.ExclusiveStartShardId
    -- ^ The shard ID of the shard to start with.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of shards to return in a single call. The default value is 100. If you specify a value greater than 100, at most 100 shards are returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStream' value with any optional fields omitted.
mkDescribeStream
    :: Types.StreamName -- ^ 'streamName'
    -> DescribeStream
mkDescribeStream streamName
  = DescribeStream'{streamName, exclusiveStartShardId = Core.Nothing,
                    limit = Core.Nothing}

-- | The name of the stream to describe.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStreamName :: Lens.Lens' DescribeStream Types.StreamName
dStreamName = Lens.field @"streamName"
{-# INLINEABLE dStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

-- | The shard ID of the shard to start with.
--
-- /Note:/ Consider using 'exclusiveStartShardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExclusiveStartShardId :: Lens.Lens' DescribeStream (Core.Maybe Types.ExclusiveStartShardId)
dExclusiveStartShardId = Lens.field @"exclusiveStartShardId"
{-# INLINEABLE dExclusiveStartShardId #-}
{-# DEPRECATED exclusiveStartShardId "Use generic-lens or generic-optics with 'exclusiveStartShardId' instead"  #-}

-- | The maximum number of shards to return in a single call. The default value is 100. If you specify a value greater than 100, at most 100 shards are returned.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLimit :: Lens.Lens' DescribeStream (Core.Maybe Core.Natural)
dLimit = Lens.field @"limit"
{-# INLINEABLE dLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

instance Core.ToQuery DescribeStream where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeStream where
        toHeaders DescribeStream{..}
          = Core.pure ("X-Amz-Target", "Kinesis_20131202.DescribeStream")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeStream where
        toJSON DescribeStream{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StreamName" Core..= streamName),
                  ("ExclusiveStartShardId" Core..=) Core.<$> exclusiveStartShardId,
                  ("Limit" Core..=) Core.<$> limit])

instance Core.AWSRequest DescribeStream where
        type Rs DescribeStream = DescribeStreamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeStreamResponse' Core.<$>
                   (x Core..: "StreamDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeStream where
        page rq rs
          | Pager.stop
              (rs Lens.^.
                 Lens.field @"streamDescription" Core.. Lens.field @"hasMoreShards")
            = Core.Nothing
          | Core.isNothing
              (rs Lens.^?
                 Lens.field @"streamDescription" Core..
                   Lens.field @"shards" Core.. Lens._last Core..
                     Lens.field @"shardId")
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"exclusiveStartShardId" Lens..~
                   rs Lens.^?
                     Lens.field @"streamDescription" Core..
                       Lens.field @"shards" Core.. Lens._last Core..
                         Lens.field @"shardId")

-- | Represents the output for @DescribeStream@ .
--
-- /See:/ 'mkDescribeStreamResponse' smart constructor.
data DescribeStreamResponse = DescribeStreamResponse'
  { streamDescription :: Types.StreamDescription
    -- ^ The current status of the stream, the stream Amazon Resource Name (ARN), an array of shard objects that comprise the stream, and whether there are more shards available.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeStreamResponse' value with any optional fields omitted.
mkDescribeStreamResponse
    :: Types.StreamDescription -- ^ 'streamDescription'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeStreamResponse
mkDescribeStreamResponse streamDescription responseStatus
  = DescribeStreamResponse'{streamDescription, responseStatus}

-- | The current status of the stream, the stream Amazon Resource Name (ARN), an array of shard objects that comprise the stream, and whether there are more shards available.
--
-- /Note:/ Consider using 'streamDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsStreamDescription :: Lens.Lens' DescribeStreamResponse Types.StreamDescription
dsrrsStreamDescription = Lens.field @"streamDescription"
{-# INLINEABLE dsrrsStreamDescription #-}
{-# DEPRECATED streamDescription "Use generic-lens or generic-optics with 'streamDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeStreamResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
