{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.GetShardIterator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an Amazon Kinesis shard iterator. A shard iterator expires 5 minutes after it is returned to the requester.
--
-- A shard iterator specifies the shard position from which to start reading data records sequentially. The position is specified using the sequence number of a data record in a shard. A sequence number is the identifier associated with every record ingested in the stream, and is assigned when a record is put into the stream. Each stream has one or more shards.
-- You must specify the shard iterator type. For example, you can set the @ShardIteratorType@ parameter to read exactly from the position denoted by a specific sequence number by using the @AT_SEQUENCE_NUMBER@ shard iterator type. Alternatively, the parameter can read right after the sequence number by using the @AFTER_SEQUENCE_NUMBER@ shard iterator type, using sequence numbers returned by earlier calls to 'PutRecord' , 'PutRecords' , 'GetRecords' , or 'DescribeStream' . In the request, you can specify the shard iterator type @AT_TIMESTAMP@ to read records from an arbitrary point in time, @TRIM_HORIZON@ to cause @ShardIterator@ to point to the last untrimmed record in the shard in the system (the oldest data record in the shard), or @LATEST@ so that you always read the most recent data in the shard.
-- When you read repeatedly from a stream, use a 'GetShardIterator' request to get the first shard iterator for use in your first 'GetRecords' request and for subsequent reads use the shard iterator returned by the 'GetRecords' request in @NextShardIterator@ . A new shard iterator is returned by every 'GetRecords' request in @NextShardIterator@ , which you use in the @ShardIterator@ parameter of the next 'GetRecords' request.
-- If a 'GetShardIterator' request is made too often, you receive a @ProvisionedThroughputExceededException@ . For more information about throughput limits, see 'GetRecords' , and <https://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Streams Limits> in the /Amazon Kinesis Data Streams Developer Guide/ .
-- If the shard is closed, 'GetShardIterator' returns a valid iterator for the last sequence number of the shard. A shard can be closed as a result of using 'SplitShard' or 'MergeShards' .
-- 'GetShardIterator' has a limit of five transactions per second per account per open shard.
module Network.AWS.Kinesis.GetShardIterator
  ( -- * Creating a request
    GetShardIterator (..),
    mkGetShardIterator,

    -- ** Request lenses
    gsiStreamName,
    gsiShardId,
    gsiShardIteratorType,
    gsiStartingSequenceNumber,
    gsiTimestamp,

    -- * Destructuring the response
    GetShardIteratorResponse (..),
    mkGetShardIteratorResponse,

    -- ** Response lenses
    gsirrsShardIterator,
    gsirrsResponseStatus,
  )
where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @GetShardIterator@ .
--
-- /See:/ 'mkGetShardIterator' smart constructor.
data GetShardIterator = GetShardIterator'
  { -- | The name of the Amazon Kinesis data stream.
    streamName :: Types.StreamName,
    -- | The shard ID of the Kinesis Data Streams shard to get the iterator for.
    shardId :: Types.ShardId,
    -- | Determines how the shard iterator is used to start reading data records from the shard.
    --
    -- The following are the valid Amazon Kinesis shard iterator types:
    --
    --     * AT_SEQUENCE_NUMBER - Start reading from the position denoted by a specific sequence number, provided in the value @StartingSequenceNumber@ .
    --
    --
    --     * AFTER_SEQUENCE_NUMBER - Start reading right after the position denoted by a specific sequence number, provided in the value @StartingSequenceNumber@ .
    --
    --
    --     * AT_TIMESTAMP - Start reading from the position denoted by a specific time stamp, provided in the value @Timestamp@ .
    --
    --
    --     * TRIM_HORIZON - Start reading at the last untrimmed record in the shard in the system, which is the oldest data record in the shard.
    --
    --
    --     * LATEST - Start reading just after the most recent record in the shard, so that you always read the most recent data in the shard.
    shardIteratorType :: Types.ShardIteratorType,
    -- | The sequence number of the data record in the shard from which to start reading. Used with shard iterator type AT_SEQUENCE_NUMBER and AFTER_SEQUENCE_NUMBER.
    startingSequenceNumber :: Core.Maybe Types.SequenceNumber,
    -- | The time stamp of the data record from which to start reading. Used with shard iterator type AT_TIMESTAMP. A time stamp is the Unix epoch date with precision in milliseconds. For example, @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@ . If a record with this exact time stamp does not exist, the iterator returned is for the next (later) record. If the time stamp is older than the current trim horizon, the iterator returned is for the oldest untrimmed data record (TRIM_HORIZON).
    timestamp :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetShardIterator' value with any optional fields omitted.
mkGetShardIterator ::
  -- | 'streamName'
  Types.StreamName ->
  -- | 'shardId'
  Types.ShardId ->
  -- | 'shardIteratorType'
  Types.ShardIteratorType ->
  GetShardIterator
mkGetShardIterator streamName shardId shardIteratorType =
  GetShardIterator'
    { streamName,
      shardId,
      shardIteratorType,
      startingSequenceNumber = Core.Nothing,
      timestamp = Core.Nothing
    }

-- | The name of the Amazon Kinesis data stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiStreamName :: Lens.Lens' GetShardIterator Types.StreamName
gsiStreamName = Lens.field @"streamName"
{-# DEPRECATED gsiStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The shard ID of the Kinesis Data Streams shard to get the iterator for.
--
-- /Note:/ Consider using 'shardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiShardId :: Lens.Lens' GetShardIterator Types.ShardId
gsiShardId = Lens.field @"shardId"
{-# DEPRECATED gsiShardId "Use generic-lens or generic-optics with 'shardId' instead." #-}

-- | Determines how the shard iterator is used to start reading data records from the shard.
--
-- The following are the valid Amazon Kinesis shard iterator types:
--
--     * AT_SEQUENCE_NUMBER - Start reading from the position denoted by a specific sequence number, provided in the value @StartingSequenceNumber@ .
--
--
--     * AFTER_SEQUENCE_NUMBER - Start reading right after the position denoted by a specific sequence number, provided in the value @StartingSequenceNumber@ .
--
--
--     * AT_TIMESTAMP - Start reading from the position denoted by a specific time stamp, provided in the value @Timestamp@ .
--
--
--     * TRIM_HORIZON - Start reading at the last untrimmed record in the shard in the system, which is the oldest data record in the shard.
--
--
--     * LATEST - Start reading just after the most recent record in the shard, so that you always read the most recent data in the shard.
--
--
--
-- /Note:/ Consider using 'shardIteratorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiShardIteratorType :: Lens.Lens' GetShardIterator Types.ShardIteratorType
gsiShardIteratorType = Lens.field @"shardIteratorType"
{-# DEPRECATED gsiShardIteratorType "Use generic-lens or generic-optics with 'shardIteratorType' instead." #-}

-- | The sequence number of the data record in the shard from which to start reading. Used with shard iterator type AT_SEQUENCE_NUMBER and AFTER_SEQUENCE_NUMBER.
--
-- /Note:/ Consider using 'startingSequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiStartingSequenceNumber :: Lens.Lens' GetShardIterator (Core.Maybe Types.SequenceNumber)
gsiStartingSequenceNumber = Lens.field @"startingSequenceNumber"
{-# DEPRECATED gsiStartingSequenceNumber "Use generic-lens or generic-optics with 'startingSequenceNumber' instead." #-}

-- | The time stamp of the data record from which to start reading. Used with shard iterator type AT_TIMESTAMP. A time stamp is the Unix epoch date with precision in milliseconds. For example, @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@ . If a record with this exact time stamp does not exist, the iterator returned is for the next (later) record. If the time stamp is older than the current trim horizon, the iterator returned is for the oldest untrimmed data record (TRIM_HORIZON).
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiTimestamp :: Lens.Lens' GetShardIterator (Core.Maybe Core.NominalDiffTime)
gsiTimestamp = Lens.field @"timestamp"
{-# DEPRECATED gsiTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Core.FromJSON GetShardIterator where
  toJSON GetShardIterator {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamName" Core..= streamName),
            Core.Just ("ShardId" Core..= shardId),
            Core.Just ("ShardIteratorType" Core..= shardIteratorType),
            ("StartingSequenceNumber" Core..=) Core.<$> startingSequenceNumber,
            ("Timestamp" Core..=) Core.<$> timestamp
          ]
      )

instance Core.AWSRequest GetShardIterator where
  type Rs GetShardIterator = GetShardIteratorResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Kinesis_20131202.GetShardIterator")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetShardIteratorResponse'
            Core.<$> (x Core..:? "ShardIterator")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output for @GetShardIterator@ .
--
-- /See:/ 'mkGetShardIteratorResponse' smart constructor.
data GetShardIteratorResponse = GetShardIteratorResponse'
  { -- | The position in the shard from which to start reading data records sequentially. A shard iterator specifies this position using the sequence number of a data record in a shard.
    shardIterator :: Core.Maybe Types.ShardIterator,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetShardIteratorResponse' value with any optional fields omitted.
mkGetShardIteratorResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetShardIteratorResponse
mkGetShardIteratorResponse responseStatus =
  GetShardIteratorResponse'
    { shardIterator = Core.Nothing,
      responseStatus
    }

-- | The position in the shard from which to start reading data records sequentially. A shard iterator specifies this position using the sequence number of a data record in a shard.
--
-- /Note:/ Consider using 'shardIterator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsirrsShardIterator :: Lens.Lens' GetShardIteratorResponse (Core.Maybe Types.ShardIterator)
gsirrsShardIterator = Lens.field @"shardIterator"
{-# DEPRECATED gsirrsShardIterator "Use generic-lens or generic-optics with 'shardIterator' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsirrsResponseStatus :: Lens.Lens' GetShardIteratorResponse Core.Int
gsirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
