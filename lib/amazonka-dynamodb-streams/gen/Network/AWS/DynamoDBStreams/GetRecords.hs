{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.GetRecords
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the stream records from a given shard.
--
-- Specify a shard iterator using the @ShardIterator@ parameter. The shard iterator specifies the position in the shard from which you want to start reading stream records sequentially. If there are no stream records available in the portion of the shard that the iterator points to, @GetRecords@ returns an empty list. Note that it might take multiple calls to get to a portion of the shard that contains stream records.
module Network.AWS.DynamoDBStreams.GetRecords
  ( -- * Creating a request
    GetRecords (..),
    mkGetRecords,

    -- ** Request lenses
    grShardIterator,
    grLimit,

    -- * Destructuring the response
    GetRecordsResponse (..),
    mkGetRecordsResponse,

    -- ** Response lenses
    grrrsNextShardIterator,
    grrrsRecords,
    grrrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDBStreams.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetRecords@ operation.
--
-- /See:/ 'mkGetRecords' smart constructor.
data GetRecords = GetRecords'
  { -- | A shard iterator that was retrieved from a previous GetShardIterator operation. This iterator can be used to access the stream records in this shard.
    shardIterator :: Types.ShardIterator,
    -- | The maximum number of records to return from the shard. The upper limit is 1000.
    limit :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRecords' value with any optional fields omitted.
mkGetRecords ::
  -- | 'shardIterator'
  Types.ShardIterator ->
  GetRecords
mkGetRecords shardIterator =
  GetRecords' {shardIterator, limit = Core.Nothing}

-- | A shard iterator that was retrieved from a previous GetShardIterator operation. This iterator can be used to access the stream records in this shard.
--
-- /Note:/ Consider using 'shardIterator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grShardIterator :: Lens.Lens' GetRecords Types.ShardIterator
grShardIterator = Lens.field @"shardIterator"
{-# DEPRECATED grShardIterator "Use generic-lens or generic-optics with 'shardIterator' instead." #-}

-- | The maximum number of records to return from the shard. The upper limit is 1000.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grLimit :: Lens.Lens' GetRecords (Core.Maybe Core.Natural)
grLimit = Lens.field @"limit"
{-# DEPRECATED grLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Core.FromJSON GetRecords where
  toJSON GetRecords {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ShardIterator" Core..= shardIterator),
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.AWSRequest GetRecords where
  type Rs GetRecords = GetRecordsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDBStreams_20120810.GetRecords")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecordsResponse'
            Core.<$> (x Core..:? "NextShardIterator")
            Core.<*> (x Core..:? "Records")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @GetRecords@ operation.
--
-- /See:/ 'mkGetRecordsResponse' smart constructor.
data GetRecordsResponse = GetRecordsResponse'
  { -- | The next position in the shard from which to start sequentially reading stream records. If set to @null@ , the shard has been closed and the requested iterator will not return any more data.
    nextShardIterator :: Core.Maybe Types.ShardIterator,
    -- | The stream records from the shard, which were retrieved using the shard iterator.
    records :: Core.Maybe [Types.Record],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetRecordsResponse' value with any optional fields omitted.
mkGetRecordsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRecordsResponse
mkGetRecordsResponse responseStatus =
  GetRecordsResponse'
    { nextShardIterator = Core.Nothing,
      records = Core.Nothing,
      responseStatus
    }

-- | The next position in the shard from which to start sequentially reading stream records. If set to @null@ , the shard has been closed and the requested iterator will not return any more data.
--
-- /Note:/ Consider using 'nextShardIterator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsNextShardIterator :: Lens.Lens' GetRecordsResponse (Core.Maybe Types.ShardIterator)
grrrsNextShardIterator = Lens.field @"nextShardIterator"
{-# DEPRECATED grrrsNextShardIterator "Use generic-lens or generic-optics with 'nextShardIterator' instead." #-}

-- | The stream records from the shard, which were retrieved using the shard iterator.
--
-- /Note:/ Consider using 'records' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsRecords :: Lens.Lens' GetRecordsResponse (Core.Maybe [Types.Record])
grrrsRecords = Lens.field @"records"
{-# DEPRECATED grrrsRecords "Use generic-lens or generic-optics with 'records' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GetRecordsResponse Core.Int
grrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
