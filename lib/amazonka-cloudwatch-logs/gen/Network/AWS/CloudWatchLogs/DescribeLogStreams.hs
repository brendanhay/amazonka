{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeLogStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the log streams for the specified log group. You can list all the log streams or filter the results by prefix. You can also control how the results are ordered.
--
-- This operation has a limit of five transactions per second, after which transactions are throttled.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeLogStreams
  ( -- * Creating a request
    DescribeLogStreams (..),
    mkDescribeLogStreams,

    -- ** Request lenses
    dlssLogGroupName,
    dlssDescending,
    dlssLimit,
    dlssLogStreamNamePrefix,
    dlssNextToken,
    dlssOrderBy,

    -- * Destructuring the response
    DescribeLogStreamsResponse (..),
    mkDescribeLogStreamsResponse,

    -- ** Response lenses
    dlsrrsLogStreams,
    dlsrrsNextToken,
    dlsrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLogStreams' smart constructor.
data DescribeLogStreams = DescribeLogStreams'
  { -- | The name of the log group.
    logGroupName :: Types.LogGroupName,
    -- | If the value is true, results are returned in descending order. If the value is to false, results are returned in ascending order. The default value is false.
    descending :: Core.Maybe Core.Bool,
    -- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
    limit :: Core.Maybe Core.Natural,
    -- | The prefix to match.
    --
    -- If @orderBy@ is @LastEventTime@ , you cannot specify this parameter.
    logStreamNamePrefix :: Core.Maybe Types.LogStreamName,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken,
    -- | If the value is @LogStreamName@ , the results are ordered by log stream name. If the value is @LastEventTime@ , the results are ordered by the event time. The default value is @LogStreamName@ .
    --
    -- If you order the results by event time, you cannot specify the @logStreamNamePrefix@ parameter.
    -- @lastEventTimeStamp@ represents the time of the most recent log event in the log stream in CloudWatch Logs. This number is expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. @lastEventTimeStamp@ updates on an eventual consistency basis. It typically updates in less than an hour from ingestion, but in rare situations might take longer.
    orderBy :: Core.Maybe Types.OrderBy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLogStreams' value with any optional fields omitted.
mkDescribeLogStreams ::
  -- | 'logGroupName'
  Types.LogGroupName ->
  DescribeLogStreams
mkDescribeLogStreams logGroupName =
  DescribeLogStreams'
    { logGroupName,
      descending = Core.Nothing,
      limit = Core.Nothing,
      logStreamNamePrefix = Core.Nothing,
      nextToken = Core.Nothing,
      orderBy = Core.Nothing
    }

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlssLogGroupName :: Lens.Lens' DescribeLogStreams Types.LogGroupName
dlssLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED dlssLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | If the value is true, results are returned in descending order. If the value is to false, results are returned in ascending order. The default value is false.
--
-- /Note:/ Consider using 'descending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlssDescending :: Lens.Lens' DescribeLogStreams (Core.Maybe Core.Bool)
dlssDescending = Lens.field @"descending"
{-# DEPRECATED dlssDescending "Use generic-lens or generic-optics with 'descending' instead." #-}

-- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlssLimit :: Lens.Lens' DescribeLogStreams (Core.Maybe Core.Natural)
dlssLimit = Lens.field @"limit"
{-# DEPRECATED dlssLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The prefix to match.
--
-- If @orderBy@ is @LastEventTime@ , you cannot specify this parameter.
--
-- /Note:/ Consider using 'logStreamNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlssLogStreamNamePrefix :: Lens.Lens' DescribeLogStreams (Core.Maybe Types.LogStreamName)
dlssLogStreamNamePrefix = Lens.field @"logStreamNamePrefix"
{-# DEPRECATED dlssLogStreamNamePrefix "Use generic-lens or generic-optics with 'logStreamNamePrefix' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlssNextToken :: Lens.Lens' DescribeLogStreams (Core.Maybe Types.NextToken)
dlssNextToken = Lens.field @"nextToken"
{-# DEPRECATED dlssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | If the value is @LogStreamName@ , the results are ordered by log stream name. If the value is @LastEventTime@ , the results are ordered by the event time. The default value is @LogStreamName@ .
--
-- If you order the results by event time, you cannot specify the @logStreamNamePrefix@ parameter.
-- @lastEventTimeStamp@ represents the time of the most recent log event in the log stream in CloudWatch Logs. This number is expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. @lastEventTimeStamp@ updates on an eventual consistency basis. It typically updates in less than an hour from ingestion, but in rare situations might take longer.
--
-- /Note:/ Consider using 'orderBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlssOrderBy :: Lens.Lens' DescribeLogStreams (Core.Maybe Types.OrderBy)
dlssOrderBy = Lens.field @"orderBy"
{-# DEPRECATED dlssOrderBy "Use generic-lens or generic-optics with 'orderBy' instead." #-}

instance Core.FromJSON DescribeLogStreams where
  toJSON DescribeLogStreams {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            ("descending" Core..=) Core.<$> descending,
            ("limit" Core..=) Core.<$> limit,
            ("logStreamNamePrefix" Core..=) Core.<$> logStreamNamePrefix,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("orderBy" Core..=) Core.<$> orderBy
          ]
      )

instance Core.AWSRequest DescribeLogStreams where
  type Rs DescribeLogStreams = DescribeLogStreamsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.DescribeLogStreams")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLogStreamsResponse'
            Core.<$> (x Core..:? "logStreams")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeLogStreams where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"logStreams" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeLogStreamsResponse' smart constructor.
data DescribeLogStreamsResponse = DescribeLogStreamsResponse'
  { -- | The log streams.
    logStreams :: Core.Maybe [Types.LogStream],
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLogStreamsResponse' value with any optional fields omitted.
mkDescribeLogStreamsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLogStreamsResponse
mkDescribeLogStreamsResponse responseStatus =
  DescribeLogStreamsResponse'
    { logStreams = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The log streams.
--
-- /Note:/ Consider using 'logStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsrrsLogStreams :: Lens.Lens' DescribeLogStreamsResponse (Core.Maybe [Types.LogStream])
dlsrrsLogStreams = Lens.field @"logStreams"
{-# DEPRECATED dlsrrsLogStreams "Use generic-lens or generic-optics with 'logStreams' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsrrsNextToken :: Lens.Lens' DescribeLogStreamsResponse (Core.Maybe Types.NextToken)
dlsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dlsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsrrsResponseStatus :: Lens.Lens' DescribeLogStreamsResponse Core.Int
dlsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
