{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.StartQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules a query of a log group using CloudWatch Logs Insights. You specify the log group and time range to query and the query string to use.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax> .
-- Queries time out after 15 minutes of execution. If your queries are timing out, reduce the time range being searched or partition your query into a number of queries.
module Network.AWS.CloudWatchLogs.StartQuery
  ( -- * Creating a request
    StartQuery (..),
    mkStartQuery,

    -- ** Request lenses
    sqStartTime,
    sqEndTime,
    sqQueryString,
    sqLimit,
    sqLogGroupName,
    sqLogGroupNames,

    -- * Destructuring the response
    StartQueryResponse (..),
    mkStartQueryResponse,

    -- ** Response lenses
    srsQueryId,
    srsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartQuery' smart constructor.
data StartQuery = StartQuery'
  { -- | The beginning of the time range to query. The range is inclusive, so the specified start time is included in the query. Specified as epoch time, the number of seconds since January 1, 1970, 00:00:00 UTC.
    startTime :: Core.Natural,
    -- | The end of the time range to query. The range is inclusive, so the specified end time is included in the query. Specified as epoch time, the number of seconds since January 1, 1970, 00:00:00 UTC.
    endTime :: Core.Natural,
    -- | The query string to use. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax> .
    queryString :: Types.QueryString,
    -- | The maximum number of log events to return in the query. If the query string uses the @fields@ command, only the specified fields and their values are returned. The default is 1000.
    limit :: Core.Maybe Core.Natural,
    -- | The log group on which to perform the query.
    --
    -- A @StartQuery@ operation must include a @logGroupNames@ or a @logGroupName@ parameter, but not both.
    logGroupName :: Core.Maybe Types.LogGroupName,
    -- | The list of log groups to be queried. You can include up to 20 log groups.
    --
    -- A @StartQuery@ operation must include a @logGroupNames@ or a @logGroupName@ parameter, but not both.
    logGroupNames :: Core.Maybe [Types.LogGroupName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartQuery' value with any optional fields omitted.
mkStartQuery ::
  -- | 'startTime'
  Core.Natural ->
  -- | 'endTime'
  Core.Natural ->
  -- | 'queryString'
  Types.QueryString ->
  StartQuery
mkStartQuery startTime endTime queryString =
  StartQuery'
    { startTime,
      endTime,
      queryString,
      limit = Core.Nothing,
      logGroupName = Core.Nothing,
      logGroupNames = Core.Nothing
    }

-- | The beginning of the time range to query. The range is inclusive, so the specified start time is included in the query. Specified as epoch time, the number of seconds since January 1, 1970, 00:00:00 UTC.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqStartTime :: Lens.Lens' StartQuery Core.Natural
sqStartTime = Lens.field @"startTime"
{-# DEPRECATED sqStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The end of the time range to query. The range is inclusive, so the specified end time is included in the query. Specified as epoch time, the number of seconds since January 1, 1970, 00:00:00 UTC.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqEndTime :: Lens.Lens' StartQuery Core.Natural
sqEndTime = Lens.field @"endTime"
{-# DEPRECATED sqEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The query string to use. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax> .
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqQueryString :: Lens.Lens' StartQuery Types.QueryString
sqQueryString = Lens.field @"queryString"
{-# DEPRECATED sqQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The maximum number of log events to return in the query. If the query string uses the @fields@ command, only the specified fields and their values are returned. The default is 1000.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqLimit :: Lens.Lens' StartQuery (Core.Maybe Core.Natural)
sqLimit = Lens.field @"limit"
{-# DEPRECATED sqLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The log group on which to perform the query.
--
-- A @StartQuery@ operation must include a @logGroupNames@ or a @logGroupName@ parameter, but not both.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqLogGroupName :: Lens.Lens' StartQuery (Core.Maybe Types.LogGroupName)
sqLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED sqLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The list of log groups to be queried. You can include up to 20 log groups.
--
-- A @StartQuery@ operation must include a @logGroupNames@ or a @logGroupName@ parameter, but not both.
--
-- /Note:/ Consider using 'logGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqLogGroupNames :: Lens.Lens' StartQuery (Core.Maybe [Types.LogGroupName])
sqLogGroupNames = Lens.field @"logGroupNames"
{-# DEPRECATED sqLogGroupNames "Use generic-lens or generic-optics with 'logGroupNames' instead." #-}

instance Core.FromJSON StartQuery where
  toJSON StartQuery {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("startTime" Core..= startTime),
            Core.Just ("endTime" Core..= endTime),
            Core.Just ("queryString" Core..= queryString),
            ("limit" Core..=) Core.<$> limit,
            ("logGroupName" Core..=) Core.<$> logGroupName,
            ("logGroupNames" Core..=) Core.<$> logGroupNames
          ]
      )

instance Core.AWSRequest StartQuery where
  type Rs StartQuery = StartQueryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.StartQuery")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartQueryResponse'
            Core.<$> (x Core..:? "queryId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartQueryResponse' smart constructor.
data StartQueryResponse = StartQueryResponse'
  { -- | The unique ID of the query.
    queryId :: Core.Maybe Types.QueryId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartQueryResponse' value with any optional fields omitted.
mkStartQueryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartQueryResponse
mkStartQueryResponse responseStatus =
  StartQueryResponse' {queryId = Core.Nothing, responseStatus}

-- | The unique ID of the query.
--
-- /Note:/ Consider using 'queryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsQueryId :: Lens.Lens' StartQueryResponse (Core.Maybe Types.QueryId)
srsQueryId = Lens.field @"queryId"
{-# DEPRECATED srsQueryId "Use generic-lens or generic-optics with 'queryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartQueryResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
