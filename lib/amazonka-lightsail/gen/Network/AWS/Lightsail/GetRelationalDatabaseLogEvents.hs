{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseLogEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of log events for a database in Amazon Lightsail.
module Network.AWS.Lightsail.GetRelationalDatabaseLogEvents
  ( -- * Creating a request
    GetRelationalDatabaseLogEvents (..),
    mkGetRelationalDatabaseLogEvents,

    -- ** Request lenses
    grdleRelationalDatabaseName,
    grdleLogStreamName,
    grdleEndTime,
    grdlePageToken,
    grdleStartFromHead,
    grdleStartTime,

    -- * Destructuring the response
    GetRelationalDatabaseLogEventsResponse (..),
    mkGetRelationalDatabaseLogEventsResponse,

    -- ** Response lenses
    grdlerrsNextBackwardToken,
    grdlerrsNextForwardToken,
    grdlerrsResourceLogEvents,
    grdlerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRelationalDatabaseLogEvents' smart constructor.
data GetRelationalDatabaseLogEvents = GetRelationalDatabaseLogEvents'
  { -- | The name of your database for which to get log events.
    relationalDatabaseName :: Types.RelationalDatabaseName,
    -- | The name of the log stream.
    --
    -- Use the @get relational database log streams@ operation to get a list of available log streams.
    logStreamName :: Types.LogStreamName,
    -- | The end of the time interval from which to get log events.
    --
    -- Constraints:
    --
    --     * Specified in Coordinated Universal Time (UTC).
    --
    --
    --     * Specified in the Unix time format.
    -- For example, if you wish to use an end time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the end time.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The token to advance to the next or previous page of results from your request.
    --
    -- To get a page token, perform an initial @GetRelationalDatabaseLogEvents@ request. If your results are paginated, the response will return a next forward token and/or next backward token that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Types.PageToken,
    -- | Parameter to specify if the log should start from head or tail. If @true@ is specified, the log event starts from the head of the log. If @false@ is specified, the log event starts from the tail of the log.
    startFromHead :: Core.Maybe Core.Bool,
    -- | The start of the time interval from which to get log events.
    --
    -- Constraints:
    --
    --     * Specified in Coordinated Universal Time (UTC).
    --
    --
    --     * Specified in the Unix time format.
    -- For example, if you wish to use a start time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the start time.
    startTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetRelationalDatabaseLogEvents' value with any optional fields omitted.
mkGetRelationalDatabaseLogEvents ::
  -- | 'relationalDatabaseName'
  Types.RelationalDatabaseName ->
  -- | 'logStreamName'
  Types.LogStreamName ->
  GetRelationalDatabaseLogEvents
mkGetRelationalDatabaseLogEvents
  relationalDatabaseName
  logStreamName =
    GetRelationalDatabaseLogEvents'
      { relationalDatabaseName,
        logStreamName,
        endTime = Core.Nothing,
        pageToken = Core.Nothing,
        startFromHead = Core.Nothing,
        startTime = Core.Nothing
      }

-- | The name of your database for which to get log events.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdleRelationalDatabaseName :: Lens.Lens' GetRelationalDatabaseLogEvents Types.RelationalDatabaseName
grdleRelationalDatabaseName = Lens.field @"relationalDatabaseName"
{-# DEPRECATED grdleRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

-- | The name of the log stream.
--
-- Use the @get relational database log streams@ operation to get a list of available log streams.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdleLogStreamName :: Lens.Lens' GetRelationalDatabaseLogEvents Types.LogStreamName
grdleLogStreamName = Lens.field @"logStreamName"
{-# DEPRECATED grdleLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

-- | The end of the time interval from which to get log events.
--
-- Constraints:
--
--     * Specified in Coordinated Universal Time (UTC).
--
--
--     * Specified in the Unix time format.
-- For example, if you wish to use an end time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the end time.
--
--
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdleEndTime :: Lens.Lens' GetRelationalDatabaseLogEvents (Core.Maybe Core.NominalDiffTime)
grdleEndTime = Lens.field @"endTime"
{-# DEPRECATED grdleEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The token to advance to the next or previous page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseLogEvents@ request. If your results are paginated, the response will return a next forward token and/or next backward token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdlePageToken :: Lens.Lens' GetRelationalDatabaseLogEvents (Core.Maybe Types.PageToken)
grdlePageToken = Lens.field @"pageToken"
{-# DEPRECATED grdlePageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | Parameter to specify if the log should start from head or tail. If @true@ is specified, the log event starts from the head of the log. If @false@ is specified, the log event starts from the tail of the log.
--
-- /Note:/ Consider using 'startFromHead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdleStartFromHead :: Lens.Lens' GetRelationalDatabaseLogEvents (Core.Maybe Core.Bool)
grdleStartFromHead = Lens.field @"startFromHead"
{-# DEPRECATED grdleStartFromHead "Use generic-lens or generic-optics with 'startFromHead' instead." #-}

-- | The start of the time interval from which to get log events.
--
-- Constraints:
--
--     * Specified in Coordinated Universal Time (UTC).
--
--
--     * Specified in the Unix time format.
-- For example, if you wish to use a start time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the start time.
--
--
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdleStartTime :: Lens.Lens' GetRelationalDatabaseLogEvents (Core.Maybe Core.NominalDiffTime)
grdleStartTime = Lens.field @"startTime"
{-# DEPRECATED grdleStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Core.FromJSON GetRelationalDatabaseLogEvents where
  toJSON GetRelationalDatabaseLogEvents {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("relationalDatabaseName" Core..= relationalDatabaseName),
            Core.Just ("logStreamName" Core..= logStreamName),
            ("endTime" Core..=) Core.<$> endTime,
            ("pageToken" Core..=) Core.<$> pageToken,
            ("startFromHead" Core..=) Core.<$> startFromHead,
            ("startTime" Core..=) Core.<$> startTime
          ]
      )

instance Core.AWSRequest GetRelationalDatabaseLogEvents where
  type
    Rs GetRelationalDatabaseLogEvents =
      GetRelationalDatabaseLogEventsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Lightsail_20161128.GetRelationalDatabaseLogEvents"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseLogEventsResponse'
            Core.<$> (x Core..:? "nextBackwardToken")
            Core.<*> (x Core..:? "nextForwardToken")
            Core.<*> (x Core..:? "resourceLogEvents")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetRelationalDatabaseLogEventsResponse' smart constructor.
data GetRelationalDatabaseLogEventsResponse = GetRelationalDatabaseLogEventsResponse'
  { -- | A token used for advancing to the previous page of results from your get relational database log events request.
    nextBackwardToken :: Core.Maybe Types.NextBackwardToken,
    -- | A token used for advancing to the next page of results from your get relational database log events request.
    nextForwardToken :: Core.Maybe Types.NextForwardToken,
    -- | An object describing the result of your get relational database log events request.
    resourceLogEvents :: Core.Maybe [Types.LogEvent],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetRelationalDatabaseLogEventsResponse' value with any optional fields omitted.
mkGetRelationalDatabaseLogEventsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRelationalDatabaseLogEventsResponse
mkGetRelationalDatabaseLogEventsResponse responseStatus =
  GetRelationalDatabaseLogEventsResponse'
    { nextBackwardToken =
        Core.Nothing,
      nextForwardToken = Core.Nothing,
      resourceLogEvents = Core.Nothing,
      responseStatus
    }

-- | A token used for advancing to the previous page of results from your get relational database log events request.
--
-- /Note:/ Consider using 'nextBackwardToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdlerrsNextBackwardToken :: Lens.Lens' GetRelationalDatabaseLogEventsResponse (Core.Maybe Types.NextBackwardToken)
grdlerrsNextBackwardToken = Lens.field @"nextBackwardToken"
{-# DEPRECATED grdlerrsNextBackwardToken "Use generic-lens or generic-optics with 'nextBackwardToken' instead." #-}

-- | A token used for advancing to the next page of results from your get relational database log events request.
--
-- /Note:/ Consider using 'nextForwardToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdlerrsNextForwardToken :: Lens.Lens' GetRelationalDatabaseLogEventsResponse (Core.Maybe Types.NextForwardToken)
grdlerrsNextForwardToken = Lens.field @"nextForwardToken"
{-# DEPRECATED grdlerrsNextForwardToken "Use generic-lens or generic-optics with 'nextForwardToken' instead." #-}

-- | An object describing the result of your get relational database log events request.
--
-- /Note:/ Consider using 'resourceLogEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdlerrsResourceLogEvents :: Lens.Lens' GetRelationalDatabaseLogEventsResponse (Core.Maybe [Types.LogEvent])
grdlerrsResourceLogEvents = Lens.field @"resourceLogEvents"
{-# DEPRECATED grdlerrsResourceLogEvents "Use generic-lens or generic-optics with 'resourceLogEvents' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdlerrsResponseStatus :: Lens.Lens' GetRelationalDatabaseLogEventsResponse Core.Int
grdlerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grdlerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
