{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetContainerLog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the log events of a container of your Amazon Lightsail container service.
--
-- If your container service has more than one node (i.e., a scale greater than 1), then the log events that are returned for the specified container are merged from all nodes on your container service.
module Network.AWS.Lightsail.GetContainerLog
  ( -- * Creating a request
    GetContainerLog (..),
    mkGetContainerLog,

    -- ** Request lenses
    gclServiceName,
    gclContainerName,
    gclEndTime,
    gclFilterPattern,
    gclPageToken,
    gclStartTime,

    -- * Destructuring the response
    GetContainerLogResponse (..),
    mkGetContainerLogResponse,

    -- ** Response lenses
    gclrrsLogEvents,
    gclrrsNextPageToken,
    gclrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetContainerLog' smart constructor.
data GetContainerLog = GetContainerLog'
  { -- | The name of the container service for which to get a container log.
    serviceName :: Types.ServiceName,
    -- | The name of the container that is either running or previously ran on the container service for which to return a log.
    containerName :: Types.String,
    -- | The end of the time interval for which to get log data.
    --
    -- Constraints:
    --
    --     * Specified in Coordinated Universal Time (UTC).
    --
    --
    --     * Specified in the Unix time format.
    -- For example, if you wish to use an end time of October 1, 2018, at 9 PM UTC, specify @1538427600@ as the end time.
    --
    --
    -- You can convert a human-friendly time to Unix time format using a converter like <https://www.epochconverter.com/ Epoch converter> .
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The pattern to use to filter the returned log events to a specific term.
    --
    -- The following are a few examples of filter patterns that you can specify:
    --
    --     * To return all log events, specify a filter pattern of @""@ .
    --
    --
    --     * To exclude log events that contain the @ERROR@ term, and return all other log events, specify a filter pattern of @"-ERROR"@ .
    --
    --
    --     * To return log events that contain the @ERROR@ term, specify a filter pattern of @"ERROR"@ .
    --
    --
    --     * To return log events that contain both the @ERROR@ and @Exception@ terms, specify a filter pattern of @"ERROR Exception"@ .
    --
    --
    --     * To return log events that contain the @ERROR@ /or/ the @Exception@ term, specify a filter pattern of @"?ERROR ?Exception"@ .
    filterPattern :: Core.Maybe Types.String,
    -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetContainerLog@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Types.String,
    -- | The start of the time interval for which to get log data.
    --
    -- Constraints:
    --
    --     * Specified in Coordinated Universal Time (UTC).
    --
    --
    --     * Specified in the Unix time format.
    -- For example, if you wish to use a start time of October 1, 2018, at 8 PM UTC, specify @1538424000@ as the start time.
    --
    --
    -- You can convert a human-friendly time to Unix time format using a converter like <https://www.epochconverter.com/ Epoch converter> .
    startTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetContainerLog' value with any optional fields omitted.
mkGetContainerLog ::
  -- | 'serviceName'
  Types.ServiceName ->
  -- | 'containerName'
  Types.String ->
  GetContainerLog
mkGetContainerLog serviceName containerName =
  GetContainerLog'
    { serviceName,
      containerName,
      endTime = Core.Nothing,
      filterPattern = Core.Nothing,
      pageToken = Core.Nothing,
      startTime = Core.Nothing
    }

-- | The name of the container service for which to get a container log.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gclServiceName :: Lens.Lens' GetContainerLog Types.ServiceName
gclServiceName = Lens.field @"serviceName"
{-# DEPRECATED gclServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The name of the container that is either running or previously ran on the container service for which to return a log.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gclContainerName :: Lens.Lens' GetContainerLog Types.String
gclContainerName = Lens.field @"containerName"
{-# DEPRECATED gclContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | The end of the time interval for which to get log data.
--
-- Constraints:
--
--     * Specified in Coordinated Universal Time (UTC).
--
--
--     * Specified in the Unix time format.
-- For example, if you wish to use an end time of October 1, 2018, at 9 PM UTC, specify @1538427600@ as the end time.
--
--
-- You can convert a human-friendly time to Unix time format using a converter like <https://www.epochconverter.com/ Epoch converter> .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gclEndTime :: Lens.Lens' GetContainerLog (Core.Maybe Core.NominalDiffTime)
gclEndTime = Lens.field @"endTime"
{-# DEPRECATED gclEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The pattern to use to filter the returned log events to a specific term.
--
-- The following are a few examples of filter patterns that you can specify:
--
--     * To return all log events, specify a filter pattern of @""@ .
--
--
--     * To exclude log events that contain the @ERROR@ term, and return all other log events, specify a filter pattern of @"-ERROR"@ .
--
--
--     * To return log events that contain the @ERROR@ term, specify a filter pattern of @"ERROR"@ .
--
--
--     * To return log events that contain both the @ERROR@ and @Exception@ terms, specify a filter pattern of @"ERROR Exception"@ .
--
--
--     * To return log events that contain the @ERROR@ /or/ the @Exception@ term, specify a filter pattern of @"?ERROR ?Exception"@ .
--
--
--
-- /Note:/ Consider using 'filterPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gclFilterPattern :: Lens.Lens' GetContainerLog (Core.Maybe Types.String)
gclFilterPattern = Lens.field @"filterPattern"
{-# DEPRECATED gclFilterPattern "Use generic-lens or generic-optics with 'filterPattern' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetContainerLog@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gclPageToken :: Lens.Lens' GetContainerLog (Core.Maybe Types.String)
gclPageToken = Lens.field @"pageToken"
{-# DEPRECATED gclPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The start of the time interval for which to get log data.
--
-- Constraints:
--
--     * Specified in Coordinated Universal Time (UTC).
--
--
--     * Specified in the Unix time format.
-- For example, if you wish to use a start time of October 1, 2018, at 8 PM UTC, specify @1538424000@ as the start time.
--
--
-- You can convert a human-friendly time to Unix time format using a converter like <https://www.epochconverter.com/ Epoch converter> .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gclStartTime :: Lens.Lens' GetContainerLog (Core.Maybe Core.NominalDiffTime)
gclStartTime = Lens.field @"startTime"
{-# DEPRECATED gclStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Core.FromJSON GetContainerLog where
  toJSON GetContainerLog {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("serviceName" Core..= serviceName),
            Core.Just ("containerName" Core..= containerName),
            ("endTime" Core..=) Core.<$> endTime,
            ("filterPattern" Core..=) Core.<$> filterPattern,
            ("pageToken" Core..=) Core.<$> pageToken,
            ("startTime" Core..=) Core.<$> startTime
          ]
      )

instance Core.AWSRequest GetContainerLog where
  type Rs GetContainerLog = GetContainerLogResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetContainerLog")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerLogResponse'
            Core.<$> (x Core..:? "logEvents")
            Core.<*> (x Core..:? "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetContainerLogResponse' smart constructor.
data GetContainerLogResponse = GetContainerLogResponse'
  { -- | An array of objects that describe the log events of a container.
    logEvents :: Core.Maybe [Types.ContainerServiceLogEvent],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetContainerLog@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetContainerLogResponse' value with any optional fields omitted.
mkGetContainerLogResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetContainerLogResponse
mkGetContainerLogResponse responseStatus =
  GetContainerLogResponse'
    { logEvents = Core.Nothing,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the log events of a container.
--
-- /Note:/ Consider using 'logEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gclrrsLogEvents :: Lens.Lens' GetContainerLogResponse (Core.Maybe [Types.ContainerServiceLogEvent])
gclrrsLogEvents = Lens.field @"logEvents"
{-# DEPRECATED gclrrsLogEvents "Use generic-lens or generic-optics with 'logEvents' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetContainerLog@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gclrrsNextPageToken :: Lens.Lens' GetContainerLogResponse (Core.Maybe Types.String)
gclrrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED gclrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gclrrsResponseStatus :: Lens.Lens' GetContainerLogResponse Core.Int
gclrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gclrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
