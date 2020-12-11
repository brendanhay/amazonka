{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gclStartTime,
    gclEndTime,
    gclPageToken,
    gclFilterPattern,
    gclServiceName,
    gclContainerName,

    -- * Destructuring the response
    GetContainerLogResponse (..),
    mkGetContainerLogResponse,

    -- ** Response lenses
    gclrsNextPageToken,
    gclrsLogEvents,
    gclrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetContainerLog' smart constructor.
data GetContainerLog = GetContainerLog'
  { startTime ::
      Lude.Maybe Lude.Timestamp,
    endTime :: Lude.Maybe Lude.Timestamp,
    pageToken :: Lude.Maybe Lude.Text,
    filterPattern :: Lude.Maybe Lude.Text,
    serviceName :: Lude.Text,
    containerName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContainerLog' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container that is either running or previously ran on the container service for which to return a log.
-- * 'endTime' - The end of the time interval for which to get log data.
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
-- * 'filterPattern' - The pattern to use to filter the returned log events to a specific term.
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
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetContainerLog@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
-- * 'serviceName' - The name of the container service for which to get a container log.
-- * 'startTime' - The start of the time interval for which to get log data.
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
mkGetContainerLog ::
  -- | 'serviceName'
  Lude.Text ->
  -- | 'containerName'
  Lude.Text ->
  GetContainerLog
mkGetContainerLog pServiceName_ pContainerName_ =
  GetContainerLog'
    { startTime = Lude.Nothing,
      endTime = Lude.Nothing,
      pageToken = Lude.Nothing,
      filterPattern = Lude.Nothing,
      serviceName = pServiceName_,
      containerName = pContainerName_
    }

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
gclStartTime :: Lens.Lens' GetContainerLog (Lude.Maybe Lude.Timestamp)
gclStartTime = Lens.lens (startTime :: GetContainerLog -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: GetContainerLog)
{-# DEPRECATED gclStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

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
gclEndTime :: Lens.Lens' GetContainerLog (Lude.Maybe Lude.Timestamp)
gclEndTime = Lens.lens (endTime :: GetContainerLog -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: GetContainerLog)
{-# DEPRECATED gclEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetContainerLog@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gclPageToken :: Lens.Lens' GetContainerLog (Lude.Maybe Lude.Text)
gclPageToken = Lens.lens (pageToken :: GetContainerLog -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetContainerLog)
{-# DEPRECATED gclPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

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
gclFilterPattern :: Lens.Lens' GetContainerLog (Lude.Maybe Lude.Text)
gclFilterPattern = Lens.lens (filterPattern :: GetContainerLog -> Lude.Maybe Lude.Text) (\s a -> s {filterPattern = a} :: GetContainerLog)
{-# DEPRECATED gclFilterPattern "Use generic-lens or generic-optics with 'filterPattern' instead." #-}

-- | The name of the container service for which to get a container log.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gclServiceName :: Lens.Lens' GetContainerLog Lude.Text
gclServiceName = Lens.lens (serviceName :: GetContainerLog -> Lude.Text) (\s a -> s {serviceName = a} :: GetContainerLog)
{-# DEPRECATED gclServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The name of the container that is either running or previously ran on the container service for which to return a log.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gclContainerName :: Lens.Lens' GetContainerLog Lude.Text
gclContainerName = Lens.lens (containerName :: GetContainerLog -> Lude.Text) (\s a -> s {containerName = a} :: GetContainerLog)
{-# DEPRECATED gclContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Lude.AWSRequest GetContainerLog where
  type Rs GetContainerLog = GetContainerLogResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetContainerLogResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "logEvents" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetContainerLog where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetContainerLog" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetContainerLog where
  toJSON GetContainerLog' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("startTime" Lude..=) Lude.<$> startTime,
            ("endTime" Lude..=) Lude.<$> endTime,
            ("pageToken" Lude..=) Lude.<$> pageToken,
            ("filterPattern" Lude..=) Lude.<$> filterPattern,
            Lude.Just ("serviceName" Lude..= serviceName),
            Lude.Just ("containerName" Lude..= containerName)
          ]
      )

instance Lude.ToPath GetContainerLog where
  toPath = Lude.const "/"

instance Lude.ToQuery GetContainerLog where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetContainerLogResponse' smart constructor.
data GetContainerLogResponse = GetContainerLogResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    logEvents ::
      Lude.Maybe [ContainerServiceLogEvent],
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

-- | Creates a value of 'GetContainerLogResponse' with the minimum fields required to make a request.
--
-- * 'logEvents' - An array of objects that describe the log events of a container.
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetContainerLog@ request and specify the next page token using the @pageToken@ parameter.
-- * 'responseStatus' - The response status code.
mkGetContainerLogResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetContainerLogResponse
mkGetContainerLogResponse pResponseStatus_ =
  GetContainerLogResponse'
    { nextPageToken = Lude.Nothing,
      logEvents = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetContainerLog@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gclrsNextPageToken :: Lens.Lens' GetContainerLogResponse (Lude.Maybe Lude.Text)
gclrsNextPageToken = Lens.lens (nextPageToken :: GetContainerLogResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetContainerLogResponse)
{-# DEPRECATED gclrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of objects that describe the log events of a container.
--
-- /Note:/ Consider using 'logEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gclrsLogEvents :: Lens.Lens' GetContainerLogResponse (Lude.Maybe [ContainerServiceLogEvent])
gclrsLogEvents = Lens.lens (logEvents :: GetContainerLogResponse -> Lude.Maybe [ContainerServiceLogEvent]) (\s a -> s {logEvents = a} :: GetContainerLogResponse)
{-# DEPRECATED gclrsLogEvents "Use generic-lens or generic-optics with 'logEvents' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gclrsResponseStatus :: Lens.Lens' GetContainerLogResponse Lude.Int
gclrsResponseStatus = Lens.lens (responseStatus :: GetContainerLogResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetContainerLogResponse)
{-# DEPRECATED gclrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
