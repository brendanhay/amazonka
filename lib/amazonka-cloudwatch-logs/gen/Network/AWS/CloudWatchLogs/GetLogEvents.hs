{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.GetLogEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists log events from the specified log stream. You can list all of the log events or filter using a time range.
--
-- By default, this operation returns as many log events as can fit in a response size of 1MB (up to 10,000 log events). You can get additional log events by specifying one of the tokens in a subsequent call. This operation can return empty results while there are more log events available through the token.
module Network.AWS.CloudWatchLogs.GetLogEvents
  ( -- * Creating a request
    GetLogEvents (..),
    mkGetLogEvents,

    -- ** Request lenses
    gleStartTime,
    gleStartFromHead,
    gleLogGroupName,
    gleNextToken,
    gleLogStreamName,
    gleEndTime,
    gleLimit,

    -- * Destructuring the response
    GetLogEventsResponse (..),
    mkGetLogEventsResponse,

    -- ** Response lenses
    glersNextBackwardToken,
    glersNextForwardToken,
    glersEvents,
    glersResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLogEvents' smart constructor.
data GetLogEvents = GetLogEvents'
  { -- | The start of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp equal to this time or later than this time are included. Events with a timestamp earlier than this time are not included.
    startTime :: Lude.Maybe Lude.Natural,
    -- | If the value is true, the earliest log events are returned first. If the value is false, the latest log events are returned first. The default value is false.
    --
    -- If you are using @nextToken@ in this operation, you must specify @true@ for @startFromHead@ .
    startFromHead :: Lude.Maybe Lude.Bool,
    -- | The name of the log group.
    logGroupName :: Lude.Text,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    --
    -- Using this token works only when you specify @true@ for @startFromHead@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the log stream.
    logStreamName :: Lude.Text,
    -- | The end of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp equal to or later than this time are not included.
    endTime :: Lude.Maybe Lude.Natural,
    -- | The maximum number of log events returned. If you don't specify a value, the maximum is as many log events as can fit in a response size of 1 MB, up to 10,000 log events.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLogEvents' with the minimum fields required to make a request.
--
-- * 'startTime' - The start of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp equal to this time or later than this time are included. Events with a timestamp earlier than this time are not included.
-- * 'startFromHead' - If the value is true, the earliest log events are returned first. If the value is false, the latest log events are returned first. The default value is false.
--
-- If you are using @nextToken@ in this operation, you must specify @true@ for @startFromHead@ .
-- * 'logGroupName' - The name of the log group.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- Using this token works only when you specify @true@ for @startFromHead@ .
-- * 'logStreamName' - The name of the log stream.
-- * 'endTime' - The end of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp equal to or later than this time are not included.
-- * 'limit' - The maximum number of log events returned. If you don't specify a value, the maximum is as many log events as can fit in a response size of 1 MB, up to 10,000 log events.
mkGetLogEvents ::
  -- | 'logGroupName'
  Lude.Text ->
  -- | 'logStreamName'
  Lude.Text ->
  GetLogEvents
mkGetLogEvents pLogGroupName_ pLogStreamName_ =
  GetLogEvents'
    { startTime = Lude.Nothing,
      startFromHead = Lude.Nothing,
      logGroupName = pLogGroupName_,
      nextToken = Lude.Nothing,
      logStreamName = pLogStreamName_,
      endTime = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The start of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp equal to this time or later than this time are included. Events with a timestamp earlier than this time are not included.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleStartTime :: Lens.Lens' GetLogEvents (Lude.Maybe Lude.Natural)
gleStartTime = Lens.lens (startTime :: GetLogEvents -> Lude.Maybe Lude.Natural) (\s a -> s {startTime = a} :: GetLogEvents)
{-# DEPRECATED gleStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | If the value is true, the earliest log events are returned first. If the value is false, the latest log events are returned first. The default value is false.
--
-- If you are using @nextToken@ in this operation, you must specify @true@ for @startFromHead@ .
--
-- /Note:/ Consider using 'startFromHead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleStartFromHead :: Lens.Lens' GetLogEvents (Lude.Maybe Lude.Bool)
gleStartFromHead = Lens.lens (startFromHead :: GetLogEvents -> Lude.Maybe Lude.Bool) (\s a -> s {startFromHead = a} :: GetLogEvents)
{-# DEPRECATED gleStartFromHead "Use generic-lens or generic-optics with 'startFromHead' instead." #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleLogGroupName :: Lens.Lens' GetLogEvents Lude.Text
gleLogGroupName = Lens.lens (logGroupName :: GetLogEvents -> Lude.Text) (\s a -> s {logGroupName = a} :: GetLogEvents)
{-# DEPRECATED gleLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- Using this token works only when you specify @true@ for @startFromHead@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleNextToken :: Lens.Lens' GetLogEvents (Lude.Maybe Lude.Text)
gleNextToken = Lens.lens (nextToken :: GetLogEvents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetLogEvents)
{-# DEPRECATED gleNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the log stream.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleLogStreamName :: Lens.Lens' GetLogEvents Lude.Text
gleLogStreamName = Lens.lens (logStreamName :: GetLogEvents -> Lude.Text) (\s a -> s {logStreamName = a} :: GetLogEvents)
{-# DEPRECATED gleLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

-- | The end of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp equal to or later than this time are not included.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleEndTime :: Lens.Lens' GetLogEvents (Lude.Maybe Lude.Natural)
gleEndTime = Lens.lens (endTime :: GetLogEvents -> Lude.Maybe Lude.Natural) (\s a -> s {endTime = a} :: GetLogEvents)
{-# DEPRECATED gleEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The maximum number of log events returned. If you don't specify a value, the maximum is as many log events as can fit in a response size of 1 MB, up to 10,000 log events.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleLimit :: Lens.Lens' GetLogEvents (Lude.Maybe Lude.Natural)
gleLimit = Lens.lens (limit :: GetLogEvents -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: GetLogEvents)
{-# DEPRECATED gleLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest GetLogEvents where
  type Rs GetLogEvents = GetLogEventsResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetLogEventsResponse'
            Lude.<$> (x Lude..?> "nextBackwardToken")
            Lude.<*> (x Lude..?> "nextForwardToken")
            Lude.<*> (x Lude..?> "events" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetLogEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.GetLogEvents" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetLogEvents where
  toJSON GetLogEvents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("startTime" Lude..=) Lude.<$> startTime,
            ("startFromHead" Lude..=) Lude.<$> startFromHead,
            Lude.Just ("logGroupName" Lude..= logGroupName),
            ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("logStreamName" Lude..= logStreamName),
            ("endTime" Lude..=) Lude.<$> endTime,
            ("limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath GetLogEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery GetLogEvents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetLogEventsResponse' smart constructor.
data GetLogEventsResponse = GetLogEventsResponse'
  { -- | The token for the next set of items in the backward direction. The token expires after 24 hours. This token is never null. If you have reached the end of the stream, it returns the same token you passed in.
    nextBackwardToken :: Lude.Maybe Lude.Text,
    -- | The token for the next set of items in the forward direction. The token expires after 24 hours. If you have reached the end of the stream, it returns the same token you passed in.
    nextForwardToken :: Lude.Maybe Lude.Text,
    -- | The events.
    events :: Lude.Maybe [OutputLogEvent],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLogEventsResponse' with the minimum fields required to make a request.
--
-- * 'nextBackwardToken' - The token for the next set of items in the backward direction. The token expires after 24 hours. This token is never null. If you have reached the end of the stream, it returns the same token you passed in.
-- * 'nextForwardToken' - The token for the next set of items in the forward direction. The token expires after 24 hours. If you have reached the end of the stream, it returns the same token you passed in.
-- * 'events' - The events.
-- * 'responseStatus' - The response status code.
mkGetLogEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetLogEventsResponse
mkGetLogEventsResponse pResponseStatus_ =
  GetLogEventsResponse'
    { nextBackwardToken = Lude.Nothing,
      nextForwardToken = Lude.Nothing,
      events = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of items in the backward direction. The token expires after 24 hours. This token is never null. If you have reached the end of the stream, it returns the same token you passed in.
--
-- /Note:/ Consider using 'nextBackwardToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glersNextBackwardToken :: Lens.Lens' GetLogEventsResponse (Lude.Maybe Lude.Text)
glersNextBackwardToken = Lens.lens (nextBackwardToken :: GetLogEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextBackwardToken = a} :: GetLogEventsResponse)
{-# DEPRECATED glersNextBackwardToken "Use generic-lens or generic-optics with 'nextBackwardToken' instead." #-}

-- | The token for the next set of items in the forward direction. The token expires after 24 hours. If you have reached the end of the stream, it returns the same token you passed in.
--
-- /Note:/ Consider using 'nextForwardToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glersNextForwardToken :: Lens.Lens' GetLogEventsResponse (Lude.Maybe Lude.Text)
glersNextForwardToken = Lens.lens (nextForwardToken :: GetLogEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextForwardToken = a} :: GetLogEventsResponse)
{-# DEPRECATED glersNextForwardToken "Use generic-lens or generic-optics with 'nextForwardToken' instead." #-}

-- | The events.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glersEvents :: Lens.Lens' GetLogEventsResponse (Lude.Maybe [OutputLogEvent])
glersEvents = Lens.lens (events :: GetLogEventsResponse -> Lude.Maybe [OutputLogEvent]) (\s a -> s {events = a} :: GetLogEventsResponse)
{-# DEPRECATED glersEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glersResponseStatus :: Lens.Lens' GetLogEventsResponse Lude.Int
glersResponseStatus = Lens.lens (responseStatus :: GetLogEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLogEventsResponse)
{-# DEPRECATED glersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
