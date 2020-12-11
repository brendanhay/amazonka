{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    grdleStartTime,
    grdleStartFromHead,
    grdleEndTime,
    grdlePageToken,
    grdleRelationalDatabaseName,
    grdleLogStreamName,

    -- * Destructuring the response
    GetRelationalDatabaseLogEventsResponse (..),
    mkGetRelationalDatabaseLogEventsResponse,

    -- ** Response lenses
    grdlersNextBackwardToken,
    grdlersResourceLogEvents,
    grdlersNextForwardToken,
    grdlersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRelationalDatabaseLogEvents' smart constructor.
data GetRelationalDatabaseLogEvents = GetRelationalDatabaseLogEvents'
  { startTime ::
      Lude.Maybe Lude.Timestamp,
    startFromHead ::
      Lude.Maybe Lude.Bool,
    endTime ::
      Lude.Maybe Lude.Timestamp,
    pageToken ::
      Lude.Maybe Lude.Text,
    relationalDatabaseName ::
      Lude.Text,
    logStreamName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRelationalDatabaseLogEvents' with the minimum fields required to make a request.
--
-- * 'endTime' - The end of the time interval from which to get log events.
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
-- * 'logStreamName' - The name of the log stream.
--
-- Use the @get relational database log streams@ operation to get a list of available log streams.
-- * 'pageToken' - The token to advance to the next or previous page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseLogEvents@ request. If your results are paginated, the response will return a next forward token and/or next backward token that you can specify as the page token in a subsequent request.
-- * 'relationalDatabaseName' - The name of your database for which to get log events.
-- * 'startFromHead' - Parameter to specify if the log should start from head or tail. If @true@ is specified, the log event starts from the head of the log. If @false@ is specified, the log event starts from the tail of the log.
-- * 'startTime' - The start of the time interval from which to get log events.
--
-- Constraints:
--
--     * Specified in Coordinated Universal Time (UTC).
--
--
--     * Specified in the Unix time format.
-- For example, if you wish to use a start time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the start time.
mkGetRelationalDatabaseLogEvents ::
  -- | 'relationalDatabaseName'
  Lude.Text ->
  -- | 'logStreamName'
  Lude.Text ->
  GetRelationalDatabaseLogEvents
mkGetRelationalDatabaseLogEvents
  pRelationalDatabaseName_
  pLogStreamName_ =
    GetRelationalDatabaseLogEvents'
      { startTime = Lude.Nothing,
        startFromHead = Lude.Nothing,
        endTime = Lude.Nothing,
        pageToken = Lude.Nothing,
        relationalDatabaseName = pRelationalDatabaseName_,
        logStreamName = pLogStreamName_
      }

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
grdleStartTime :: Lens.Lens' GetRelationalDatabaseLogEvents (Lude.Maybe Lude.Timestamp)
grdleStartTime = Lens.lens (startTime :: GetRelationalDatabaseLogEvents -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: GetRelationalDatabaseLogEvents)
{-# DEPRECATED grdleStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Parameter to specify if the log should start from head or tail. If @true@ is specified, the log event starts from the head of the log. If @false@ is specified, the log event starts from the tail of the log.
--
-- /Note:/ Consider using 'startFromHead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdleStartFromHead :: Lens.Lens' GetRelationalDatabaseLogEvents (Lude.Maybe Lude.Bool)
grdleStartFromHead = Lens.lens (startFromHead :: GetRelationalDatabaseLogEvents -> Lude.Maybe Lude.Bool) (\s a -> s {startFromHead = a} :: GetRelationalDatabaseLogEvents)
{-# DEPRECATED grdleStartFromHead "Use generic-lens or generic-optics with 'startFromHead' instead." #-}

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
grdleEndTime :: Lens.Lens' GetRelationalDatabaseLogEvents (Lude.Maybe Lude.Timestamp)
grdleEndTime = Lens.lens (endTime :: GetRelationalDatabaseLogEvents -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: GetRelationalDatabaseLogEvents)
{-# DEPRECATED grdleEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The token to advance to the next or previous page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseLogEvents@ request. If your results are paginated, the response will return a next forward token and/or next backward token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdlePageToken :: Lens.Lens' GetRelationalDatabaseLogEvents (Lude.Maybe Lude.Text)
grdlePageToken = Lens.lens (pageToken :: GetRelationalDatabaseLogEvents -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetRelationalDatabaseLogEvents)
{-# DEPRECATED grdlePageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The name of your database for which to get log events.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdleRelationalDatabaseName :: Lens.Lens' GetRelationalDatabaseLogEvents Lude.Text
grdleRelationalDatabaseName = Lens.lens (relationalDatabaseName :: GetRelationalDatabaseLogEvents -> Lude.Text) (\s a -> s {relationalDatabaseName = a} :: GetRelationalDatabaseLogEvents)
{-# DEPRECATED grdleRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

-- | The name of the log stream.
--
-- Use the @get relational database log streams@ operation to get a list of available log streams.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdleLogStreamName :: Lens.Lens' GetRelationalDatabaseLogEvents Lude.Text
grdleLogStreamName = Lens.lens (logStreamName :: GetRelationalDatabaseLogEvents -> Lude.Text) (\s a -> s {logStreamName = a} :: GetRelationalDatabaseLogEvents)
{-# DEPRECATED grdleLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

instance Lude.AWSRequest GetRelationalDatabaseLogEvents where
  type
    Rs GetRelationalDatabaseLogEvents =
      GetRelationalDatabaseLogEventsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseLogEventsResponse'
            Lude.<$> (x Lude..?> "nextBackwardToken")
            Lude.<*> (x Lude..?> "resourceLogEvents" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextForwardToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRelationalDatabaseLogEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.GetRelationalDatabaseLogEvents" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRelationalDatabaseLogEvents where
  toJSON GetRelationalDatabaseLogEvents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("startTime" Lude..=) Lude.<$> startTime,
            ("startFromHead" Lude..=) Lude.<$> startFromHead,
            ("endTime" Lude..=) Lude.<$> endTime,
            ("pageToken" Lude..=) Lude.<$> pageToken,
            Lude.Just
              ("relationalDatabaseName" Lude..= relationalDatabaseName),
            Lude.Just ("logStreamName" Lude..= logStreamName)
          ]
      )

instance Lude.ToPath GetRelationalDatabaseLogEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRelationalDatabaseLogEvents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRelationalDatabaseLogEventsResponse' smart constructor.
data GetRelationalDatabaseLogEventsResponse = GetRelationalDatabaseLogEventsResponse'
  { nextBackwardToken ::
      Lude.Maybe
        Lude.Text,
    resourceLogEvents ::
      Lude.Maybe
        [LogEvent],
    nextForwardToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRelationalDatabaseLogEventsResponse' with the minimum fields required to make a request.
--
-- * 'nextBackwardToken' - A token used for advancing to the previous page of results from your get relational database log events request.
-- * 'nextForwardToken' - A token used for advancing to the next page of results from your get relational database log events request.
-- * 'resourceLogEvents' - An object describing the result of your get relational database log events request.
-- * 'responseStatus' - The response status code.
mkGetRelationalDatabaseLogEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRelationalDatabaseLogEventsResponse
mkGetRelationalDatabaseLogEventsResponse pResponseStatus_ =
  GetRelationalDatabaseLogEventsResponse'
    { nextBackwardToken =
        Lude.Nothing,
      resourceLogEvents = Lude.Nothing,
      nextForwardToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A token used for advancing to the previous page of results from your get relational database log events request.
--
-- /Note:/ Consider using 'nextBackwardToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdlersNextBackwardToken :: Lens.Lens' GetRelationalDatabaseLogEventsResponse (Lude.Maybe Lude.Text)
grdlersNextBackwardToken = Lens.lens (nextBackwardToken :: GetRelationalDatabaseLogEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextBackwardToken = a} :: GetRelationalDatabaseLogEventsResponse)
{-# DEPRECATED grdlersNextBackwardToken "Use generic-lens or generic-optics with 'nextBackwardToken' instead." #-}

-- | An object describing the result of your get relational database log events request.
--
-- /Note:/ Consider using 'resourceLogEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdlersResourceLogEvents :: Lens.Lens' GetRelationalDatabaseLogEventsResponse (Lude.Maybe [LogEvent])
grdlersResourceLogEvents = Lens.lens (resourceLogEvents :: GetRelationalDatabaseLogEventsResponse -> Lude.Maybe [LogEvent]) (\s a -> s {resourceLogEvents = a} :: GetRelationalDatabaseLogEventsResponse)
{-# DEPRECATED grdlersResourceLogEvents "Use generic-lens or generic-optics with 'resourceLogEvents' instead." #-}

-- | A token used for advancing to the next page of results from your get relational database log events request.
--
-- /Note:/ Consider using 'nextForwardToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdlersNextForwardToken :: Lens.Lens' GetRelationalDatabaseLogEventsResponse (Lude.Maybe Lude.Text)
grdlersNextForwardToken = Lens.lens (nextForwardToken :: GetRelationalDatabaseLogEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextForwardToken = a} :: GetRelationalDatabaseLogEventsResponse)
{-# DEPRECATED grdlersNextForwardToken "Use generic-lens or generic-optics with 'nextForwardToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdlersResponseStatus :: Lens.Lens' GetRelationalDatabaseLogEventsResponse Lude.Int
grdlersResponseStatus = Lens.lens (responseStatus :: GetRelationalDatabaseLogEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRelationalDatabaseLogEventsResponse)
{-# DEPRECATED grdlersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
