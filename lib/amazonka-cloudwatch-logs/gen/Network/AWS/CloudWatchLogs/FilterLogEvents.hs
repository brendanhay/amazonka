{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.FilterLogEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists log events from the specified log group. You can list all the log events or filter the results using a filter pattern, a time range, and the name of the log stream.
--
-- By default, this operation returns as many log events as can fit in 1 MB (up to 10,000 log events) or all the events found within the time range that you specify. If the results include a token, then there are more log events available, and you can get additional results by specifying the token in a subsequent call. This operation can return empty results while there are more log events available through the token.
-- The returned log events are sorted by event timestamp, the timestamp when the event was ingested by CloudWatch Logs, and the ID of the @PutLogEvents@ request.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.FilterLogEvents
  ( -- * Creating a request
    FilterLogEvents (..),
    mkFilterLogEvents,

    -- ** Request lenses
    fleStartTime,
    fleNextToken,
    fleLogStreamNames,
    fleLogStreamNamePrefix,
    fleEndTime,
    fleLimit,
    fleFilterPattern,
    fleInterleaved,
    fleLogGroupName,

    -- * Destructuring the response
    FilterLogEventsResponse (..),
    mkFilterLogEventsResponse,

    -- ** Response lenses
    flersSearchedLogStreams,
    flersNextToken,
    flersEvents,
    flersResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkFilterLogEvents' smart constructor.
data FilterLogEvents = FilterLogEvents'
  { startTime ::
      Lude.Maybe Lude.Natural,
    nextToken :: Lude.Maybe Lude.Text,
    logStreamNames :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    logStreamNamePrefix :: Lude.Maybe Lude.Text,
    endTime :: Lude.Maybe Lude.Natural,
    limit :: Lude.Maybe Lude.Natural,
    filterPattern :: Lude.Maybe Lude.Text,
    interleaved :: Lude.Maybe Lude.Bool,
    logGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FilterLogEvents' with the minimum fields required to make a request.
--
-- * 'endTime' - The end of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time are not returned.
-- * 'filterPattern' - The filter pattern to use. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/FilterAndPatternSyntax.html Filter and Pattern Syntax> .
--
-- If not provided, all the events are matched.
-- * 'interleaved' - If the value is true, the operation makes a best effort to provide responses that contain events from multiple log streams within the log group, interleaved in a single response. If the value is false, all the matched log events in the first log stream are searched first, then those in the next log stream, and so on. The default is false.
--
-- __Important:__ Starting on June 17, 2019, this parameter is ignored and the value is assumed to be true. The response from this operation always interleaves events from multiple log streams within a log group.
-- * 'limit' - The maximum number of events to return. The default is 10,000 events.
-- * 'logGroupName' - The name of the log group to search.
-- * 'logStreamNamePrefix' - Filters the results to include only events from log streams that have names starting with this prefix.
--
-- If you specify a value for both @logStreamNamePrefix@ and @logStreamNames@ , but the value for @logStreamNamePrefix@ does not match any log stream names specified in @logStreamNames@ , the action returns an @InvalidParameterException@ error.
-- * 'logStreamNames' - Filters the results to only logs from the log streams in this list.
--
-- If you specify a value for both @logStreamNamePrefix@ and @logStreamNames@ , the action returns an @InvalidParameterException@ error.
-- * 'nextToken' - The token for the next set of events to return. (You received this token from a previous call.)
-- * 'startTime' - The start of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp before this time are not returned.
--
-- If you omit @startTime@ and @endTime@ the most recent log events are retrieved, to up 1 MB or 10,000 log events.
mkFilterLogEvents ::
  -- | 'logGroupName'
  Lude.Text ->
  FilterLogEvents
mkFilterLogEvents pLogGroupName_ =
  FilterLogEvents'
    { startTime = Lude.Nothing,
      nextToken = Lude.Nothing,
      logStreamNames = Lude.Nothing,
      logStreamNamePrefix = Lude.Nothing,
      endTime = Lude.Nothing,
      limit = Lude.Nothing,
      filterPattern = Lude.Nothing,
      interleaved = Lude.Nothing,
      logGroupName = pLogGroupName_
    }

-- | The start of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp before this time are not returned.
--
-- If you omit @startTime@ and @endTime@ the most recent log events are retrieved, to up 1 MB or 10,000 log events.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleStartTime :: Lens.Lens' FilterLogEvents (Lude.Maybe Lude.Natural)
fleStartTime = Lens.lens (startTime :: FilterLogEvents -> Lude.Maybe Lude.Natural) (\s a -> s {startTime = a} :: FilterLogEvents)
{-# DEPRECATED fleStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The token for the next set of events to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleNextToken :: Lens.Lens' FilterLogEvents (Lude.Maybe Lude.Text)
fleNextToken = Lens.lens (nextToken :: FilterLogEvents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: FilterLogEvents)
{-# DEPRECATED fleNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters the results to only logs from the log streams in this list.
--
-- If you specify a value for both @logStreamNamePrefix@ and @logStreamNames@ , the action returns an @InvalidParameterException@ error.
--
-- /Note:/ Consider using 'logStreamNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleLogStreamNames :: Lens.Lens' FilterLogEvents (Lude.Maybe (Lude.NonEmpty Lude.Text))
fleLogStreamNames = Lens.lens (logStreamNames :: FilterLogEvents -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {logStreamNames = a} :: FilterLogEvents)
{-# DEPRECATED fleLogStreamNames "Use generic-lens or generic-optics with 'logStreamNames' instead." #-}

-- | Filters the results to include only events from log streams that have names starting with this prefix.
--
-- If you specify a value for both @logStreamNamePrefix@ and @logStreamNames@ , but the value for @logStreamNamePrefix@ does not match any log stream names specified in @logStreamNames@ , the action returns an @InvalidParameterException@ error.
--
-- /Note:/ Consider using 'logStreamNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleLogStreamNamePrefix :: Lens.Lens' FilterLogEvents (Lude.Maybe Lude.Text)
fleLogStreamNamePrefix = Lens.lens (logStreamNamePrefix :: FilterLogEvents -> Lude.Maybe Lude.Text) (\s a -> s {logStreamNamePrefix = a} :: FilterLogEvents)
{-# DEPRECATED fleLogStreamNamePrefix "Use generic-lens or generic-optics with 'logStreamNamePrefix' instead." #-}

-- | The end of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time are not returned.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleEndTime :: Lens.Lens' FilterLogEvents (Lude.Maybe Lude.Natural)
fleEndTime = Lens.lens (endTime :: FilterLogEvents -> Lude.Maybe Lude.Natural) (\s a -> s {endTime = a} :: FilterLogEvents)
{-# DEPRECATED fleEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The maximum number of events to return. The default is 10,000 events.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleLimit :: Lens.Lens' FilterLogEvents (Lude.Maybe Lude.Natural)
fleLimit = Lens.lens (limit :: FilterLogEvents -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: FilterLogEvents)
{-# DEPRECATED fleLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The filter pattern to use. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/FilterAndPatternSyntax.html Filter and Pattern Syntax> .
--
-- If not provided, all the events are matched.
--
-- /Note:/ Consider using 'filterPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleFilterPattern :: Lens.Lens' FilterLogEvents (Lude.Maybe Lude.Text)
fleFilterPattern = Lens.lens (filterPattern :: FilterLogEvents -> Lude.Maybe Lude.Text) (\s a -> s {filterPattern = a} :: FilterLogEvents)
{-# DEPRECATED fleFilterPattern "Use generic-lens or generic-optics with 'filterPattern' instead." #-}

-- | If the value is true, the operation makes a best effort to provide responses that contain events from multiple log streams within the log group, interleaved in a single response. If the value is false, all the matched log events in the first log stream are searched first, then those in the next log stream, and so on. The default is false.
--
-- __Important:__ Starting on June 17, 2019, this parameter is ignored and the value is assumed to be true. The response from this operation always interleaves events from multiple log streams within a log group.
--
-- /Note:/ Consider using 'interleaved' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleInterleaved :: Lens.Lens' FilterLogEvents (Lude.Maybe Lude.Bool)
fleInterleaved = Lens.lens (interleaved :: FilterLogEvents -> Lude.Maybe Lude.Bool) (\s a -> s {interleaved = a} :: FilterLogEvents)
{-# DEPRECATED fleInterleaved "Use generic-lens or generic-optics with 'interleaved' instead." #-}

-- | The name of the log group to search.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleLogGroupName :: Lens.Lens' FilterLogEvents Lude.Text
fleLogGroupName = Lens.lens (logGroupName :: FilterLogEvents -> Lude.Text) (\s a -> s {logGroupName = a} :: FilterLogEvents)
{-# DEPRECATED fleLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

instance Page.AWSPager FilterLogEvents where
  page rq rs
    | Page.stop (rs Lens.^. flersNextToken) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& fleNextToken Lens..~ rs Lens.^. flersNextToken

instance Lude.AWSRequest FilterLogEvents where
  type Rs FilterLogEvents = FilterLogEventsResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          FilterLogEventsResponse'
            Lude.<$> (x Lude..?> "searchedLogStreams" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "events" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders FilterLogEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.FilterLogEvents" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON FilterLogEvents where
  toJSON FilterLogEvents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("startTime" Lude..=) Lude.<$> startTime,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("logStreamNames" Lude..=) Lude.<$> logStreamNames,
            ("logStreamNamePrefix" Lude..=) Lude.<$> logStreamNamePrefix,
            ("endTime" Lude..=) Lude.<$> endTime,
            ("limit" Lude..=) Lude.<$> limit,
            ("filterPattern" Lude..=) Lude.<$> filterPattern,
            ("interleaved" Lude..=) Lude.<$> interleaved,
            Lude.Just ("logGroupName" Lude..= logGroupName)
          ]
      )

instance Lude.ToPath FilterLogEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery FilterLogEvents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkFilterLogEventsResponse' smart constructor.
data FilterLogEventsResponse = FilterLogEventsResponse'
  { searchedLogStreams ::
      Lude.Maybe [SearchedLogStream],
    nextToken :: Lude.Maybe Lude.Text,
    events :: Lude.Maybe [FilteredLogEvent],
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

-- | Creates a value of 'FilterLogEventsResponse' with the minimum fields required to make a request.
--
-- * 'events' - The matched events.
-- * 'nextToken' - The token to use when requesting the next set of items. The token expires after 24 hours.
-- * 'responseStatus' - The response status code.
-- * 'searchedLogStreams' - __IMPORTANT__ Starting on May 15, 2020, this parameter will be deprecated. This parameter will be an empty list after the deprecation occurs.
--
-- Indicates which log streams have been searched and whether each has been searched completely.
mkFilterLogEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  FilterLogEventsResponse
mkFilterLogEventsResponse pResponseStatus_ =
  FilterLogEventsResponse'
    { searchedLogStreams = Lude.Nothing,
      nextToken = Lude.Nothing,
      events = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | __IMPORTANT__ Starting on May 15, 2020, this parameter will be deprecated. This parameter will be an empty list after the deprecation occurs.
--
-- Indicates which log streams have been searched and whether each has been searched completely.
--
-- /Note:/ Consider using 'searchedLogStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flersSearchedLogStreams :: Lens.Lens' FilterLogEventsResponse (Lude.Maybe [SearchedLogStream])
flersSearchedLogStreams = Lens.lens (searchedLogStreams :: FilterLogEventsResponse -> Lude.Maybe [SearchedLogStream]) (\s a -> s {searchedLogStreams = a} :: FilterLogEventsResponse)
{-# DEPRECATED flersSearchedLogStreams "Use generic-lens or generic-optics with 'searchedLogStreams' instead." #-}

-- | The token to use when requesting the next set of items. The token expires after 24 hours.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flersNextToken :: Lens.Lens' FilterLogEventsResponse (Lude.Maybe Lude.Text)
flersNextToken = Lens.lens (nextToken :: FilterLogEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: FilterLogEventsResponse)
{-# DEPRECATED flersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The matched events.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flersEvents :: Lens.Lens' FilterLogEventsResponse (Lude.Maybe [FilteredLogEvent])
flersEvents = Lens.lens (events :: FilterLogEventsResponse -> Lude.Maybe [FilteredLogEvent]) (\s a -> s {events = a} :: FilterLogEventsResponse)
{-# DEPRECATED flersEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flersResponseStatus :: Lens.Lens' FilterLogEventsResponse Lude.Int
flersResponseStatus = Lens.lens (responseStatus :: FilterLogEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: FilterLogEventsResponse)
{-# DEPRECATED flersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
