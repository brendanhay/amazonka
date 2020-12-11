{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DescribeEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns events related to DAX clusters and parameter groups. You can obtain events specific to a particular DAX cluster or parameter group by providing the name as a parameter.
--
-- By default, only the events occurring within the last 24 hours are returned; however, you can retrieve up to 14 days' worth of events if necessary.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeEvents
  ( -- * Creating a request
    DescribeEvents (..),
    mkDescribeEvents,

    -- ** Request lenses
    deSourceName,
    deStartTime,
    deSourceType,
    deNextToken,
    deEndTime,
    deDuration,
    deMaxResults,

    -- * Destructuring the response
    DescribeEventsResponse (..),
    mkDescribeEventsResponse,

    -- ** Response lenses
    dersNextToken,
    dersEvents,
    dersResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { sourceName ::
      Lude.Maybe Lude.Text,
    startTime :: Lude.Maybe Lude.Timestamp,
    sourceType :: Lude.Maybe SourceType,
    nextToken :: Lude.Maybe Lude.Text,
    endTime :: Lude.Maybe Lude.Timestamp,
    duration :: Lude.Maybe Lude.Int,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEvents' with the minimum fields required to make a request.
--
-- * 'duration' - The number of minutes' worth of events to retrieve.
-- * 'endTime' - The end of the time interval for which to retrieve events, specified in ISO 8601 format.
-- * 'maxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
-- * 'sourceName' - The identifier of the event source for which events will be returned. If not specified, then all sources are included in the response.
-- * 'sourceType' - The event source to retrieve events for. If no value is specified, all events are returned.
-- * 'startTime' - The beginning of the time interval to retrieve events for, specified in ISO 8601 format.
mkDescribeEvents ::
  DescribeEvents
mkDescribeEvents =
  DescribeEvents'
    { sourceName = Lude.Nothing,
      startTime = Lude.Nothing,
      sourceType = Lude.Nothing,
      nextToken = Lude.Nothing,
      endTime = Lude.Nothing,
      duration = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The identifier of the event source for which events will be returned. If not specified, then all sources are included in the response.
--
-- /Note:/ Consider using 'sourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSourceName :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
deSourceName = Lens.lens (sourceName :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {sourceName = a} :: DescribeEvents)
{-# DEPRECATED deSourceName "Use generic-lens or generic-optics with 'sourceName' instead." #-}

-- | The beginning of the time interval to retrieve events for, specified in ISO 8601 format.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deStartTime :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Timestamp)
deStartTime = Lens.lens (startTime :: DescribeEvents -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: DescribeEvents)
{-# DEPRECATED deStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The event source to retrieve events for. If no value is specified, all events are returned.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSourceType :: Lens.Lens' DescribeEvents (Lude.Maybe SourceType)
deSourceType = Lens.lens (sourceType :: DescribeEvents -> Lude.Maybe SourceType) (\s a -> s {sourceType = a} :: DescribeEvents)
{-# DEPRECATED deSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deNextToken :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
deNextToken = Lens.lens (nextToken :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEvents)
{-# DEPRECATED deNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The end of the time interval for which to retrieve events, specified in ISO 8601 format.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndTime :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Timestamp)
deEndTime = Lens.lens (endTime :: DescribeEvents -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: DescribeEvents)
{-# DEPRECATED deEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The number of minutes' worth of events to retrieve.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDuration :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Int)
deDuration = Lens.lens (duration :: DescribeEvents -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: DescribeEvents)
{-# DEPRECATED deDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMaxResults :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Int)
deMaxResults = Lens.lens (maxResults :: DescribeEvents -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeEvents)
{-# DEPRECATED deMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeEvents where
  page rq rs
    | Page.stop (rs Lens.^. dersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dersEvents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& deNextToken Lens..~ rs Lens.^. dersNextToken

instance Lude.AWSRequest DescribeEvents where
  type Rs DescribeEvents = DescribeEventsResponse
  request = Req.postJSON daxService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEventsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Events" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDAXV3.DescribeEvents" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEvents where
  toJSON DescribeEvents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SourceName" Lude..=) Lude.<$> sourceName,
            ("StartTime" Lude..=) Lude.<$> startTime,
            ("SourceType" Lude..=) Lude.<$> sourceType,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("EndTime" Lude..=) Lude.<$> endTime,
            ("Duration" Lude..=) Lude.<$> duration,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEvents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    events :: Lude.Maybe [Event],
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

-- | Creates a value of 'DescribeEventsResponse' with the minimum fields required to make a request.
--
-- * 'events' - An array of events. Each element in the array represents one event.
-- * 'nextToken' - Provides an identifier to allow retrieval of paginated results.
-- * 'responseStatus' - The response status code.
mkDescribeEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventsResponse
mkDescribeEventsResponse pResponseStatus_ =
  DescribeEventsResponse'
    { nextToken = Lude.Nothing,
      events = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersNextToken :: Lens.Lens' DescribeEventsResponse (Lude.Maybe Lude.Text)
dersNextToken = Lens.lens (nextToken :: DescribeEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEventsResponse)
{-# DEPRECATED dersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of events. Each element in the array represents one event.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersEvents :: Lens.Lens' DescribeEventsResponse (Lude.Maybe [Event])
dersEvents = Lens.lens (events :: DescribeEventsResponse -> Lude.Maybe [Event]) (\s a -> s {events = a} :: DescribeEventsResponse)
{-# DEPRECATED dersEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResponseStatus :: Lens.Lens' DescribeEventsResponse Lude.Int
dersResponseStatus = Lens.lens (responseStatus :: DescribeEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventsResponse)
{-# DEPRECATED dersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
