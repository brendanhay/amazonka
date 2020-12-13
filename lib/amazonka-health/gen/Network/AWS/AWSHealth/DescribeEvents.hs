{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about events that meet the specified filter criteria. Events are returned in a summary form and do not include the detailed description, any additional metadata that depends on the event type, or any affected resources. To retrieve that information, use the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetails.html DescribeEventDetails> and <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntities.html DescribeAffectedEntities> operations.
--
-- If no filter criteria are specified, all events are returned. Results are sorted by @lastModifiedTime@ , starting with the most recent event.
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeEvents
  ( -- * Creating a request
    DescribeEvents (..),
    mkDescribeEvents,

    -- ** Request lenses
    deLocale,
    deNextToken,
    deFilter,
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

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { -- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
    locale :: Lude.Maybe Lude.Text,
    -- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Values to narrow the results returned.
    filter :: Lude.Maybe EventFilter,
    -- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEvents' with the minimum fields required to make a request.
--
-- * 'locale' - The locale (language) to return information in. English (en) is the default and the only supported value at this time.
-- * 'nextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
-- * 'filter' - Values to narrow the results returned.
-- * 'maxResults' - The maximum number of items to return in one batch, between 10 and 100, inclusive.
mkDescribeEvents ::
  DescribeEvents
mkDescribeEvents =
  DescribeEvents'
    { locale = Lude.Nothing,
      nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deLocale :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
deLocale = Lens.lens (locale :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {locale = a} :: DescribeEvents)
{-# DEPRECATED deLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deNextToken :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
deNextToken = Lens.lens (nextToken :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEvents)
{-# DEPRECATED deNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Values to narrow the results returned.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deFilter :: Lens.Lens' DescribeEvents (Lude.Maybe EventFilter)
deFilter = Lens.lens (filter :: DescribeEvents -> Lude.Maybe EventFilter) (\s a -> s {filter = a} :: DescribeEvents)
{-# DEPRECATED deFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMaxResults :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Natural)
deMaxResults = Lens.lens (maxResults :: DescribeEvents -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeEvents)
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
  request = Req.postJSON awsHealthService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEventsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "events" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSHealth_20160804.DescribeEvents" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEvents where
  toJSON DescribeEvents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("locale" Lude..=) Lude.<$> locale,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("filter" Lude..=) Lude.<$> filter,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEvents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { -- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The events that match the specified filter criteria.
    events :: Lude.Maybe [Event],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
-- * 'events' - The events that match the specified filter criteria.
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

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersNextToken :: Lens.Lens' DescribeEventsResponse (Lude.Maybe Lude.Text)
dersNextToken = Lens.lens (nextToken :: DescribeEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEventsResponse)
{-# DEPRECATED dersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The events that match the specified filter criteria.
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
