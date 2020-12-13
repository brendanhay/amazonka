{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeEventAggregates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of events of each event type (issue, scheduled change, and account notification). If no filter is specified, the counts of all events in each category are returned.
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeEventAggregates
  ( -- * Creating a request
    DescribeEventAggregates (..),
    mkDescribeEventAggregates,

    -- ** Request lenses
    deaNextToken,
    deaFilter,
    deaAggregateField,
    deaMaxResults,

    -- * Destructuring the response
    DescribeEventAggregatesResponse (..),
    mkDescribeEventAggregatesResponse,

    -- ** Response lenses
    drsNextToken,
    drsEventAggregates,
    drsResponseStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEventAggregates' smart constructor.
data DescribeEventAggregates = DescribeEventAggregates'
  { -- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Values to narrow the results returned.
    filter :: Lude.Maybe EventFilter,
    -- | The only currently supported value is @eventTypeCategory@ .
    aggregateField :: EventAggregateField,
    -- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventAggregates' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
-- * 'filter' - Values to narrow the results returned.
-- * 'aggregateField' - The only currently supported value is @eventTypeCategory@ .
-- * 'maxResults' - The maximum number of items to return in one batch, between 10 and 100, inclusive.
mkDescribeEventAggregates ::
  -- | 'aggregateField'
  EventAggregateField ->
  DescribeEventAggregates
mkDescribeEventAggregates pAggregateField_ =
  DescribeEventAggregates'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      aggregateField = pAggregateField_,
      maxResults = Lude.Nothing
    }

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deaNextToken :: Lens.Lens' DescribeEventAggregates (Lude.Maybe Lude.Text)
deaNextToken = Lens.lens (nextToken :: DescribeEventAggregates -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEventAggregates)
{-# DEPRECATED deaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Values to narrow the results returned.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deaFilter :: Lens.Lens' DescribeEventAggregates (Lude.Maybe EventFilter)
deaFilter = Lens.lens (filter :: DescribeEventAggregates -> Lude.Maybe EventFilter) (\s a -> s {filter = a} :: DescribeEventAggregates)
{-# DEPRECATED deaFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The only currently supported value is @eventTypeCategory@ .
--
-- /Note:/ Consider using 'aggregateField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deaAggregateField :: Lens.Lens' DescribeEventAggregates EventAggregateField
deaAggregateField = Lens.lens (aggregateField :: DescribeEventAggregates -> EventAggregateField) (\s a -> s {aggregateField = a} :: DescribeEventAggregates)
{-# DEPRECATED deaAggregateField "Use generic-lens or generic-optics with 'aggregateField' instead." #-}

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deaMaxResults :: Lens.Lens' DescribeEventAggregates (Lude.Maybe Lude.Natural)
deaMaxResults = Lens.lens (maxResults :: DescribeEventAggregates -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeEventAggregates)
{-# DEPRECATED deaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeEventAggregates where
  page rq rs
    | Page.stop (rs Lens.^. drsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drsEventAggregates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& deaNextToken Lens..~ rs Lens.^. drsNextToken

instance Lude.AWSRequest DescribeEventAggregates where
  type Rs DescribeEventAggregates = DescribeEventAggregatesResponse
  request = Req.postJSON awsHealthService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEventAggregatesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "eventAggregates" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEventAggregates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSHealth_20160804.DescribeEventAggregates" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEventAggregates where
  toJSON DescribeEventAggregates' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("filter" Lude..=) Lude.<$> filter,
            Lude.Just ("aggregateField" Lude..= aggregateField),
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeEventAggregates where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEventAggregates where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEventAggregatesResponse' smart constructor.
data DescribeEventAggregatesResponse = DescribeEventAggregatesResponse'
  { -- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The number of events in each category that meet the optional filter criteria.
    eventAggregates :: Lude.Maybe [EventAggregate],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventAggregatesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
-- * 'eventAggregates' - The number of events in each category that meet the optional filter criteria.
-- * 'responseStatus' - The response status code.
mkDescribeEventAggregatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventAggregatesResponse
mkDescribeEventAggregatesResponse pResponseStatus_ =
  DescribeEventAggregatesResponse'
    { nextToken = Lude.Nothing,
      eventAggregates = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeEventAggregatesResponse (Lude.Maybe Lude.Text)
drsNextToken = Lens.lens (nextToken :: DescribeEventAggregatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEventAggregatesResponse)
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of events in each category that meet the optional filter criteria.
--
-- /Note:/ Consider using 'eventAggregates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEventAggregates :: Lens.Lens' DescribeEventAggregatesResponse (Lude.Maybe [EventAggregate])
drsEventAggregates = Lens.lens (eventAggregates :: DescribeEventAggregatesResponse -> Lude.Maybe [EventAggregate]) (\s a -> s {eventAggregates = a} :: DescribeEventAggregatesResponse)
{-# DEPRECATED drsEventAggregates "Use generic-lens or generic-optics with 'eventAggregates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeEventAggregatesResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeEventAggregatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventAggregatesResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
