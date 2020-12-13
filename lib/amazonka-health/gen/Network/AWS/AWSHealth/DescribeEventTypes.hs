{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeEventTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the event types that meet the specified filter criteria. If no filter criteria are specified, all event types are returned, in no particular order.
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeEventTypes
  ( -- * Creating a request
    DescribeEventTypes (..),
    mkDescribeEventTypes,

    -- ** Request lenses
    detLocale,
    detNextToken,
    detFilter,
    detMaxResults,

    -- * Destructuring the response
    DescribeEventTypesResponse (..),
    mkDescribeEventTypesResponse,

    -- ** Response lenses
    detrsEventTypes,
    detrsNextToken,
    detrsResponseStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEventTypes' smart constructor.
data DescribeEventTypes = DescribeEventTypes'
  { -- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
    locale :: Lude.Maybe Lude.Text,
    -- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Values to narrow the results returned.
    filter :: Lude.Maybe EventTypeFilter,
    -- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventTypes' with the minimum fields required to make a request.
--
-- * 'locale' - The locale (language) to return information in. English (en) is the default and the only supported value at this time.
-- * 'nextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
-- * 'filter' - Values to narrow the results returned.
-- * 'maxResults' - The maximum number of items to return in one batch, between 10 and 100, inclusive.
mkDescribeEventTypes ::
  DescribeEventTypes
mkDescribeEventTypes =
  DescribeEventTypes'
    { locale = Lude.Nothing,
      nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detLocale :: Lens.Lens' DescribeEventTypes (Lude.Maybe Lude.Text)
detLocale = Lens.lens (locale :: DescribeEventTypes -> Lude.Maybe Lude.Text) (\s a -> s {locale = a} :: DescribeEventTypes)
{-# DEPRECATED detLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detNextToken :: Lens.Lens' DescribeEventTypes (Lude.Maybe Lude.Text)
detNextToken = Lens.lens (nextToken :: DescribeEventTypes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEventTypes)
{-# DEPRECATED detNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Values to narrow the results returned.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detFilter :: Lens.Lens' DescribeEventTypes (Lude.Maybe EventTypeFilter)
detFilter = Lens.lens (filter :: DescribeEventTypes -> Lude.Maybe EventTypeFilter) (\s a -> s {filter = a} :: DescribeEventTypes)
{-# DEPRECATED detFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detMaxResults :: Lens.Lens' DescribeEventTypes (Lude.Maybe Lude.Natural)
detMaxResults = Lens.lens (maxResults :: DescribeEventTypes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeEventTypes)
{-# DEPRECATED detMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeEventTypes where
  page rq rs
    | Page.stop (rs Lens.^. detrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. detrsEventTypes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& detNextToken Lens..~ rs Lens.^. detrsNextToken

instance Lude.AWSRequest DescribeEventTypes where
  type Rs DescribeEventTypes = DescribeEventTypesResponse
  request = Req.postJSON awsHealthService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEventTypesResponse'
            Lude.<$> (x Lude..?> "eventTypes" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEventTypes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSHealth_20160804.DescribeEventTypes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEventTypes where
  toJSON DescribeEventTypes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("locale" Lude..=) Lude.<$> locale,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("filter" Lude..=) Lude.<$> filter,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeEventTypes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEventTypes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEventTypesResponse' smart constructor.
data DescribeEventTypesResponse = DescribeEventTypesResponse'
  { -- | A list of event types that match the filter criteria. Event types have a category (@issue@ , @accountNotification@ , or @scheduledChange@ ), a service (for example, @EC2@ , @RDS@ , @DATAPIPELINE@ , @BILLING@ ), and a code (in the format @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ ).
    eventTypes :: Lude.Maybe [EventType],
    -- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventTypesResponse' with the minimum fields required to make a request.
--
-- * 'eventTypes' - A list of event types that match the filter criteria. Event types have a category (@issue@ , @accountNotification@ , or @scheduledChange@ ), a service (for example, @EC2@ , @RDS@ , @DATAPIPELINE@ , @BILLING@ ), and a code (in the format @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ ).
-- * 'nextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
-- * 'responseStatus' - The response status code.
mkDescribeEventTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventTypesResponse
mkDescribeEventTypesResponse pResponseStatus_ =
  DescribeEventTypesResponse'
    { eventTypes = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of event types that match the filter criteria. Event types have a category (@issue@ , @accountNotification@ , or @scheduledChange@ ), a service (for example, @EC2@ , @RDS@ , @DATAPIPELINE@ , @BILLING@ ), and a code (in the format @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ ).
--
-- /Note:/ Consider using 'eventTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsEventTypes :: Lens.Lens' DescribeEventTypesResponse (Lude.Maybe [EventType])
detrsEventTypes = Lens.lens (eventTypes :: DescribeEventTypesResponse -> Lude.Maybe [EventType]) (\s a -> s {eventTypes = a} :: DescribeEventTypesResponse)
{-# DEPRECATED detrsEventTypes "Use generic-lens or generic-optics with 'eventTypes' instead." #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsNextToken :: Lens.Lens' DescribeEventTypesResponse (Lude.Maybe Lude.Text)
detrsNextToken = Lens.lens (nextToken :: DescribeEventTypesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEventTypesResponse)
{-# DEPRECATED detrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsResponseStatus :: Lens.Lens' DescribeEventTypesResponse Lude.Int
detrsResponseStatus = Lens.lens (responseStatus :: DescribeEventTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventTypesResponse)
{-# DEPRECATED detrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
