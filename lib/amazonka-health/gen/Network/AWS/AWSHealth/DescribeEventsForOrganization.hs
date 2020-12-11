{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeEventsForOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about events across your organization in AWS Organizations. You can use the@filters@ parameter to specify the events that you want to return. Events are returned in a summary form and don't include the affected accounts, detailed description, any additional metadata that depends on the event type, or any affected resources. To retrieve that information, use the following operations:
--
--
--     * <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedAccountsForOrganization.html DescribeAffectedAccountsForOrganization>
--
--
--     * <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization>
--
--
--     * <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntitiesForOrganization.html DescribeAffectedEntitiesForOrganization>
--
--
-- If you don't specify a @filter@ , the @DescribeEventsForOrganizations@ returns all events across your organization. Results are sorted by @lastModifiedTime@ , starting with the most recent event.
-- For more information about the different types of AWS Health events, see <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event> .
-- Before you can call this operation, you must first enable AWS Health to work with AWS Organizations. To do this, call the <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization> operation from your organization's master AWS account.
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeEventsForOrganization
  ( -- * Creating a request
    DescribeEventsForOrganization (..),
    mkDescribeEventsForOrganization,

    -- ** Request lenses
    defoLocale,
    defoNextToken,
    defoFilter,
    defoMaxResults,

    -- * Destructuring the response
    DescribeEventsForOrganizationResponse (..),
    mkDescribeEventsForOrganizationResponse,

    -- ** Response lenses
    deforsNextToken,
    deforsEvents,
    deforsResponseStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEventsForOrganization' smart constructor.
data DescribeEventsForOrganization = DescribeEventsForOrganization'
  { locale ::
      Lude.Maybe Lude.Text,
    nextToken ::
      Lude.Maybe Lude.Text,
    filter ::
      Lude.Maybe
        OrganizationEventFilter,
    maxResults ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventsForOrganization' with the minimum fields required to make a request.
--
-- * 'filter' - Values to narrow the results returned.
-- * 'locale' - The locale (language) to return information in. English (en) is the default and the only supported value at this time.
-- * 'maxResults' - The maximum number of items to return in one batch, between 10 and 100, inclusive.
-- * 'nextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
mkDescribeEventsForOrganization ::
  DescribeEventsForOrganization
mkDescribeEventsForOrganization =
  DescribeEventsForOrganization'
    { locale = Lude.Nothing,
      nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defoLocale :: Lens.Lens' DescribeEventsForOrganization (Lude.Maybe Lude.Text)
defoLocale = Lens.lens (locale :: DescribeEventsForOrganization -> Lude.Maybe Lude.Text) (\s a -> s {locale = a} :: DescribeEventsForOrganization)
{-# DEPRECATED defoLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defoNextToken :: Lens.Lens' DescribeEventsForOrganization (Lude.Maybe Lude.Text)
defoNextToken = Lens.lens (nextToken :: DescribeEventsForOrganization -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEventsForOrganization)
{-# DEPRECATED defoNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Values to narrow the results returned.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defoFilter :: Lens.Lens' DescribeEventsForOrganization (Lude.Maybe OrganizationEventFilter)
defoFilter = Lens.lens (filter :: DescribeEventsForOrganization -> Lude.Maybe OrganizationEventFilter) (\s a -> s {filter = a} :: DescribeEventsForOrganization)
{-# DEPRECATED defoFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defoMaxResults :: Lens.Lens' DescribeEventsForOrganization (Lude.Maybe Lude.Natural)
defoMaxResults = Lens.lens (maxResults :: DescribeEventsForOrganization -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeEventsForOrganization)
{-# DEPRECATED defoMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeEventsForOrganization where
  page rq rs
    | Page.stop (rs Lens.^. deforsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. deforsEvents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& defoNextToken Lens..~ rs Lens.^. deforsNextToken

instance Lude.AWSRequest DescribeEventsForOrganization where
  type
    Rs DescribeEventsForOrganization =
      DescribeEventsForOrganizationResponse
  request = Req.postJSON awsHealthService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEventsForOrganizationResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "events" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEventsForOrganization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSHealth_20160804.DescribeEventsForOrganization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEventsForOrganization where
  toJSON DescribeEventsForOrganization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("locale" Lude..=) Lude.<$> locale,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("filter" Lude..=) Lude.<$> filter,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeEventsForOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEventsForOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEventsForOrganizationResponse' smart constructor.
data DescribeEventsForOrganizationResponse = DescribeEventsForOrganizationResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    events ::
      Lude.Maybe
        [OrganizationEvent],
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

-- | Creates a value of 'DescribeEventsForOrganizationResponse' with the minimum fields required to make a request.
--
-- * 'events' - The events that match the specified filter criteria.
-- * 'nextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
-- * 'responseStatus' - The response status code.
mkDescribeEventsForOrganizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventsForOrganizationResponse
mkDescribeEventsForOrganizationResponse pResponseStatus_ =
  DescribeEventsForOrganizationResponse'
    { nextToken = Lude.Nothing,
      events = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deforsNextToken :: Lens.Lens' DescribeEventsForOrganizationResponse (Lude.Maybe Lude.Text)
deforsNextToken = Lens.lens (nextToken :: DescribeEventsForOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEventsForOrganizationResponse)
{-# DEPRECATED deforsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The events that match the specified filter criteria.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deforsEvents :: Lens.Lens' DescribeEventsForOrganizationResponse (Lude.Maybe [OrganizationEvent])
deforsEvents = Lens.lens (events :: DescribeEventsForOrganizationResponse -> Lude.Maybe [OrganizationEvent]) (\s a -> s {events = a} :: DescribeEventsForOrganizationResponse)
{-# DEPRECATED deforsEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deforsResponseStatus :: Lens.Lens' DescribeEventsForOrganizationResponse Lude.Int
deforsResponseStatus = Lens.lens (responseStatus :: DescribeEventsForOrganizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventsForOrganizationResponse)
{-# DEPRECATED deforsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
