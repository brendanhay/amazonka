{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.DescribeEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes events for a specified server. Results are ordered by time, with newest events first.
--
-- This operation is synchronous.
-- A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
--
-- This operation returns paginated results.
module Network.AWS.OpsWorksCM.DescribeEvents
  ( -- * Creating a request
    DescribeEvents (..),
    mkDescribeEvents,

    -- ** Request lenses
    deServerName,
    deNextToken,
    deMaxResults,

    -- * Destructuring the response
    DescribeEventsResponse (..),
    mkDescribeEventsResponse,

    -- ** Response lenses
    dersServerEvents,
    dersNextToken,
    dersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { -- | The name of the server for which you want to view events.
    serverName :: Lude.Text,
    -- | NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeEvents@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
    nextToken :: Lude.Maybe Lude.Text,
    -- | To receive a paginated response, use this parameter to specify the maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEvents' with the minimum fields required to make a request.
--
-- * 'serverName' - The name of the server for which you want to view events.
-- * 'nextToken' - NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeEvents@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
-- * 'maxResults' - To receive a paginated response, use this parameter to specify the maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
mkDescribeEvents ::
  -- | 'serverName'
  Lude.Text ->
  DescribeEvents
mkDescribeEvents pServerName_ =
  DescribeEvents'
    { serverName = pServerName_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The name of the server for which you want to view events.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deServerName :: Lens.Lens' DescribeEvents Lude.Text
deServerName = Lens.lens (serverName :: DescribeEvents -> Lude.Text) (\s a -> s {serverName = a} :: DescribeEvents)
{-# DEPRECATED deServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeEvents@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deNextToken :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
deNextToken = Lens.lens (nextToken :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEvents)
{-# DEPRECATED deNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | To receive a paginated response, use this parameter to specify the maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMaxResults :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Natural)
deMaxResults = Lens.lens (maxResults :: DescribeEvents -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeEvents)
{-# DEPRECATED deMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeEvents where
  page rq rs
    | Page.stop (rs Lens.^. dersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dersServerEvents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& deNextToken Lens..~ rs Lens.^. dersNextToken

instance Lude.AWSRequest DescribeEvents where
  type Rs DescribeEvents = DescribeEventsResponse
  request = Req.postJSON opsWorksCMService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEventsResponse'
            Lude.<$> (x Lude..?> "ServerEvents" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorksCM_V2016_11_01.DescribeEvents" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEvents where
  toJSON DescribeEvents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ServerName" Lude..= serverName),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEvents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { -- | Contains the response to a @DescribeEvents@ request.
    serverEvents :: Lude.Maybe [ServerEvent],
    -- | NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeEvents@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventsResponse' with the minimum fields required to make a request.
--
-- * 'serverEvents' - Contains the response to a @DescribeEvents@ request.
-- * 'nextToken' - NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeEvents@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
-- * 'responseStatus' - The response status code.
mkDescribeEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventsResponse
mkDescribeEventsResponse pResponseStatus_ =
  DescribeEventsResponse'
    { serverEvents = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the response to a @DescribeEvents@ request.
--
-- /Note:/ Consider using 'serverEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersServerEvents :: Lens.Lens' DescribeEventsResponse (Lude.Maybe [ServerEvent])
dersServerEvents = Lens.lens (serverEvents :: DescribeEventsResponse -> Lude.Maybe [ServerEvent]) (\s a -> s {serverEvents = a} :: DescribeEventsResponse)
{-# DEPRECATED dersServerEvents "Use generic-lens or generic-optics with 'serverEvents' instead." #-}

-- | NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeEvents@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersNextToken :: Lens.Lens' DescribeEventsResponse (Lude.Maybe Lude.Text)
dersNextToken = Lens.lens (nextToken :: DescribeEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEventsResponse)
{-# DEPRECATED dersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResponseStatus :: Lens.Lens' DescribeEventsResponse Lude.Int
dersResponseStatus = Lens.lens (responseStatus :: DescribeEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventsResponse)
{-# DEPRECATED dersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
