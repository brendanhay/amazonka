{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all active sessions (both connected and disconnected) or terminated sessions from the past 30 days.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeSessions
  ( -- * Creating a request
    DescribeSessions (..),
    mkDescribeSessions,

    -- ** Request lenses
    dsState,
    dsFilters,
    dsNextToken,
    dsMaxResults,

    -- * Destructuring the response
    DescribeSessionsResponse (..),
    mkDescribeSessionsResponse,

    -- ** Response lenses
    dsrsNextToken,
    dsrsSessions,
    dsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeSessions' smart constructor.
data DescribeSessions = DescribeSessions'
  { -- | The session status to retrieve a list of sessions for. For example, "Active".
    state :: SessionState,
    -- | One or more filters to limit the type of sessions returned by the request.
    filters :: Lude.Maybe (Lude.NonEmpty SessionFilter),
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSessions' with the minimum fields required to make a request.
--
-- * 'state' - The session status to retrieve a list of sessions for. For example, "Active".
-- * 'filters' - One or more filters to limit the type of sessions returned by the request.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
mkDescribeSessions ::
  -- | 'state'
  SessionState ->
  DescribeSessions
mkDescribeSessions pState_ =
  DescribeSessions'
    { state = pState_,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The session status to retrieve a list of sessions for. For example, "Active".
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsState :: Lens.Lens' DescribeSessions SessionState
dsState = Lens.lens (state :: DescribeSessions -> SessionState) (\s a -> s {state = a} :: DescribeSessions)
{-# DEPRECATED dsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | One or more filters to limit the type of sessions returned by the request.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFilters :: Lens.Lens' DescribeSessions (Lude.Maybe (Lude.NonEmpty SessionFilter))
dsFilters = Lens.lens (filters :: DescribeSessions -> Lude.Maybe (Lude.NonEmpty SessionFilter)) (\s a -> s {filters = a} :: DescribeSessions)
{-# DEPRECATED dsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNextToken :: Lens.Lens' DescribeSessions (Lude.Maybe Lude.Text)
dsNextToken = Lens.lens (nextToken :: DescribeSessions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSessions)
{-# DEPRECATED dsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMaxResults :: Lens.Lens' DescribeSessions (Lude.Maybe Lude.Natural)
dsMaxResults = Lens.lens (maxResults :: DescribeSessions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeSessions)
{-# DEPRECATED dsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeSessions where
  page rq rs
    | Page.stop (rs Lens.^. dsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsrsSessions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsNextToken Lens..~ rs Lens.^. dsrsNextToken

instance Lude.AWSRequest DescribeSessions where
  type Rs DescribeSessions = DescribeSessionsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSessionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Sessions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSessions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeSessions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSessions where
  toJSON DescribeSessions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("State" Lude..= state),
            ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeSessions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSessions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeSessionsResponse' smart constructor.
data DescribeSessionsResponse = DescribeSessionsResponse'
  { -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of sessions meeting the request parameters.
    sessions :: Lude.Maybe [Session],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSessionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'sessions' - A list of sessions meeting the request parameters.
-- * 'responseStatus' - The response status code.
mkDescribeSessionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSessionsResponse
mkDescribeSessionsResponse pResponseStatus_ =
  DescribeSessionsResponse'
    { nextToken = Lude.Nothing,
      sessions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsNextToken :: Lens.Lens' DescribeSessionsResponse (Lude.Maybe Lude.Text)
dsrsNextToken = Lens.lens (nextToken :: DescribeSessionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSessionsResponse)
{-# DEPRECATED dsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of sessions meeting the request parameters.
--
-- /Note:/ Consider using 'sessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsSessions :: Lens.Lens' DescribeSessionsResponse (Lude.Maybe [Session])
dsrsSessions = Lens.lens (sessions :: DescribeSessionsResponse -> Lude.Maybe [Session]) (\s a -> s {sessions = a} :: DescribeSessionsResponse)
{-# DEPRECATED dsrsSessions "Use generic-lens or generic-optics with 'sessions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DescribeSessionsResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DescribeSessionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSessionsResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
