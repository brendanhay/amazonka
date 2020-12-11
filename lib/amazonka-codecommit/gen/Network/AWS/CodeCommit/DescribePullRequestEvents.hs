{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.DescribePullRequestEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more pull request events.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.DescribePullRequestEvents
  ( -- * Creating a request
    DescribePullRequestEvents (..),
    mkDescribePullRequestEvents,

    -- ** Request lenses
    dprePullRequestEventType,
    dpreActorARN,
    dpreNextToken,
    dpreMaxResults,
    dprePullRequestId,

    -- * Destructuring the response
    DescribePullRequestEventsResponse (..),
    mkDescribePullRequestEventsResponse,

    -- ** Response lenses
    dprersNextToken,
    dprersResponseStatus,
    dprersPullRequestEvents,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribePullRequestEvents' smart constructor.
data DescribePullRequestEvents = DescribePullRequestEvents'
  { pullRequestEventType ::
      Lude.Maybe PullRequestEventType,
    actorARN :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int,
    pullRequestId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePullRequestEvents' with the minimum fields required to make a request.
--
-- * 'actorARN' - The Amazon Resource Name (ARN) of the user whose actions resulted in the event. Examples include updating the pull request with more commits or changing the status of a pull request.
-- * 'maxResults' - A non-zero, non-negative integer used to limit the number of returned results. The default is 100 events, which is also the maximum number of events that can be returned in a result.
-- * 'nextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
-- * 'pullRequestEventType' - Optional. The pull request event type about which you want to return information.
-- * 'pullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
mkDescribePullRequestEvents ::
  -- | 'pullRequestId'
  Lude.Text ->
  DescribePullRequestEvents
mkDescribePullRequestEvents pPullRequestId_ =
  DescribePullRequestEvents'
    { pullRequestEventType = Lude.Nothing,
      actorARN = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      pullRequestId = pPullRequestId_
    }

-- | Optional. The pull request event type about which you want to return information.
--
-- /Note:/ Consider using 'pullRequestEventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprePullRequestEventType :: Lens.Lens' DescribePullRequestEvents (Lude.Maybe PullRequestEventType)
dprePullRequestEventType = Lens.lens (pullRequestEventType :: DescribePullRequestEvents -> Lude.Maybe PullRequestEventType) (\s a -> s {pullRequestEventType = a} :: DescribePullRequestEvents)
{-# DEPRECATED dprePullRequestEventType "Use generic-lens or generic-optics with 'pullRequestEventType' instead." #-}

-- | The Amazon Resource Name (ARN) of the user whose actions resulted in the event. Examples include updating the pull request with more commits or changing the status of a pull request.
--
-- /Note:/ Consider using 'actorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpreActorARN :: Lens.Lens' DescribePullRequestEvents (Lude.Maybe Lude.Text)
dpreActorARN = Lens.lens (actorARN :: DescribePullRequestEvents -> Lude.Maybe Lude.Text) (\s a -> s {actorARN = a} :: DescribePullRequestEvents)
{-# DEPRECATED dpreActorARN "Use generic-lens or generic-optics with 'actorARN' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpreNextToken :: Lens.Lens' DescribePullRequestEvents (Lude.Maybe Lude.Text)
dpreNextToken = Lens.lens (nextToken :: DescribePullRequestEvents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePullRequestEvents)
{-# DEPRECATED dpreNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A non-zero, non-negative integer used to limit the number of returned results. The default is 100 events, which is also the maximum number of events that can be returned in a result.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpreMaxResults :: Lens.Lens' DescribePullRequestEvents (Lude.Maybe Lude.Int)
dpreMaxResults = Lens.lens (maxResults :: DescribePullRequestEvents -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribePullRequestEvents)
{-# DEPRECATED dpreMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprePullRequestId :: Lens.Lens' DescribePullRequestEvents Lude.Text
dprePullRequestId = Lens.lens (pullRequestId :: DescribePullRequestEvents -> Lude.Text) (\s a -> s {pullRequestId = a} :: DescribePullRequestEvents)
{-# DEPRECATED dprePullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

instance Page.AWSPager DescribePullRequestEvents where
  page rq rs
    | Page.stop (rs Lens.^. dprersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dprersPullRequestEvents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dpreNextToken Lens..~ rs Lens.^. dprersNextToken

instance Lude.AWSRequest DescribePullRequestEvents where
  type
    Rs DescribePullRequestEvents =
      DescribePullRequestEventsResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePullRequestEventsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "pullRequestEvents" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders DescribePullRequestEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.DescribePullRequestEvents" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribePullRequestEvents where
  toJSON DescribePullRequestEvents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("pullRequestEventType" Lude..=) Lude.<$> pullRequestEventType,
            ("actorArn" Lude..=) Lude.<$> actorARN,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("pullRequestId" Lude..= pullRequestId)
          ]
      )

instance Lude.ToPath DescribePullRequestEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePullRequestEvents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribePullRequestEventsResponse' smart constructor.
data DescribePullRequestEventsResponse = DescribePullRequestEventsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int,
    pullRequestEvents ::
      [PullRequestEvent]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePullRequestEventsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An enumeration token that can be used in a request to return the next batch of the results.
-- * 'pullRequestEvents' - Information about the pull request events.
-- * 'responseStatus' - The response status code.
mkDescribePullRequestEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePullRequestEventsResponse
mkDescribePullRequestEventsResponse pResponseStatus_ =
  DescribePullRequestEventsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      pullRequestEvents = Lude.mempty
    }

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprersNextToken :: Lens.Lens' DescribePullRequestEventsResponse (Lude.Maybe Lude.Text)
dprersNextToken = Lens.lens (nextToken :: DescribePullRequestEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePullRequestEventsResponse)
{-# DEPRECATED dprersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprersResponseStatus :: Lens.Lens' DescribePullRequestEventsResponse Lude.Int
dprersResponseStatus = Lens.lens (responseStatus :: DescribePullRequestEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePullRequestEventsResponse)
{-# DEPRECATED dprersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Information about the pull request events.
--
-- /Note:/ Consider using 'pullRequestEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprersPullRequestEvents :: Lens.Lens' DescribePullRequestEventsResponse [PullRequestEvent]
dprersPullRequestEvents = Lens.lens (pullRequestEvents :: DescribePullRequestEventsResponse -> [PullRequestEvent]) (\s a -> s {pullRequestEvents = a} :: DescribePullRequestEventsResponse)
{-# DEPRECATED dprersPullRequestEvents "Use generic-lens or generic-optics with 'pullRequestEvents' instead." #-}
