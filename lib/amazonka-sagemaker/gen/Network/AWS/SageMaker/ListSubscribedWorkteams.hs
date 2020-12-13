{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListSubscribedWorkteams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the work teams that you are subscribed to in the AWS Marketplace. The list may be empty if no work team satisfies the filter specified in the @NameContains@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListSubscribedWorkteams
  ( -- * Creating a request
    ListSubscribedWorkteams (..),
    mkListSubscribedWorkteams,

    -- ** Request lenses
    lswNameContains,
    lswNextToken,
    lswMaxResults,

    -- * Destructuring the response
    ListSubscribedWorkteamsResponse (..),
    mkListSubscribedWorkteamsResponse,

    -- ** Response lenses
    lswrsNextToken,
    lswrsSubscribedWorkteams,
    lswrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListSubscribedWorkteams' smart constructor.
data ListSubscribedWorkteams = ListSubscribedWorkteams'
  { -- | A string in the work team name. This filter returns only work teams whose name contains the specified string.
    nameContains :: Lude.Maybe Lude.Text,
    -- | If the result of the previous @ListSubscribedWorkteams@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of work teams to return in each page of the response.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSubscribedWorkteams' with the minimum fields required to make a request.
--
-- * 'nameContains' - A string in the work team name. This filter returns only work teams whose name contains the specified string.
-- * 'nextToken' - If the result of the previous @ListSubscribedWorkteams@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
-- * 'maxResults' - The maximum number of work teams to return in each page of the response.
mkListSubscribedWorkteams ::
  ListSubscribedWorkteams
mkListSubscribedWorkteams =
  ListSubscribedWorkteams'
    { nameContains = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A string in the work team name. This filter returns only work teams whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lswNameContains :: Lens.Lens' ListSubscribedWorkteams (Lude.Maybe Lude.Text)
lswNameContains = Lens.lens (nameContains :: ListSubscribedWorkteams -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListSubscribedWorkteams)
{-# DEPRECATED lswNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the result of the previous @ListSubscribedWorkteams@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lswNextToken :: Lens.Lens' ListSubscribedWorkteams (Lude.Maybe Lude.Text)
lswNextToken = Lens.lens (nextToken :: ListSubscribedWorkteams -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSubscribedWorkteams)
{-# DEPRECATED lswNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of work teams to return in each page of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lswMaxResults :: Lens.Lens' ListSubscribedWorkteams (Lude.Maybe Lude.Natural)
lswMaxResults = Lens.lens (maxResults :: ListSubscribedWorkteams -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSubscribedWorkteams)
{-# DEPRECATED lswMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListSubscribedWorkteams where
  page rq rs
    | Page.stop (rs Lens.^. lswrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lswrsSubscribedWorkteams) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lswNextToken Lens..~ rs Lens.^. lswrsNextToken

instance Lude.AWSRequest ListSubscribedWorkteams where
  type Rs ListSubscribedWorkteams = ListSubscribedWorkteamsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSubscribedWorkteamsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "SubscribedWorkteams" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSubscribedWorkteams where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListSubscribedWorkteams" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSubscribedWorkteams where
  toJSON ListSubscribedWorkteams' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListSubscribedWorkteams where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSubscribedWorkteams where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSubscribedWorkteamsResponse' smart constructor.
data ListSubscribedWorkteamsResponse = ListSubscribedWorkteamsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of work teams, use it in the subsequent request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array of @Workteam@ objects, each describing a work team.
    subscribedWorkteams :: [SubscribedWorkteam],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSubscribedWorkteamsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of work teams, use it in the subsequent request.
-- * 'subscribedWorkteams' - An array of @Workteam@ objects, each describing a work team.
-- * 'responseStatus' - The response status code.
mkListSubscribedWorkteamsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSubscribedWorkteamsResponse
mkListSubscribedWorkteamsResponse pResponseStatus_ =
  ListSubscribedWorkteamsResponse'
    { nextToken = Lude.Nothing,
      subscribedWorkteams = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of work teams, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lswrsNextToken :: Lens.Lens' ListSubscribedWorkteamsResponse (Lude.Maybe Lude.Text)
lswrsNextToken = Lens.lens (nextToken :: ListSubscribedWorkteamsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSubscribedWorkteamsResponse)
{-# DEPRECATED lswrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of @Workteam@ objects, each describing a work team.
--
-- /Note:/ Consider using 'subscribedWorkteams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lswrsSubscribedWorkteams :: Lens.Lens' ListSubscribedWorkteamsResponse [SubscribedWorkteam]
lswrsSubscribedWorkteams = Lens.lens (subscribedWorkteams :: ListSubscribedWorkteamsResponse -> [SubscribedWorkteam]) (\s a -> s {subscribedWorkteams = a} :: ListSubscribedWorkteamsResponse)
{-# DEPRECATED lswrsSubscribedWorkteams "Use generic-lens or generic-optics with 'subscribedWorkteams' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lswrsResponseStatus :: Lens.Lens' ListSubscribedWorkteamsResponse Lude.Int
lswrsResponseStatus = Lens.lens (responseStatus :: ListSubscribedWorkteamsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSubscribedWorkteamsResponse)
{-# DEPRECATED lswrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
