{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.ListStateMachines
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing state machines.
--
-- If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- This operation returns paginated results.
module Network.AWS.StepFunctions.ListStateMachines
  ( -- * Creating a request
    ListStateMachines (..),
    mkListStateMachines,

    -- ** Request lenses
    lsmNextToken,
    lsmMaxResults,

    -- * Destructuring the response
    ListStateMachinesResponse (..),
    mkListStateMachinesResponse,

    -- ** Response lenses
    lsmrsStateMachines,
    lsmrsNextToken,
    lsmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkListStateMachines' smart constructor.
data ListStateMachines = ListStateMachines'
  { -- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
    --
    -- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStateMachines' with the minimum fields required to make a request.
--
-- * 'nextToken' - If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
-- * 'maxResults' - The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
mkListStateMachines ::
  ListStateMachines
mkListStateMachines =
  ListStateMachines'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmNextToken :: Lens.Lens' ListStateMachines (Lude.Maybe Lude.Text)
lsmNextToken = Lens.lens (nextToken :: ListStateMachines -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStateMachines)
{-# DEPRECATED lsmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmMaxResults :: Lens.Lens' ListStateMachines (Lude.Maybe Lude.Natural)
lsmMaxResults = Lens.lens (maxResults :: ListStateMachines -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListStateMachines)
{-# DEPRECATED lsmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListStateMachines where
  page rq rs
    | Page.stop (rs Lens.^. lsmrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsmrsStateMachines) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsmNextToken Lens..~ rs Lens.^. lsmrsNextToken

instance Lude.AWSRequest ListStateMachines where
  type Rs ListStateMachines = ListStateMachinesResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListStateMachinesResponse'
            Lude.<$> (x Lude..?> "stateMachines" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStateMachines where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSStepFunctions.ListStateMachines" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListStateMachines where
  toJSON ListStateMachines' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListStateMachines where
  toPath = Lude.const "/"

instance Lude.ToQuery ListStateMachines where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListStateMachinesResponse' smart constructor.
data ListStateMachinesResponse = ListStateMachinesResponse'
  { stateMachines :: [StateMachineListItem],
    -- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStateMachinesResponse' with the minimum fields required to make a request.
--
-- * 'stateMachines' -
-- * 'nextToken' - If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
-- * 'responseStatus' - The response status code.
mkListStateMachinesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListStateMachinesResponse
mkListStateMachinesResponse pResponseStatus_ =
  ListStateMachinesResponse'
    { stateMachines = Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'stateMachines' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmrsStateMachines :: Lens.Lens' ListStateMachinesResponse [StateMachineListItem]
lsmrsStateMachines = Lens.lens (stateMachines :: ListStateMachinesResponse -> [StateMachineListItem]) (\s a -> s {stateMachines = a} :: ListStateMachinesResponse)
{-# DEPRECATED lsmrsStateMachines "Use generic-lens or generic-optics with 'stateMachines' instead." #-}

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmrsNextToken :: Lens.Lens' ListStateMachinesResponse (Lude.Maybe Lude.Text)
lsmrsNextToken = Lens.lens (nextToken :: ListStateMachinesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStateMachinesResponse)
{-# DEPRECATED lsmrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmrsResponseStatus :: Lens.Lens' ListStateMachinesResponse Lude.Int
lsmrsResponseStatus = Lens.lens (responseStatus :: ListStateMachinesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStateMachinesResponse)
{-# DEPRECATED lsmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
