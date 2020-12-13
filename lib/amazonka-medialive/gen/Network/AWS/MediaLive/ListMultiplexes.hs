{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.ListMultiplexes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of the existing multiplexes.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListMultiplexes
  ( -- * Creating a request
    ListMultiplexes (..),
    mkListMultiplexes,

    -- ** Request lenses
    lmNextToken,
    lmMaxResults,

    -- * Destructuring the response
    ListMultiplexesResponse (..),
    mkListMultiplexesResponse,

    -- ** Response lenses
    lmrsNextToken,
    lmrsMultiplexes,
    lmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for ListMultiplexesRequest
--
-- /See:/ 'mkListMultiplexes' smart constructor.
data ListMultiplexes = ListMultiplexes'
  { -- | The token to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMultiplexes' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to retrieve the next page of results.
-- * 'maxResults' - The maximum number of items to return.
mkListMultiplexes ::
  ListMultiplexes
mkListMultiplexes =
  ListMultiplexes'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmNextToken :: Lens.Lens' ListMultiplexes (Lude.Maybe Lude.Text)
lmNextToken = Lens.lens (nextToken :: ListMultiplexes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMultiplexes)
{-# DEPRECATED lmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmMaxResults :: Lens.Lens' ListMultiplexes (Lude.Maybe Lude.Natural)
lmMaxResults = Lens.lens (maxResults :: ListMultiplexes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListMultiplexes)
{-# DEPRECATED lmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListMultiplexes where
  page rq rs
    | Page.stop (rs Lens.^. lmrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lmrsMultiplexes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lmNextToken Lens..~ rs Lens.^. lmrsNextToken

instance Lude.AWSRequest ListMultiplexes where
  type Rs ListMultiplexes = ListMultiplexesResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListMultiplexesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "multiplexes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListMultiplexes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListMultiplexes where
  toPath = Lude.const "/prod/multiplexes"

instance Lude.ToQuery ListMultiplexes where
  toQuery ListMultiplexes' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | Placeholder documentation for ListMultiplexesResponse
--
-- /See:/ 'mkListMultiplexesResponse' smart constructor.
data ListMultiplexesResponse = ListMultiplexesResponse'
  { -- | Token for the next ListMultiplexes request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | List of multiplexes.
    multiplexes :: Lude.Maybe [MultiplexSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMultiplexesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Token for the next ListMultiplexes request.
-- * 'multiplexes' - List of multiplexes.
-- * 'responseStatus' - The response status code.
mkListMultiplexesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMultiplexesResponse
mkListMultiplexesResponse pResponseStatus_ =
  ListMultiplexesResponse'
    { nextToken = Lude.Nothing,
      multiplexes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Token for the next ListMultiplexes request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrsNextToken :: Lens.Lens' ListMultiplexesResponse (Lude.Maybe Lude.Text)
lmrsNextToken = Lens.lens (nextToken :: ListMultiplexesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMultiplexesResponse)
{-# DEPRECATED lmrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of multiplexes.
--
-- /Note:/ Consider using 'multiplexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrsMultiplexes :: Lens.Lens' ListMultiplexesResponse (Lude.Maybe [MultiplexSummary])
lmrsMultiplexes = Lens.lens (multiplexes :: ListMultiplexesResponse -> Lude.Maybe [MultiplexSummary]) (\s a -> s {multiplexes = a} :: ListMultiplexesResponse)
{-# DEPRECATED lmrsMultiplexes "Use generic-lens or generic-optics with 'multiplexes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrsResponseStatus :: Lens.Lens' ListMultiplexesResponse Lude.Int
lmrsResponseStatus = Lens.lens (responseStatus :: ListMultiplexesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMultiplexesResponse)
{-# DEPRECATED lmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
