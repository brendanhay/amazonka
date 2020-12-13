{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the streams in your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListStreams
  ( -- * Creating a request
    ListStreams (..),
    mkListStreams,

    -- ** Request lenses
    lsNextToken,
    lsAscendingOrder,
    lsMaxResults,

    -- * Destructuring the response
    ListStreamsResponse (..),
    mkListStreamsResponse,

    -- ** Response lenses
    lsrsNextToken,
    lsrsStreams,
    lsrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListStreams' smart constructor.
data ListStreams = ListStreams'
  { -- | A token used to get the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Set to true to return the list of streams in ascending order.
    ascendingOrder :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return at a time.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStreams' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token used to get the next set of results.
-- * 'ascendingOrder' - Set to true to return the list of streams in ascending order.
-- * 'maxResults' - The maximum number of results to return at a time.
mkListStreams ::
  ListStreams
mkListStreams =
  ListStreams'
    { nextToken = Lude.Nothing,
      ascendingOrder = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A token used to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListStreams (Lude.Maybe Lude.Text)
lsNextToken = Lens.lens (nextToken :: ListStreams -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStreams)
{-# DEPRECATED lsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Set to true to return the list of streams in ascending order.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsAscendingOrder :: Lens.Lens' ListStreams (Lude.Maybe Lude.Bool)
lsAscendingOrder = Lens.lens (ascendingOrder :: ListStreams -> Lude.Maybe Lude.Bool) (\s a -> s {ascendingOrder = a} :: ListStreams)
{-# DEPRECATED lsAscendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead." #-}

-- | The maximum number of results to return at a time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMaxResults :: Lens.Lens' ListStreams (Lude.Maybe Lude.Natural)
lsMaxResults = Lens.lens (maxResults :: ListStreams -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListStreams)
{-# DEPRECATED lsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListStreams where
  page rq rs
    | Page.stop (rs Lens.^. lsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsrsStreams) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsNextToken Lens..~ rs Lens.^. lsrsNextToken

instance Lude.AWSRequest ListStreams where
  type Rs ListStreams = ListStreamsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListStreamsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "streams" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStreams where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListStreams where
  toPath = Lude.const "/streams"

instance Lude.ToQuery ListStreams where
  toQuery ListStreams' {..} =
    Lude.mconcat
      [ "nextToken" Lude.=: nextToken,
        "isAscendingOrder" Lude.=: ascendingOrder,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { -- | A token used to get the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of streams.
    streams :: Lude.Maybe [StreamSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStreamsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token used to get the next set of results.
-- * 'streams' - A list of streams.
-- * 'responseStatus' - The response status code.
mkListStreamsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListStreamsResponse
mkListStreamsResponse pResponseStatus_ =
  ListStreamsResponse'
    { nextToken = Lude.Nothing,
      streams = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A token used to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsNextToken :: Lens.Lens' ListStreamsResponse (Lude.Maybe Lude.Text)
lsrsNextToken = Lens.lens (nextToken :: ListStreamsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStreamsResponse)
{-# DEPRECATED lsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of streams.
--
-- /Note:/ Consider using 'streams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsStreams :: Lens.Lens' ListStreamsResponse (Lude.Maybe [StreamSummary])
lsrsStreams = Lens.lens (streams :: ListStreamsResponse -> Lude.Maybe [StreamSummary]) (\s a -> s {streams = a} :: ListStreamsResponse)
{-# DEPRECATED lsrsStreams "Use generic-lens or generic-optics with 'streams' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsResponseStatus :: Lens.Lens' ListStreamsResponse Lude.Int
lsrsResponseStatus = Lens.lens (responseStatus :: ListStreamsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStreamsResponse)
{-# DEPRECATED lsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
