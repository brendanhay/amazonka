{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.ListMultiplexPrograms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the programs that currently exist for a specific multiplex.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListMultiplexPrograms
  ( -- * Creating a request
    ListMultiplexPrograms (..),
    mkListMultiplexPrograms,

    -- ** Request lenses
    lmpMultiplexId,
    lmpNextToken,
    lmpMaxResults,

    -- * Destructuring the response
    ListMultiplexProgramsResponse (..),
    mkListMultiplexProgramsResponse,

    -- ** Response lenses
    lmprsNextToken,
    lmprsMultiplexPrograms,
    lmprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for ListMultiplexProgramsRequest
--
-- /See:/ 'mkListMultiplexPrograms' smart constructor.
data ListMultiplexPrograms = ListMultiplexPrograms'
  { -- | The ID of the multiplex that the programs belong to.
    multiplexId :: Lude.Text,
    -- | The token to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMultiplexPrograms' with the minimum fields required to make a request.
--
-- * 'multiplexId' - The ID of the multiplex that the programs belong to.
-- * 'nextToken' - The token to retrieve the next page of results.
-- * 'maxResults' - The maximum number of items to return.
mkListMultiplexPrograms ::
  -- | 'multiplexId'
  Lude.Text ->
  ListMultiplexPrograms
mkListMultiplexPrograms pMultiplexId_ =
  ListMultiplexPrograms'
    { multiplexId = pMultiplexId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the multiplex that the programs belong to.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpMultiplexId :: Lens.Lens' ListMultiplexPrograms Lude.Text
lmpMultiplexId = Lens.lens (multiplexId :: ListMultiplexPrograms -> Lude.Text) (\s a -> s {multiplexId = a} :: ListMultiplexPrograms)
{-# DEPRECATED lmpMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpNextToken :: Lens.Lens' ListMultiplexPrograms (Lude.Maybe Lude.Text)
lmpNextToken = Lens.lens (nextToken :: ListMultiplexPrograms -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMultiplexPrograms)
{-# DEPRECATED lmpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpMaxResults :: Lens.Lens' ListMultiplexPrograms (Lude.Maybe Lude.Natural)
lmpMaxResults = Lens.lens (maxResults :: ListMultiplexPrograms -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListMultiplexPrograms)
{-# DEPRECATED lmpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListMultiplexPrograms where
  page rq rs
    | Page.stop (rs Lens.^. lmprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lmprsMultiplexPrograms) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lmpNextToken Lens..~ rs Lens.^. lmprsNextToken

instance Lude.AWSRequest ListMultiplexPrograms where
  type Rs ListMultiplexPrograms = ListMultiplexProgramsResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListMultiplexProgramsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "multiplexPrograms" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListMultiplexPrograms where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListMultiplexPrograms where
  toPath ListMultiplexPrograms' {..} =
    Lude.mconcat
      ["/prod/multiplexes/", Lude.toBS multiplexId, "/programs"]

instance Lude.ToQuery ListMultiplexPrograms where
  toQuery ListMultiplexPrograms' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | Placeholder documentation for ListMultiplexProgramsResponse
--
-- /See:/ 'mkListMultiplexProgramsResponse' smart constructor.
data ListMultiplexProgramsResponse = ListMultiplexProgramsResponse'
  { -- | Token for the next ListMultiplexProgram request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | List of multiplex programs.
    multiplexPrograms :: Lude.Maybe [MultiplexProgramSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMultiplexProgramsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Token for the next ListMultiplexProgram request.
-- * 'multiplexPrograms' - List of multiplex programs.
-- * 'responseStatus' - The response status code.
mkListMultiplexProgramsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMultiplexProgramsResponse
mkListMultiplexProgramsResponse pResponseStatus_ =
  ListMultiplexProgramsResponse'
    { nextToken = Lude.Nothing,
      multiplexPrograms = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Token for the next ListMultiplexProgram request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprsNextToken :: Lens.Lens' ListMultiplexProgramsResponse (Lude.Maybe Lude.Text)
lmprsNextToken = Lens.lens (nextToken :: ListMultiplexProgramsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMultiplexProgramsResponse)
{-# DEPRECATED lmprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of multiplex programs.
--
-- /Note:/ Consider using 'multiplexPrograms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprsMultiplexPrograms :: Lens.Lens' ListMultiplexProgramsResponse (Lude.Maybe [MultiplexProgramSummary])
lmprsMultiplexPrograms = Lens.lens (multiplexPrograms :: ListMultiplexProgramsResponse -> Lude.Maybe [MultiplexProgramSummary]) (\s a -> s {multiplexPrograms = a} :: ListMultiplexProgramsResponse)
{-# DEPRECATED lmprsMultiplexPrograms "Use generic-lens or generic-optics with 'multiplexPrograms' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprsResponseStatus :: Lens.Lens' ListMultiplexProgramsResponse Lude.Int
lmprsResponseStatus = Lens.lens (responseStatus :: ListMultiplexProgramsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMultiplexProgramsResponse)
{-# DEPRECATED lmprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
