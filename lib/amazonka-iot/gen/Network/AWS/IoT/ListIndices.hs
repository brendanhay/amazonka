{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListIndices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the search indices.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListIndices
  ( -- * Creating a request
    ListIndices (..),
    mkListIndices,

    -- ** Request lenses
    liNextToken,
    liMaxResults,

    -- * Destructuring the response
    ListIndicesResponse (..),
    mkListIndicesResponse,

    -- ** Response lenses
    lirsNextToken,
    lirsIndexNames,
    lirsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListIndices' smart constructor.
data ListIndices = ListIndices'
  { -- | The token used to get the next set of results, or @null@ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListIndices' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token used to get the next set of results, or @null@ if there are no additional results.
-- * 'maxResults' - The maximum number of results to return at one time.
mkListIndices ::
  ListIndices
mkListIndices =
  ListIndices' {nextToken = Lude.Nothing, maxResults = Lude.Nothing}

-- | The token used to get the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListIndices (Lude.Maybe Lude.Text)
liNextToken = Lens.lens (nextToken :: ListIndices -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIndices)
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListIndices (Lude.Maybe Lude.Natural)
liMaxResults = Lens.lens (maxResults :: ListIndices -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListIndices)
{-# DEPRECATED liMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListIndices where
  page rq rs
    | Page.stop (rs Lens.^. lirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lirsIndexNames) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& liNextToken Lens..~ rs Lens.^. lirsNextToken

instance Lude.AWSRequest ListIndices where
  type Rs ListIndices = ListIndicesResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListIndicesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "indexNames" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListIndices where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListIndices where
  toPath = Lude.const "/indices"

instance Lude.ToQuery ListIndices where
  toQuery ListIndices' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListIndicesResponse' smart constructor.
data ListIndicesResponse = ListIndicesResponse'
  { -- | The token used to get the next set of results, or @null@ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The index names.
    indexNames :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListIndicesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token used to get the next set of results, or @null@ if there are no additional results.
-- * 'indexNames' - The index names.
-- * 'responseStatus' - The response status code.
mkListIndicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListIndicesResponse
mkListIndicesResponse pResponseStatus_ =
  ListIndicesResponse'
    { nextToken = Lude.Nothing,
      indexNames = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token used to get the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsNextToken :: Lens.Lens' ListIndicesResponse (Lude.Maybe Lude.Text)
lirsNextToken = Lens.lens (nextToken :: ListIndicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIndicesResponse)
{-# DEPRECATED lirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The index names.
--
-- /Note:/ Consider using 'indexNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsIndexNames :: Lens.Lens' ListIndicesResponse (Lude.Maybe [Lude.Text])
lirsIndexNames = Lens.lens (indexNames :: ListIndicesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {indexNames = a} :: ListIndicesResponse)
{-# DEPRECATED lirsIndexNames "Use generic-lens or generic-optics with 'indexNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsResponseStatus :: Lens.Lens' ListIndicesResponse Lude.Int
lirsResponseStatus = Lens.lens (responseStatus :: ListIndicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListIndicesResponse)
{-# DEPRECATED lirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
