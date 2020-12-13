{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListDimensions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the set of dimensions that are defined for your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListDimensions
  ( -- * Creating a request
    ListDimensions (..),
    mkListDimensions,

    -- ** Request lenses
    ldNextToken,
    ldMaxResults,

    -- * Destructuring the response
    ListDimensionsResponse (..),
    mkListDimensionsResponse,

    -- ** Response lenses
    ldrsNextToken,
    ldrsDimensionNames,
    ldrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDimensions' smart constructor.
data ListDimensions = ListDimensions'
  { -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to retrieve at one time.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDimensions' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results.
-- * 'maxResults' - The maximum number of results to retrieve at one time.
mkListDimensions ::
  ListDimensions
mkListDimensions =
  ListDimensions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDimensions (Lude.Maybe Lude.Text)
ldNextToken = Lens.lens (nextToken :: ListDimensions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDimensions)
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to retrieve at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxResults :: Lens.Lens' ListDimensions (Lude.Maybe Lude.Natural)
ldMaxResults = Lens.lens (maxResults :: ListDimensions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDimensions)
{-# DEPRECATED ldMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDimensions where
  page rq rs
    | Page.stop (rs Lens.^. ldrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldrsDimensionNames) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldNextToken Lens..~ rs Lens.^. ldrsNextToken

instance Lude.AWSRequest ListDimensions where
  type Rs ListDimensions = ListDimensionsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDimensionsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "dimensionNames" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDimensions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListDimensions where
  toPath = Lude.const "/dimensions"

instance Lude.ToQuery ListDimensions where
  toQuery ListDimensions' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListDimensionsResponse' smart constructor.
data ListDimensionsResponse = ListDimensionsResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of the names of the defined dimensions. Use @DescribeDimension@ to get details for a dimension.
    dimensionNames :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDimensionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
-- * 'dimensionNames' - A list of the names of the defined dimensions. Use @DescribeDimension@ to get details for a dimension.
-- * 'responseStatus' - The response status code.
mkListDimensionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDimensionsResponse
mkListDimensionsResponse pResponseStatus_ =
  ListDimensionsResponse'
    { nextToken = Lude.Nothing,
      dimensionNames = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsNextToken :: Lens.Lens' ListDimensionsResponse (Lude.Maybe Lude.Text)
ldrsNextToken = Lens.lens (nextToken :: ListDimensionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDimensionsResponse)
{-# DEPRECATED ldrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of the names of the defined dimensions. Use @DescribeDimension@ to get details for a dimension.
--
-- /Note:/ Consider using 'dimensionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsDimensionNames :: Lens.Lens' ListDimensionsResponse (Lude.Maybe [Lude.Text])
ldrsDimensionNames = Lens.lens (dimensionNames :: ListDimensionsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {dimensionNames = a} :: ListDimensionsResponse)
{-# DEPRECATED ldrsDimensionNames "Use generic-lens or generic-optics with 'dimensionNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsResponseStatus :: Lens.Lens' ListDimensionsResponse Lude.Int
ldrsResponseStatus = Lens.lens (responseStatus :: ListDimensionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDimensionsResponse)
{-# DEPRECATED ldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
