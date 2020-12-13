{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeFleets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified fleets, if the fleet names are provided. Otherwise, all fleets in the account are described.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeFleets
  ( -- * Creating a request
    DescribeFleets (..),
    mkDescribeFleets,

    -- ** Request lenses
    dfNextToken,
    dfNames,

    -- * Destructuring the response
    DescribeFleetsResponse (..),
    mkDescribeFleetsResponse,

    -- ** Response lenses
    dfsrsNextToken,
    dfsrsFleets,
    dfsrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeFleets' smart constructor.
data DescribeFleets = DescribeFleets'
  { -- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The names of the fleets to describe.
    names :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleets' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
-- * 'names' - The names of the fleets to describe.
mkDescribeFleets ::
  DescribeFleets
mkDescribeFleets =
  DescribeFleets' {nextToken = Lude.Nothing, names = Lude.Nothing}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfNextToken :: Lens.Lens' DescribeFleets (Lude.Maybe Lude.Text)
dfNextToken = Lens.lens (nextToken :: DescribeFleets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFleets)
{-# DEPRECATED dfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of the fleets to describe.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfNames :: Lens.Lens' DescribeFleets (Lude.Maybe [Lude.Text])
dfNames = Lens.lens (names :: DescribeFleets -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: DescribeFleets)
{-# DEPRECATED dfNames "Use generic-lens or generic-optics with 'names' instead." #-}

instance Page.AWSPager DescribeFleets where
  page rq rs
    | Page.stop (rs Lens.^. dfsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dfsrsFleets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dfNextToken Lens..~ rs Lens.^. dfsrsNextToken

instance Lude.AWSRequest DescribeFleets where
  type Rs DescribeFleets = DescribeFleetsResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeFleetsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Fleets" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFleets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.DescribeFleets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeFleets where
  toJSON DescribeFleets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Names" Lude..=) Lude.<$> names
          ]
      )

instance Lude.ToPath DescribeFleets where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeFleets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeFleetsResponse' smart constructor.
data DescribeFleetsResponse = DescribeFleetsResponse'
  { -- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the fleets.
    fleets :: Lude.Maybe [Fleet],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleetsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
-- * 'fleets' - Information about the fleets.
-- * 'responseStatus' - The response status code.
mkDescribeFleetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFleetsResponse
mkDescribeFleetsResponse pResponseStatus_ =
  DescribeFleetsResponse'
    { nextToken = Lude.Nothing,
      fleets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsNextToken :: Lens.Lens' DescribeFleetsResponse (Lude.Maybe Lude.Text)
dfsrsNextToken = Lens.lens (nextToken :: DescribeFleetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFleetsResponse)
{-# DEPRECATED dfsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the fleets.
--
-- /Note:/ Consider using 'fleets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsFleets :: Lens.Lens' DescribeFleetsResponse (Lude.Maybe [Fleet])
dfsrsFleets = Lens.lens (fleets :: DescribeFleetsResponse -> Lude.Maybe [Fleet]) (\s a -> s {fleets = a} :: DescribeFleetsResponse)
{-# DEPRECATED dfsrsFleets "Use generic-lens or generic-optics with 'fleets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsResponseStatus :: Lens.Lens' DescribeFleetsResponse Lude.Int
dfsrsResponseStatus = Lens.lens (responseStatus :: DescribeFleetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFleetsResponse)
{-# DEPRECATED dfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
