{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeStacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified stacks, if the stack names are provided. Otherwise, all stacks in the account are described.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeStacks
  ( -- * Creating a request
    DescribeStacks (..),
    mkDescribeStacks,

    -- ** Request lenses
    dsNextToken,
    dsNames,

    -- * Destructuring the response
    DescribeStacksResponse (..),
    mkDescribeStacksResponse,

    -- ** Response lenses
    drsNextToken,
    drsStacks,
    drsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeStacks' smart constructor.
data DescribeStacks = DescribeStacks'
  { -- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The names of the stacks to describe.
    names :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStacks' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
-- * 'names' - The names of the stacks to describe.
mkDescribeStacks ::
  DescribeStacks
mkDescribeStacks =
  DescribeStacks' {nextToken = Lude.Nothing, names = Lude.Nothing}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNextToken :: Lens.Lens' DescribeStacks (Lude.Maybe Lude.Text)
dsNextToken = Lens.lens (nextToken :: DescribeStacks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeStacks)
{-# DEPRECATED dsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of the stacks to describe.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNames :: Lens.Lens' DescribeStacks (Lude.Maybe [Lude.Text])
dsNames = Lens.lens (names :: DescribeStacks -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: DescribeStacks)
{-# DEPRECATED dsNames "Use generic-lens or generic-optics with 'names' instead." #-}

instance Page.AWSPager DescribeStacks where
  page rq rs
    | Page.stop (rs Lens.^. drsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drsStacks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsNextToken Lens..~ rs Lens.^. drsNextToken

instance Lude.AWSRequest DescribeStacks where
  type Rs DescribeStacks = DescribeStacksResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeStacksResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Stacks" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStacks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.DescribeStacks" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeStacks where
  toJSON DescribeStacks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Names" Lude..=) Lude.<$> names
          ]
      )

instance Lude.ToPath DescribeStacks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStacks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeStacksResponse' smart constructor.
data DescribeStacksResponse = DescribeStacksResponse'
  { -- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the stacks.
    stacks :: Lude.Maybe [Stack],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStacksResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
-- * 'stacks' - Information about the stacks.
-- * 'responseStatus' - The response status code.
mkDescribeStacksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStacksResponse
mkDescribeStacksResponse pResponseStatus_ =
  DescribeStacksResponse'
    { nextToken = Lude.Nothing,
      stacks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeStacksResponse (Lude.Maybe Lude.Text)
drsNextToken = Lens.lens (nextToken :: DescribeStacksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeStacksResponse)
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the stacks.
--
-- /Note:/ Consider using 'stacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStacks :: Lens.Lens' DescribeStacksResponse (Lude.Maybe [Stack])
drsStacks = Lens.lens (stacks :: DescribeStacksResponse -> Lude.Maybe [Stack]) (\s a -> s {stacks = a} :: DescribeStacksResponse)
{-# DEPRECATED drsStacks "Use generic-lens or generic-optics with 'stacks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeStacksResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeStacksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStacksResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
