{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeImageBuilders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified image builders, if the image builder names are provided. Otherwise, all image builders in the account are described.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeImageBuilders
  ( -- * Creating a request
    DescribeImageBuilders (..),
    mkDescribeImageBuilders,

    -- ** Request lenses
    dibNextToken,
    dibNames,
    dibMaxResults,

    -- * Destructuring the response
    DescribeImageBuildersResponse (..),
    mkDescribeImageBuildersResponse,

    -- ** Response lenses
    dibrsImageBuilders,
    dibrsNextToken,
    dibrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeImageBuilders' smart constructor.
data DescribeImageBuilders = DescribeImageBuilders'
  { -- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The names of the image builders to describe.
    names :: Lude.Maybe [Lude.Text],
    -- | The maximum size of each page of results.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImageBuilders' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
-- * 'names' - The names of the image builders to describe.
-- * 'maxResults' - The maximum size of each page of results.
mkDescribeImageBuilders ::
  DescribeImageBuilders
mkDescribeImageBuilders =
  DescribeImageBuilders'
    { nextToken = Lude.Nothing,
      names = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibNextToken :: Lens.Lens' DescribeImageBuilders (Lude.Maybe Lude.Text)
dibNextToken = Lens.lens (nextToken :: DescribeImageBuilders -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeImageBuilders)
{-# DEPRECATED dibNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of the image builders to describe.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibNames :: Lens.Lens' DescribeImageBuilders (Lude.Maybe [Lude.Text])
dibNames = Lens.lens (names :: DescribeImageBuilders -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: DescribeImageBuilders)
{-# DEPRECATED dibNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The maximum size of each page of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibMaxResults :: Lens.Lens' DescribeImageBuilders (Lude.Maybe Lude.Int)
dibMaxResults = Lens.lens (maxResults :: DescribeImageBuilders -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeImageBuilders)
{-# DEPRECATED dibMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeImageBuilders where
  page rq rs
    | Page.stop (rs Lens.^. dibrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dibrsImageBuilders) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dibNextToken Lens..~ rs Lens.^. dibrsNextToken

instance Lude.AWSRequest DescribeImageBuilders where
  type Rs DescribeImageBuilders = DescribeImageBuildersResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeImageBuildersResponse'
            Lude.<$> (x Lude..?> "ImageBuilders" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeImageBuilders where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.DescribeImageBuilders" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeImageBuilders where
  toJSON DescribeImageBuilders' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Names" Lude..=) Lude.<$> names,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeImageBuilders where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeImageBuilders where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeImageBuildersResponse' smart constructor.
data DescribeImageBuildersResponse = DescribeImageBuildersResponse'
  { -- | Information about the image builders.
    imageBuilders :: Lude.Maybe [ImageBuilder],
    -- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImageBuildersResponse' with the minimum fields required to make a request.
--
-- * 'imageBuilders' - Information about the image builders.
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
-- * 'responseStatus' - The response status code.
mkDescribeImageBuildersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeImageBuildersResponse
mkDescribeImageBuildersResponse pResponseStatus_ =
  DescribeImageBuildersResponse'
    { imageBuilders = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the image builders.
--
-- /Note:/ Consider using 'imageBuilders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibrsImageBuilders :: Lens.Lens' DescribeImageBuildersResponse (Lude.Maybe [ImageBuilder])
dibrsImageBuilders = Lens.lens (imageBuilders :: DescribeImageBuildersResponse -> Lude.Maybe [ImageBuilder]) (\s a -> s {imageBuilders = a} :: DescribeImageBuildersResponse)
{-# DEPRECATED dibrsImageBuilders "Use generic-lens or generic-optics with 'imageBuilders' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibrsNextToken :: Lens.Lens' DescribeImageBuildersResponse (Lude.Maybe Lude.Text)
dibrsNextToken = Lens.lens (nextToken :: DescribeImageBuildersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeImageBuildersResponse)
{-# DEPRECATED dibrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibrsResponseStatus :: Lens.Lens' DescribeImageBuildersResponse Lude.Int
dibrsResponseStatus = Lens.lens (responseStatus :: DescribeImageBuildersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeImageBuildersResponse)
{-# DEPRECATED dibrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
