{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified images, if the image names or image ARNs are provided. Otherwise, all images in the account are described.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeImages
  ( -- * Creating a request
    DescribeImages (..),
    mkDescribeImages,

    -- ** Request lenses
    diNextToken,
    diNames,
    diType,
    diARNs,
    diMaxResults,

    -- * Destructuring the response
    DescribeImagesResponse (..),
    mkDescribeImagesResponse,

    -- ** Response lenses
    diirsImages,
    diirsNextToken,
    diirsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeImages' smart constructor.
data DescribeImages = DescribeImages'
  { nextToken ::
      Lude.Maybe Lude.Text,
    names :: Lude.Maybe [Lude.Text],
    type' :: Lude.Maybe VisibilityType,
    arns :: Lude.Maybe [Lude.Text],
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImages' with the minimum fields required to make a request.
--
-- * 'arns' - The ARNs of the public, private, and shared images to describe.
-- * 'maxResults' - The maximum size of each page of results.
-- * 'names' - The names of the public or private images to describe.
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
-- * 'type'' - The type of image (public, private, or shared) to describe.
mkDescribeImages ::
  DescribeImages
mkDescribeImages =
  DescribeImages'
    { nextToken = Lude.Nothing,
      names = Lude.Nothing,
      type' = Lude.Nothing,
      arns = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diNextToken :: Lens.Lens' DescribeImages (Lude.Maybe Lude.Text)
diNextToken = Lens.lens (nextToken :: DescribeImages -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeImages)
{-# DEPRECATED diNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of the public or private images to describe.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diNames :: Lens.Lens' DescribeImages (Lude.Maybe [Lude.Text])
diNames = Lens.lens (names :: DescribeImages -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: DescribeImages)
{-# DEPRECATED diNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The type of image (public, private, or shared) to describe.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diType :: Lens.Lens' DescribeImages (Lude.Maybe VisibilityType)
diType = Lens.lens (type' :: DescribeImages -> Lude.Maybe VisibilityType) (\s a -> s {type' = a} :: DescribeImages)
{-# DEPRECATED diType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The ARNs of the public, private, and shared images to describe.
--
-- /Note:/ Consider using 'arns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diARNs :: Lens.Lens' DescribeImages (Lude.Maybe [Lude.Text])
diARNs = Lens.lens (arns :: DescribeImages -> Lude.Maybe [Lude.Text]) (\s a -> s {arns = a} :: DescribeImages)
{-# DEPRECATED diARNs "Use generic-lens or generic-optics with 'arns' instead." #-}

-- | The maximum size of each page of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diMaxResults :: Lens.Lens' DescribeImages (Lude.Maybe Lude.Natural)
diMaxResults = Lens.lens (maxResults :: DescribeImages -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeImages)
{-# DEPRECATED diMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeImages where
  page rq rs
    | Page.stop (rs Lens.^. diirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. diirsImages) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& diNextToken Lens..~ rs Lens.^. diirsNextToken

instance Lude.AWSRequest DescribeImages where
  type Rs DescribeImages = DescribeImagesResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeImagesResponse'
            Lude.<$> (x Lude..?> "Images" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeImages where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.DescribeImages" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeImages where
  toJSON DescribeImages' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Names" Lude..=) Lude.<$> names,
            ("Type" Lude..=) Lude.<$> type',
            ("Arns" Lude..=) Lude.<$> arns,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeImages where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeImages where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeImagesResponse' smart constructor.
data DescribeImagesResponse = DescribeImagesResponse'
  { images ::
      Lude.Maybe [Image],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImagesResponse' with the minimum fields required to make a request.
--
-- * 'images' - Information about the images.
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
-- * 'responseStatus' - The response status code.
mkDescribeImagesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeImagesResponse
mkDescribeImagesResponse pResponseStatus_ =
  DescribeImagesResponse'
    { images = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the images.
--
-- /Note:/ Consider using 'images' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsImages :: Lens.Lens' DescribeImagesResponse (Lude.Maybe [Image])
diirsImages = Lens.lens (images :: DescribeImagesResponse -> Lude.Maybe [Image]) (\s a -> s {images = a} :: DescribeImagesResponse)
{-# DEPRECATED diirsImages "Use generic-lens or generic-optics with 'images' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsNextToken :: Lens.Lens' DescribeImagesResponse (Lude.Maybe Lude.Text)
diirsNextToken = Lens.lens (nextToken :: DescribeImagesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeImagesResponse)
{-# DEPRECATED diirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsResponseStatus :: Lens.Lens' DescribeImagesResponse Lude.Int
diirsResponseStatus = Lens.lens (responseStatus :: DescribeImagesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeImagesResponse)
{-# DEPRECATED diirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
