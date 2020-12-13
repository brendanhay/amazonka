{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the images in your account and their properties. The list can be filtered by creation time or modified time, and whether the image name contains a specified string.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListImages
  ( -- * Creating a request
    ListImages (..),
    mkListImages,

    -- ** Request lenses
    liNameContains,
    liLastModifiedTimeBefore,
    liCreationTimeAfter,
    liNextToken,
    liSortOrder,
    liLastModifiedTimeAfter,
    liCreationTimeBefore,
    liMaxResults,
    liSortBy,

    -- * Destructuring the response
    ListImagesResponse (..),
    mkListImagesResponse,

    -- ** Response lenses
    lirsImages,
    lirsNextToken,
    lirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListImages' smart constructor.
data ListImages = ListImages'
  { -- | A filter that returns only images whose name contains the specified string.
    nameContains :: Lude.Maybe Lude.Text,
    -- | A filter that returns only images modified on or before the specified time.
    lastModifiedTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only images created on or after the specified time.
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | If the previous call to @ListImages@ didn't return the full set of images, the call returns a token for getting the next set of images.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The sort order. The default value is @DESCENDING@ .
    sortOrder :: Lude.Maybe ImageSortOrder,
    -- | A filter that returns only images modified on or after the specified time.
    lastModifiedTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only images created on or before the specified time.
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | The maximum number of images to return in the response. The default value is 10.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The property used to sort results. The default value is @CREATION_TIME@ .
    sortBy :: Lude.Maybe ImageSortBy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListImages' with the minimum fields required to make a request.
--
-- * 'nameContains' - A filter that returns only images whose name contains the specified string.
-- * 'lastModifiedTimeBefore' - A filter that returns only images modified on or before the specified time.
-- * 'creationTimeAfter' - A filter that returns only images created on or after the specified time.
-- * 'nextToken' - If the previous call to @ListImages@ didn't return the full set of images, the call returns a token for getting the next set of images.
-- * 'sortOrder' - The sort order. The default value is @DESCENDING@ .
-- * 'lastModifiedTimeAfter' - A filter that returns only images modified on or after the specified time.
-- * 'creationTimeBefore' - A filter that returns only images created on or before the specified time.
-- * 'maxResults' - The maximum number of images to return in the response. The default value is 10.
-- * 'sortBy' - The property used to sort results. The default value is @CREATION_TIME@ .
mkListImages ::
  ListImages
mkListImages =
  ListImages'
    { nameContains = Lude.Nothing,
      lastModifiedTimeBefore = Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      lastModifiedTimeAfter = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A filter that returns only images whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNameContains :: Lens.Lens' ListImages (Lude.Maybe Lude.Text)
liNameContains = Lens.lens (nameContains :: ListImages -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListImages)
{-# DEPRECATED liNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns only images modified on or before the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liLastModifiedTimeBefore :: Lens.Lens' ListImages (Lude.Maybe Lude.Timestamp)
liLastModifiedTimeBefore = Lens.lens (lastModifiedTimeBefore :: ListImages -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeBefore = a} :: ListImages)
{-# DEPRECATED liLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | A filter that returns only images created on or after the specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liCreationTimeAfter :: Lens.Lens' ListImages (Lude.Maybe Lude.Timestamp)
liCreationTimeAfter = Lens.lens (creationTimeAfter :: ListImages -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListImages)
{-# DEPRECATED liCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the previous call to @ListImages@ didn't return the full set of images, the call returns a token for getting the next set of images.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListImages (Lude.Maybe Lude.Text)
liNextToken = Lens.lens (nextToken :: ListImages -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListImages)
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order. The default value is @DESCENDING@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liSortOrder :: Lens.Lens' ListImages (Lude.Maybe ImageSortOrder)
liSortOrder = Lens.lens (sortOrder :: ListImages -> Lude.Maybe ImageSortOrder) (\s a -> s {sortOrder = a} :: ListImages)
{-# DEPRECATED liSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only images modified on or after the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liLastModifiedTimeAfter :: Lens.Lens' ListImages (Lude.Maybe Lude.Timestamp)
liLastModifiedTimeAfter = Lens.lens (lastModifiedTimeAfter :: ListImages -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeAfter = a} :: ListImages)
{-# DEPRECATED liLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only images created on or before the specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liCreationTimeBefore :: Lens.Lens' ListImages (Lude.Maybe Lude.Timestamp)
liCreationTimeBefore = Lens.lens (creationTimeBefore :: ListImages -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListImages)
{-# DEPRECATED liCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The maximum number of images to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListImages (Lude.Maybe Lude.Natural)
liMaxResults = Lens.lens (maxResults :: ListImages -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListImages)
{-# DEPRECATED liMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The property used to sort results. The default value is @CREATION_TIME@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liSortBy :: Lens.Lens' ListImages (Lude.Maybe ImageSortBy)
liSortBy = Lens.lens (sortBy :: ListImages -> Lude.Maybe ImageSortBy) (\s a -> s {sortBy = a} :: ListImages)
{-# DEPRECATED liSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListImages where
  page rq rs
    | Page.stop (rs Lens.^. lirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lirsImages) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& liNextToken Lens..~ rs Lens.^. lirsNextToken

instance Lude.AWSRequest ListImages where
  type Rs ListImages = ListImagesResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListImagesResponse'
            Lude.<$> (x Lude..?> "Images" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListImages where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListImages" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListImages where
  toJSON ListImages' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("LastModifiedTimeBefore" Lude..=) Lude.<$> lastModifiedTimeBefore,
            ("CreationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("LastModifiedTimeAfter" Lude..=) Lude.<$> lastModifiedTimeAfter,
            ("CreationTimeBefore" Lude..=) Lude.<$> creationTimeBefore,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListImages where
  toPath = Lude.const "/"

instance Lude.ToQuery ListImages where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListImagesResponse' smart constructor.
data ListImagesResponse = ListImagesResponse'
  { -- | A list of images and their properties.
    images :: Lude.Maybe [Image],
    -- | A token for getting the next set of images, if there are any.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListImagesResponse' with the minimum fields required to make a request.
--
-- * 'images' - A list of images and their properties.
-- * 'nextToken' - A token for getting the next set of images, if there are any.
-- * 'responseStatus' - The response status code.
mkListImagesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListImagesResponse
mkListImagesResponse pResponseStatus_ =
  ListImagesResponse'
    { images = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of images and their properties.
--
-- /Note:/ Consider using 'images' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsImages :: Lens.Lens' ListImagesResponse (Lude.Maybe [Image])
lirsImages = Lens.lens (images :: ListImagesResponse -> Lude.Maybe [Image]) (\s a -> s {images = a} :: ListImagesResponse)
{-# DEPRECATED lirsImages "Use generic-lens or generic-optics with 'images' instead." #-}

-- | A token for getting the next set of images, if there are any.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsNextToken :: Lens.Lens' ListImagesResponse (Lude.Maybe Lude.Text)
lirsNextToken = Lens.lens (nextToken :: ListImagesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListImagesResponse)
{-# DEPRECATED lirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsResponseStatus :: Lens.Lens' ListImagesResponse Lude.Int
lirsResponseStatus = Lens.lens (responseStatus :: ListImagesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListImagesResponse)
{-# DEPRECATED lirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
