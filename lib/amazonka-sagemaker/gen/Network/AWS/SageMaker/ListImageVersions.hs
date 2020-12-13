{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListImageVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a specified image and their properties. The list can be filtered by creation time or modified time.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListImageVersions
  ( -- * Creating a request
    ListImageVersions (..),
    mkListImageVersions,

    -- ** Request lenses
    livLastModifiedTimeBefore,
    livCreationTimeAfter,
    livNextToken,
    livSortOrder,
    livLastModifiedTimeAfter,
    livCreationTimeBefore,
    livImageName,
    livMaxResults,
    livSortBy,

    -- * Destructuring the response
    ListImageVersionsResponse (..),
    mkListImageVersionsResponse,

    -- ** Response lenses
    livrsNextToken,
    livrsImageVersions,
    livrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListImageVersions' smart constructor.
data ListImageVersions = ListImageVersions'
  { -- | A filter that returns only versions modified on or before the specified time.
    lastModifiedTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only versions created on or after the specified time.
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | If the previous call to @ListImageVersions@ didn't return the full set of versions, the call returns a token for getting the next set of versions.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The sort order. The default value is @DESCENDING@ .
    sortOrder :: Lude.Maybe ImageVersionSortOrder,
    -- | A filter that returns only versions modified on or after the specified time.
    lastModifiedTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only versions created on or before the specified time.
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | The name of the image to list the versions of.
    imageName :: Lude.Text,
    -- | The maximum number of versions to return in the response. The default value is 10.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The property used to sort results. The default value is @CREATION_TIME@ .
    sortBy :: Lude.Maybe ImageVersionSortBy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListImageVersions' with the minimum fields required to make a request.
--
-- * 'lastModifiedTimeBefore' - A filter that returns only versions modified on or before the specified time.
-- * 'creationTimeAfter' - A filter that returns only versions created on or after the specified time.
-- * 'nextToken' - If the previous call to @ListImageVersions@ didn't return the full set of versions, the call returns a token for getting the next set of versions.
-- * 'sortOrder' - The sort order. The default value is @DESCENDING@ .
-- * 'lastModifiedTimeAfter' - A filter that returns only versions modified on or after the specified time.
-- * 'creationTimeBefore' - A filter that returns only versions created on or before the specified time.
-- * 'imageName' - The name of the image to list the versions of.
-- * 'maxResults' - The maximum number of versions to return in the response. The default value is 10.
-- * 'sortBy' - The property used to sort results. The default value is @CREATION_TIME@ .
mkListImageVersions ::
  -- | 'imageName'
  Lude.Text ->
  ListImageVersions
mkListImageVersions pImageName_ =
  ListImageVersions'
    { lastModifiedTimeBefore = Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      lastModifiedTimeAfter = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      imageName = pImageName_,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A filter that returns only versions modified on or before the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livLastModifiedTimeBefore :: Lens.Lens' ListImageVersions (Lude.Maybe Lude.Timestamp)
livLastModifiedTimeBefore = Lens.lens (lastModifiedTimeBefore :: ListImageVersions -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeBefore = a} :: ListImageVersions)
{-# DEPRECATED livLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | A filter that returns only versions created on or after the specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livCreationTimeAfter :: Lens.Lens' ListImageVersions (Lude.Maybe Lude.Timestamp)
livCreationTimeAfter = Lens.lens (creationTimeAfter :: ListImageVersions -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListImageVersions)
{-# DEPRECATED livCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the previous call to @ListImageVersions@ didn't return the full set of versions, the call returns a token for getting the next set of versions.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livNextToken :: Lens.Lens' ListImageVersions (Lude.Maybe Lude.Text)
livNextToken = Lens.lens (nextToken :: ListImageVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListImageVersions)
{-# DEPRECATED livNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order. The default value is @DESCENDING@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livSortOrder :: Lens.Lens' ListImageVersions (Lude.Maybe ImageVersionSortOrder)
livSortOrder = Lens.lens (sortOrder :: ListImageVersions -> Lude.Maybe ImageVersionSortOrder) (\s a -> s {sortOrder = a} :: ListImageVersions)
{-# DEPRECATED livSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only versions modified on or after the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livLastModifiedTimeAfter :: Lens.Lens' ListImageVersions (Lude.Maybe Lude.Timestamp)
livLastModifiedTimeAfter = Lens.lens (lastModifiedTimeAfter :: ListImageVersions -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeAfter = a} :: ListImageVersions)
{-# DEPRECATED livLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only versions created on or before the specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livCreationTimeBefore :: Lens.Lens' ListImageVersions (Lude.Maybe Lude.Timestamp)
livCreationTimeBefore = Lens.lens (creationTimeBefore :: ListImageVersions -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListImageVersions)
{-# DEPRECATED livCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The name of the image to list the versions of.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livImageName :: Lens.Lens' ListImageVersions Lude.Text
livImageName = Lens.lens (imageName :: ListImageVersions -> Lude.Text) (\s a -> s {imageName = a} :: ListImageVersions)
{-# DEPRECATED livImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The maximum number of versions to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livMaxResults :: Lens.Lens' ListImageVersions (Lude.Maybe Lude.Natural)
livMaxResults = Lens.lens (maxResults :: ListImageVersions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListImageVersions)
{-# DEPRECATED livMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The property used to sort results. The default value is @CREATION_TIME@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livSortBy :: Lens.Lens' ListImageVersions (Lude.Maybe ImageVersionSortBy)
livSortBy = Lens.lens (sortBy :: ListImageVersions -> Lude.Maybe ImageVersionSortBy) (\s a -> s {sortBy = a} :: ListImageVersions)
{-# DEPRECATED livSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListImageVersions where
  page rq rs
    | Page.stop (rs Lens.^. livrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. livrsImageVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& livNextToken Lens..~ rs Lens.^. livrsNextToken

instance Lude.AWSRequest ListImageVersions where
  type Rs ListImageVersions = ListImageVersionsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListImageVersionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ImageVersions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListImageVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListImageVersions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListImageVersions where
  toJSON ListImageVersions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LastModifiedTimeBefore" Lude..=)
              Lude.<$> lastModifiedTimeBefore,
            ("CreationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("LastModifiedTimeAfter" Lude..=) Lude.<$> lastModifiedTimeAfter,
            ("CreationTimeBefore" Lude..=) Lude.<$> creationTimeBefore,
            Lude.Just ("ImageName" Lude..= imageName),
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListImageVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListImageVersions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListImageVersionsResponse' smart constructor.
data ListImageVersionsResponse = ListImageVersionsResponse'
  { -- | A token for getting the next set of versions, if there are any.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of versions and their properties.
    imageVersions :: Lude.Maybe [ImageVersion],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListImageVersionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token for getting the next set of versions, if there are any.
-- * 'imageVersions' - A list of versions and their properties.
-- * 'responseStatus' - The response status code.
mkListImageVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListImageVersionsResponse
mkListImageVersionsResponse pResponseStatus_ =
  ListImageVersionsResponse'
    { nextToken = Lude.Nothing,
      imageVersions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A token for getting the next set of versions, if there are any.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livrsNextToken :: Lens.Lens' ListImageVersionsResponse (Lude.Maybe Lude.Text)
livrsNextToken = Lens.lens (nextToken :: ListImageVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListImageVersionsResponse)
{-# DEPRECATED livrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of versions and their properties.
--
-- /Note:/ Consider using 'imageVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livrsImageVersions :: Lens.Lens' ListImageVersionsResponse (Lude.Maybe [ImageVersion])
livrsImageVersions = Lens.lens (imageVersions :: ListImageVersionsResponse -> Lude.Maybe [ImageVersion]) (\s a -> s {imageVersions = a} :: ListImageVersionsResponse)
{-# DEPRECATED livrsImageVersions "Use generic-lens or generic-optics with 'imageVersions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livrsResponseStatus :: Lens.Lens' ListImageVersionsResponse Lude.Int
livrsResponseStatus = Lens.lens (responseStatus :: ListImageVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListImageVersionsResponse)
{-# DEPRECATED livrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
