{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified images, if the image identifiers are provided. Otherwise, all images in the account are described.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspaceImages
  ( -- * Creating a request
    DescribeWorkspaceImages (..),
    mkDescribeWorkspaceImages,

    -- ** Request lenses
    dwiImageIds,
    dwiNextToken,
    dwiImageType,
    dwiMaxResults,

    -- * Destructuring the response
    DescribeWorkspaceImagesResponse (..),
    mkDescribeWorkspaceImagesResponse,

    -- ** Response lenses
    drsImages,
    drsNextToken,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDescribeWorkspaceImages' smart constructor.
data DescribeWorkspaceImages = DescribeWorkspaceImages'
  { -- | The identifier of the image.
    imageIds :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The type (owned or shared) of the image.
    imageType :: Lude.Maybe ImageType,
    -- | The maximum number of items to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkspaceImages' with the minimum fields required to make a request.
--
-- * 'imageIds' - The identifier of the image.
-- * 'nextToken' - If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
-- * 'imageType' - The type (owned or shared) of the image.
-- * 'maxResults' - The maximum number of items to return.
mkDescribeWorkspaceImages ::
  DescribeWorkspaceImages
mkDescribeWorkspaceImages =
  DescribeWorkspaceImages'
    { imageIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      imageType = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The identifier of the image.
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiImageIds :: Lens.Lens' DescribeWorkspaceImages (Lude.Maybe (Lude.NonEmpty Lude.Text))
dwiImageIds = Lens.lens (imageIds :: DescribeWorkspaceImages -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {imageIds = a} :: DescribeWorkspaceImages)
{-# DEPRECATED dwiImageIds "Use generic-lens or generic-optics with 'imageIds' instead." #-}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiNextToken :: Lens.Lens' DescribeWorkspaceImages (Lude.Maybe Lude.Text)
dwiNextToken = Lens.lens (nextToken :: DescribeWorkspaceImages -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeWorkspaceImages)
{-# DEPRECATED dwiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The type (owned or shared) of the image.
--
-- /Note:/ Consider using 'imageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiImageType :: Lens.Lens' DescribeWorkspaceImages (Lude.Maybe ImageType)
dwiImageType = Lens.lens (imageType :: DescribeWorkspaceImages -> Lude.Maybe ImageType) (\s a -> s {imageType = a} :: DescribeWorkspaceImages)
{-# DEPRECATED dwiImageType "Use generic-lens or generic-optics with 'imageType' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiMaxResults :: Lens.Lens' DescribeWorkspaceImages (Lude.Maybe Lude.Natural)
dwiMaxResults = Lens.lens (maxResults :: DescribeWorkspaceImages -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeWorkspaceImages)
{-# DEPRECATED dwiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeWorkspaceImages where
  page rq rs
    | Page.stop (rs Lens.^. drsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drsImages) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dwiNextToken Lens..~ rs Lens.^. drsNextToken

instance Lude.AWSRequest DescribeWorkspaceImages where
  type Rs DescribeWorkspaceImages = DescribeWorkspaceImagesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeWorkspaceImagesResponse'
            Lude.<$> (x Lude..?> "Images" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeWorkspaceImages where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.DescribeWorkspaceImages" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeWorkspaceImages where
  toJSON DescribeWorkspaceImages' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ImageIds" Lude..=) Lude.<$> imageIds,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("ImageType" Lude..=) Lude.<$> imageType,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeWorkspaceImages where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeWorkspaceImages where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeWorkspaceImagesResponse' smart constructor.
data DescribeWorkspaceImagesResponse = DescribeWorkspaceImagesResponse'
  { -- | Information about the images.
    images :: Lude.Maybe [WorkspaceImage],
    -- | The token to use to retrieve the next set of results, or null if no more results are available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkspaceImagesResponse' with the minimum fields required to make a request.
--
-- * 'images' - Information about the images.
-- * 'nextToken' - The token to use to retrieve the next set of results, or null if no more results are available.
-- * 'responseStatus' - The response status code.
mkDescribeWorkspaceImagesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeWorkspaceImagesResponse
mkDescribeWorkspaceImagesResponse pResponseStatus_ =
  DescribeWorkspaceImagesResponse'
    { images = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the images.
--
-- /Note:/ Consider using 'images' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsImages :: Lens.Lens' DescribeWorkspaceImagesResponse (Lude.Maybe [WorkspaceImage])
drsImages = Lens.lens (images :: DescribeWorkspaceImagesResponse -> Lude.Maybe [WorkspaceImage]) (\s a -> s {images = a} :: DescribeWorkspaceImagesResponse)
{-# DEPRECATED drsImages "Use generic-lens or generic-optics with 'images' instead." #-}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeWorkspaceImagesResponse (Lude.Maybe Lude.Text)
drsNextToken = Lens.lens (nextToken :: DescribeWorkspaceImagesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeWorkspaceImagesResponse)
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeWorkspaceImagesResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeWorkspaceImagesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeWorkspaceImagesResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
