{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.DescribeImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about the images in a repository.
--
-- This operation returns paginated results.
module Network.AWS.ECR.DescribeImages
  ( -- * Creating a request
    DescribeImages (..),
    mkDescribeImages,

    -- ** Request lenses
    diRegistryId,
    diImageIds,
    diNextToken,
    diRepositoryName,
    diFilter,
    diMaxResults,

    -- * Destructuring the response
    DescribeImagesResponse (..),
    mkDescribeImagesResponse,

    -- ** Response lenses
    dirsImageDetails,
    dirsNextToken,
    dirsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeImages' smart constructor.
data DescribeImages = DescribeImages'
  { -- | The AWS account ID associated with the registry that contains the repository in which to describe images. If you do not specify a registry, the default registry is assumed.
    registryId :: Lude.Maybe Lude.Text,
    -- | The list of image IDs for the requested repository.
    imageIds :: Lude.Maybe [ImageIdentifier],
    -- | The @nextToken@ value returned from a previous paginated @DescribeImages@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return. This option cannot be used when you specify images with @imageIds@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The repository that contains the images to describe.
    repositoryName :: Lude.Text,
    -- | The filter key and value with which to filter your @DescribeImages@ results.
    filter :: Lude.Maybe DescribeImagesFilter,
    -- | The maximum number of repository results returned by @DescribeImages@ in paginated output. When this parameter is used, @DescribeImages@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeImages@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeImages@ returns up to 100 results and a @nextToken@ value, if applicable. This option cannot be used when you specify images with @imageIds@ .
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImages' with the minimum fields required to make a request.
--
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository in which to describe images. If you do not specify a registry, the default registry is assumed.
-- * 'imageIds' - The list of image IDs for the requested repository.
-- * 'nextToken' - The @nextToken@ value returned from a previous paginated @DescribeImages@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return. This option cannot be used when you specify images with @imageIds@ .
-- * 'repositoryName' - The repository that contains the images to describe.
-- * 'filter' - The filter key and value with which to filter your @DescribeImages@ results.
-- * 'maxResults' - The maximum number of repository results returned by @DescribeImages@ in paginated output. When this parameter is used, @DescribeImages@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeImages@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeImages@ returns up to 100 results and a @nextToken@ value, if applicable. This option cannot be used when you specify images with @imageIds@ .
mkDescribeImages ::
  -- | 'repositoryName'
  Lude.Text ->
  DescribeImages
mkDescribeImages pRepositoryName_ =
  DescribeImages'
    { registryId = Lude.Nothing,
      imageIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      repositoryName = pRepositoryName_,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The AWS account ID associated with the registry that contains the repository in which to describe images. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diRegistryId :: Lens.Lens' DescribeImages (Lude.Maybe Lude.Text)
diRegistryId = Lens.lens (registryId :: DescribeImages -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: DescribeImages)
{-# DEPRECATED diRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The list of image IDs for the requested repository.
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diImageIds :: Lens.Lens' DescribeImages (Lude.Maybe [ImageIdentifier])
diImageIds = Lens.lens (imageIds :: DescribeImages -> Lude.Maybe [ImageIdentifier]) (\s a -> s {imageIds = a} :: DescribeImages)
{-# DEPRECATED diImageIds "Use generic-lens or generic-optics with 'imageIds' instead." #-}

-- | The @nextToken@ value returned from a previous paginated @DescribeImages@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return. This option cannot be used when you specify images with @imageIds@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diNextToken :: Lens.Lens' DescribeImages (Lude.Maybe Lude.Text)
diNextToken = Lens.lens (nextToken :: DescribeImages -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeImages)
{-# DEPRECATED diNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The repository that contains the images to describe.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diRepositoryName :: Lens.Lens' DescribeImages Lude.Text
diRepositoryName = Lens.lens (repositoryName :: DescribeImages -> Lude.Text) (\s a -> s {repositoryName = a} :: DescribeImages)
{-# DEPRECATED diRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The filter key and value with which to filter your @DescribeImages@ results.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diFilter :: Lens.Lens' DescribeImages (Lude.Maybe DescribeImagesFilter)
diFilter = Lens.lens (filter :: DescribeImages -> Lude.Maybe DescribeImagesFilter) (\s a -> s {filter = a} :: DescribeImages)
{-# DEPRECATED diFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of repository results returned by @DescribeImages@ in paginated output. When this parameter is used, @DescribeImages@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeImages@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeImages@ returns up to 100 results and a @nextToken@ value, if applicable. This option cannot be used when you specify images with @imageIds@ .
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diMaxResults :: Lens.Lens' DescribeImages (Lude.Maybe Lude.Natural)
diMaxResults = Lens.lens (maxResults :: DescribeImages -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeImages)
{-# DEPRECATED diMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeImages where
  page rq rs
    | Page.stop (rs Lens.^. dirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dirsImageDetails) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& diNextToken Lens..~ rs Lens.^. dirsNextToken

instance Lude.AWSRequest DescribeImages where
  type Rs DescribeImages = DescribeImagesResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeImagesResponse'
            Lude.<$> (x Lude..?> "imageDetails" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeImages where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.DescribeImages" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeImages where
  toJSON DescribeImages' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            ("imageIds" Lude..=) Lude.<$> imageIds,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            ("filter" Lude..=) Lude.<$> filter,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeImages where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeImages where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeImagesResponse' smart constructor.
data DescribeImagesResponse = DescribeImagesResponse'
  { -- | A list of 'ImageDetail' objects that contain data about the image.
    imageDetails :: Lude.Maybe [ImageDetail],
    -- | The @nextToken@ value to include in a future @DescribeImages@ request. When the results of a @DescribeImages@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImagesResponse' with the minimum fields required to make a request.
--
-- * 'imageDetails' - A list of 'ImageDetail' objects that contain data about the image.
-- * 'nextToken' - The @nextToken@ value to include in a future @DescribeImages@ request. When the results of a @DescribeImages@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeImagesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeImagesResponse
mkDescribeImagesResponse pResponseStatus_ =
  DescribeImagesResponse'
    { imageDetails = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of 'ImageDetail' objects that contain data about the image.
--
-- /Note:/ Consider using 'imageDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsImageDetails :: Lens.Lens' DescribeImagesResponse (Lude.Maybe [ImageDetail])
dirsImageDetails = Lens.lens (imageDetails :: DescribeImagesResponse -> Lude.Maybe [ImageDetail]) (\s a -> s {imageDetails = a} :: DescribeImagesResponse)
{-# DEPRECATED dirsImageDetails "Use generic-lens or generic-optics with 'imageDetails' instead." #-}

-- | The @nextToken@ value to include in a future @DescribeImages@ request. When the results of a @DescribeImages@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsNextToken :: Lens.Lens' DescribeImagesResponse (Lude.Maybe Lude.Text)
dirsNextToken = Lens.lens (nextToken :: DescribeImagesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeImagesResponse)
{-# DEPRECATED dirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DescribeImagesResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DescribeImagesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeImagesResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
