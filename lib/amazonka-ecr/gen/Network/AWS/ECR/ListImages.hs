{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.ListImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the image IDs for the specified repository.
--
-- You can filter images based on whether or not they are tagged by using the @tagStatus@ filter and specifying either @TAGGED@ , @UNTAGGED@ or @ANY@ . For example, you can filter your results to return only @UNTAGGED@ images and then pipe that result to a 'BatchDeleteImage' operation to delete them. Or, you can filter your results to return only @TAGGED@ images to list all of the tags in your repository.
--
-- This operation returns paginated results.
module Network.AWS.ECR.ListImages
  ( -- * Creating a request
    ListImages (..),
    mkListImages,

    -- ** Request lenses
    liRegistryId,
    liNextToken,
    liFilter,
    liMaxResults,
    liRepositoryName,

    -- * Destructuring the response
    ListImagesResponse (..),
    mkListImagesResponse,

    -- ** Response lenses
    lirsImageIds,
    lirsNextToken,
    lirsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListImages' smart constructor.
data ListImages = ListImages'
  { registryId :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    filter :: Lude.Maybe ListImagesFilter,
    maxResults :: Lude.Maybe Lude.Natural,
    repositoryName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListImages' with the minimum fields required to make a request.
--
-- * 'filter' - The filter key and value with which to filter your @ListImages@ results.
-- * 'maxResults' - The maximum number of image results returned by @ListImages@ in paginated output. When this parameter is used, @ListImages@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListImages@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @ListImages@ returns up to 100 results and a @nextToken@ value, if applicable.
-- * 'nextToken' - The @nextToken@ value returned from a previous paginated @ListImages@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository in which to list images. If you do not specify a registry, the default registry is assumed.
-- * 'repositoryName' - The repository with image IDs to be listed.
mkListImages ::
  -- | 'repositoryName'
  Lude.Text ->
  ListImages
mkListImages pRepositoryName_ =
  ListImages'
    { registryId = Lude.Nothing,
      nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the repository in which to list images. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liRegistryId :: Lens.Lens' ListImages (Lude.Maybe Lude.Text)
liRegistryId = Lens.lens (registryId :: ListImages -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: ListImages)
{-# DEPRECATED liRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The @nextToken@ value returned from a previous paginated @ListImages@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListImages (Lude.Maybe Lude.Text)
liNextToken = Lens.lens (nextToken :: ListImages -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListImages)
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The filter key and value with which to filter your @ListImages@ results.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liFilter :: Lens.Lens' ListImages (Lude.Maybe ListImagesFilter)
liFilter = Lens.lens (filter :: ListImages -> Lude.Maybe ListImagesFilter) (\s a -> s {filter = a} :: ListImages)
{-# DEPRECATED liFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of image results returned by @ListImages@ in paginated output. When this parameter is used, @ListImages@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListImages@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @ListImages@ returns up to 100 results and a @nextToken@ value, if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListImages (Lude.Maybe Lude.Natural)
liMaxResults = Lens.lens (maxResults :: ListImages -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListImages)
{-# DEPRECATED liMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The repository with image IDs to be listed.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liRepositoryName :: Lens.Lens' ListImages Lude.Text
liRepositoryName = Lens.lens (repositoryName :: ListImages -> Lude.Text) (\s a -> s {repositoryName = a} :: ListImages)
{-# DEPRECATED liRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Page.AWSPager ListImages where
  page rq rs
    | Page.stop (rs Lens.^. lirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lirsImageIds) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& liNextToken Lens..~ rs Lens.^. lirsNextToken

instance Lude.AWSRequest ListImages where
  type Rs ListImages = ListImagesResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListImagesResponse'
            Lude.<$> (x Lude..?> "imageIds" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListImages where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.ListImages" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListImages where
  toJSON ListImages' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("filter" Lude..=) Lude.<$> filter,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath ListImages where
  toPath = Lude.const "/"

instance Lude.ToQuery ListImages where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListImagesResponse' smart constructor.
data ListImagesResponse = ListImagesResponse'
  { imageIds ::
      Lude.Maybe [ImageIdentifier],
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

-- | Creates a value of 'ListImagesResponse' with the minimum fields required to make a request.
--
-- * 'imageIds' - The list of image IDs for the requested repository.
-- * 'nextToken' - The @nextToken@ value to include in a future @ListImages@ request. When the results of a @ListImages@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkListImagesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListImagesResponse
mkListImagesResponse pResponseStatus_ =
  ListImagesResponse'
    { imageIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of image IDs for the requested repository.
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsImageIds :: Lens.Lens' ListImagesResponse (Lude.Maybe [ImageIdentifier])
lirsImageIds = Lens.lens (imageIds :: ListImagesResponse -> Lude.Maybe [ImageIdentifier]) (\s a -> s {imageIds = a} :: ListImagesResponse)
{-# DEPRECATED lirsImageIds "Use generic-lens or generic-optics with 'imageIds' instead." #-}

-- | The @nextToken@ value to include in a future @ListImages@ request. When the results of a @ListImages@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
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
