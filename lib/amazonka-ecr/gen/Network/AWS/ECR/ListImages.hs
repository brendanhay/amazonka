{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    liRepositoryName,
    liFilter,
    liMaxResults,
    liNextToken,
    liRegistryId,

    -- * Destructuring the response
    ListImagesResponse (..),
    mkListImagesResponse,

    -- ** Response lenses
    lirrsImageIds,
    lirrsNextToken,
    lirrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListImages' smart constructor.
data ListImages = ListImages'
  { -- | The repository with image IDs to be listed.
    repositoryName :: Types.RepositoryName,
    -- | The filter key and value with which to filter your @ListImages@ results.
    filter :: Core.Maybe Types.ListImagesFilter,
    -- | The maximum number of image results returned by @ListImages@ in paginated output. When this parameter is used, @ListImages@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListImages@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @ListImages@ returns up to 100 results and a @nextToken@ value, if applicable.
    maxResults :: Core.Maybe Core.Natural,
    -- | The @nextToken@ value returned from a previous paginated @ListImages@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The AWS account ID associated with the registry that contains the repository in which to list images. If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListImages' value with any optional fields omitted.
mkListImages ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  ListImages
mkListImages repositoryName =
  ListImages'
    { repositoryName,
      filter = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      registryId = Core.Nothing
    }

-- | The repository with image IDs to be listed.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liRepositoryName :: Lens.Lens' ListImages Types.RepositoryName
liRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED liRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The filter key and value with which to filter your @ListImages@ results.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liFilter :: Lens.Lens' ListImages (Core.Maybe Types.ListImagesFilter)
liFilter = Lens.field @"filter"
{-# DEPRECATED liFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of image results returned by @ListImages@ in paginated output. When this parameter is used, @ListImages@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListImages@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @ListImages@ returns up to 100 results and a @nextToken@ value, if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListImages (Core.Maybe Core.Natural)
liMaxResults = Lens.field @"maxResults"
{-# DEPRECATED liMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @nextToken@ value returned from a previous paginated @ListImages@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListImages (Core.Maybe Types.NextToken)
liNextToken = Lens.field @"nextToken"
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository in which to list images. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liRegistryId :: Lens.Lens' ListImages (Core.Maybe Types.RegistryId)
liRegistryId = Lens.field @"registryId"
{-# DEPRECATED liRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON ListImages where
  toJSON ListImages {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            ("filter" Core..=) Core.<$> filter,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("registryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest ListImages where
  type Rs ListImages = ListImagesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonEC2ContainerRegistry_V20150921.ListImages")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImagesResponse'
            Core.<$> (x Core..:? "imageIds")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListImages where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"imageIds" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListImagesResponse' smart constructor.
data ListImagesResponse = ListImagesResponse'
  { -- | The list of image IDs for the requested repository.
    imageIds :: Core.Maybe [Types.ImageIdentifier],
    -- | The @nextToken@ value to include in a future @ListImages@ request. When the results of a @ListImages@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListImagesResponse' value with any optional fields omitted.
mkListImagesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListImagesResponse
mkListImagesResponse responseStatus =
  ListImagesResponse'
    { imageIds = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of image IDs for the requested repository.
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsImageIds :: Lens.Lens' ListImagesResponse (Core.Maybe [Types.ImageIdentifier])
lirrsImageIds = Lens.field @"imageIds"
{-# DEPRECATED lirrsImageIds "Use generic-lens or generic-optics with 'imageIds' instead." #-}

-- | The @nextToken@ value to include in a future @ListImages@ request. When the results of a @ListImages@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsNextToken :: Lens.Lens' ListImagesResponse (Core.Maybe Types.NextToken)
lirrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsResponseStatus :: Lens.Lens' ListImagesResponse Core.Int
lirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
