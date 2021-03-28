{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeImages (..)
    , mkDescribeImages
    -- ** Request lenses
    , diRepositoryName
    , diFilter
    , diImageIds
    , diMaxResults
    , diNextToken
    , diRegistryId

    -- * Destructuring the response
    , DescribeImagesResponse (..)
    , mkDescribeImagesResponse
    -- ** Response lenses
    , dirrsImageDetails
    , dirrsNextToken
    , dirrsResponseStatus
    ) where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeImages' smart constructor.
data DescribeImages = DescribeImages'
  { repositoryName :: Types.RepositoryName
    -- ^ The repository that contains the images to describe.
  , filter :: Core.Maybe Types.DescribeImagesFilter
    -- ^ The filter key and value with which to filter your @DescribeImages@ results.
  , imageIds :: Core.Maybe [Types.ImageIdentifier]
    -- ^ The list of image IDs for the requested repository.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of repository results returned by @DescribeImages@ in paginated output. When this parameter is used, @DescribeImages@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeImages@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeImages@ returns up to 100 results and a @nextToken@ value, if applicable. This option cannot be used when you specify images with @imageIds@ .
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ value returned from a previous paginated @DescribeImages@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return. This option cannot be used when you specify images with @imageIds@ .
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry that contains the repository in which to describe images. If you do not specify a registry, the default registry is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImages' value with any optional fields omitted.
mkDescribeImages
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> DescribeImages
mkDescribeImages repositoryName
  = DescribeImages'{repositoryName, filter = Core.Nothing,
                    imageIds = Core.Nothing, maxResults = Core.Nothing,
                    nextToken = Core.Nothing, registryId = Core.Nothing}

-- | The repository that contains the images to describe.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diRepositoryName :: Lens.Lens' DescribeImages Types.RepositoryName
diRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE diRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The filter key and value with which to filter your @DescribeImages@ results.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diFilter :: Lens.Lens' DescribeImages (Core.Maybe Types.DescribeImagesFilter)
diFilter = Lens.field @"filter"
{-# INLINEABLE diFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The list of image IDs for the requested repository.
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diImageIds :: Lens.Lens' DescribeImages (Core.Maybe [Types.ImageIdentifier])
diImageIds = Lens.field @"imageIds"
{-# INLINEABLE diImageIds #-}
{-# DEPRECATED imageIds "Use generic-lens or generic-optics with 'imageIds' instead"  #-}

-- | The maximum number of repository results returned by @DescribeImages@ in paginated output. When this parameter is used, @DescribeImages@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeImages@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeImages@ returns up to 100 results and a @nextToken@ value, if applicable. This option cannot be used when you specify images with @imageIds@ .
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diMaxResults :: Lens.Lens' DescribeImages (Core.Maybe Core.Natural)
diMaxResults = Lens.field @"maxResults"
{-# INLINEABLE diMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The @nextToken@ value returned from a previous paginated @DescribeImages@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return. This option cannot be used when you specify images with @imageIds@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diNextToken :: Lens.Lens' DescribeImages (Core.Maybe Types.NextToken)
diNextToken = Lens.field @"nextToken"
{-# INLINEABLE diNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The AWS account ID associated with the registry that contains the repository in which to describe images. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diRegistryId :: Lens.Lens' DescribeImages (Core.Maybe Types.RegistryId)
diRegistryId = Lens.field @"registryId"
{-# INLINEABLE diRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery DescribeImages where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeImages where
        toHeaders DescribeImages{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerRegistry_V20150921.DescribeImages")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeImages where
        toJSON DescribeImages{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  ("filter" Core..=) Core.<$> filter,
                  ("imageIds" Core..=) Core.<$> imageIds,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("registryId" Core..=) Core.<$> registryId])

instance Core.AWSRequest DescribeImages where
        type Rs DescribeImages = DescribeImagesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeImagesResponse' Core.<$>
                   (x Core..:? "imageDetails") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeImages where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"imageDetails" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeImagesResponse' smart constructor.
data DescribeImagesResponse = DescribeImagesResponse'
  { imageDetails :: Core.Maybe [Types.ImageDetail]
    -- ^ A list of 'ImageDetail' objects that contain data about the image.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ value to include in a future @DescribeImages@ request. When the results of a @DescribeImages@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeImagesResponse' value with any optional fields omitted.
mkDescribeImagesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeImagesResponse
mkDescribeImagesResponse responseStatus
  = DescribeImagesResponse'{imageDetails = Core.Nothing,
                            nextToken = Core.Nothing, responseStatus}

-- | A list of 'ImageDetail' objects that contain data about the image.
--
-- /Note:/ Consider using 'imageDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsImageDetails :: Lens.Lens' DescribeImagesResponse (Core.Maybe [Types.ImageDetail])
dirrsImageDetails = Lens.field @"imageDetails"
{-# INLINEABLE dirrsImageDetails #-}
{-# DEPRECATED imageDetails "Use generic-lens or generic-optics with 'imageDetails' instead"  #-}

-- | The @nextToken@ value to include in a future @DescribeImages@ request. When the results of a @DescribeImages@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsNextToken :: Lens.Lens' DescribeImagesResponse (Core.Maybe Types.NextToken)
dirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DescribeImagesResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
