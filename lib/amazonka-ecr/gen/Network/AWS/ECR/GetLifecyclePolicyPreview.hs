{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.GetLifecyclePolicyPreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the results of the lifecycle policy preview request for the specified repository.
--
-- This operation returns paginated results.
module Network.AWS.ECR.GetLifecyclePolicyPreview
    (
    -- * Creating a request
      GetLifecyclePolicyPreview (..)
    , mkGetLifecyclePolicyPreview
    -- ** Request lenses
    , glppRepositoryName
    , glppFilter
    , glppImageIds
    , glppMaxResults
    , glppNextToken
    , glppRegistryId

    -- * Destructuring the response
    , GetLifecyclePolicyPreviewResponse (..)
    , mkGetLifecyclePolicyPreviewResponse
    -- ** Response lenses
    , glpprrsLifecyclePolicyText
    , glpprrsNextToken
    , glpprrsPreviewResults
    , glpprrsRegistryId
    , glpprrsRepositoryName
    , glpprrsStatus
    , glpprrsSummary
    , glpprrsResponseStatus
    ) where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLifecyclePolicyPreview' smart constructor.
data GetLifecyclePolicyPreview = GetLifecyclePolicyPreview'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository.
  , filter :: Core.Maybe Types.LifecyclePolicyPreviewFilter
    -- ^ An optional parameter that filters results based on image tag status and all tags, if tagged.
  , imageIds :: Core.Maybe [Types.ImageIdentifier]
    -- ^ The list of imageIDs to be included.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of repository results returned by @GetLifecyclePolicyPreviewRequest@ in  paginated output. When this parameter is used, @GetLifecyclePolicyPreviewRequest@ only returns  @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending  another @GetLifecyclePolicyPreviewRequest@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this  parameter is not used, then @GetLifecyclePolicyPreviewRequest@ returns up to  100 results and a @nextToken@ value, if  applicable. This option cannot be used when you specify images with @imageIds@ .
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ value returned from a previous paginated  @GetLifecyclePolicyPreviewRequest@ request where @maxResults@ was used and the  results exceeded the value of that parameter. Pagination continues from the end of the  previous results that returned the @nextToken@ value. This value is  @null@ when there are no more results to return. This option cannot be used when you specify images with @imageIds@ .
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLifecyclePolicyPreview' value with any optional fields omitted.
mkGetLifecyclePolicyPreview
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> GetLifecyclePolicyPreview
mkGetLifecyclePolicyPreview repositoryName
  = GetLifecyclePolicyPreview'{repositoryName, filter = Core.Nothing,
                               imageIds = Core.Nothing, maxResults = Core.Nothing,
                               nextToken = Core.Nothing, registryId = Core.Nothing}

-- | The name of the repository.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppRepositoryName :: Lens.Lens' GetLifecyclePolicyPreview Types.RepositoryName
glppRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE glppRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | An optional parameter that filters results based on image tag status and all tags, if tagged.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppFilter :: Lens.Lens' GetLifecyclePolicyPreview (Core.Maybe Types.LifecyclePolicyPreviewFilter)
glppFilter = Lens.field @"filter"
{-# INLINEABLE glppFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The list of imageIDs to be included.
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppImageIds :: Lens.Lens' GetLifecyclePolicyPreview (Core.Maybe [Types.ImageIdentifier])
glppImageIds = Lens.field @"imageIds"
{-# INLINEABLE glppImageIds #-}
{-# DEPRECATED imageIds "Use generic-lens or generic-optics with 'imageIds' instead"  #-}

-- | The maximum number of repository results returned by @GetLifecyclePolicyPreviewRequest@ in  paginated output. When this parameter is used, @GetLifecyclePolicyPreviewRequest@ only returns  @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending  another @GetLifecyclePolicyPreviewRequest@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this  parameter is not used, then @GetLifecyclePolicyPreviewRequest@ returns up to  100 results and a @nextToken@ value, if  applicable. This option cannot be used when you specify images with @imageIds@ .
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppMaxResults :: Lens.Lens' GetLifecyclePolicyPreview (Core.Maybe Core.Natural)
glppMaxResults = Lens.field @"maxResults"
{-# INLINEABLE glppMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The @nextToken@ value returned from a previous paginated  @GetLifecyclePolicyPreviewRequest@ request where @maxResults@ was used and the  results exceeded the value of that parameter. Pagination continues from the end of the  previous results that returned the @nextToken@ value. This value is  @null@ when there are no more results to return. This option cannot be used when you specify images with @imageIds@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppNextToken :: Lens.Lens' GetLifecyclePolicyPreview (Core.Maybe Types.NextToken)
glppNextToken = Lens.field @"nextToken"
{-# INLINEABLE glppNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppRegistryId :: Lens.Lens' GetLifecyclePolicyPreview (Core.Maybe Types.RegistryId)
glppRegistryId = Lens.field @"registryId"
{-# INLINEABLE glppRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery GetLifecyclePolicyPreview where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetLifecyclePolicyPreview where
        toHeaders GetLifecyclePolicyPreview{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerRegistry_V20150921.GetLifecyclePolicyPreview")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetLifecyclePolicyPreview where
        toJSON GetLifecyclePolicyPreview{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  ("filter" Core..=) Core.<$> filter,
                  ("imageIds" Core..=) Core.<$> imageIds,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("registryId" Core..=) Core.<$> registryId])

instance Core.AWSRequest GetLifecyclePolicyPreview where
        type Rs GetLifecyclePolicyPreview =
             GetLifecyclePolicyPreviewResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetLifecyclePolicyPreviewResponse' Core.<$>
                   (x Core..:? "lifecyclePolicyText") Core.<*> x Core..:? "nextToken"
                     Core.<*> x Core..:? "previewResults"
                     Core.<*> x Core..:? "registryId"
                     Core.<*> x Core..:? "repositoryName"
                     Core.<*> x Core..:? "status"
                     Core.<*> x Core..:? "summary"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetLifecyclePolicyPreview where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"previewResults" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetLifecyclePolicyPreviewResponse' smart constructor.
data GetLifecyclePolicyPreviewResponse = GetLifecyclePolicyPreviewResponse'
  { lifecyclePolicyText :: Core.Maybe Types.LifecyclePolicyText
    -- ^ The JSON lifecycle policy text.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ value to include in a future @GetLifecyclePolicyPreview@ request. When the results of a @GetLifecyclePolicyPreview@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , previewResults :: Core.Maybe [Types.LifecyclePolicyPreviewResult]
    -- ^ The results of the lifecycle policy preview request.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The registry ID associated with the request.
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The repository name associated with the request.
  , status :: Core.Maybe Types.LifecyclePolicyPreviewStatus
    -- ^ The status of the lifecycle policy preview request.
  , summary :: Core.Maybe Types.LifecyclePolicyPreviewSummary
    -- ^ The list of images that is returned as a result of the action.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetLifecyclePolicyPreviewResponse' value with any optional fields omitted.
mkGetLifecyclePolicyPreviewResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetLifecyclePolicyPreviewResponse
mkGetLifecyclePolicyPreviewResponse responseStatus
  = GetLifecyclePolicyPreviewResponse'{lifecyclePolicyText =
                                         Core.Nothing,
                                       nextToken = Core.Nothing, previewResults = Core.Nothing,
                                       registryId = Core.Nothing, repositoryName = Core.Nothing,
                                       status = Core.Nothing, summary = Core.Nothing,
                                       responseStatus}

-- | The JSON lifecycle policy text.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprrsLifecyclePolicyText :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe Types.LifecyclePolicyText)
glpprrsLifecyclePolicyText = Lens.field @"lifecyclePolicyText"
{-# INLINEABLE glpprrsLifecyclePolicyText #-}
{-# DEPRECATED lifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead"  #-}

-- | The @nextToken@ value to include in a future @GetLifecyclePolicyPreview@ request. When the results of a @GetLifecyclePolicyPreview@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprrsNextToken :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe Types.NextToken)
glpprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE glpprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The results of the lifecycle policy preview request.
--
-- /Note:/ Consider using 'previewResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprrsPreviewResults :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe [Types.LifecyclePolicyPreviewResult])
glpprrsPreviewResults = Lens.field @"previewResults"
{-# INLINEABLE glpprrsPreviewResults #-}
{-# DEPRECATED previewResults "Use generic-lens or generic-optics with 'previewResults' instead"  #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprrsRegistryId :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe Types.RegistryId)
glpprrsRegistryId = Lens.field @"registryId"
{-# INLINEABLE glpprrsRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprrsRepositoryName :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe Types.RepositoryName)
glpprrsRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE glpprrsRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The status of the lifecycle policy preview request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprrsStatus :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe Types.LifecyclePolicyPreviewStatus)
glpprrsStatus = Lens.field @"status"
{-# INLINEABLE glpprrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The list of images that is returned as a result of the action.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprrsSummary :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe Types.LifecyclePolicyPreviewSummary)
glpprrsSummary = Lens.field @"summary"
{-# INLINEABLE glpprrsSummary #-}
{-# DEPRECATED summary "Use generic-lens or generic-optics with 'summary' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprrsResponseStatus :: Lens.Lens' GetLifecyclePolicyPreviewResponse Core.Int
glpprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE glpprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
