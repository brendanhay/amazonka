{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetLifecyclePolicyPreview (..),
    mkGetLifecyclePolicyPreview,

    -- ** Request lenses
    glppRepositoryName,
    glppFilter,
    glppImageIds,
    glppMaxResults,
    glppNextToken,
    glppRegistryId,

    -- * Destructuring the response
    GetLifecyclePolicyPreviewResponse (..),
    mkGetLifecyclePolicyPreviewResponse,

    -- ** Response lenses
    glpprrsLifecyclePolicyText,
    glpprrsNextToken,
    glpprrsPreviewResults,
    glpprrsRegistryId,
    glpprrsRepositoryName,
    glpprrsStatus,
    glpprrsSummary,
    glpprrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLifecyclePolicyPreview' smart constructor.
data GetLifecyclePolicyPreview = GetLifecyclePolicyPreview'
  { -- | The name of the repository.
    repositoryName :: Types.RepositoryName,
    -- | An optional parameter that filters results based on image tag status and all tags, if tagged.
    filter :: Core.Maybe Types.LifecyclePolicyPreviewFilter,
    -- | The list of imageIDs to be included.
    imageIds :: Core.Maybe [Types.ImageIdentifier],
    -- | The maximum number of repository results returned by @GetLifecyclePolicyPreviewRequest@ in  paginated output. When this parameter is used, @GetLifecyclePolicyPreviewRequest@ only returns  @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending  another @GetLifecyclePolicyPreviewRequest@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this  parameter is not used, then @GetLifecyclePolicyPreviewRequest@ returns up to  100 results and a @nextToken@ value, if  applicable. This option cannot be used when you specify images with @imageIds@ .
    maxResults :: Core.Maybe Core.Natural,
    -- | The @nextToken@ value returned from a previous paginated  @GetLifecyclePolicyPreviewRequest@ request where @maxResults@ was used and the  results exceeded the value of that parameter. Pagination continues from the end of the  previous results that returned the @nextToken@ value. This value is  @null@ when there are no more results to return. This option cannot be used when you specify images with @imageIds@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLifecyclePolicyPreview' value with any optional fields omitted.
mkGetLifecyclePolicyPreview ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  GetLifecyclePolicyPreview
mkGetLifecyclePolicyPreview repositoryName =
  GetLifecyclePolicyPreview'
    { repositoryName,
      filter = Core.Nothing,
      imageIds = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      registryId = Core.Nothing
    }

-- | The name of the repository.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppRepositoryName :: Lens.Lens' GetLifecyclePolicyPreview Types.RepositoryName
glppRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED glppRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | An optional parameter that filters results based on image tag status and all tags, if tagged.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppFilter :: Lens.Lens' GetLifecyclePolicyPreview (Core.Maybe Types.LifecyclePolicyPreviewFilter)
glppFilter = Lens.field @"filter"
{-# DEPRECATED glppFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The list of imageIDs to be included.
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppImageIds :: Lens.Lens' GetLifecyclePolicyPreview (Core.Maybe [Types.ImageIdentifier])
glppImageIds = Lens.field @"imageIds"
{-# DEPRECATED glppImageIds "Use generic-lens or generic-optics with 'imageIds' instead." #-}

-- | The maximum number of repository results returned by @GetLifecyclePolicyPreviewRequest@ in  paginated output. When this parameter is used, @GetLifecyclePolicyPreviewRequest@ only returns  @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending  another @GetLifecyclePolicyPreviewRequest@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this  parameter is not used, then @GetLifecyclePolicyPreviewRequest@ returns up to  100 results and a @nextToken@ value, if  applicable. This option cannot be used when you specify images with @imageIds@ .
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppMaxResults :: Lens.Lens' GetLifecyclePolicyPreview (Core.Maybe Core.Natural)
glppMaxResults = Lens.field @"maxResults"
{-# DEPRECATED glppMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @nextToken@ value returned from a previous paginated  @GetLifecyclePolicyPreviewRequest@ request where @maxResults@ was used and the  results exceeded the value of that parameter. Pagination continues from the end of the  previous results that returned the @nextToken@ value. This value is  @null@ when there are no more results to return. This option cannot be used when you specify images with @imageIds@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppNextToken :: Lens.Lens' GetLifecyclePolicyPreview (Core.Maybe Types.NextToken)
glppNextToken = Lens.field @"nextToken"
{-# DEPRECATED glppNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppRegistryId :: Lens.Lens' GetLifecyclePolicyPreview (Core.Maybe Types.RegistryId)
glppRegistryId = Lens.field @"registryId"
{-# DEPRECATED glppRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON GetLifecyclePolicyPreview where
  toJSON GetLifecyclePolicyPreview {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            ("filter" Core..=) Core.<$> filter,
            ("imageIds" Core..=) Core.<$> imageIds,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("registryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest GetLifecyclePolicyPreview where
  type
    Rs GetLifecyclePolicyPreview =
      GetLifecyclePolicyPreviewResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.GetLifecyclePolicyPreview"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLifecyclePolicyPreviewResponse'
            Core.<$> (x Core..:? "lifecyclePolicyText")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "previewResults")
            Core.<*> (x Core..:? "registryId")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "summary")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetLifecyclePolicyPreview where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"previewResults" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetLifecyclePolicyPreviewResponse' smart constructor.
data GetLifecyclePolicyPreviewResponse = GetLifecyclePolicyPreviewResponse'
  { -- | The JSON lifecycle policy text.
    lifecyclePolicyText :: Core.Maybe Types.LifecyclePolicyText,
    -- | The @nextToken@ value to include in a future @GetLifecyclePolicyPreview@ request. When the results of a @GetLifecyclePolicyPreview@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The results of the lifecycle policy preview request.
    previewResults :: Core.Maybe [Types.LifecyclePolicyPreviewResult],
    -- | The registry ID associated with the request.
    registryId :: Core.Maybe Types.RegistryId,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Types.RepositoryName,
    -- | The status of the lifecycle policy preview request.
    status :: Core.Maybe Types.LifecyclePolicyPreviewStatus,
    -- | The list of images that is returned as a result of the action.
    summary :: Core.Maybe Types.LifecyclePolicyPreviewSummary,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetLifecyclePolicyPreviewResponse' value with any optional fields omitted.
mkGetLifecyclePolicyPreviewResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetLifecyclePolicyPreviewResponse
mkGetLifecyclePolicyPreviewResponse responseStatus =
  GetLifecyclePolicyPreviewResponse'
    { lifecyclePolicyText =
        Core.Nothing,
      nextToken = Core.Nothing,
      previewResults = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      status = Core.Nothing,
      summary = Core.Nothing,
      responseStatus
    }

-- | The JSON lifecycle policy text.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprrsLifecyclePolicyText :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe Types.LifecyclePolicyText)
glpprrsLifecyclePolicyText = Lens.field @"lifecyclePolicyText"
{-# DEPRECATED glpprrsLifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead." #-}

-- | The @nextToken@ value to include in a future @GetLifecyclePolicyPreview@ request. When the results of a @GetLifecyclePolicyPreview@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprrsNextToken :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe Types.NextToken)
glpprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED glpprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The results of the lifecycle policy preview request.
--
-- /Note:/ Consider using 'previewResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprrsPreviewResults :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe [Types.LifecyclePolicyPreviewResult])
glpprrsPreviewResults = Lens.field @"previewResults"
{-# DEPRECATED glpprrsPreviewResults "Use generic-lens or generic-optics with 'previewResults' instead." #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprrsRegistryId :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe Types.RegistryId)
glpprrsRegistryId = Lens.field @"registryId"
{-# DEPRECATED glpprrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprrsRepositoryName :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe Types.RepositoryName)
glpprrsRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED glpprrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The status of the lifecycle policy preview request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprrsStatus :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe Types.LifecyclePolicyPreviewStatus)
glpprrsStatus = Lens.field @"status"
{-# DEPRECATED glpprrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The list of images that is returned as a result of the action.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprrsSummary :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe Types.LifecyclePolicyPreviewSummary)
glpprrsSummary = Lens.field @"summary"
{-# DEPRECATED glpprrsSummary "Use generic-lens or generic-optics with 'summary' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprrsResponseStatus :: Lens.Lens' GetLifecyclePolicyPreviewResponse Core.Int
glpprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED glpprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
