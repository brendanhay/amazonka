{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.DescribeImageScanFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the scan findings for the specified image.
--
-- This operation returns paginated results.
module Network.AWS.ECR.DescribeImageScanFindings
  ( -- * Creating a request
    DescribeImageScanFindings (..),
    mkDescribeImageScanFindings,

    -- ** Request lenses
    disfRepositoryName,
    disfImageId,
    disfMaxResults,
    disfNextToken,
    disfRegistryId,

    -- * Destructuring the response
    DescribeImageScanFindingsResponse (..),
    mkDescribeImageScanFindingsResponse,

    -- ** Response lenses
    disfrrsImageId,
    disfrrsImageScanFindings,
    disfrrsImageScanStatus,
    disfrrsNextToken,
    disfrrsRegistryId,
    disfrrsRepositoryName,
    disfrrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeImageScanFindings' smart constructor.
data DescribeImageScanFindings = DescribeImageScanFindings'
  { -- | The repository for the image for which to describe the scan findings.
    repositoryName :: Types.RepositoryName,
    imageId :: Types.ImageIdentifier,
    -- | The maximum number of image scan results returned by @DescribeImageScanFindings@ in paginated output. When this parameter is used, @DescribeImageScanFindings@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeImageScanFindings@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeImageScanFindings@ returns up to 100 results and a @nextToken@ value, if applicable.
    maxResults :: Core.Maybe Core.Natural,
    -- | The @nextToken@ value returned from a previous paginated @DescribeImageScanFindings@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is null when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The AWS account ID associated with the registry that contains the repository in which to describe the image scan findings for. If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImageScanFindings' value with any optional fields omitted.
mkDescribeImageScanFindings ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'imageId'
  Types.ImageIdentifier ->
  DescribeImageScanFindings
mkDescribeImageScanFindings repositoryName imageId =
  DescribeImageScanFindings'
    { repositoryName,
      imageId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      registryId = Core.Nothing
    }

-- | The repository for the image for which to describe the scan findings.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfRepositoryName :: Lens.Lens' DescribeImageScanFindings Types.RepositoryName
disfRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED disfRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfImageId :: Lens.Lens' DescribeImageScanFindings Types.ImageIdentifier
disfImageId = Lens.field @"imageId"
{-# DEPRECATED disfImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The maximum number of image scan results returned by @DescribeImageScanFindings@ in paginated output. When this parameter is used, @DescribeImageScanFindings@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeImageScanFindings@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeImageScanFindings@ returns up to 100 results and a @nextToken@ value, if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfMaxResults :: Lens.Lens' DescribeImageScanFindings (Core.Maybe Core.Natural)
disfMaxResults = Lens.field @"maxResults"
{-# DEPRECATED disfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @nextToken@ value returned from a previous paginated @DescribeImageScanFindings@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfNextToken :: Lens.Lens' DescribeImageScanFindings (Core.Maybe Types.NextToken)
disfNextToken = Lens.field @"nextToken"
{-# DEPRECATED disfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository in which to describe the image scan findings for. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfRegistryId :: Lens.Lens' DescribeImageScanFindings (Core.Maybe Types.RegistryId)
disfRegistryId = Lens.field @"registryId"
{-# DEPRECATED disfRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON DescribeImageScanFindings where
  toJSON DescribeImageScanFindings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("imageId" Core..= imageId),
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("registryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest DescribeImageScanFindings where
  type
    Rs DescribeImageScanFindings =
      DescribeImageScanFindingsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.DescribeImageScanFindings"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImageScanFindingsResponse'
            Core.<$> (x Core..:? "imageId")
            Core.<*> (x Core..:? "imageScanFindings")
            Core.<*> (x Core..:? "imageScanStatus")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "registryId")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeImageScanFindings where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"imageScanFindings" Core.. Lens._Just
              Core.. Lens.field @"findings"
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeImageScanFindingsResponse' smart constructor.
data DescribeImageScanFindingsResponse = DescribeImageScanFindingsResponse'
  { imageId :: Core.Maybe Types.ImageIdentifier,
    -- | The information contained in the image scan findings.
    imageScanFindings :: Core.Maybe Types.ImageScanFindings,
    -- | The current state of the scan.
    imageScanStatus :: Core.Maybe Types.ImageScanStatus,
    -- | The @nextToken@ value to include in a future @DescribeImageScanFindings@ request. When the results of a @DescribeImageScanFindings@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is null when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The registry ID associated with the request.
    registryId :: Core.Maybe Types.RegistryId,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Types.RepositoryName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeImageScanFindingsResponse' value with any optional fields omitted.
mkDescribeImageScanFindingsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeImageScanFindingsResponse
mkDescribeImageScanFindingsResponse responseStatus =
  DescribeImageScanFindingsResponse'
    { imageId = Core.Nothing,
      imageScanFindings = Core.Nothing,
      imageScanStatus = Core.Nothing,
      nextToken = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfrrsImageId :: Lens.Lens' DescribeImageScanFindingsResponse (Core.Maybe Types.ImageIdentifier)
disfrrsImageId = Lens.field @"imageId"
{-# DEPRECATED disfrrsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The information contained in the image scan findings.
--
-- /Note:/ Consider using 'imageScanFindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfrrsImageScanFindings :: Lens.Lens' DescribeImageScanFindingsResponse (Core.Maybe Types.ImageScanFindings)
disfrrsImageScanFindings = Lens.field @"imageScanFindings"
{-# DEPRECATED disfrrsImageScanFindings "Use generic-lens or generic-optics with 'imageScanFindings' instead." #-}

-- | The current state of the scan.
--
-- /Note:/ Consider using 'imageScanStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfrrsImageScanStatus :: Lens.Lens' DescribeImageScanFindingsResponse (Core.Maybe Types.ImageScanStatus)
disfrrsImageScanStatus = Lens.field @"imageScanStatus"
{-# DEPRECATED disfrrsImageScanStatus "Use generic-lens or generic-optics with 'imageScanStatus' instead." #-}

-- | The @nextToken@ value to include in a future @DescribeImageScanFindings@ request. When the results of a @DescribeImageScanFindings@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfrrsNextToken :: Lens.Lens' DescribeImageScanFindingsResponse (Core.Maybe Types.NextToken)
disfrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED disfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfrrsRegistryId :: Lens.Lens' DescribeImageScanFindingsResponse (Core.Maybe Types.RegistryId)
disfrrsRegistryId = Lens.field @"registryId"
{-# DEPRECATED disfrrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfrrsRepositoryName :: Lens.Lens' DescribeImageScanFindingsResponse (Core.Maybe Types.RepositoryName)
disfrrsRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED disfrrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfrrsResponseStatus :: Lens.Lens' DescribeImageScanFindingsResponse Core.Int
disfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED disfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
