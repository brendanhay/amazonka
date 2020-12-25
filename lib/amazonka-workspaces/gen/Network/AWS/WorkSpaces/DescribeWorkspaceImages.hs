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
    dwiImageType,
    dwiMaxResults,
    dwiNextToken,

    -- * Destructuring the response
    DescribeWorkspaceImagesResponse (..),
    mkDescribeWorkspaceImagesResponse,

    -- ** Response lenses
    dwirrsImages,
    dwirrsNextToken,
    dwirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDescribeWorkspaceImages' smart constructor.
data DescribeWorkspaceImages = DescribeWorkspaceImages'
  { -- | The identifier of the image.
    imageIds :: Core.Maybe (Core.NonEmpty Types.WorkspaceImageId),
    -- | The type (owned or shared) of the image.
    imageType :: Core.Maybe Types.ImageType,
    -- | The maximum number of items to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkspaceImages' value with any optional fields omitted.
mkDescribeWorkspaceImages ::
  DescribeWorkspaceImages
mkDescribeWorkspaceImages =
  DescribeWorkspaceImages'
    { imageIds = Core.Nothing,
      imageType = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the image.
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiImageIds :: Lens.Lens' DescribeWorkspaceImages (Core.Maybe (Core.NonEmpty Types.WorkspaceImageId))
dwiImageIds = Lens.field @"imageIds"
{-# DEPRECATED dwiImageIds "Use generic-lens or generic-optics with 'imageIds' instead." #-}

-- | The type (owned or shared) of the image.
--
-- /Note:/ Consider using 'imageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiImageType :: Lens.Lens' DescribeWorkspaceImages (Core.Maybe Types.ImageType)
dwiImageType = Lens.field @"imageType"
{-# DEPRECATED dwiImageType "Use generic-lens or generic-optics with 'imageType' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiMaxResults :: Lens.Lens' DescribeWorkspaceImages (Core.Maybe Core.Natural)
dwiMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dwiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiNextToken :: Lens.Lens' DescribeWorkspaceImages (Core.Maybe Types.PaginationToken)
dwiNextToken = Lens.field @"nextToken"
{-# DEPRECATED dwiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeWorkspaceImages where
  toJSON DescribeWorkspaceImages {..} =
    Core.object
      ( Core.catMaybes
          [ ("ImageIds" Core..=) Core.<$> imageIds,
            ("ImageType" Core..=) Core.<$> imageType,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeWorkspaceImages where
  type Rs DescribeWorkspaceImages = DescribeWorkspaceImagesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkspacesService.DescribeWorkspaceImages")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspaceImagesResponse'
            Core.<$> (x Core..:? "Images")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeWorkspaceImages where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"images" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeWorkspaceImagesResponse' smart constructor.
data DescribeWorkspaceImagesResponse = DescribeWorkspaceImagesResponse'
  { -- | Information about the images.
    images :: Core.Maybe [Types.WorkspaceImage],
    -- | The token to use to retrieve the next set of results, or null if no more results are available.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeWorkspaceImagesResponse' value with any optional fields omitted.
mkDescribeWorkspaceImagesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeWorkspaceImagesResponse
mkDescribeWorkspaceImagesResponse responseStatus =
  DescribeWorkspaceImagesResponse'
    { images = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the images.
--
-- /Note:/ Consider using 'images' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwirrsImages :: Lens.Lens' DescribeWorkspaceImagesResponse (Core.Maybe [Types.WorkspaceImage])
dwirrsImages = Lens.field @"images"
{-# DEPRECATED dwirrsImages "Use generic-lens or generic-optics with 'images' instead." #-}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwirrsNextToken :: Lens.Lens' DescribeWorkspaceImagesResponse (Core.Maybe Types.PaginationToken)
dwirrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dwirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwirrsResponseStatus :: Lens.Lens' DescribeWorkspaceImagesResponse Core.Int
dwirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dwirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
