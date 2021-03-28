{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeWorkspaceImages (..)
    , mkDescribeWorkspaceImages
    -- ** Request lenses
    , dwiImageIds
    , dwiImageType
    , dwiMaxResults
    , dwiNextToken

    -- * Destructuring the response
    , DescribeWorkspaceImagesResponse (..)
    , mkDescribeWorkspaceImagesResponse
    -- ** Response lenses
    , dwirrsImages
    , dwirrsNextToken
    , dwirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDescribeWorkspaceImages' smart constructor.
data DescribeWorkspaceImages = DescribeWorkspaceImages'
  { imageIds :: Core.Maybe (Core.NonEmpty Types.WorkspaceImageId)
    -- ^ The identifier of the image.
  , imageType :: Core.Maybe Types.ImageType
    -- ^ The type (owned or shared) of the image.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkspaceImages' value with any optional fields omitted.
mkDescribeWorkspaceImages
    :: DescribeWorkspaceImages
mkDescribeWorkspaceImages
  = DescribeWorkspaceImages'{imageIds = Core.Nothing,
                             imageType = Core.Nothing, maxResults = Core.Nothing,
                             nextToken = Core.Nothing}

-- | The identifier of the image.
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiImageIds :: Lens.Lens' DescribeWorkspaceImages (Core.Maybe (Core.NonEmpty Types.WorkspaceImageId))
dwiImageIds = Lens.field @"imageIds"
{-# INLINEABLE dwiImageIds #-}
{-# DEPRECATED imageIds "Use generic-lens or generic-optics with 'imageIds' instead"  #-}

-- | The type (owned or shared) of the image.
--
-- /Note:/ Consider using 'imageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiImageType :: Lens.Lens' DescribeWorkspaceImages (Core.Maybe Types.ImageType)
dwiImageType = Lens.field @"imageType"
{-# INLINEABLE dwiImageType #-}
{-# DEPRECATED imageType "Use generic-lens or generic-optics with 'imageType' instead"  #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiMaxResults :: Lens.Lens' DescribeWorkspaceImages (Core.Maybe Core.Natural)
dwiMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dwiMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiNextToken :: Lens.Lens' DescribeWorkspaceImages (Core.Maybe Types.PaginationToken)
dwiNextToken = Lens.field @"nextToken"
{-# INLINEABLE dwiNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeWorkspaceImages where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeWorkspaceImages where
        toHeaders DescribeWorkspaceImages{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.DescribeWorkspaceImages")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeWorkspaceImages where
        toJSON DescribeWorkspaceImages{..}
          = Core.object
              (Core.catMaybes
                 [("ImageIds" Core..=) Core.<$> imageIds,
                  ("ImageType" Core..=) Core.<$> imageType,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeWorkspaceImages where
        type Rs DescribeWorkspaceImages = DescribeWorkspaceImagesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeWorkspaceImagesResponse' Core.<$>
                   (x Core..:? "Images") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeWorkspaceImages where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"images" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeWorkspaceImagesResponse' smart constructor.
data DescribeWorkspaceImagesResponse = DescribeWorkspaceImagesResponse'
  { images :: Core.Maybe [Types.WorkspaceImage]
    -- ^ Information about the images.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The token to use to retrieve the next set of results, or null if no more results are available.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeWorkspaceImagesResponse' value with any optional fields omitted.
mkDescribeWorkspaceImagesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeWorkspaceImagesResponse
mkDescribeWorkspaceImagesResponse responseStatus
  = DescribeWorkspaceImagesResponse'{images = Core.Nothing,
                                     nextToken = Core.Nothing, responseStatus}

-- | Information about the images.
--
-- /Note:/ Consider using 'images' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwirrsImages :: Lens.Lens' DescribeWorkspaceImagesResponse (Core.Maybe [Types.WorkspaceImage])
dwirrsImages = Lens.field @"images"
{-# INLINEABLE dwirrsImages #-}
{-# DEPRECATED images "Use generic-lens or generic-optics with 'images' instead"  #-}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwirrsNextToken :: Lens.Lens' DescribeWorkspaceImagesResponse (Core.Maybe Types.PaginationToken)
dwirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dwirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwirrsResponseStatus :: Lens.Lens' DescribeWorkspaceImagesResponse Core.Int
dwirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dwirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
