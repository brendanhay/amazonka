{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified images, if the image names or image ARNs are provided. Otherwise, all images in the account are described.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeImages
  ( -- * Creating a request
    DescribeImages (..),
    mkDescribeImages,

    -- ** Request lenses
    diArns,
    diMaxResults,
    diNames,
    diNextToken,
    diType,

    -- * Destructuring the response
    DescribeImagesResponse (..),
    mkDescribeImagesResponse,

    -- ** Response lenses
    dirfrsImages,
    dirfrsNextToken,
    dirfrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeImages' smart constructor.
data DescribeImages = DescribeImages'
  { -- | The ARNs of the public, private, and shared images to describe.
    arns :: Core.Maybe [Types.Arn],
    -- | The maximum size of each page of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The names of the public or private images to describe.
    names :: Core.Maybe [Types.String],
    -- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Types.String,
    -- | The type of image (public, private, or shared) to describe.
    type' :: Core.Maybe Types.VisibilityType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImages' value with any optional fields omitted.
mkDescribeImages ::
  DescribeImages
mkDescribeImages =
  DescribeImages'
    { arns = Core.Nothing,
      maxResults = Core.Nothing,
      names = Core.Nothing,
      nextToken = Core.Nothing,
      type' = Core.Nothing
    }

-- | The ARNs of the public, private, and shared images to describe.
--
-- /Note:/ Consider using 'arns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diArns :: Lens.Lens' DescribeImages (Core.Maybe [Types.Arn])
diArns = Lens.field @"arns"
{-# DEPRECATED diArns "Use generic-lens or generic-optics with 'arns' instead." #-}

-- | The maximum size of each page of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diMaxResults :: Lens.Lens' DescribeImages (Core.Maybe Core.Natural)
diMaxResults = Lens.field @"maxResults"
{-# DEPRECATED diMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The names of the public or private images to describe.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diNames :: Lens.Lens' DescribeImages (Core.Maybe [Types.String])
diNames = Lens.field @"names"
{-# DEPRECATED diNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diNextToken :: Lens.Lens' DescribeImages (Core.Maybe Types.String)
diNextToken = Lens.field @"nextToken"
{-# DEPRECATED diNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The type of image (public, private, or shared) to describe.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diType :: Lens.Lens' DescribeImages (Core.Maybe Types.VisibilityType)
diType = Lens.field @"type'"
{-# DEPRECATED diType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON DescribeImages where
  toJSON DescribeImages {..} =
    Core.object
      ( Core.catMaybes
          [ ("Arns" Core..=) Core.<$> arns,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Names" Core..=) Core.<$> names,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Type" Core..=) Core.<$> type'
          ]
      )

instance Core.AWSRequest DescribeImages where
  type Rs DescribeImages = DescribeImagesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "PhotonAdminProxyService.DescribeImages")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImagesResponse'
            Core.<$> (x Core..:? "Images")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeImages where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"images" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeImagesResponse' smart constructor.
data DescribeImagesResponse = DescribeImagesResponse'
  { -- | Information about the images.
    images :: Core.Maybe [Types.Image],
    -- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeImagesResponse' value with any optional fields omitted.
mkDescribeImagesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeImagesResponse
mkDescribeImagesResponse responseStatus =
  DescribeImagesResponse'
    { images = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the images.
--
-- /Note:/ Consider using 'images' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsImages :: Lens.Lens' DescribeImagesResponse (Core.Maybe [Types.Image])
dirfrsImages = Lens.field @"images"
{-# DEPRECATED dirfrsImages "Use generic-lens or generic-optics with 'images' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsNextToken :: Lens.Lens' DescribeImagesResponse (Core.Maybe Types.String)
dirfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dirfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsResponseStatus :: Lens.Lens' DescribeImagesResponse Core.Int
dirfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dirfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
