{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the images in your account and their properties. The list can be filtered by creation time or modified time, and whether the image name contains a specified string.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListImages
  ( -- * Creating a request
    ListImages (..),
    mkListImages,

    -- ** Request lenses
    liCreationTimeAfter,
    liCreationTimeBefore,
    liLastModifiedTimeAfter,
    liLastModifiedTimeBefore,
    liMaxResults,
    liNameContains,
    liNextToken,
    liSortBy,
    liSortOrder,

    -- * Destructuring the response
    ListImagesResponse (..),
    mkListImagesResponse,

    -- ** Response lenses
    lirrsImages,
    lirrsNextToken,
    lirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListImages' smart constructor.
data ListImages = ListImages'
  { -- | A filter that returns only images created on or after the specified time.
    creationTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only images created on or before the specified time.
    creationTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only images modified on or after the specified time.
    lastModifiedTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only images modified on or before the specified time.
    lastModifiedTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of images to return in the response. The default value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only images whose name contains the specified string.
    nameContains :: Core.Maybe Types.ImageNameContains,
    -- | If the previous call to @ListImages@ didn't return the full set of images, the call returns a token for getting the next set of images.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The property used to sort results. The default value is @CREATION_TIME@ .
    sortBy :: Core.Maybe Types.ImageSortBy,
    -- | The sort order. The default value is @DESCENDING@ .
    sortOrder :: Core.Maybe Types.ImageSortOrder
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListImages' value with any optional fields omitted.
mkListImages ::
  ListImages
mkListImages =
  ListImages'
    { creationTimeAfter = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      lastModifiedTimeAfter = Core.Nothing,
      lastModifiedTimeBefore = Core.Nothing,
      maxResults = Core.Nothing,
      nameContains = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | A filter that returns only images created on or after the specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liCreationTimeAfter :: Lens.Lens' ListImages (Core.Maybe Core.NominalDiffTime)
liCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# DEPRECATED liCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A filter that returns only images created on or before the specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liCreationTimeBefore :: Lens.Lens' ListImages (Core.Maybe Core.NominalDiffTime)
liCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# DEPRECATED liCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that returns only images modified on or after the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liLastModifiedTimeAfter :: Lens.Lens' ListImages (Core.Maybe Core.NominalDiffTime)
liLastModifiedTimeAfter = Lens.field @"lastModifiedTimeAfter"
{-# DEPRECATED liLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only images modified on or before the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liLastModifiedTimeBefore :: Lens.Lens' ListImages (Core.Maybe Core.NominalDiffTime)
liLastModifiedTimeBefore = Lens.field @"lastModifiedTimeBefore"
{-# DEPRECATED liLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | The maximum number of images to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListImages (Core.Maybe Core.Natural)
liMaxResults = Lens.field @"maxResults"
{-# DEPRECATED liMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A filter that returns only images whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNameContains :: Lens.Lens' ListImages (Core.Maybe Types.ImageNameContains)
liNameContains = Lens.field @"nameContains"
{-# DEPRECATED liNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the previous call to @ListImages@ didn't return the full set of images, the call returns a token for getting the next set of images.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListImages (Core.Maybe Types.NextToken)
liNextToken = Lens.field @"nextToken"
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The property used to sort results. The default value is @CREATION_TIME@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liSortBy :: Lens.Lens' ListImages (Core.Maybe Types.ImageSortBy)
liSortBy = Lens.field @"sortBy"
{-# DEPRECATED liSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order. The default value is @DESCENDING@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liSortOrder :: Lens.Lens' ListImages (Core.Maybe Types.ImageSortOrder)
liSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED liSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON ListImages where
  toJSON ListImages {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
            ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Core..=) Core.<$> lastModifiedTimeAfter,
            ("LastModifiedTimeBefore" Core..=) Core.<$> lastModifiedTimeBefore,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder
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
          Core.pure ("X-Amz-Target", "SageMaker.ListImages")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImagesResponse'
            Core.<$> (x Core..:? "Images")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListImages where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"images" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListImagesResponse' smart constructor.
data ListImagesResponse = ListImagesResponse'
  { -- | A list of images and their properties.
    images :: Core.Maybe [Types.Image],
    -- | A token for getting the next set of images, if there are any.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListImagesResponse' value with any optional fields omitted.
mkListImagesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListImagesResponse
mkListImagesResponse responseStatus =
  ListImagesResponse'
    { images = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of images and their properties.
--
-- /Note:/ Consider using 'images' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsImages :: Lens.Lens' ListImagesResponse (Core.Maybe [Types.Image])
lirrsImages = Lens.field @"images"
{-# DEPRECATED lirrsImages "Use generic-lens or generic-optics with 'images' instead." #-}

-- | A token for getting the next set of images, if there are any.
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
