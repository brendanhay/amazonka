{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeImageBuilders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified image builders, if the image builder names are provided. Otherwise, all image builders in the account are described.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeImageBuilders
  ( -- * Creating a request
    DescribeImageBuilders (..),
    mkDescribeImageBuilders,

    -- ** Request lenses
    dibMaxResults,
    dibNames,
    dibNextToken,

    -- * Destructuring the response
    DescribeImageBuildersResponse (..),
    mkDescribeImageBuildersResponse,

    -- ** Response lenses
    dibrfrsImageBuilders,
    dibrfrsNextToken,
    dibrfrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeImageBuilders' smart constructor.
data DescribeImageBuilders = DescribeImageBuilders'
  { -- | The maximum size of each page of results.
    maxResults :: Core.Maybe Core.Int,
    -- | The names of the image builders to describe.
    names :: Core.Maybe [Types.String],
    -- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImageBuilders' value with any optional fields omitted.
mkDescribeImageBuilders ::
  DescribeImageBuilders
mkDescribeImageBuilders =
  DescribeImageBuilders'
    { maxResults = Core.Nothing,
      names = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum size of each page of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibMaxResults :: Lens.Lens' DescribeImageBuilders (Core.Maybe Core.Int)
dibMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dibMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The names of the image builders to describe.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibNames :: Lens.Lens' DescribeImageBuilders (Core.Maybe [Types.String])
dibNames = Lens.field @"names"
{-# DEPRECATED dibNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibNextToken :: Lens.Lens' DescribeImageBuilders (Core.Maybe Types.String)
dibNextToken = Lens.field @"nextToken"
{-# DEPRECATED dibNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeImageBuilders where
  toJSON DescribeImageBuilders {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("Names" Core..=) Core.<$> names,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeImageBuilders where
  type Rs DescribeImageBuilders = DescribeImageBuildersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "PhotonAdminProxyService.DescribeImageBuilders")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImageBuildersResponse'
            Core.<$> (x Core..:? "ImageBuilders")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeImageBuilders where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"imageBuilders" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeImageBuildersResponse' smart constructor.
data DescribeImageBuildersResponse = DescribeImageBuildersResponse'
  { -- | Information about the image builders.
    imageBuilders :: Core.Maybe [Types.ImageBuilder],
    -- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeImageBuildersResponse' value with any optional fields omitted.
mkDescribeImageBuildersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeImageBuildersResponse
mkDescribeImageBuildersResponse responseStatus =
  DescribeImageBuildersResponse'
    { imageBuilders = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the image builders.
--
-- /Note:/ Consider using 'imageBuilders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibrfrsImageBuilders :: Lens.Lens' DescribeImageBuildersResponse (Core.Maybe [Types.ImageBuilder])
dibrfrsImageBuilders = Lens.field @"imageBuilders"
{-# DEPRECATED dibrfrsImageBuilders "Use generic-lens or generic-optics with 'imageBuilders' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibrfrsNextToken :: Lens.Lens' DescribeImageBuildersResponse (Core.Maybe Types.String)
dibrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dibrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibrfrsResponseStatus :: Lens.Lens' DescribeImageBuildersResponse Core.Int
dibrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dibrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
