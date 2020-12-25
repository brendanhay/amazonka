{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.ListTagsForProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the tags for a project.
module Network.AWS.CodeStar.ListTagsForProject
  ( -- * Creating a request
    ListTagsForProject (..),
    mkListTagsForProject,

    -- ** Request lenses
    ltfpId,
    ltfpMaxResults,
    ltfpNextToken,

    -- * Destructuring the response
    ListTagsForProjectResponse (..),
    mkListTagsForProjectResponse,

    -- ** Response lenses
    ltfprrsNextToken,
    ltfprrsTags,
    ltfprrsResponseStatus,
  )
where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTagsForProject' smart constructor.
data ListTagsForProject = ListTagsForProject'
  { -- | The ID of the project to get tags for.
    id :: Types.Id,
    -- | Reserved for future use.
    maxResults :: Core.Maybe Core.Natural,
    -- | Reserved for future use.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForProject' value with any optional fields omitted.
mkListTagsForProject ::
  -- | 'id'
  Types.Id ->
  ListTagsForProject
mkListTagsForProject id =
  ListTagsForProject'
    { id,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the project to get tags for.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpId :: Lens.Lens' ListTagsForProject Types.Id
ltfpId = Lens.field @"id"
{-# DEPRECATED ltfpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpMaxResults :: Lens.Lens' ListTagsForProject (Core.Maybe Core.Natural)
ltfpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltfpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpNextToken :: Lens.Lens' ListTagsForProject (Core.Maybe Types.PaginationToken)
ltfpNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltfpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListTagsForProject where
  toJSON ListTagsForProject {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("id" Core..= id),
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListTagsForProject where
  type Rs ListTagsForProject = ListTagsForProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeStar_20170419.ListTagsForProject")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForProjectResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListTagsForProjectResponse' smart constructor.
data ListTagsForProjectResponse = ListTagsForProjectResponse'
  { -- | Reserved for future use.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The tags for the project.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForProjectResponse' value with any optional fields omitted.
mkListTagsForProjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTagsForProjectResponse
mkListTagsForProjectResponse responseStatus =
  ListTagsForProjectResponse'
    { nextToken = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | Reserved for future use.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprrsNextToken :: Lens.Lens' ListTagsForProjectResponse (Core.Maybe Types.PaginationToken)
ltfprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltfprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The tags for the project.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprrsTags :: Lens.Lens' ListTagsForProjectResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
ltfprrsTags = Lens.field @"tags"
{-# DEPRECATED ltfprrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprrsResponseStatus :: Lens.Lens' ListTagsForProjectResponse Core.Int
ltfprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltfprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
