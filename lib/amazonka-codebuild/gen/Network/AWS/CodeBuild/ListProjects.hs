{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of build project names, with each build project name representing a single build project.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListProjects
  ( -- * Creating a request
    ListProjects (..),
    mkListProjects,

    -- ** Request lenses
    lpNextToken,
    lpSortBy,
    lpSortOrder,

    -- * Destructuring the response
    ListProjectsResponse (..),
    mkListProjectsResponse,

    -- ** Response lenses
    lprrsNextToken,
    lprrsProjects,
    lprrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListProjects' smart constructor.
data ListProjects = ListProjects'
  { -- | During a previous call, if there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
    nextToken :: Core.Maybe Types.NonEmptyString,
    -- | The criterion to be used to list build project names. Valid values include:
    --
    --
    --     * @CREATED_TIME@ : List based on when each build project was created.
    --
    --
    --     * @LAST_MODIFIED_TIME@ : List based on when information about each build project was last changed.
    --
    --
    --     * @NAME@ : List based on each build project's name.
    --
    --
    -- Use @sortOrder@ to specify in what order to list the build project names based on the preceding criteria.
    sortBy :: Core.Maybe Types.ProjectSortByType,
    -- | The order in which to list build projects. Valid values include:
    --
    --
    --     * @ASCENDING@ : List in ascending order.
    --
    --
    --     * @DESCENDING@ : List in descending order.
    --
    --
    -- Use @sortBy@ to specify the criterion to be used to list build project names.
    sortOrder :: Core.Maybe Types.SortOrderType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProjects' value with any optional fields omitted.
mkListProjects ::
  ListProjects
mkListProjects =
  ListProjects'
    { nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | During a previous call, if there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListProjects (Core.Maybe Types.NonEmptyString)
lpNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The criterion to be used to list build project names. Valid values include:
--
--
--     * @CREATED_TIME@ : List based on when each build project was created.
--
--
--     * @LAST_MODIFIED_TIME@ : List based on when information about each build project was last changed.
--
--
--     * @NAME@ : List based on each build project's name.
--
--
-- Use @sortOrder@ to specify in what order to list the build project names based on the preceding criteria.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpSortBy :: Lens.Lens' ListProjects (Core.Maybe Types.ProjectSortByType)
lpSortBy = Lens.field @"sortBy"
{-# DEPRECATED lpSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The order in which to list build projects. Valid values include:
--
--
--     * @ASCENDING@ : List in ascending order.
--
--
--     * @DESCENDING@ : List in descending order.
--
--
-- Use @sortBy@ to specify the criterion to be used to list build project names.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpSortOrder :: Lens.Lens' ListProjects (Core.Maybe Types.SortOrderType)
lpSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lpSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON ListProjects where
  toJSON ListProjects {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("sortBy" Core..=) Core.<$> sortBy,
            ("sortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest ListProjects where
  type Rs ListProjects = ListProjectsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.ListProjects")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProjectsResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "projects")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListProjects where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"projects" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { -- | If there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
    nextToken :: Core.Maybe Types.String,
    -- | The list of build project names, with each build project name representing a single build project.
    projects :: Core.Maybe (Core.NonEmpty Types.NonEmptyString),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProjectsResponse' value with any optional fields omitted.
mkListProjectsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListProjectsResponse
mkListProjectsResponse responseStatus =
  ListProjectsResponse'
    { nextToken = Core.Nothing,
      projects = Core.Nothing,
      responseStatus
    }

-- | If there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsNextToken :: Lens.Lens' ListProjectsResponse (Core.Maybe Types.String)
lprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of build project names, with each build project name representing a single build project.
--
-- /Note:/ Consider using 'projects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsProjects :: Lens.Lens' ListProjectsResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
lprrsProjects = Lens.field @"projects"
{-# DEPRECATED lprrsProjects "Use generic-lens or generic-optics with 'projects' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListProjectsResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
