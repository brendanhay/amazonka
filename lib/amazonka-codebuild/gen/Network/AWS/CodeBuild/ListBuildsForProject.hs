{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListBuildsForProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of build IDs for the specified build project, with each build ID representing a single build.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListBuildsForProject
  ( -- * Creating a request
    ListBuildsForProject (..),
    mkListBuildsForProject,

    -- ** Request lenses
    lbfpProjectName,
    lbfpNextToken,
    lbfpSortOrder,

    -- * Destructuring the response
    ListBuildsForProjectResponse (..),
    mkListBuildsForProjectResponse,

    -- ** Response lenses
    lbfprrsIds,
    lbfprrsNextToken,
    lbfprrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListBuildsForProject' smart constructor.
data ListBuildsForProject = ListBuildsForProject'
  { -- | The name of the AWS CodeBuild project.
    projectName :: Types.NonEmptyString,
    -- | During a previous call, if there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The order to list build IDs. Valid values include:
    --
    --
    --     * @ASCENDING@ : List the build IDs in ascending order by build ID.
    --
    --
    --     * @DESCENDING@ : List the build IDs in descending order by build ID.
    sortOrder :: Core.Maybe Types.SortOrderType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBuildsForProject' value with any optional fields omitted.
mkListBuildsForProject ::
  -- | 'projectName'
  Types.NonEmptyString ->
  ListBuildsForProject
mkListBuildsForProject projectName =
  ListBuildsForProject'
    { projectName,
      nextToken = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | The name of the AWS CodeBuild project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfpProjectName :: Lens.Lens' ListBuildsForProject Types.NonEmptyString
lbfpProjectName = Lens.field @"projectName"
{-# DEPRECATED lbfpProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

-- | During a previous call, if there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfpNextToken :: Lens.Lens' ListBuildsForProject (Core.Maybe Types.NextToken)
lbfpNextToken = Lens.field @"nextToken"
{-# DEPRECATED lbfpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The order to list build IDs. Valid values include:
--
--
--     * @ASCENDING@ : List the build IDs in ascending order by build ID.
--
--
--     * @DESCENDING@ : List the build IDs in descending order by build ID.
--
--
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfpSortOrder :: Lens.Lens' ListBuildsForProject (Core.Maybe Types.SortOrderType)
lbfpSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lbfpSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON ListBuildsForProject where
  toJSON ListBuildsForProject {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("projectName" Core..= projectName),
            ("nextToken" Core..=) Core.<$> nextToken,
            ("sortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest ListBuildsForProject where
  type Rs ListBuildsForProject = ListBuildsForProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeBuild_20161006.ListBuildsForProject")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBuildsForProjectResponse'
            Core.<$> (x Core..:? "ids")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListBuildsForProject where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"ids" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListBuildsForProjectResponse' smart constructor.
data ListBuildsForProjectResponse = ListBuildsForProjectResponse'
  { -- | A list of build IDs for the specified build project, with each build ID representing a single build.
    ids :: Core.Maybe (Core.NonEmpty Types.NonEmptyString),
    -- | If there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBuildsForProjectResponse' value with any optional fields omitted.
mkListBuildsForProjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListBuildsForProjectResponse
mkListBuildsForProjectResponse responseStatus =
  ListBuildsForProjectResponse'
    { ids = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of build IDs for the specified build project, with each build ID representing a single build.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfprrsIds :: Lens.Lens' ListBuildsForProjectResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
lbfprrsIds = Lens.field @"ids"
{-# DEPRECATED lbfprrsIds "Use generic-lens or generic-optics with 'ids' instead." #-}

-- | If there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfprrsNextToken :: Lens.Lens' ListBuildsForProjectResponse (Core.Maybe Types.String)
lbfprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lbfprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfprrsResponseStatus :: Lens.Lens' ListBuildsForProjectResponse Core.Int
lbfprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lbfprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
