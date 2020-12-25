{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.ListTeamMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all team members associated with a project.
--
-- This operation returns paginated results.
module Network.AWS.CodeStar.ListTeamMembers
  ( -- * Creating a request
    ListTeamMembers (..),
    mkListTeamMembers,

    -- ** Request lenses
    ltmProjectId,
    ltmMaxResults,
    ltmNextToken,

    -- * Destructuring the response
    ListTeamMembersResponse (..),
    mkListTeamMembersResponse,

    -- ** Response lenses
    ltmrrsTeamMembers,
    ltmrrsNextToken,
    ltmrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTeamMembers' smart constructor.
data ListTeamMembers = ListTeamMembers'
  { -- | The ID of the project for which you want to list team members.
    projectId :: Types.ProjectId,
    -- | The maximum number of team members you want returned in a response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The continuation token for the next set of results, if the results cannot be returned in one response.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTeamMembers' value with any optional fields omitted.
mkListTeamMembers ::
  -- | 'projectId'
  Types.ProjectId ->
  ListTeamMembers
mkListTeamMembers projectId =
  ListTeamMembers'
    { projectId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the project for which you want to list team members.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmProjectId :: Lens.Lens' ListTeamMembers Types.ProjectId
ltmProjectId = Lens.field @"projectId"
{-# DEPRECATED ltmProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

-- | The maximum number of team members you want returned in a response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmMaxResults :: Lens.Lens' ListTeamMembers (Core.Maybe Core.Natural)
ltmMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The continuation token for the next set of results, if the results cannot be returned in one response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmNextToken :: Lens.Lens' ListTeamMembers (Core.Maybe Types.PaginationToken)
ltmNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListTeamMembers where
  toJSON ListTeamMembers {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("projectId" Core..= projectId),
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListTeamMembers where
  type Rs ListTeamMembers = ListTeamMembersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeStar_20170419.ListTeamMembers")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTeamMembersResponse'
            Core.<$> (x Core..:? "teamMembers" Core..!= Core.mempty)
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListTeamMembers where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"teamMembers") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListTeamMembersResponse' smart constructor.
data ListTeamMembersResponse = ListTeamMembersResponse'
  { -- | A list of team member objects for the project.
    teamMembers :: [Types.TeamMember],
    -- | The continuation token to use when requesting the next set of results, if there are more results to be returned.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTeamMembersResponse' value with any optional fields omitted.
mkListTeamMembersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTeamMembersResponse
mkListTeamMembersResponse responseStatus =
  ListTeamMembersResponse'
    { teamMembers = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of team member objects for the project.
--
-- /Note:/ Consider using 'teamMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmrrsTeamMembers :: Lens.Lens' ListTeamMembersResponse [Types.TeamMember]
ltmrrsTeamMembers = Lens.field @"teamMembers"
{-# DEPRECATED ltmrrsTeamMembers "Use generic-lens or generic-optics with 'teamMembers' instead." #-}

-- | The continuation token to use when requesting the next set of results, if there are more results to be returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmrrsNextToken :: Lens.Lens' ListTeamMembersResponse (Core.Maybe Types.PaginationToken)
ltmrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltmrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmrrsResponseStatus :: Lens.Lens' ListTeamMembersResponse Core.Int
ltmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
