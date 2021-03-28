{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListTeamMembers (..)
    , mkListTeamMembers
    -- ** Request lenses
    , ltmProjectId
    , ltmMaxResults
    , ltmNextToken

    -- * Destructuring the response
    , ListTeamMembersResponse (..)
    , mkListTeamMembersResponse
    -- ** Response lenses
    , ltmrrsTeamMembers
    , ltmrrsNextToken
    , ltmrrsResponseStatus
    ) where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTeamMembers' smart constructor.
data ListTeamMembers = ListTeamMembers'
  { projectId :: Types.ProjectId
    -- ^ The ID of the project for which you want to list team members.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of team members you want returned in a response.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The continuation token for the next set of results, if the results cannot be returned in one response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTeamMembers' value with any optional fields omitted.
mkListTeamMembers
    :: Types.ProjectId -- ^ 'projectId'
    -> ListTeamMembers
mkListTeamMembers projectId
  = ListTeamMembers'{projectId, maxResults = Core.Nothing,
                     nextToken = Core.Nothing}

-- | The ID of the project for which you want to list team members.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmProjectId :: Lens.Lens' ListTeamMembers Types.ProjectId
ltmProjectId = Lens.field @"projectId"
{-# INLINEABLE ltmProjectId #-}
{-# DEPRECATED projectId "Use generic-lens or generic-optics with 'projectId' instead"  #-}

-- | The maximum number of team members you want returned in a response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmMaxResults :: Lens.Lens' ListTeamMembers (Core.Maybe Core.Natural)
ltmMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltmMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The continuation token for the next set of results, if the results cannot be returned in one response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmNextToken :: Lens.Lens' ListTeamMembers (Core.Maybe Types.PaginationToken)
ltmNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltmNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListTeamMembers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTeamMembers where
        toHeaders ListTeamMembers{..}
          = Core.pure ("X-Amz-Target", "CodeStar_20170419.ListTeamMembers")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTeamMembers where
        toJSON ListTeamMembers{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("projectId" Core..= projectId),
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListTeamMembers where
        type Rs ListTeamMembers = ListTeamMembersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTeamMembersResponse' Core.<$>
                   (x Core..:? "teamMembers" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTeamMembers where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"teamMembers") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListTeamMembersResponse' smart constructor.
data ListTeamMembersResponse = ListTeamMembersResponse'
  { teamMembers :: [Types.TeamMember]
    -- ^ A list of team member objects for the project.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The continuation token to use when requesting the next set of results, if there are more results to be returned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTeamMembersResponse' value with any optional fields omitted.
mkListTeamMembersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTeamMembersResponse
mkListTeamMembersResponse responseStatus
  = ListTeamMembersResponse'{teamMembers = Core.mempty,
                             nextToken = Core.Nothing, responseStatus}

-- | A list of team member objects for the project.
--
-- /Note:/ Consider using 'teamMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmrrsTeamMembers :: Lens.Lens' ListTeamMembersResponse [Types.TeamMember]
ltmrrsTeamMembers = Lens.field @"teamMembers"
{-# INLINEABLE ltmrrsTeamMembers #-}
{-# DEPRECATED teamMembers "Use generic-lens or generic-optics with 'teamMembers' instead"  #-}

-- | The continuation token to use when requesting the next set of results, if there are more results to be returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmrrsNextToken :: Lens.Lens' ListTeamMembersResponse (Core.Maybe Types.PaginationToken)
ltmrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltmrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmrrsResponseStatus :: Lens.Lens' ListTeamMembersResponse Core.Int
ltmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
