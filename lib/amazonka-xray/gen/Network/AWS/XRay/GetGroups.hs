{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all active group details.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetGroups
  ( -- * Creating a request
    GetGroups (..),
    mkGetGroups,

    -- ** Request lenses
    ggNextToken,

    -- * Destructuring the response
    GetGroupsResponse (..),
    mkGetGroupsResponse,

    -- ** Response lenses
    ggrrsGroups,
    ggrrsNextToken,
    ggrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkGetGroups' smart constructor.
newtype GetGroups = GetGroups'
  { -- | Pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroups' value with any optional fields omitted.
mkGetGroups ::
  GetGroups
mkGetGroups = GetGroups' {nextToken = Core.Nothing}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggNextToken :: Lens.Lens' GetGroups (Core.Maybe Types.NextToken)
ggNextToken = Lens.field @"nextToken"
{-# DEPRECATED ggNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetGroups where
  toJSON GetGroups {..} =
    Core.object
      (Core.catMaybes [("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetGroups where
  type Rs GetGroups = GetGroupsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/Groups",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupsResponse'
            Core.<$> (x Core..:? "Groups")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"groups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetGroupsResponse' smart constructor.
data GetGroupsResponse = GetGroupsResponse'
  { -- | The collection of all active groups.
    groups :: Core.Maybe [Types.GroupSummary],
    -- | Pagination token.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroupsResponse' value with any optional fields omitted.
mkGetGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetGroupsResponse
mkGetGroupsResponse responseStatus =
  GetGroupsResponse'
    { groups = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The collection of all active groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsGroups :: Lens.Lens' GetGroupsResponse (Core.Maybe [Types.GroupSummary])
ggrrsGroups = Lens.field @"groups"
{-# DEPRECATED ggrrsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsNextToken :: Lens.Lens' GetGroupsResponse (Core.Maybe Types.String)
ggrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ggrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsResponseStatus :: Lens.Lens' GetGroupsResponse Core.Int
ggrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ggrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
