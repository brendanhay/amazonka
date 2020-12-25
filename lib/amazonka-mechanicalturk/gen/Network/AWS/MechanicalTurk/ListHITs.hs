{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListHITs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListHITs@ operation returns all of a Requester's HITs. The operation returns HITs of any status, except for HITs that have been deleted of with the DeleteHIT operation or that have been auto-deleted.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListHITs
  ( -- * Creating a request
    ListHITs (..),
    mkListHITs,

    -- ** Request lenses
    lhitMaxResults,
    lhitNextToken,

    -- * Destructuring the response
    ListHITsResponse (..),
    mkListHITsResponse,

    -- ** Response lenses
    lhitrrsHITs,
    lhitrrsNextToken,
    lhitrrsNumResults,
    lhitrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListHITs' smart constructor.
data ListHITs = ListHITs'
  { maxResults :: Core.Maybe Core.Natural,
    -- | Pagination token
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListHITs' value with any optional fields omitted.
mkListHITs ::
  ListHITs
mkListHITs =
  ListHITs' {maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitMaxResults :: Lens.Lens' ListHITs (Core.Maybe Core.Natural)
lhitMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lhitMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Pagination token
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitNextToken :: Lens.Lens' ListHITs (Core.Maybe Types.PaginationToken)
lhitNextToken = Lens.field @"nextToken"
{-# DEPRECATED lhitNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListHITs where
  toJSON ListHITs {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListHITs where
  type Rs ListHITs = ListHITsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "MTurkRequesterServiceV20170117.ListHITs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHITsResponse'
            Core.<$> (x Core..:? "HITs")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "NumResults")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListHITs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"hITs" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListHITsResponse' smart constructor.
data ListHITsResponse = ListHITsResponse'
  { -- | The list of HIT elements returned by the query.
    hITs :: Core.Maybe [Types.HIT],
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The number of HITs on this page in the filtered results list, equivalent to the number of HITs being returned by this call.
    numResults :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListHITsResponse' value with any optional fields omitted.
mkListHITsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListHITsResponse
mkListHITsResponse responseStatus =
  ListHITsResponse'
    { hITs = Core.Nothing,
      nextToken = Core.Nothing,
      numResults = Core.Nothing,
      responseStatus
    }

-- | The list of HIT elements returned by the query.
--
-- /Note:/ Consider using 'hITs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitrrsHITs :: Lens.Lens' ListHITsResponse (Core.Maybe [Types.HIT])
lhitrrsHITs = Lens.field @"hITs"
{-# DEPRECATED lhitrrsHITs "Use generic-lens or generic-optics with 'hITs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitrrsNextToken :: Lens.Lens' ListHITsResponse (Core.Maybe Types.PaginationToken)
lhitrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lhitrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of HITs on this page in the filtered results list, equivalent to the number of HITs being returned by this call.
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitrrsNumResults :: Lens.Lens' ListHITsResponse (Core.Maybe Core.Int)
lhitrrsNumResults = Lens.field @"numResults"
{-# DEPRECATED lhitrrsNumResults "Use generic-lens or generic-optics with 'numResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitrrsResponseStatus :: Lens.Lens' ListHITsResponse Core.Int
lhitrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lhitrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
