{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListReviewableHITs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListReviewableHITs@ operation retrieves the HITs with Status equal to Reviewable or Status equal to Reviewing that belong to the Requester calling the operation.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListReviewableHITs
  ( -- * Creating a request
    ListReviewableHITs (..),
    mkListReviewableHITs,

    -- ** Request lenses
    lrhitHITTypeId,
    lrhitMaxResults,
    lrhitNextToken,
    lrhitStatus,

    -- * Destructuring the response
    ListReviewableHITsResponse (..),
    mkListReviewableHITsResponse,

    -- ** Response lenses
    lrhitrrsHITs,
    lrhitrrsNextToken,
    lrhitrrsNumResults,
    lrhitrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListReviewableHITs' smart constructor.
data ListReviewableHITs = ListReviewableHITs'
  { -- | The ID of the HIT type of the HITs to consider for the query. If not specified, all HITs for the Reviewer are considered
    hITTypeId :: Core.Maybe Types.HITTypeId,
    -- | Limit the number of results returned.
    maxResults :: Core.Maybe Core.Natural,
    -- | Pagination Token
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | Can be either @Reviewable@ or @Reviewing@ . Reviewable is the default value.
    status :: Core.Maybe Types.ReviewableHITStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListReviewableHITs' value with any optional fields omitted.
mkListReviewableHITs ::
  ListReviewableHITs
mkListReviewableHITs =
  ListReviewableHITs'
    { hITTypeId = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      status = Core.Nothing
    }

-- | The ID of the HIT type of the HITs to consider for the query. If not specified, all HITs for the Reviewer are considered
--
-- /Note:/ Consider using 'hITTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhitHITTypeId :: Lens.Lens' ListReviewableHITs (Core.Maybe Types.HITTypeId)
lrhitHITTypeId = Lens.field @"hITTypeId"
{-# DEPRECATED lrhitHITTypeId "Use generic-lens or generic-optics with 'hITTypeId' instead." #-}

-- | Limit the number of results returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhitMaxResults :: Lens.Lens' ListReviewableHITs (Core.Maybe Core.Natural)
lrhitMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lrhitMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Pagination Token
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhitNextToken :: Lens.Lens' ListReviewableHITs (Core.Maybe Types.PaginationToken)
lrhitNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrhitNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Can be either @Reviewable@ or @Reviewing@ . Reviewable is the default value.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhitStatus :: Lens.Lens' ListReviewableHITs (Core.Maybe Types.ReviewableHITStatus)
lrhitStatus = Lens.field @"status"
{-# DEPRECATED lrhitStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON ListReviewableHITs where
  toJSON ListReviewableHITs {..} =
    Core.object
      ( Core.catMaybes
          [ ("HITTypeId" Core..=) Core.<$> hITTypeId,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Status" Core..=) Core.<$> status
          ]
      )

instance Core.AWSRequest ListReviewableHITs where
  type Rs ListReviewableHITs = ListReviewableHITsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "MTurkRequesterServiceV20170117.ListReviewableHITs"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReviewableHITsResponse'
            Core.<$> (x Core..:? "HITs")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "NumResults")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListReviewableHITs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"hITs" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListReviewableHITsResponse' smart constructor.
data ListReviewableHITsResponse = ListReviewableHITsResponse'
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

-- | Creates a 'ListReviewableHITsResponse' value with any optional fields omitted.
mkListReviewableHITsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListReviewableHITsResponse
mkListReviewableHITsResponse responseStatus =
  ListReviewableHITsResponse'
    { hITs = Core.Nothing,
      nextToken = Core.Nothing,
      numResults = Core.Nothing,
      responseStatus
    }

-- | The list of HIT elements returned by the query.
--
-- /Note:/ Consider using 'hITs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhitrrsHITs :: Lens.Lens' ListReviewableHITsResponse (Core.Maybe [Types.HIT])
lrhitrrsHITs = Lens.field @"hITs"
{-# DEPRECATED lrhitrrsHITs "Use generic-lens or generic-optics with 'hITs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhitrrsNextToken :: Lens.Lens' ListReviewableHITsResponse (Core.Maybe Types.PaginationToken)
lrhitrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrhitrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of HITs on this page in the filtered results list, equivalent to the number of HITs being returned by this call.
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhitrrsNumResults :: Lens.Lens' ListReviewableHITsResponse (Core.Maybe Core.Int)
lrhitrrsNumResults = Lens.field @"numResults"
{-# DEPRECATED lrhitrrsNumResults "Use generic-lens or generic-optics with 'numResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhitrrsResponseStatus :: Lens.Lens' ListReviewableHITsResponse Core.Int
lrhitrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrhitrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
