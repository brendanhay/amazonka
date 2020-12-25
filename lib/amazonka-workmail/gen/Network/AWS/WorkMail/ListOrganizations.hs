{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListOrganizations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the customer's organizations.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListOrganizations
  ( -- * Creating a request
    ListOrganizations (..),
    mkListOrganizations,

    -- ** Request lenses
    loMaxResults,
    loNextToken,

    -- * Destructuring the response
    ListOrganizationsResponse (..),
    mkListOrganizationsResponse,

    -- ** Response lenses
    lorrsNextToken,
    lorrsOrganizationSummaries,
    lorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkListOrganizations' smart constructor.
data ListOrganizations = ListOrganizations'
  { -- | The maximum number of results to return in a single call.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOrganizations' value with any optional fields omitted.
mkListOrganizations ::
  ListOrganizations
mkListOrganizations =
  ListOrganizations'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMaxResults :: Lens.Lens' ListOrganizations (Core.Maybe Core.Natural)
loMaxResults = Lens.field @"maxResults"
{-# DEPRECATED loMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loNextToken :: Lens.Lens' ListOrganizations (Core.Maybe Types.NextToken)
loNextToken = Lens.field @"nextToken"
{-# DEPRECATED loNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListOrganizations where
  toJSON ListOrganizations {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListOrganizations where
  type Rs ListOrganizations = ListOrganizationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.ListOrganizations")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOrganizationsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "OrganizationSummaries")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListOrganizations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"organizationSummaries" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListOrganizationsResponse' smart constructor.
data ListOrganizationsResponse = ListOrganizationsResponse'
  { -- | The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The overview of owned organizations presented as a list of organization summaries.
    organizationSummaries :: Core.Maybe [Types.OrganizationSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOrganizationsResponse' value with any optional fields omitted.
mkListOrganizationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListOrganizationsResponse
mkListOrganizationsResponse responseStatus =
  ListOrganizationsResponse'
    { nextToken = Core.Nothing,
      organizationSummaries = Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsNextToken :: Lens.Lens' ListOrganizationsResponse (Core.Maybe Types.NextToken)
lorrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lorrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The overview of owned organizations presented as a list of organization summaries.
--
-- /Note:/ Consider using 'organizationSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsOrganizationSummaries :: Lens.Lens' ListOrganizationsResponse (Core.Maybe [Types.OrganizationSummary])
lorrsOrganizationSummaries = Lens.field @"organizationSummaries"
{-# DEPRECATED lorrsOrganizationSummaries "Use generic-lens or generic-optics with 'organizationSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsResponseStatus :: Lens.Lens' ListOrganizationsResponse Core.Int
lorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
