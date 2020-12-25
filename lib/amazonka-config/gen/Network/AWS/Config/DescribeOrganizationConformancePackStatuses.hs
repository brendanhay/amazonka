{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeOrganizationConformancePackStatuses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides organization conformance pack deployment status for an organization.
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
module Network.AWS.Config.DescribeOrganizationConformancePackStatuses
  ( -- * Creating a request
    DescribeOrganizationConformancePackStatuses (..),
    mkDescribeOrganizationConformancePackStatuses,

    -- ** Request lenses
    docpsLimit,
    docpsNextToken,
    docpsOrganizationConformancePackNames,

    -- * Destructuring the response
    DescribeOrganizationConformancePackStatusesResponse (..),
    mkDescribeOrganizationConformancePackStatusesResponse,

    -- ** Response lenses
    docpsrrsNextToken,
    docpsrrsOrganizationConformancePackStatuses,
    docpsrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeOrganizationConformancePackStatuses' smart constructor.
data DescribeOrganizationConformancePackStatuses = DescribeOrganizationConformancePackStatuses'
  { -- | The maximum number of OrganizationConformancePackStatuses returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
    limit :: Core.Maybe Core.Natural,
    -- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.String,
    -- | The names of organization conformance packs for which you want status details. If you do not specify any names, AWS Config returns details for all your organization conformance packs.
    organizationConformancePackNames :: Core.Maybe [Types.OrganizationConformancePackName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrganizationConformancePackStatuses' value with any optional fields omitted.
mkDescribeOrganizationConformancePackStatuses ::
  DescribeOrganizationConformancePackStatuses
mkDescribeOrganizationConformancePackStatuses =
  DescribeOrganizationConformancePackStatuses'
    { limit =
        Core.Nothing,
      nextToken = Core.Nothing,
      organizationConformancePackNames = Core.Nothing
    }

-- | The maximum number of OrganizationConformancePackStatuses returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpsLimit :: Lens.Lens' DescribeOrganizationConformancePackStatuses (Core.Maybe Core.Natural)
docpsLimit = Lens.field @"limit"
{-# DEPRECATED docpsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpsNextToken :: Lens.Lens' DescribeOrganizationConformancePackStatuses (Core.Maybe Types.String)
docpsNextToken = Lens.field @"nextToken"
{-# DEPRECATED docpsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of organization conformance packs for which you want status details. If you do not specify any names, AWS Config returns details for all your organization conformance packs.
--
-- /Note:/ Consider using 'organizationConformancePackNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpsOrganizationConformancePackNames :: Lens.Lens' DescribeOrganizationConformancePackStatuses (Core.Maybe [Types.OrganizationConformancePackName])
docpsOrganizationConformancePackNames = Lens.field @"organizationConformancePackNames"
{-# DEPRECATED docpsOrganizationConformancePackNames "Use generic-lens or generic-optics with 'organizationConformancePackNames' instead." #-}

instance Core.FromJSON DescribeOrganizationConformancePackStatuses where
  toJSON DescribeOrganizationConformancePackStatuses {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("OrganizationConformancePackNames" Core..=)
              Core.<$> organizationConformancePackNames
          ]
      )

instance
  Core.AWSRequest
    DescribeOrganizationConformancePackStatuses
  where
  type
    Rs DescribeOrganizationConformancePackStatuses =
      DescribeOrganizationConformancePackStatusesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DescribeOrganizationConformancePackStatuses"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationConformancePackStatusesResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "OrganizationConformancePackStatuses")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeOrganizationConformancePackStatusesResponse' smart constructor.
data DescribeOrganizationConformancePackStatusesResponse = DescribeOrganizationConformancePackStatusesResponse'
  { -- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.String,
    -- | A list of @OrganizationConformancePackStatus@ objects.
    organizationConformancePackStatuses :: Core.Maybe [Types.OrganizationConformancePackStatus],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeOrganizationConformancePackStatusesResponse' value with any optional fields omitted.
mkDescribeOrganizationConformancePackStatusesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeOrganizationConformancePackStatusesResponse
mkDescribeOrganizationConformancePackStatusesResponse
  responseStatus =
    DescribeOrganizationConformancePackStatusesResponse'
      { nextToken =
          Core.Nothing,
        organizationConformancePackStatuses =
          Core.Nothing,
        responseStatus
      }

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpsrrsNextToken :: Lens.Lens' DescribeOrganizationConformancePackStatusesResponse (Core.Maybe Types.String)
docpsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED docpsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @OrganizationConformancePackStatus@ objects.
--
-- /Note:/ Consider using 'organizationConformancePackStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpsrrsOrganizationConformancePackStatuses :: Lens.Lens' DescribeOrganizationConformancePackStatusesResponse (Core.Maybe [Types.OrganizationConformancePackStatus])
docpsrrsOrganizationConformancePackStatuses = Lens.field @"organizationConformancePackStatuses"
{-# DEPRECATED docpsrrsOrganizationConformancePackStatuses "Use generic-lens or generic-optics with 'organizationConformancePackStatuses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpsrrsResponseStatus :: Lens.Lens' DescribeOrganizationConformancePackStatusesResponse Core.Int
docpsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED docpsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
