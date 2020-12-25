{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetOrganizationConformancePackDetailedStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed status for each member account within an organization for a given organization conformance pack.
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
module Network.AWS.Config.GetOrganizationConformancePackDetailedStatus
  ( -- * Creating a request
    GetOrganizationConformancePackDetailedStatus (..),
    mkGetOrganizationConformancePackDetailedStatus,

    -- ** Request lenses
    gocpdsOrganizationConformancePackName,
    gocpdsFilters,
    gocpdsLimit,
    gocpdsNextToken,

    -- * Destructuring the response
    GetOrganizationConformancePackDetailedStatusResponse (..),
    mkGetOrganizationConformancePackDetailedStatusResponse,

    -- ** Response lenses
    gocpdsrrsNextToken,
    gocpdsrrsOrganizationConformancePackDetailedStatuses,
    gocpdsrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetOrganizationConformancePackDetailedStatus' smart constructor.
data GetOrganizationConformancePackDetailedStatus = GetOrganizationConformancePackDetailedStatus'
  { -- | The name of organization conformance pack for which you want status details for member accounts.
    organizationConformancePackName :: Types.OrganizationConformancePackName,
    -- | An @OrganizationResourceDetailedStatusFilters@ object.
    filters :: Core.Maybe Types.OrganizationResourceDetailedStatusFilters,
    -- | The maximum number of @OrganizationConformancePackDetailedStatuses@ returned on each page. If you do not specify a number, AWS Config uses the default. The default is 100.
    limit :: Core.Maybe Core.Natural,
    -- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetOrganizationConformancePackDetailedStatus' value with any optional fields omitted.
mkGetOrganizationConformancePackDetailedStatus ::
  -- | 'organizationConformancePackName'
  Types.OrganizationConformancePackName ->
  GetOrganizationConformancePackDetailedStatus
mkGetOrganizationConformancePackDetailedStatus
  organizationConformancePackName =
    GetOrganizationConformancePackDetailedStatus'
      { organizationConformancePackName,
        filters = Core.Nothing,
        limit = Core.Nothing,
        nextToken = Core.Nothing
      }

-- | The name of organization conformance pack for which you want status details for member accounts.
--
-- /Note:/ Consider using 'organizationConformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocpdsOrganizationConformancePackName :: Lens.Lens' GetOrganizationConformancePackDetailedStatus Types.OrganizationConformancePackName
gocpdsOrganizationConformancePackName = Lens.field @"organizationConformancePackName"
{-# DEPRECATED gocpdsOrganizationConformancePackName "Use generic-lens or generic-optics with 'organizationConformancePackName' instead." #-}

-- | An @OrganizationResourceDetailedStatusFilters@ object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocpdsFilters :: Lens.Lens' GetOrganizationConformancePackDetailedStatus (Core.Maybe Types.OrganizationResourceDetailedStatusFilters)
gocpdsFilters = Lens.field @"filters"
{-# DEPRECATED gocpdsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of @OrganizationConformancePackDetailedStatuses@ returned on each page. If you do not specify a number, AWS Config uses the default. The default is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocpdsLimit :: Lens.Lens' GetOrganizationConformancePackDetailedStatus (Core.Maybe Core.Natural)
gocpdsLimit = Lens.field @"limit"
{-# DEPRECATED gocpdsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocpdsNextToken :: Lens.Lens' GetOrganizationConformancePackDetailedStatus (Core.Maybe Types.NextToken)
gocpdsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gocpdsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetOrganizationConformancePackDetailedStatus where
  toJSON GetOrganizationConformancePackDetailedStatus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "OrganizationConformancePackName"
                  Core..= organizationConformancePackName
              ),
            ("Filters" Core..=) Core.<$> filters,
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance
  Core.AWSRequest
    GetOrganizationConformancePackDetailedStatus
  where
  type
    Rs GetOrganizationConformancePackDetailedStatus =
      GetOrganizationConformancePackDetailedStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.GetOrganizationConformancePackDetailedStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOrganizationConformancePackDetailedStatusResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "OrganizationConformancePackDetailedStatuses")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetOrganizationConformancePackDetailedStatusResponse' smart constructor.
data GetOrganizationConformancePackDetailedStatusResponse = GetOrganizationConformancePackDetailedStatusResponse'
  { -- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.String,
    -- | A list of @OrganizationConformancePackDetailedStatus@ objects.
    organizationConformancePackDetailedStatuses :: Core.Maybe [Types.OrganizationConformancePackDetailedStatus],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetOrganizationConformancePackDetailedStatusResponse' value with any optional fields omitted.
mkGetOrganizationConformancePackDetailedStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetOrganizationConformancePackDetailedStatusResponse
mkGetOrganizationConformancePackDetailedStatusResponse
  responseStatus =
    GetOrganizationConformancePackDetailedStatusResponse'
      { nextToken =
          Core.Nothing,
        organizationConformancePackDetailedStatuses =
          Core.Nothing,
        responseStatus
      }

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocpdsrrsNextToken :: Lens.Lens' GetOrganizationConformancePackDetailedStatusResponse (Core.Maybe Types.String)
gocpdsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gocpdsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @OrganizationConformancePackDetailedStatus@ objects.
--
-- /Note:/ Consider using 'organizationConformancePackDetailedStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocpdsrrsOrganizationConformancePackDetailedStatuses :: Lens.Lens' GetOrganizationConformancePackDetailedStatusResponse (Core.Maybe [Types.OrganizationConformancePackDetailedStatus])
gocpdsrrsOrganizationConformancePackDetailedStatuses = Lens.field @"organizationConformancePackDetailedStatuses"
{-# DEPRECATED gocpdsrrsOrganizationConformancePackDetailedStatuses "Use generic-lens or generic-optics with 'organizationConformancePackDetailedStatuses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocpdsrrsResponseStatus :: Lens.Lens' GetOrganizationConformancePackDetailedStatusResponse Core.Int
gocpdsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gocpdsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
