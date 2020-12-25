{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetOrganizationConfigRuleDetailedStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed status for each member account within an organization for a given organization config rule.
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
module Network.AWS.Config.GetOrganizationConfigRuleDetailedStatus
  ( -- * Creating a request
    GetOrganizationConfigRuleDetailedStatus (..),
    mkGetOrganizationConfigRuleDetailedStatus,

    -- ** Request lenses
    gocrdsOrganizationConfigRuleName,
    gocrdsFilters,
    gocrdsLimit,
    gocrdsNextToken,

    -- * Destructuring the response
    GetOrganizationConfigRuleDetailedStatusResponse (..),
    mkGetOrganizationConfigRuleDetailedStatusResponse,

    -- ** Response lenses
    gocrdsrrsNextToken,
    gocrdsrrsOrganizationConfigRuleDetailedStatus,
    gocrdsrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetOrganizationConfigRuleDetailedStatus' smart constructor.
data GetOrganizationConfigRuleDetailedStatus = GetOrganizationConfigRuleDetailedStatus'
  { -- | The name of organization config rule for which you want status details for member accounts.
    organizationConfigRuleName :: Types.OrganizationConfigRuleName,
    -- | A @StatusDetailFilters@ object.
    filters :: Core.Maybe Types.StatusDetailFilters,
    -- | The maximum number of @OrganizationConfigRuleDetailedStatus@ returned on each page. If you do not specify a number, AWS Config uses the default. The default is 100.
    limit :: Core.Maybe Core.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetOrganizationConfigRuleDetailedStatus' value with any optional fields omitted.
mkGetOrganizationConfigRuleDetailedStatus ::
  -- | 'organizationConfigRuleName'
  Types.OrganizationConfigRuleName ->
  GetOrganizationConfigRuleDetailedStatus
mkGetOrganizationConfigRuleDetailedStatus
  organizationConfigRuleName =
    GetOrganizationConfigRuleDetailedStatus'
      { organizationConfigRuleName,
        filters = Core.Nothing,
        limit = Core.Nothing,
        nextToken = Core.Nothing
      }

-- | The name of organization config rule for which you want status details for member accounts.
--
-- /Note:/ Consider using 'organizationConfigRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocrdsOrganizationConfigRuleName :: Lens.Lens' GetOrganizationConfigRuleDetailedStatus Types.OrganizationConfigRuleName
gocrdsOrganizationConfigRuleName = Lens.field @"organizationConfigRuleName"
{-# DEPRECATED gocrdsOrganizationConfigRuleName "Use generic-lens or generic-optics with 'organizationConfigRuleName' instead." #-}

-- | A @StatusDetailFilters@ object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocrdsFilters :: Lens.Lens' GetOrganizationConfigRuleDetailedStatus (Core.Maybe Types.StatusDetailFilters)
gocrdsFilters = Lens.field @"filters"
{-# DEPRECATED gocrdsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of @OrganizationConfigRuleDetailedStatus@ returned on each page. If you do not specify a number, AWS Config uses the default. The default is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocrdsLimit :: Lens.Lens' GetOrganizationConfigRuleDetailedStatus (Core.Maybe Core.Natural)
gocrdsLimit = Lens.field @"limit"
{-# DEPRECATED gocrdsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocrdsNextToken :: Lens.Lens' GetOrganizationConfigRuleDetailedStatus (Core.Maybe Types.String)
gocrdsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gocrdsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetOrganizationConfigRuleDetailedStatus where
  toJSON GetOrganizationConfigRuleDetailedStatus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("OrganizationConfigRuleName" Core..= organizationConfigRuleName),
            ("Filters" Core..=) Core.<$> filters,
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetOrganizationConfigRuleDetailedStatus where
  type
    Rs GetOrganizationConfigRuleDetailedStatus =
      GetOrganizationConfigRuleDetailedStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.GetOrganizationConfigRuleDetailedStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOrganizationConfigRuleDetailedStatusResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "OrganizationConfigRuleDetailedStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetOrganizationConfigRuleDetailedStatusResponse' smart constructor.
data GetOrganizationConfigRuleDetailedStatusResponse = GetOrganizationConfigRuleDetailedStatusResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.String,
    -- | A list of @MemberAccountStatus@ objects.
    organizationConfigRuleDetailedStatus :: Core.Maybe [Types.MemberAccountStatus],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetOrganizationConfigRuleDetailedStatusResponse' value with any optional fields omitted.
mkGetOrganizationConfigRuleDetailedStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetOrganizationConfigRuleDetailedStatusResponse
mkGetOrganizationConfigRuleDetailedStatusResponse responseStatus =
  GetOrganizationConfigRuleDetailedStatusResponse'
    { nextToken =
        Core.Nothing,
      organizationConfigRuleDetailedStatus =
        Core.Nothing,
      responseStatus
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocrdsrrsNextToken :: Lens.Lens' GetOrganizationConfigRuleDetailedStatusResponse (Core.Maybe Types.String)
gocrdsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gocrdsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @MemberAccountStatus@ objects.
--
-- /Note:/ Consider using 'organizationConfigRuleDetailedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocrdsrrsOrganizationConfigRuleDetailedStatus :: Lens.Lens' GetOrganizationConfigRuleDetailedStatusResponse (Core.Maybe [Types.MemberAccountStatus])
gocrdsrrsOrganizationConfigRuleDetailedStatus = Lens.field @"organizationConfigRuleDetailedStatus"
{-# DEPRECATED gocrdsrrsOrganizationConfigRuleDetailedStatus "Use generic-lens or generic-optics with 'organizationConfigRuleDetailedStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocrdsrrsResponseStatus :: Lens.Lens' GetOrganizationConfigRuleDetailedStatusResponse Core.Int
gocrdsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gocrdsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
