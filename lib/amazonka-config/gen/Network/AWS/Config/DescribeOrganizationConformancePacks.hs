{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeOrganizationConformancePacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of organization conformance packs.
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
module Network.AWS.Config.DescribeOrganizationConformancePacks
  ( -- * Creating a request
    DescribeOrganizationConformancePacks (..),
    mkDescribeOrganizationConformancePacks,

    -- ** Request lenses
    docpLimit,
    docpNextToken,
    docpOrganizationConformancePackNames,

    -- * Destructuring the response
    DescribeOrganizationConformancePacksResponse (..),
    mkDescribeOrganizationConformancePacksResponse,

    -- ** Response lenses
    docprrsNextToken,
    docprrsOrganizationConformancePacks,
    docprrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeOrganizationConformancePacks' smart constructor.
data DescribeOrganizationConformancePacks = DescribeOrganizationConformancePacks'
  { -- | The maximum number of organization config packs returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
    limit :: Core.Maybe Core.Natural,
    -- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.String,
    -- | The name that you assign to an organization conformance pack.
    organizationConformancePackNames :: Core.Maybe [Types.OrganizationConformancePackName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrganizationConformancePacks' value with any optional fields omitted.
mkDescribeOrganizationConformancePacks ::
  DescribeOrganizationConformancePacks
mkDescribeOrganizationConformancePacks =
  DescribeOrganizationConformancePacks'
    { limit = Core.Nothing,
      nextToken = Core.Nothing,
      organizationConformancePackNames = Core.Nothing
    }

-- | The maximum number of organization config packs returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpLimit :: Lens.Lens' DescribeOrganizationConformancePacks (Core.Maybe Core.Natural)
docpLimit = Lens.field @"limit"
{-# DEPRECATED docpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpNextToken :: Lens.Lens' DescribeOrganizationConformancePacks (Core.Maybe Types.String)
docpNextToken = Lens.field @"nextToken"
{-# DEPRECATED docpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name that you assign to an organization conformance pack.
--
-- /Note:/ Consider using 'organizationConformancePackNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpOrganizationConformancePackNames :: Lens.Lens' DescribeOrganizationConformancePacks (Core.Maybe [Types.OrganizationConformancePackName])
docpOrganizationConformancePackNames = Lens.field @"organizationConformancePackNames"
{-# DEPRECATED docpOrganizationConformancePackNames "Use generic-lens or generic-optics with 'organizationConformancePackNames' instead." #-}

instance Core.FromJSON DescribeOrganizationConformancePacks where
  toJSON DescribeOrganizationConformancePacks {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("OrganizationConformancePackNames" Core..=)
              Core.<$> organizationConformancePackNames
          ]
      )

instance Core.AWSRequest DescribeOrganizationConformancePacks where
  type
    Rs DescribeOrganizationConformancePacks =
      DescribeOrganizationConformancePacksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DescribeOrganizationConformancePacks"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationConformancePacksResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "OrganizationConformancePacks")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeOrganizationConformancePacksResponse' smart constructor.
data DescribeOrganizationConformancePacksResponse = DescribeOrganizationConformancePacksResponse'
  { -- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Returns a list of OrganizationConformancePacks objects.
    organizationConformancePacks :: Core.Maybe [Types.OrganizationConformancePack],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeOrganizationConformancePacksResponse' value with any optional fields omitted.
mkDescribeOrganizationConformancePacksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeOrganizationConformancePacksResponse
mkDescribeOrganizationConformancePacksResponse responseStatus =
  DescribeOrganizationConformancePacksResponse'
    { nextToken =
        Core.Nothing,
      organizationConformancePacks = Core.Nothing,
      responseStatus
    }

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docprrsNextToken :: Lens.Lens' DescribeOrganizationConformancePacksResponse (Core.Maybe Types.NextToken)
docprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED docprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns a list of OrganizationConformancePacks objects.
--
-- /Note:/ Consider using 'organizationConformancePacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docprrsOrganizationConformancePacks :: Lens.Lens' DescribeOrganizationConformancePacksResponse (Core.Maybe [Types.OrganizationConformancePack])
docprrsOrganizationConformancePacks = Lens.field @"organizationConformancePacks"
{-# DEPRECATED docprrsOrganizationConformancePacks "Use generic-lens or generic-optics with 'organizationConformancePacks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docprrsResponseStatus :: Lens.Lens' DescribeOrganizationConformancePacksResponse Core.Int
docprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED docprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
