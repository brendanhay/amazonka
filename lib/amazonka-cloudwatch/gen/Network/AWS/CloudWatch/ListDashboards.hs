{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.ListDashboards
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the dashboards for your account. If you include @DashboardNamePrefix@ , only those dashboards with names starting with the prefix are listed. Otherwise, all dashboards in your account are listed.
--
-- @ListDashboards@ returns up to 1000 results on one page. If there are more than 1000 dashboards, you can call @ListDashboards@ again and include the value you received for @NextToken@ in the first call, to receive the next 1000 results.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatch.ListDashboards
  ( -- * Creating a request
    ListDashboards (..),
    mkListDashboards,

    -- ** Request lenses
    ldDashboardNamePrefix,
    ldNextToken,

    -- * Destructuring the response
    ListDashboardsResponse (..),
    mkListDashboardsResponse,

    -- ** Response lenses
    ldrrsDashboardEntries,
    ldrrsNextToken,
    ldrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDashboards' smart constructor.
data ListDashboards = ListDashboards'
  { -- | If you specify this parameter, only the dashboards with names starting with the specified string are listed. The maximum length is 255, and valid characters are A-Z, a-z, 0-9, ".", "-", and "_".
    dashboardNamePrefix :: Core.Maybe Types.DashboardNamePrefix,
    -- | The token returned by a previous call to indicate that there is more data available.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDashboards' value with any optional fields omitted.
mkListDashboards ::
  ListDashboards
mkListDashboards =
  ListDashboards'
    { dashboardNamePrefix = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | If you specify this parameter, only the dashboards with names starting with the specified string are listed. The maximum length is 255, and valid characters are A-Z, a-z, 0-9, ".", "-", and "_".
--
-- /Note:/ Consider using 'dashboardNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldDashboardNamePrefix :: Lens.Lens' ListDashboards (Core.Maybe Types.DashboardNamePrefix)
ldDashboardNamePrefix = Lens.field @"dashboardNamePrefix"
{-# DEPRECATED ldDashboardNamePrefix "Use generic-lens or generic-optics with 'dashboardNamePrefix' instead." #-}

-- | The token returned by a previous call to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDashboards (Core.Maybe Types.NextToken)
ldNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListDashboards where
  type Rs ListDashboards = ListDashboardsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ListDashboards")
                Core.<> (Core.pure ("Version", "2010-08-01"))
                Core.<> ( Core.toQueryValue "DashboardNamePrefix"
                            Core.<$> dashboardNamePrefix
                        )
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListDashboardsResult"
      ( \s h x ->
          ListDashboardsResponse'
            Core.<$> ( x Core..@? "DashboardEntries"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDashboards where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"dashboardEntries" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListDashboardsResponse' smart constructor.
data ListDashboardsResponse = ListDashboardsResponse'
  { -- | The list of matching dashboards.
    dashboardEntries :: Core.Maybe [Types.DashboardEntry],
    -- | The token that marks the start of the next batch of returned results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDashboardsResponse' value with any optional fields omitted.
mkListDashboardsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDashboardsResponse
mkListDashboardsResponse responseStatus =
  ListDashboardsResponse'
    { dashboardEntries = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of matching dashboards.
--
-- /Note:/ Consider using 'dashboardEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsDashboardEntries :: Lens.Lens' ListDashboardsResponse (Core.Maybe [Types.DashboardEntry])
ldrrsDashboardEntries = Lens.field @"dashboardEntries"
{-# DEPRECATED ldrrsDashboardEntries "Use generic-lens or generic-optics with 'dashboardEntries' instead." #-}

-- | The token that marks the start of the next batch of returned results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsNextToken :: Lens.Lens' ListDashboardsResponse (Core.Maybe Types.NextToken)
ldrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResponseStatus :: Lens.Lens' ListDashboardsResponse Core.Int
ldrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
