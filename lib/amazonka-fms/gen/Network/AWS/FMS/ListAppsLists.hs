{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.ListAppsLists
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @AppsListDataSummary@ objects.
module Network.AWS.FMS.ListAppsLists
  ( -- * Creating a request
    ListAppsLists (..),
    mkListAppsLists,

    -- ** Request lenses
    lalMaxResults,
    lalDefaultLists,
    lalNextToken,

    -- * Destructuring the response
    ListAppsListsResponse (..),
    mkListAppsListsResponse,

    -- ** Response lenses
    lalrrsAppsLists,
    lalrrsNextToken,
    lalrrsResponseStatus,
  )
where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAppsLists' smart constructor.
data ListAppsLists = ListAppsLists'
  { -- | The maximum number of objects that you want AWS Firewall Manager to return for this request. If more objects are available, in the response, AWS Firewall Manager provides a @NextToken@ value that you can use in a subsequent call to get the next batch of objects.
    --
    -- If you don't specify this, AWS Firewall Manager returns all available objects.
    maxResults :: Core.Natural,
    -- | Specifies whether the lists to retrieve are default lists owned by AWS Firewall Manager.
    defaultLists :: Core.Maybe Core.Bool,
    -- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. For all but the first request, you provide the token returned by the prior request in the request parameters, to retrieve the next batch of objects.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAppsLists' value with any optional fields omitted.
mkListAppsLists ::
  -- | 'maxResults'
  Core.Natural ->
  ListAppsLists
mkListAppsLists maxResults =
  ListAppsLists'
    { maxResults,
      defaultLists = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of objects that you want AWS Firewall Manager to return for this request. If more objects are available, in the response, AWS Firewall Manager provides a @NextToken@ value that you can use in a subsequent call to get the next batch of objects.
--
-- If you don't specify this, AWS Firewall Manager returns all available objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalMaxResults :: Lens.Lens' ListAppsLists Core.Natural
lalMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lalMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Specifies whether the lists to retrieve are default lists owned by AWS Firewall Manager.
--
-- /Note:/ Consider using 'defaultLists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalDefaultLists :: Lens.Lens' ListAppsLists (Core.Maybe Core.Bool)
lalDefaultLists = Lens.field @"defaultLists"
{-# DEPRECATED lalDefaultLists "Use generic-lens or generic-optics with 'defaultLists' instead." #-}

-- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. For all but the first request, you provide the token returned by the prior request in the request parameters, to retrieve the next batch of objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalNextToken :: Lens.Lens' ListAppsLists (Core.Maybe Types.PaginationToken)
lalNextToken = Lens.field @"nextToken"
{-# DEPRECATED lalNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListAppsLists where
  toJSON ListAppsLists {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MaxResults" Core..= maxResults),
            ("DefaultLists" Core..=) Core.<$> defaultLists,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListAppsLists where
  type Rs ListAppsLists = ListAppsListsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSFMS_20180101.ListAppsLists")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppsListsResponse'
            Core.<$> (x Core..:? "AppsLists")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListAppsListsResponse' smart constructor.
data ListAppsListsResponse = ListAppsListsResponse'
  { -- | An array of @AppsListDataSummary@ objects.
    appsLists :: Core.Maybe [Types.AppsListDataSummary],
    -- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. You can use this token in subsequent requests to retrieve the next batch of objects.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAppsListsResponse' value with any optional fields omitted.
mkListAppsListsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAppsListsResponse
mkListAppsListsResponse responseStatus =
  ListAppsListsResponse'
    { appsLists = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of @AppsListDataSummary@ objects.
--
-- /Note:/ Consider using 'appsLists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalrrsAppsLists :: Lens.Lens' ListAppsListsResponse (Core.Maybe [Types.AppsListDataSummary])
lalrrsAppsLists = Lens.field @"appsLists"
{-# DEPRECATED lalrrsAppsLists "Use generic-lens or generic-optics with 'appsLists' instead." #-}

-- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. You can use this token in subsequent requests to retrieve the next batch of objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalrrsNextToken :: Lens.Lens' ListAppsListsResponse (Core.Maybe Types.PaginationToken)
lalrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lalrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalrrsResponseStatus :: Lens.Lens' ListAppsListsResponse Core.Int
lalrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lalrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
