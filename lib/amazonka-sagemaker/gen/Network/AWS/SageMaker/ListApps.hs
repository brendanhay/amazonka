{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListApps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists apps.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListApps
  ( -- * Creating a request
    ListApps (..),
    mkListApps,

    -- ** Request lenses
    laDomainIdEquals,
    laMaxResults,
    laNextToken,
    laSortBy,
    laSortOrder,
    laUserProfileNameEquals,

    -- * Destructuring the response
    ListAppsResponse (..),
    mkListAppsResponse,

    -- ** Response lenses
    larrsApps,
    larrsNextToken,
    larrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListApps' smart constructor.
data ListApps = ListApps'
  { -- | A parameter to search for the domain ID.
    domainIdEquals :: Core.Maybe Types.DomainId,
    -- | Returns a list up to a specified limit.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The parameter by which to sort the results. The default is CreationTime.
    sortBy :: Core.Maybe Types.AppSortKey,
    -- | The sort order for the results. The default is Ascending.
    sortOrder :: Core.Maybe Types.SortOrder,
    -- | A parameter to search by user profile name.
    userProfileNameEquals :: Core.Maybe Types.UserProfileName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApps' value with any optional fields omitted.
mkListApps ::
  ListApps
mkListApps =
  ListApps'
    { domainIdEquals = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing,
      userProfileNameEquals = Core.Nothing
    }

-- | A parameter to search for the domain ID.
--
-- /Note:/ Consider using 'domainIdEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laDomainIdEquals :: Lens.Lens' ListApps (Core.Maybe Types.DomainId)
laDomainIdEquals = Lens.field @"domainIdEquals"
{-# DEPRECATED laDomainIdEquals "Use generic-lens or generic-optics with 'domainIdEquals' instead." #-}

-- | Returns a list up to a specified limit.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxResults :: Lens.Lens' ListApps (Core.Maybe Core.Natural)
laMaxResults = Lens.field @"maxResults"
{-# DEPRECATED laMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListApps (Core.Maybe Types.NextToken)
laNextToken = Lens.field @"nextToken"
{-# DEPRECATED laNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The parameter by which to sort the results. The default is CreationTime.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laSortBy :: Lens.Lens' ListApps (Core.Maybe Types.AppSortKey)
laSortBy = Lens.field @"sortBy"
{-# DEPRECATED laSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order for the results. The default is Ascending.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laSortOrder :: Lens.Lens' ListApps (Core.Maybe Types.SortOrder)
laSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED laSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A parameter to search by user profile name.
--
-- /Note:/ Consider using 'userProfileNameEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laUserProfileNameEquals :: Lens.Lens' ListApps (Core.Maybe Types.UserProfileName)
laUserProfileNameEquals = Lens.field @"userProfileNameEquals"
{-# DEPRECATED laUserProfileNameEquals "Use generic-lens or generic-optics with 'userProfileNameEquals' instead." #-}

instance Core.FromJSON ListApps where
  toJSON ListApps {..} =
    Core.object
      ( Core.catMaybes
          [ ("DomainIdEquals" Core..=) Core.<$> domainIdEquals,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("UserProfileNameEquals" Core..=) Core.<$> userProfileNameEquals
          ]
      )

instance Core.AWSRequest ListApps where
  type Rs ListApps = ListAppsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListApps")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppsResponse'
            Core.<$> (x Core..:? "Apps")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListApps where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"apps" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListAppsResponse' smart constructor.
data ListAppsResponse = ListAppsResponse'
  { -- | The list of apps.
    apps :: Core.Maybe [Types.AppDetails],
    -- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAppsResponse' value with any optional fields omitted.
mkListAppsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAppsResponse
mkListAppsResponse responseStatus =
  ListAppsResponse'
    { apps = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of apps.
--
-- /Note:/ Consider using 'apps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsApps :: Lens.Lens' ListAppsResponse (Core.Maybe [Types.AppDetails])
larrsApps = Lens.field @"apps"
{-# DEPRECATED larrsApps "Use generic-lens or generic-optics with 'apps' instead." #-}

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsNextToken :: Lens.Lens' ListAppsResponse (Core.Maybe Types.NextToken)
larrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED larrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsResponseStatus :: Lens.Lens' ListAppsResponse Core.Int
larrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED larrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
