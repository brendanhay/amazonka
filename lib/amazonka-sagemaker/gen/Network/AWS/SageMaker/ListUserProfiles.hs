{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListUserProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists user profiles.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListUserProfiles
  ( -- * Creating a request
    ListUserProfiles (..),
    mkListUserProfiles,

    -- ** Request lenses
    lupDomainIdEquals,
    lupMaxResults,
    lupNextToken,
    lupSortBy,
    lupSortOrder,
    lupUserProfileNameContains,

    -- * Destructuring the response
    ListUserProfilesResponse (..),
    mkListUserProfilesResponse,

    -- ** Response lenses
    luprrsNextToken,
    luprrsUserProfiles,
    luprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListUserProfiles' smart constructor.
data ListUserProfiles = ListUserProfiles'
  { -- | A parameter by which to filter the results.
    domainIdEquals :: Core.Maybe Types.DomainIdEquals,
    -- | Returns a list up to a specified limit.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The parameter by which to sort the results. The default is CreationTime.
    sortBy :: Core.Maybe Types.UserProfileSortKey,
    -- | The sort order for the results. The default is Ascending.
    sortOrder :: Core.Maybe Types.SortOrder,
    -- | A parameter by which to filter the results.
    userProfileNameContains :: Core.Maybe Types.UserProfileNameContains
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUserProfiles' value with any optional fields omitted.
mkListUserProfiles ::
  ListUserProfiles
mkListUserProfiles =
  ListUserProfiles'
    { domainIdEquals = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing,
      userProfileNameContains = Core.Nothing
    }

-- | A parameter by which to filter the results.
--
-- /Note:/ Consider using 'domainIdEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupDomainIdEquals :: Lens.Lens' ListUserProfiles (Core.Maybe Types.DomainIdEquals)
lupDomainIdEquals = Lens.field @"domainIdEquals"
{-# DEPRECATED lupDomainIdEquals "Use generic-lens or generic-optics with 'domainIdEquals' instead." #-}

-- | Returns a list up to a specified limit.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupMaxResults :: Lens.Lens' ListUserProfiles (Core.Maybe Core.Natural)
lupMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lupMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupNextToken :: Lens.Lens' ListUserProfiles (Core.Maybe Types.NextToken)
lupNextToken = Lens.field @"nextToken"
{-# DEPRECATED lupNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The parameter by which to sort the results. The default is CreationTime.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupSortBy :: Lens.Lens' ListUserProfiles (Core.Maybe Types.UserProfileSortKey)
lupSortBy = Lens.field @"sortBy"
{-# DEPRECATED lupSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order for the results. The default is Ascending.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupSortOrder :: Lens.Lens' ListUserProfiles (Core.Maybe Types.SortOrder)
lupSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lupSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A parameter by which to filter the results.
--
-- /Note:/ Consider using 'userProfileNameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupUserProfileNameContains :: Lens.Lens' ListUserProfiles (Core.Maybe Types.UserProfileNameContains)
lupUserProfileNameContains = Lens.field @"userProfileNameContains"
{-# DEPRECATED lupUserProfileNameContains "Use generic-lens or generic-optics with 'userProfileNameContains' instead." #-}

instance Core.FromJSON ListUserProfiles where
  toJSON ListUserProfiles {..} =
    Core.object
      ( Core.catMaybes
          [ ("DomainIdEquals" Core..=) Core.<$> domainIdEquals,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("UserProfileNameContains" Core..=)
              Core.<$> userProfileNameContains
          ]
      )

instance Core.AWSRequest ListUserProfiles where
  type Rs ListUserProfiles = ListUserProfilesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListUserProfiles")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUserProfilesResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "UserProfiles")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListUserProfiles where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"userProfiles" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListUserProfilesResponse' smart constructor.
data ListUserProfilesResponse = ListUserProfilesResponse'
  { -- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The list of user profiles.
    userProfiles :: Core.Maybe [Types.UserProfileDetails],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListUserProfilesResponse' value with any optional fields omitted.
mkListUserProfilesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListUserProfilesResponse
mkListUserProfilesResponse responseStatus =
  ListUserProfilesResponse'
    { nextToken = Core.Nothing,
      userProfiles = Core.Nothing,
      responseStatus
    }

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprrsNextToken :: Lens.Lens' ListUserProfilesResponse (Core.Maybe Types.NextToken)
luprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED luprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of user profiles.
--
-- /Note:/ Consider using 'userProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprrsUserProfiles :: Lens.Lens' ListUserProfilesResponse (Core.Maybe [Types.UserProfileDetails])
luprrsUserProfiles = Lens.field @"userProfiles"
{-# DEPRECATED luprrsUserProfiles "Use generic-lens or generic-optics with 'userProfiles' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprrsResponseStatus :: Lens.Lens' ListUserProfilesResponse Core.Int
luprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED luprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
