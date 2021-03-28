{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.ListUserProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the user profiles configured for your AWS account in AWS CodeStar.
--
-- This operation returns paginated results.
module Network.AWS.CodeStar.ListUserProfiles
    (
    -- * Creating a request
      ListUserProfiles (..)
    , mkListUserProfiles
    -- ** Request lenses
    , lupMaxResults
    , lupNextToken

    -- * Destructuring the response
    , ListUserProfilesResponse (..)
    , mkListUserProfilesResponse
    -- ** Response lenses
    , luprrsUserProfiles
    , luprrsNextToken
    , luprrsResponseStatus
    ) where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListUserProfiles' smart constructor.
data ListUserProfiles = ListUserProfiles'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in a response.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The continuation token for the next set of results, if the results cannot be returned in one response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUserProfiles' value with any optional fields omitted.
mkListUserProfiles
    :: ListUserProfiles
mkListUserProfiles
  = ListUserProfiles'{maxResults = Core.Nothing,
                      nextToken = Core.Nothing}

-- | The maximum number of results to return in a response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupMaxResults :: Lens.Lens' ListUserProfiles (Core.Maybe Core.Natural)
lupMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lupMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The continuation token for the next set of results, if the results cannot be returned in one response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupNextToken :: Lens.Lens' ListUserProfiles (Core.Maybe Types.PaginationToken)
lupNextToken = Lens.field @"nextToken"
{-# INLINEABLE lupNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListUserProfiles where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListUserProfiles where
        toHeaders ListUserProfiles{..}
          = Core.pure ("X-Amz-Target", "CodeStar_20170419.ListUserProfiles")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListUserProfiles where
        toJSON ListUserProfiles{..}
          = Core.object
              (Core.catMaybes
                 [("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListUserProfiles where
        type Rs ListUserProfiles = ListUserProfilesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListUserProfilesResponse' Core.<$>
                   (x Core..:? "userProfiles" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListUserProfiles where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"userProfiles") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListUserProfilesResponse' smart constructor.
data ListUserProfilesResponse = ListUserProfilesResponse'
  { userProfiles :: [Types.UserProfileSummary]
    -- ^ All the user profiles configured in AWS CodeStar for an AWS account.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The continuation token to use when requesting the next set of results, if there are more results to be returned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUserProfilesResponse' value with any optional fields omitted.
mkListUserProfilesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListUserProfilesResponse
mkListUserProfilesResponse responseStatus
  = ListUserProfilesResponse'{userProfiles = Core.mempty,
                              nextToken = Core.Nothing, responseStatus}

-- | All the user profiles configured in AWS CodeStar for an AWS account.
--
-- /Note:/ Consider using 'userProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprrsUserProfiles :: Lens.Lens' ListUserProfilesResponse [Types.UserProfileSummary]
luprrsUserProfiles = Lens.field @"userProfiles"
{-# INLINEABLE luprrsUserProfiles #-}
{-# DEPRECATED userProfiles "Use generic-lens or generic-optics with 'userProfiles' instead"  #-}

-- | The continuation token to use when requesting the next set of results, if there are more results to be returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprrsNextToken :: Lens.Lens' ListUserProfilesResponse (Core.Maybe Types.PaginationToken)
luprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE luprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprrsResponseStatus :: Lens.Lens' ListUserProfilesResponse Core.Int
luprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE luprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
