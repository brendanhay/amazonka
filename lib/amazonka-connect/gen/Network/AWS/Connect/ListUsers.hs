{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the users for the specified Amazon Connect instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListUsers
    (
    -- * Creating a request
      ListUsers (..)
    , mkListUsers
    -- ** Request lenses
    , luInstanceId
    , luMaxResults
    , luNextToken

    -- * Destructuring the response
    , ListUsersResponse (..)
    , mkListUsersResponse
    -- ** Response lenses
    , lurrsNextToken
    , lurrsUserSummaryList
    , lurrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListUsers' smart constructor.
data ListUsers = ListUsers'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximimum number of results to return per page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUsers' value with any optional fields omitted.
mkListUsers
    :: Types.InstanceId -- ^ 'instanceId'
    -> ListUsers
mkListUsers instanceId
  = ListUsers'{instanceId, maxResults = Core.Nothing,
               nextToken = Core.Nothing}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luInstanceId :: Lens.Lens' ListUsers Types.InstanceId
luInstanceId = Lens.field @"instanceId"
{-# INLINEABLE luInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luMaxResults :: Lens.Lens' ListUsers (Core.Maybe Core.Natural)
luMaxResults = Lens.field @"maxResults"
{-# INLINEABLE luMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luNextToken :: Lens.Lens' ListUsers (Core.Maybe Types.NextToken)
luNextToken = Lens.field @"nextToken"
{-# INLINEABLE luNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListUsers where
        toQuery ListUsers{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListUsers where
        toHeaders ListUsers{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListUsers where
        type Rs ListUsers = ListUsersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/users-summary/" Core.<> Core.toText instanceId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListUsersResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "UserSummaryList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListUsers where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"userSummaryList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If there are additional results, this is the token for the next set of results.
  , userSummaryList :: Core.Maybe [Types.UserSummary]
    -- ^ Information about the users.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUsersResponse' value with any optional fields omitted.
mkListUsersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListUsersResponse
mkListUsersResponse responseStatus
  = ListUsersResponse'{nextToken = Core.Nothing,
                       userSummaryList = Core.Nothing, responseStatus}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lurrsNextToken :: Lens.Lens' ListUsersResponse (Core.Maybe Types.NextToken)
lurrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lurrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the users.
--
-- /Note:/ Consider using 'userSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lurrsUserSummaryList :: Lens.Lens' ListUsersResponse (Core.Maybe [Types.UserSummary])
lurrsUserSummaryList = Lens.field @"userSummaryList"
{-# INLINEABLE lurrsUserSummaryList #-}
{-# DEPRECATED userSummaryList "Use generic-lens or generic-optics with 'userSummaryList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lurrsResponseStatus :: Lens.Lens' ListUsersResponse Core.Int
lurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
