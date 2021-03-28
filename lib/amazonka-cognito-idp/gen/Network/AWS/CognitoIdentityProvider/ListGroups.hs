{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the groups associated with a user pool.
--
-- Calling this action requires developer credentials.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListGroups
    (
    -- * Creating a request
      ListGroups (..)
    , mkListGroups
    -- ** Request lenses
    , lgUserPoolId
    , lgLimit
    , lgNextToken

    -- * Destructuring the response
    , ListGroupsResponse (..)
    , mkListGroupsResponse
    -- ** Response lenses
    , lgrrsGroups
    , lgrrsNextToken
    , lgrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListGroups' smart constructor.
data ListGroups = ListGroups'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool.
  , limit :: Core.Maybe Core.Natural
    -- ^ The limit of the request to list groups.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGroups' value with any optional fields omitted.
mkListGroups
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> ListGroups
mkListGroups userPoolId
  = ListGroups'{userPoolId, limit = Core.Nothing,
                nextToken = Core.Nothing}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgUserPoolId :: Lens.Lens' ListGroups Types.UserPoolId
lgUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE lgUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The limit of the request to list groups.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgLimit :: Lens.Lens' ListGroups (Core.Maybe Core.Natural)
lgLimit = Lens.field @"limit"
{-# INLINEABLE lgLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgNextToken :: Lens.Lens' ListGroups (Core.Maybe Types.NextToken)
lgNextToken = Lens.field @"nextToken"
{-# INLINEABLE lgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListGroups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListGroups where
        toHeaders ListGroups{..}
          = Core.pure
              ("X-Amz-Target", "AWSCognitoIdentityProviderService.ListGroups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListGroups where
        toJSON ListGroups{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListGroups where
        type Rs ListGroups = ListGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListGroupsResponse' Core.<$>
                   (x Core..:? "Groups") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"groups" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { groups :: Core.Maybe [Types.GroupType]
    -- ^ The group objects for the groups.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListGroupsResponse' value with any optional fields omitted.
mkListGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListGroupsResponse
mkListGroupsResponse responseStatus
  = ListGroupsResponse'{groups = Core.Nothing,
                        nextToken = Core.Nothing, responseStatus}

-- | The group objects for the groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsGroups :: Lens.Lens' ListGroupsResponse (Core.Maybe [Types.GroupType])
lgrrsGroups = Lens.field @"groups"
{-# INLINEABLE lgrrsGroups #-}
{-# DEPRECATED groups "Use generic-lens or generic-optics with 'groups' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsNextToken :: Lens.Lens' ListGroupsResponse (Core.Maybe Types.NextToken)
lgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsResponseStatus :: Lens.Lens' ListGroupsResponse Core.Int
lgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
