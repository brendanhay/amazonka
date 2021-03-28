{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListUserPoolClients
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the clients that have been created for the specified user pool.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListUserPoolClients
    (
    -- * Creating a request
      ListUserPoolClients (..)
    , mkListUserPoolClients
    -- ** Request lenses
    , lupcUserPoolId
    , lupcMaxResults
    , lupcNextToken

    -- * Destructuring the response
    , ListUserPoolClientsResponse (..)
    , mkListUserPoolClientsResponse
    -- ** Response lenses
    , lupcrrsNextToken
    , lupcrrsUserPoolClients
    , lupcrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list the user pool clients.
--
-- /See:/ 'mkListUserPoolClients' smart constructor.
data ListUserPoolClients = ListUserPoolClients'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool where you want to list user pool clients.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results you want the request to return when listing the user pool clients.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUserPoolClients' value with any optional fields omitted.
mkListUserPoolClients
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> ListUserPoolClients
mkListUserPoolClients userPoolId
  = ListUserPoolClients'{userPoolId, maxResults = Core.Nothing,
                         nextToken = Core.Nothing}

-- | The user pool ID for the user pool where you want to list user pool clients.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupcUserPoolId :: Lens.Lens' ListUserPoolClients Types.UserPoolId
lupcUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE lupcUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The maximum number of results you want the request to return when listing the user pool clients.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupcMaxResults :: Lens.Lens' ListUserPoolClients (Core.Maybe Core.Natural)
lupcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lupcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupcNextToken :: Lens.Lens' ListUserPoolClients (Core.Maybe Types.NextToken)
lupcNextToken = Lens.field @"nextToken"
{-# INLINEABLE lupcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListUserPoolClients where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListUserPoolClients where
        toHeaders ListUserPoolClients{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.ListUserPoolClients")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListUserPoolClients where
        toJSON ListUserPoolClients{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListUserPoolClients where
        type Rs ListUserPoolClients = ListUserPoolClientsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListUserPoolClientsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "UserPoolClients"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListUserPoolClients where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"userPoolClients" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the response from the server that lists user pool clients.
--
-- /See:/ 'mkListUserPoolClientsResponse' smart constructor.
data ListUserPoolClientsResponse = ListUserPoolClientsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  , userPoolClients :: Core.Maybe [Types.UserPoolClientDescription]
    -- ^ The user pool clients in the response that lists user pool clients.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUserPoolClientsResponse' value with any optional fields omitted.
mkListUserPoolClientsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListUserPoolClientsResponse
mkListUserPoolClientsResponse responseStatus
  = ListUserPoolClientsResponse'{nextToken = Core.Nothing,
                                 userPoolClients = Core.Nothing, responseStatus}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupcrrsNextToken :: Lens.Lens' ListUserPoolClientsResponse (Core.Maybe Types.NextToken)
lupcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lupcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The user pool clients in the response that lists user pool clients.
--
-- /Note:/ Consider using 'userPoolClients' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupcrrsUserPoolClients :: Lens.Lens' ListUserPoolClientsResponse (Core.Maybe [Types.UserPoolClientDescription])
lupcrrsUserPoolClients = Lens.field @"userPoolClients"
{-# INLINEABLE lupcrrsUserPoolClients #-}
{-# DEPRECATED userPoolClients "Use generic-lens or generic-optics with 'userPoolClients' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupcrrsResponseStatus :: Lens.Lens' ListUserPoolClientsResponse Core.Int
lupcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lupcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
