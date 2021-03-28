{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListResourceServers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource servers for a user pool.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListResourceServers
    (
    -- * Creating a request
      ListResourceServers (..)
    , mkListResourceServers
    -- ** Request lenses
    , lrsUserPoolId
    , lrsMaxResults
    , lrsNextToken

    -- * Destructuring the response
    , ListResourceServersResponse (..)
    , mkListResourceServersResponse
    -- ** Response lenses
    , lrsrrsResourceServers
    , lrsrrsNextToken
    , lrsrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListResourceServers' smart constructor.
data ListResourceServers = ListResourceServers'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of resource servers to return.
  , nextToken :: Core.Maybe Types.PaginationKeyType
    -- ^ A pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceServers' value with any optional fields omitted.
mkListResourceServers
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> ListResourceServers
mkListResourceServers userPoolId
  = ListResourceServers'{userPoolId, maxResults = Core.Nothing,
                         nextToken = Core.Nothing}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsUserPoolId :: Lens.Lens' ListResourceServers Types.UserPoolId
lrsUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE lrsUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The maximum number of resource servers to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsMaxResults :: Lens.Lens' ListResourceServers (Core.Maybe Core.Natural)
lrsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lrsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListResourceServers (Core.Maybe Types.PaginationKeyType)
lrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListResourceServers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListResourceServers where
        toHeaders ListResourceServers{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.ListResourceServers")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListResourceServers where
        toJSON ListResourceServers{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListResourceServers where
        type Rs ListResourceServers = ListResourceServersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListResourceServersResponse' Core.<$>
                   (x Core..:? "ResourceServers" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListResourceServers where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"resourceServers") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListResourceServersResponse' smart constructor.
data ListResourceServersResponse = ListResourceServersResponse'
  { resourceServers :: [Types.ResourceServerType]
    -- ^ The resource servers.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceServersResponse' value with any optional fields omitted.
mkListResourceServersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListResourceServersResponse
mkListResourceServersResponse responseStatus
  = ListResourceServersResponse'{resourceServers = Core.mempty,
                                 nextToken = Core.Nothing, responseStatus}

-- | The resource servers.
--
-- /Note:/ Consider using 'resourceServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsrrsResourceServers :: Lens.Lens' ListResourceServersResponse [Types.ResourceServerType]
lrsrrsResourceServers = Lens.field @"resourceServers"
{-# INLINEABLE lrsrrsResourceServers #-}
{-# DEPRECATED resourceServers "Use generic-lens or generic-optics with 'resourceServers' instead"  #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsrrsNextToken :: Lens.Lens' ListResourceServersResponse (Core.Maybe Types.NextToken)
lrsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsrrsResponseStatus :: Lens.Lens' ListResourceServersResponse Core.Int
lrsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
