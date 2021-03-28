{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListSecurityKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all security keys associated with the instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListSecurityKeys
    (
    -- * Creating a request
      ListSecurityKeys (..)
    , mkListSecurityKeys
    -- ** Request lenses
    , lskInstanceId
    , lskMaxResults
    , lskNextToken

    -- * Destructuring the response
    , ListSecurityKeysResponse (..)
    , mkListSecurityKeysResponse
    -- ** Response lenses
    , lskrrsNextToken
    , lskrrsSecurityKeys
    , lskrrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSecurityKeys' smart constructor.
data ListSecurityKeys = ListSecurityKeys'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximimum number of results to return per page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSecurityKeys' value with any optional fields omitted.
mkListSecurityKeys
    :: Types.InstanceId -- ^ 'instanceId'
    -> ListSecurityKeys
mkListSecurityKeys instanceId
  = ListSecurityKeys'{instanceId, maxResults = Core.Nothing,
                      nextToken = Core.Nothing}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskInstanceId :: Lens.Lens' ListSecurityKeys Types.InstanceId
lskInstanceId = Lens.field @"instanceId"
{-# INLINEABLE lskInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskMaxResults :: Lens.Lens' ListSecurityKeys (Core.Maybe Core.Natural)
lskMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lskMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskNextToken :: Lens.Lens' ListSecurityKeys (Core.Maybe Types.NextToken)
lskNextToken = Lens.field @"nextToken"
{-# INLINEABLE lskNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListSecurityKeys where
        toQuery ListSecurityKeys{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListSecurityKeys where
        toHeaders ListSecurityKeys{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListSecurityKeys where
        type Rs ListSecurityKeys = ListSecurityKeysResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/instance/" Core.<> Core.toText instanceId Core.<>
                             "/security-keys",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListSecurityKeysResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "SecurityKeys"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListSecurityKeys where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"securityKeys" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListSecurityKeysResponse' smart constructor.
data ListSecurityKeysResponse = ListSecurityKeysResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If there are additional results, this is the token for the next set of results.
  , securityKeys :: Core.Maybe [Types.SecurityKey]
    -- ^ The security keys.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListSecurityKeysResponse' value with any optional fields omitted.
mkListSecurityKeysResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListSecurityKeysResponse
mkListSecurityKeysResponse responseStatus
  = ListSecurityKeysResponse'{nextToken = Core.Nothing,
                              securityKeys = Core.Nothing, responseStatus}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskrrsNextToken :: Lens.Lens' ListSecurityKeysResponse (Core.Maybe Types.NextToken)
lskrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lskrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The security keys.
--
-- /Note:/ Consider using 'securityKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskrrsSecurityKeys :: Lens.Lens' ListSecurityKeysResponse (Core.Maybe [Types.SecurityKey])
lskrrsSecurityKeys = Lens.field @"securityKeys"
{-# INLINEABLE lskrrsSecurityKeys #-}
{-# DEPRECATED securityKeys "Use generic-lens or generic-optics with 'securityKeys' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskrrsResponseStatus :: Lens.Lens' ListSecurityKeysResponse Core.Int
lskrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lskrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
