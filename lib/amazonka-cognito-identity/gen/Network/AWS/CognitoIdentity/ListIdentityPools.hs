{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.ListIdentityPools
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the Cognito identity pools registered for your account.
--
-- You must use AWS Developer credentials to call this API.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentity.ListIdentityPools
    (
    -- * Creating a request
      ListIdentityPools (..)
    , mkListIdentityPools
    -- ** Request lenses
    , lipMaxResults
    , lipNextToken

    -- * Destructuring the response
    , ListIdentityPoolsResponse (..)
    , mkListIdentityPoolsResponse
    -- ** Response lenses
    , liprrsIdentityPools
    , liprrsNextToken
    , liprrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the ListIdentityPools action.
--
-- /See:/ 'mkListIdentityPools' smart constructor.
data ListIdentityPools = ListIdentityPools'
  { maxResults :: Core.Natural
    -- ^ The maximum number of identities to return.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListIdentityPools' value with any optional fields omitted.
mkListIdentityPools
    :: Core.Natural -- ^ 'maxResults'
    -> ListIdentityPools
mkListIdentityPools maxResults
  = ListIdentityPools'{maxResults, nextToken = Core.Nothing}

-- | The maximum number of identities to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipMaxResults :: Lens.Lens' ListIdentityPools Core.Natural
lipMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lipMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipNextToken :: Lens.Lens' ListIdentityPools (Core.Maybe Types.NextToken)
lipNextToken = Lens.field @"nextToken"
{-# INLINEABLE lipNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListIdentityPools where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListIdentityPools where
        toHeaders ListIdentityPools{..}
          = Core.pure
              ("X-Amz-Target", "AWSCognitoIdentityService.ListIdentityPools")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListIdentityPools where
        toJSON ListIdentityPools{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MaxResults" Core..= maxResults),
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListIdentityPools where
        type Rs ListIdentityPools = ListIdentityPoolsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListIdentityPoolsResponse' Core.<$>
                   (x Core..:? "IdentityPools") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListIdentityPools where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"identityPools" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | The result of a successful ListIdentityPools action.
--
-- /See:/ 'mkListIdentityPoolsResponse' smart constructor.
data ListIdentityPoolsResponse = ListIdentityPoolsResponse'
  { identityPools :: Core.Maybe [Types.IdentityPoolShortDescription]
    -- ^ The identity pools returned by the ListIdentityPools action.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListIdentityPoolsResponse' value with any optional fields omitted.
mkListIdentityPoolsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListIdentityPoolsResponse
mkListIdentityPoolsResponse responseStatus
  = ListIdentityPoolsResponse'{identityPools = Core.Nothing,
                               nextToken = Core.Nothing, responseStatus}

-- | The identity pools returned by the ListIdentityPools action.
--
-- /Note:/ Consider using 'identityPools' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprrsIdentityPools :: Lens.Lens' ListIdentityPoolsResponse (Core.Maybe [Types.IdentityPoolShortDescription])
liprrsIdentityPools = Lens.field @"identityPools"
{-# INLINEABLE liprrsIdentityPools #-}
{-# DEPRECATED identityPools "Use generic-lens or generic-optics with 'identityPools' instead"  #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprrsNextToken :: Lens.Lens' ListIdentityPoolsResponse (Core.Maybe Types.NextToken)
liprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE liprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprrsResponseStatus :: Lens.Lens' ListIdentityPoolsResponse Core.Int
liprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE liprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
