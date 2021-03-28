{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListIdentityProviders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about all identity providers for a user pool.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListIdentityProviders
    (
    -- * Creating a request
      ListIdentityProviders (..)
    , mkListIdentityProviders
    -- ** Request lenses
    , lipUserPoolId
    , lipMaxResults
    , lipNextToken

    -- * Destructuring the response
    , ListIdentityProvidersResponse (..)
    , mkListIdentityProvidersResponse
    -- ** Response lenses
    , liprrsProviders
    , liprrsNextToken
    , liprrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListIdentityProviders' smart constructor.
data ListIdentityProviders = ListIdentityProviders'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of identity providers to return.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListIdentityProviders' value with any optional fields omitted.
mkListIdentityProviders
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> ListIdentityProviders
mkListIdentityProviders userPoolId
  = ListIdentityProviders'{userPoolId, maxResults = Core.Nothing,
                           nextToken = Core.Nothing}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipUserPoolId :: Lens.Lens' ListIdentityProviders Types.UserPoolId
lipUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE lipUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The maximum number of identity providers to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipMaxResults :: Lens.Lens' ListIdentityProviders (Core.Maybe Core.Natural)
lipMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lipMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipNextToken :: Lens.Lens' ListIdentityProviders (Core.Maybe Types.NextToken)
lipNextToken = Lens.field @"nextToken"
{-# INLINEABLE lipNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListIdentityProviders where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListIdentityProviders where
        toHeaders ListIdentityProviders{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.ListIdentityProviders")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListIdentityProviders where
        toJSON ListIdentityProviders{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListIdentityProviders where
        type Rs ListIdentityProviders = ListIdentityProvidersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListIdentityProvidersResponse' Core.<$>
                   (x Core..:? "Providers" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListIdentityProviders where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"providers") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListIdentityProvidersResponse' smart constructor.
data ListIdentityProvidersResponse = ListIdentityProvidersResponse'
  { providers :: [Types.ProviderDescription]
    -- ^ A list of identity provider objects.
  , nextToken :: Core.Maybe Types.PaginationKeyType
    -- ^ A pagination token.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListIdentityProvidersResponse' value with any optional fields omitted.
mkListIdentityProvidersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListIdentityProvidersResponse
mkListIdentityProvidersResponse responseStatus
  = ListIdentityProvidersResponse'{providers = Core.mempty,
                                   nextToken = Core.Nothing, responseStatus}

-- | A list of identity provider objects.
--
-- /Note:/ Consider using 'providers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprrsProviders :: Lens.Lens' ListIdentityProvidersResponse [Types.ProviderDescription]
liprrsProviders = Lens.field @"providers"
{-# INLINEABLE liprrsProviders #-}
{-# DEPRECATED providers "Use generic-lens or generic-optics with 'providers' instead"  #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprrsNextToken :: Lens.Lens' ListIdentityProvidersResponse (Core.Maybe Types.PaginationKeyType)
liprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE liprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprrsResponseStatus :: Lens.Lens' ListIdentityProvidersResponse Core.Int
liprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE liprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
