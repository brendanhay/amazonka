{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListOpenIDConnectProviders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about the IAM OpenID Connect (OIDC) provider resource objects defined in the AWS account.
module Network.AWS.IAM.ListOpenIDConnectProviders
    (
    -- * Creating a request
      ListOpenIDConnectProviders (..)
    , mkListOpenIDConnectProviders

    -- * Destructuring the response
    , ListOpenIDConnectProvidersResponse (..)
    , mkListOpenIDConnectProvidersResponse
    -- ** Response lenses
    , loidcprrsOpenIDConnectProviderList
    , loidcprrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListOpenIDConnectProviders' smart constructor.
data ListOpenIDConnectProviders = ListOpenIDConnectProviders'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOpenIDConnectProviders' value with any optional fields omitted.
mkListOpenIDConnectProviders
    :: ListOpenIDConnectProviders
mkListOpenIDConnectProviders = ListOpenIDConnectProviders'

instance Core.ToQuery ListOpenIDConnectProviders where
        toQuery ListOpenIDConnectProviders{..}
          = Core.toQueryPair "Action"
              ("ListOpenIDConnectProviders" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)

instance Core.ToHeaders ListOpenIDConnectProviders where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListOpenIDConnectProviders where
        type Rs ListOpenIDConnectProviders =
             ListOpenIDConnectProvidersResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ListOpenIDConnectProvidersResult"
              (\ s h x ->
                 ListOpenIDConnectProvidersResponse' Core.<$>
                   (x Core..@? "OpenIDConnectProviderList" Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'ListOpenIDConnectProviders' request. 
--
-- /See:/ 'mkListOpenIDConnectProvidersResponse' smart constructor.
data ListOpenIDConnectProvidersResponse = ListOpenIDConnectProvidersResponse'
  { openIDConnectProviderList :: Core.Maybe [Types.OpenIDConnectProviderListEntry]
    -- ^ The list of IAM OIDC provider resource objects defined in the AWS account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOpenIDConnectProvidersResponse' value with any optional fields omitted.
mkListOpenIDConnectProvidersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListOpenIDConnectProvidersResponse
mkListOpenIDConnectProvidersResponse responseStatus
  = ListOpenIDConnectProvidersResponse'{openIDConnectProviderList =
                                          Core.Nothing,
                                        responseStatus}

-- | The list of IAM OIDC provider resource objects defined in the AWS account.
--
-- /Note:/ Consider using 'openIDConnectProviderList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loidcprrsOpenIDConnectProviderList :: Lens.Lens' ListOpenIDConnectProvidersResponse (Core.Maybe [Types.OpenIDConnectProviderListEntry])
loidcprrsOpenIDConnectProviderList = Lens.field @"openIDConnectProviderList"
{-# INLINEABLE loidcprrsOpenIDConnectProviderList #-}
{-# DEPRECATED openIDConnectProviderList "Use generic-lens or generic-optics with 'openIDConnectProviderList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loidcprrsResponseStatus :: Lens.Lens' ListOpenIDConnectProvidersResponse Core.Int
loidcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE loidcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
