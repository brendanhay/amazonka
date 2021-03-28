{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListSAMLProviders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the SAML provider resource objects defined in IAM in the account.
module Network.AWS.IAM.ListSAMLProviders
    (
    -- * Creating a request
      ListSAMLProviders (..)
    , mkListSAMLProviders

    -- * Destructuring the response
    , ListSAMLProvidersResponse (..)
    , mkListSAMLProvidersResponse
    -- ** Response lenses
    , lsamlprrsSAMLProviderList
    , lsamlprrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSAMLProviders' smart constructor.
data ListSAMLProviders = ListSAMLProviders'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSAMLProviders' value with any optional fields omitted.
mkListSAMLProviders
    :: ListSAMLProviders
mkListSAMLProviders = ListSAMLProviders'

instance Core.ToQuery ListSAMLProviders where
        toQuery ListSAMLProviders{..}
          = Core.toQueryPair "Action" ("ListSAMLProviders" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)

instance Core.ToHeaders ListSAMLProviders where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListSAMLProviders where
        type Rs ListSAMLProviders = ListSAMLProvidersResponse
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
          = Response.receiveXMLWrapper "ListSAMLProvidersResult"
              (\ s h x ->
                 ListSAMLProvidersResponse' Core.<$>
                   (x Core..@? "SAMLProviderList" Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'ListSAMLProviders' request. 
--
-- /See:/ 'mkListSAMLProvidersResponse' smart constructor.
data ListSAMLProvidersResponse = ListSAMLProvidersResponse'
  { sAMLProviderList :: Core.Maybe [Types.SAMLProviderListEntry]
    -- ^ The list of SAML provider resource objects defined in IAM for this AWS account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListSAMLProvidersResponse' value with any optional fields omitted.
mkListSAMLProvidersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListSAMLProvidersResponse
mkListSAMLProvidersResponse responseStatus
  = ListSAMLProvidersResponse'{sAMLProviderList = Core.Nothing,
                               responseStatus}

-- | The list of SAML provider resource objects defined in IAM for this AWS account.
--
-- /Note:/ Consider using 'sAMLProviderList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsamlprrsSAMLProviderList :: Lens.Lens' ListSAMLProvidersResponse (Core.Maybe [Types.SAMLProviderListEntry])
lsamlprrsSAMLProviderList = Lens.field @"sAMLProviderList"
{-# INLINEABLE lsamlprrsSAMLProviderList #-}
{-# DEPRECATED sAMLProviderList "Use generic-lens or generic-optics with 'sAMLProviderList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsamlprrsResponseStatus :: Lens.Lens' ListSAMLProvidersResponse Core.Int
lsamlprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsamlprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
