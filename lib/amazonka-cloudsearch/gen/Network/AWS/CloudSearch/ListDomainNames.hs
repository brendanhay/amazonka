{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.ListDomainNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all search domains owned by an account.
module Network.AWS.CloudSearch.ListDomainNames
    (
    -- * Creating a request
      ListDomainNames (..)
    , mkListDomainNames

    -- * Destructuring the response
    , ListDomainNamesResponse (..)
    , mkListDomainNamesResponse
    -- ** Response lenses
    , ldnrrsDomainNames
    , ldnrrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDomainNames' smart constructor.
data ListDomainNames = ListDomainNames'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDomainNames' value with any optional fields omitted.
mkListDomainNames
    :: ListDomainNames
mkListDomainNames = ListDomainNames'

instance Core.ToQuery ListDomainNames where
        toQuery ListDomainNames{..}
          = Core.toQueryPair "Action" ("ListDomainNames" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)

instance Core.ToHeaders ListDomainNames where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListDomainNames where
        type Rs ListDomainNames = ListDomainNamesResponse
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
          = Response.receiveXMLWrapper "ListDomainNamesResult"
              (\ s h x ->
                 ListDomainNamesResponse' Core.<$>
                   (x Core..@? "DomainNames" Core..<@>
                      Core.parseXMLMap "entry" "key" "value")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @ListDomainNames@ request. Contains a list of the domains owned by an account.
--
-- /See:/ 'mkListDomainNamesResponse' smart constructor.
data ListDomainNamesResponse = ListDomainNamesResponse'
  { domainNames :: Core.Maybe (Core.HashMap Types.DomainName Types.APIVersion)
    -- ^ The names of the search domains owned by an account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDomainNamesResponse' value with any optional fields omitted.
mkListDomainNamesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDomainNamesResponse
mkListDomainNamesResponse responseStatus
  = ListDomainNamesResponse'{domainNames = Core.Nothing,
                             responseStatus}

-- | The names of the search domains owned by an account.
--
-- /Note:/ Consider using 'domainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldnrrsDomainNames :: Lens.Lens' ListDomainNamesResponse (Core.Maybe (Core.HashMap Types.DomainName Types.APIVersion))
ldnrrsDomainNames = Lens.field @"domainNames"
{-# INLINEABLE ldnrrsDomainNames #-}
{-# DEPRECATED domainNames "Use generic-lens or generic-optics with 'domainNames' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldnrrsResponseStatus :: Lens.Lens' ListDomainNamesResponse Core.Int
ldnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
