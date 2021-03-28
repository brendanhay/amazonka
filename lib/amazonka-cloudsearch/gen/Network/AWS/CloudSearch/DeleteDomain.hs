{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a search domain and all of its data. Once a domain has been deleted, it cannot be recovered. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/deleting-domains.html Deleting a Search Domain> in the /Amazon CloudSearch Developer Guide/ . 
module Network.AWS.CloudSearch.DeleteDomain
    (
    -- * Creating a request
      DeleteDomain (..)
    , mkDeleteDomain
    -- ** Request lenses
    , ddDomainName

    -- * Destructuring the response
    , DeleteDomainResponse (..)
    , mkDeleteDomainResponse
    -- ** Response lenses
    , ddrrsDomainStatus
    , ddrrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DeleteDomain' @ operation. Specifies the name of the domain you want to delete.
--
-- /See:/ 'mkDeleteDomain' smart constructor.
newtype DeleteDomain = DeleteDomain'
  { domainName :: Types.DomainName
    -- ^ The name of the domain you want to permanently delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomain' value with any optional fields omitted.
mkDeleteDomain
    :: Types.DomainName -- ^ 'domainName'
    -> DeleteDomain
mkDeleteDomain domainName = DeleteDomain'{domainName}

-- | The name of the domain you want to permanently delete.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomainName :: Lens.Lens' DeleteDomain Types.DomainName
ddDomainName = Lens.field @"domainName"
{-# INLINEABLE ddDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery DeleteDomain where
        toQuery DeleteDomain{..}
          = Core.toQueryPair "Action" ("DeleteDomain" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName

instance Core.ToHeaders DeleteDomain where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDomain where
        type Rs DeleteDomain = DeleteDomainResponse
        toRequest x@Core.Request{..}
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
          = Response.receiveXMLWrapper "DeleteDomainResult"
              (\ s h x ->
                 DeleteDomainResponse' Core.<$>
                   (x Core..@? "DomainStatus") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @DeleteDomain@ request. Contains the status of a newly deleted domain, or no status if the domain has already been completely deleted.
--
-- /See:/ 'mkDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  { domainStatus :: Core.Maybe Types.DomainStatus
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomainResponse' value with any optional fields omitted.
mkDeleteDomainResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDomainResponse
mkDeleteDomainResponse responseStatus
  = DeleteDomainResponse'{domainStatus = Core.Nothing,
                          responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDomainStatus :: Lens.Lens' DeleteDomainResponse (Core.Maybe Types.DomainStatus)
ddrrsDomainStatus = Lens.field @"domainStatus"
{-# INLINEABLE ddrrsDomainStatus #-}
{-# DEPRECATED domainStatus "Use generic-lens or generic-optics with 'domainStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DeleteDomainResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
