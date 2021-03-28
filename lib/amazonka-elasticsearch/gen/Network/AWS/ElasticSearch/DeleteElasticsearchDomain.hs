{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DeleteElasticsearchDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified Elasticsearch domain and all of its data. Once a domain is deleted, it cannot be recovered.
module Network.AWS.ElasticSearch.DeleteElasticsearchDomain
    (
    -- * Creating a request
      DeleteElasticsearchDomain (..)
    , mkDeleteElasticsearchDomain
    -- ** Request lenses
    , dedfDomainName

    -- * Destructuring the response
    , DeleteElasticsearchDomainResponse (..)
    , mkDeleteElasticsearchDomainResponse
    -- ** Response lenses
    , dedrgrsDomainStatus
    , dedrgrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DeleteElasticsearchDomain' @ operation. Specifies the name of the Elasticsearch domain that you want to delete.
--
-- /See:/ 'mkDeleteElasticsearchDomain' smart constructor.
newtype DeleteElasticsearchDomain = DeleteElasticsearchDomain'
  { domainName :: Types.DomainName
    -- ^ The name of the Elasticsearch domain that you want to permanently delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteElasticsearchDomain' value with any optional fields omitted.
mkDeleteElasticsearchDomain
    :: Types.DomainName -- ^ 'domainName'
    -> DeleteElasticsearchDomain
mkDeleteElasticsearchDomain domainName
  = DeleteElasticsearchDomain'{domainName}

-- | The name of the Elasticsearch domain that you want to permanently delete.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedfDomainName :: Lens.Lens' DeleteElasticsearchDomain Types.DomainName
dedfDomainName = Lens.field @"domainName"
{-# INLINEABLE dedfDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery DeleteElasticsearchDomain where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteElasticsearchDomain where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteElasticsearchDomain where
        type Rs DeleteElasticsearchDomain =
             DeleteElasticsearchDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/2015-01-01/es/domain/" Core.<> Core.toText domainName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteElasticsearchDomainResponse' Core.<$>
                   (x Core..:? "DomainStatus") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @DeleteElasticsearchDomain@ request. Contains the status of the pending deletion, or no status if the domain and all of its resources have been deleted.
--
-- /See:/ 'mkDeleteElasticsearchDomainResponse' smart constructor.
data DeleteElasticsearchDomainResponse = DeleteElasticsearchDomainResponse'
  { domainStatus :: Core.Maybe Types.ElasticsearchDomainStatus
    -- ^ The status of the Elasticsearch domain being deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteElasticsearchDomainResponse' value with any optional fields omitted.
mkDeleteElasticsearchDomainResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteElasticsearchDomainResponse
mkDeleteElasticsearchDomainResponse responseStatus
  = DeleteElasticsearchDomainResponse'{domainStatus = Core.Nothing,
                                       responseStatus}

-- | The status of the Elasticsearch domain being deleted.
--
-- /Note:/ Consider using 'domainStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedrgrsDomainStatus :: Lens.Lens' DeleteElasticsearchDomainResponse (Core.Maybe Types.ElasticsearchDomainStatus)
dedrgrsDomainStatus = Lens.field @"domainStatus"
{-# INLINEABLE dedrgrsDomainStatus #-}
{-# DEPRECATED domainStatus "Use generic-lens or generic-optics with 'domainStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedrgrsResponseStatus :: Lens.Lens' DeleteElasticsearchDomainResponse Core.Int
dedrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dedrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
