{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteElasticsearchDomain (..),
    mkDeleteElasticsearchDomain,

    -- ** Request lenses
    dedfDomainName,

    -- * Destructuring the response
    DeleteElasticsearchDomainResponse (..),
    mkDeleteElasticsearchDomainResponse,

    -- ** Response lenses
    dedrgrsDomainStatus,
    dedrgrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DeleteElasticsearchDomain' @ operation. Specifies the name of the Elasticsearch domain that you want to delete.
--
-- /See:/ 'mkDeleteElasticsearchDomain' smart constructor.
newtype DeleteElasticsearchDomain = DeleteElasticsearchDomain'
  { -- | The name of the Elasticsearch domain that you want to permanently delete.
    domainName :: Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteElasticsearchDomain' value with any optional fields omitted.
mkDeleteElasticsearchDomain ::
  -- | 'domainName'
  Types.DomainName ->
  DeleteElasticsearchDomain
mkDeleteElasticsearchDomain domainName =
  DeleteElasticsearchDomain' {domainName}

-- | The name of the Elasticsearch domain that you want to permanently delete.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedfDomainName :: Lens.Lens' DeleteElasticsearchDomain Types.DomainName
dedfDomainName = Lens.field @"domainName"
{-# DEPRECATED dedfDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.AWSRequest DeleteElasticsearchDomain where
  type
    Rs DeleteElasticsearchDomain =
      DeleteElasticsearchDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ("/2015-01-01/es/domain/" Core.<> (Core.toText domainName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteElasticsearchDomainResponse'
            Core.<$> (x Core..:? "DomainStatus") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @DeleteElasticsearchDomain@ request. Contains the status of the pending deletion, or no status if the domain and all of its resources have been deleted.
--
-- /See:/ 'mkDeleteElasticsearchDomainResponse' smart constructor.
data DeleteElasticsearchDomainResponse = DeleteElasticsearchDomainResponse'
  { -- | The status of the Elasticsearch domain being deleted.
    domainStatus :: Core.Maybe Types.ElasticsearchDomainStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteElasticsearchDomainResponse' value with any optional fields omitted.
mkDeleteElasticsearchDomainResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteElasticsearchDomainResponse
mkDeleteElasticsearchDomainResponse responseStatus =
  DeleteElasticsearchDomainResponse'
    { domainStatus = Core.Nothing,
      responseStatus
    }

-- | The status of the Elasticsearch domain being deleted.
--
-- /Note:/ Consider using 'domainStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedrgrsDomainStatus :: Lens.Lens' DeleteElasticsearchDomainResponse (Core.Maybe Types.ElasticsearchDomainStatus)
dedrgrsDomainStatus = Lens.field @"domainStatus"
{-# DEPRECATED dedrgrsDomainStatus "Use generic-lens or generic-optics with 'domainStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedrgrsResponseStatus :: Lens.Lens' DeleteElasticsearchDomainResponse Core.Int
dedrgrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dedrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
