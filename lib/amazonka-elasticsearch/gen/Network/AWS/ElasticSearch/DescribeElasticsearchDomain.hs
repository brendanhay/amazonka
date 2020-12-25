{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DescribeElasticsearchDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns domain configuration information about the specified Elasticsearch domain, including the domain ID, domain endpoint, and domain ARN.
module Network.AWS.ElasticSearch.DescribeElasticsearchDomain
  ( -- * Creating a request
    DescribeElasticsearchDomain (..),
    mkDescribeElasticsearchDomain,

    -- ** Request lenses
    dedDomainName,

    -- * Destructuring the response
    DescribeElasticsearchDomainResponse (..),
    mkDescribeElasticsearchDomainResponse,

    -- ** Response lenses
    dedrrsDomainStatus,
    dedrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DescribeElasticsearchDomain' @ operation.
--
-- /See:/ 'mkDescribeElasticsearchDomain' smart constructor.
newtype DescribeElasticsearchDomain = DescribeElasticsearchDomain'
  { -- | The name of the Elasticsearch domain for which you want information.
    domainName :: Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeElasticsearchDomain' value with any optional fields omitted.
mkDescribeElasticsearchDomain ::
  -- | 'domainName'
  Types.DomainName ->
  DescribeElasticsearchDomain
mkDescribeElasticsearchDomain domainName =
  DescribeElasticsearchDomain' {domainName}

-- | The name of the Elasticsearch domain for which you want information.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedDomainName :: Lens.Lens' DescribeElasticsearchDomain Types.DomainName
dedDomainName = Lens.field @"domainName"
{-# DEPRECATED dedDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.AWSRequest DescribeElasticsearchDomain where
  type
    Rs DescribeElasticsearchDomain =
      DescribeElasticsearchDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
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
          DescribeElasticsearchDomainResponse'
            Core.<$> (x Core..: "DomainStatus") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @DescribeElasticsearchDomain@ request. Contains the status of the domain specified in the request.
--
-- /See:/ 'mkDescribeElasticsearchDomainResponse' smart constructor.
data DescribeElasticsearchDomainResponse = DescribeElasticsearchDomainResponse'
  { -- | The current status of the Elasticsearch domain.
    domainStatus :: Types.ElasticsearchDomainStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeElasticsearchDomainResponse' value with any optional fields omitted.
mkDescribeElasticsearchDomainResponse ::
  -- | 'domainStatus'
  Types.ElasticsearchDomainStatus ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeElasticsearchDomainResponse
mkDescribeElasticsearchDomainResponse domainStatus responseStatus =
  DescribeElasticsearchDomainResponse'
    { domainStatus,
      responseStatus
    }

-- | The current status of the Elasticsearch domain.
--
-- /Note:/ Consider using 'domainStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedrrsDomainStatus :: Lens.Lens' DescribeElasticsearchDomainResponse Types.ElasticsearchDomainStatus
dedrrsDomainStatus = Lens.field @"domainStatus"
{-# DEPRECATED dedrrsDomainStatus "Use generic-lens or generic-optics with 'domainStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedrrsResponseStatus :: Lens.Lens' DescribeElasticsearchDomainResponse Core.Int
dedrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dedrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
