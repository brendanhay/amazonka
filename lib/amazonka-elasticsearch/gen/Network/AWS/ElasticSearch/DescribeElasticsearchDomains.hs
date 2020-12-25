{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DescribeElasticsearchDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns domain configuration information about the specified Elasticsearch domains, including the domain ID, domain endpoint, and domain ARN.
module Network.AWS.ElasticSearch.DescribeElasticsearchDomains
  ( -- * Creating a request
    DescribeElasticsearchDomains (..),
    mkDescribeElasticsearchDomains,

    -- ** Request lenses
    dedDomainNames,

    -- * Destructuring the response
    DescribeElasticsearchDomainsResponse (..),
    mkDescribeElasticsearchDomainsResponse,

    -- ** Response lenses
    dedrfrsDomainStatusList,
    dedrfrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DescribeElasticsearchDomains' @ operation. By default, the API returns the status of all Elasticsearch domains.
--
-- /See:/ 'mkDescribeElasticsearchDomains' smart constructor.
newtype DescribeElasticsearchDomains = DescribeElasticsearchDomains'
  { -- | The Elasticsearch domains for which you want information.
    domainNames :: [Types.DomainName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeElasticsearchDomains' value with any optional fields omitted.
mkDescribeElasticsearchDomains ::
  DescribeElasticsearchDomains
mkDescribeElasticsearchDomains =
  DescribeElasticsearchDomains' {domainNames = Core.mempty}

-- | The Elasticsearch domains for which you want information.
--
-- /Note:/ Consider using 'domainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedDomainNames :: Lens.Lens' DescribeElasticsearchDomains [Types.DomainName]
dedDomainNames = Lens.field @"domainNames"
{-# DEPRECATED dedDomainNames "Use generic-lens or generic-optics with 'domainNames' instead." #-}

instance Core.FromJSON DescribeElasticsearchDomains where
  toJSON DescribeElasticsearchDomains {..} =
    Core.object
      (Core.catMaybes [Core.Just ("DomainNames" Core..= domainNames)])

instance Core.AWSRequest DescribeElasticsearchDomains where
  type
    Rs DescribeElasticsearchDomains =
      DescribeElasticsearchDomainsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2015-01-01/es/domain-info",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeElasticsearchDomainsResponse'
            Core.<$> (x Core..:? "DomainStatusList" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @DescribeElasticsearchDomains@ request. Contains the status of the specified domains or all domains owned by the account.
--
-- /See:/ 'mkDescribeElasticsearchDomainsResponse' smart constructor.
data DescribeElasticsearchDomainsResponse = DescribeElasticsearchDomainsResponse'
  { -- | The status of the domains requested in the @DescribeElasticsearchDomains@ request.
    domainStatusList :: [Types.ElasticsearchDomainStatus],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeElasticsearchDomainsResponse' value with any optional fields omitted.
mkDescribeElasticsearchDomainsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeElasticsearchDomainsResponse
mkDescribeElasticsearchDomainsResponse responseStatus =
  DescribeElasticsearchDomainsResponse'
    { domainStatusList =
        Core.mempty,
      responseStatus
    }

-- | The status of the domains requested in the @DescribeElasticsearchDomains@ request.
--
-- /Note:/ Consider using 'domainStatusList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedrfrsDomainStatusList :: Lens.Lens' DescribeElasticsearchDomainsResponse [Types.ElasticsearchDomainStatus]
dedrfrsDomainStatusList = Lens.field @"domainStatusList"
{-# DEPRECATED dedrfrsDomainStatusList "Use generic-lens or generic-optics with 'domainStatusList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedrfrsResponseStatus :: Lens.Lens' DescribeElasticsearchDomainsResponse Core.Int
dedrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dedrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
