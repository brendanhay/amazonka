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
    dedsrsDomainStatusList,
    dedsrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DescribeElasticsearchDomains' @ operation. By default, the API returns the status of all Elasticsearch domains.
--
-- /See:/ 'mkDescribeElasticsearchDomains' smart constructor.
newtype DescribeElasticsearchDomains = DescribeElasticsearchDomains'
  { -- | The Elasticsearch domains for which you want information.
    domainNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeElasticsearchDomains' with the minimum fields required to make a request.
--
-- * 'domainNames' - The Elasticsearch domains for which you want information.
mkDescribeElasticsearchDomains ::
  DescribeElasticsearchDomains
mkDescribeElasticsearchDomains =
  DescribeElasticsearchDomains' {domainNames = Lude.mempty}

-- | The Elasticsearch domains for which you want information.
--
-- /Note:/ Consider using 'domainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedDomainNames :: Lens.Lens' DescribeElasticsearchDomains [Lude.Text]
dedDomainNames = Lens.lens (domainNames :: DescribeElasticsearchDomains -> [Lude.Text]) (\s a -> s {domainNames = a} :: DescribeElasticsearchDomains)
{-# DEPRECATED dedDomainNames "Use generic-lens or generic-optics with 'domainNames' instead." #-}

instance Lude.AWSRequest DescribeElasticsearchDomains where
  type
    Rs DescribeElasticsearchDomains =
      DescribeElasticsearchDomainsResponse
  request = Req.postJSON elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeElasticsearchDomainsResponse'
            Lude.<$> (x Lude..?> "DomainStatusList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeElasticsearchDomains where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DescribeElasticsearchDomains where
  toJSON DescribeElasticsearchDomains' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DomainNames" Lude..= domainNames)])

instance Lude.ToPath DescribeElasticsearchDomains where
  toPath = Lude.const "/2015-01-01/es/domain-info"

instance Lude.ToQuery DescribeElasticsearchDomains where
  toQuery = Lude.const Lude.mempty

-- | The result of a @DescribeElasticsearchDomains@ request. Contains the status of the specified domains or all domains owned by the account.
--
-- /See:/ 'mkDescribeElasticsearchDomainsResponse' smart constructor.
data DescribeElasticsearchDomainsResponse = DescribeElasticsearchDomainsResponse'
  { -- | The status of the domains requested in the @DescribeElasticsearchDomains@ request.
    domainStatusList :: [ElasticsearchDomainStatus],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeElasticsearchDomainsResponse' with the minimum fields required to make a request.
--
-- * 'domainStatusList' - The status of the domains requested in the @DescribeElasticsearchDomains@ request.
-- * 'responseStatus' - The response status code.
mkDescribeElasticsearchDomainsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeElasticsearchDomainsResponse
mkDescribeElasticsearchDomainsResponse pResponseStatus_ =
  DescribeElasticsearchDomainsResponse'
    { domainStatusList =
        Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | The status of the domains requested in the @DescribeElasticsearchDomains@ request.
--
-- /Note:/ Consider using 'domainStatusList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedsrsDomainStatusList :: Lens.Lens' DescribeElasticsearchDomainsResponse [ElasticsearchDomainStatus]
dedsrsDomainStatusList = Lens.lens (domainStatusList :: DescribeElasticsearchDomainsResponse -> [ElasticsearchDomainStatus]) (\s a -> s {domainStatusList = a} :: DescribeElasticsearchDomainsResponse)
{-# DEPRECATED dedsrsDomainStatusList "Use generic-lens or generic-optics with 'domainStatusList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedsrsResponseStatus :: Lens.Lens' DescribeElasticsearchDomainsResponse Lude.Int
dedsrsResponseStatus = Lens.lens (responseStatus :: DescribeElasticsearchDomainsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeElasticsearchDomainsResponse)
{-# DEPRECATED dedsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
