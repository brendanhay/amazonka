{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dedrsResponseStatus,
    dedrsDomainStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DescribeElasticsearchDomain' @ operation.
--
-- /See:/ 'mkDescribeElasticsearchDomain' smart constructor.
newtype DescribeElasticsearchDomain = DescribeElasticsearchDomain'
  { domainName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeElasticsearchDomain' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the Elasticsearch domain for which you want information.
mkDescribeElasticsearchDomain ::
  -- | 'domainName'
  Lude.Text ->
  DescribeElasticsearchDomain
mkDescribeElasticsearchDomain pDomainName_ =
  DescribeElasticsearchDomain' {domainName = pDomainName_}

-- | The name of the Elasticsearch domain for which you want information.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedDomainName :: Lens.Lens' DescribeElasticsearchDomain Lude.Text
dedDomainName = Lens.lens (domainName :: DescribeElasticsearchDomain -> Lude.Text) (\s a -> s {domainName = a} :: DescribeElasticsearchDomain)
{-# DEPRECATED dedDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DescribeElasticsearchDomain where
  type
    Rs DescribeElasticsearchDomain =
      DescribeElasticsearchDomainResponse
  request = Req.get elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeElasticsearchDomainResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "DomainStatus")
      )

instance Lude.ToHeaders DescribeElasticsearchDomain where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeElasticsearchDomain where
  toPath DescribeElasticsearchDomain' {..} =
    Lude.mconcat ["/2015-01-01/es/domain/", Lude.toBS domainName]

instance Lude.ToQuery DescribeElasticsearchDomain where
  toQuery = Lude.const Lude.mempty

-- | The result of a @DescribeElasticsearchDomain@ request. Contains the status of the domain specified in the request.
--
-- /See:/ 'mkDescribeElasticsearchDomainResponse' smart constructor.
data DescribeElasticsearchDomainResponse = DescribeElasticsearchDomainResponse'
  { responseStatus ::
      Lude.Int,
    domainStatus ::
      ElasticsearchDomainStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeElasticsearchDomainResponse' with the minimum fields required to make a request.
--
-- * 'domainStatus' - The current status of the Elasticsearch domain.
-- * 'responseStatus' - The response status code.
mkDescribeElasticsearchDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'domainStatus'
  ElasticsearchDomainStatus ->
  DescribeElasticsearchDomainResponse
mkDescribeElasticsearchDomainResponse
  pResponseStatus_
  pDomainStatus_ =
    DescribeElasticsearchDomainResponse'
      { responseStatus =
          pResponseStatus_,
        domainStatus = pDomainStatus_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedrsResponseStatus :: Lens.Lens' DescribeElasticsearchDomainResponse Lude.Int
dedrsResponseStatus = Lens.lens (responseStatus :: DescribeElasticsearchDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeElasticsearchDomainResponse)
{-# DEPRECATED dedrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The current status of the Elasticsearch domain.
--
-- /Note:/ Consider using 'domainStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedrsDomainStatus :: Lens.Lens' DescribeElasticsearchDomainResponse ElasticsearchDomainStatus
dedrsDomainStatus = Lens.lens (domainStatus :: DescribeElasticsearchDomainResponse -> ElasticsearchDomainStatus) (\s a -> s {domainStatus = a} :: DescribeElasticsearchDomainResponse)
{-# DEPRECATED dedrsDomainStatus "Use generic-lens or generic-optics with 'domainStatus' instead." #-}
