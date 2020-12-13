{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DescribeElasticsearchDomainConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides cluster configuration information about the specified Elasticsearch domain, such as the state, creation date, update version, and update date for cluster options.
module Network.AWS.ElasticSearch.DescribeElasticsearchDomainConfig
  ( -- * Creating a request
    DescribeElasticsearchDomainConfig (..),
    mkDescribeElasticsearchDomainConfig,

    -- ** Request lenses
    dedcDomainName,

    -- * Destructuring the response
    DescribeElasticsearchDomainConfigResponse (..),
    mkDescribeElasticsearchDomainConfigResponse,

    -- ** Response lenses
    dedcrsDomainConfig,
    dedcrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @DescribeElasticsearchDomainConfig@ operation. Specifies the domain name for which you want configuration information.
--
-- /See:/ 'mkDescribeElasticsearchDomainConfig' smart constructor.
newtype DescribeElasticsearchDomainConfig = DescribeElasticsearchDomainConfig'
  { -- | The Elasticsearch domain that you want to get information about.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeElasticsearchDomainConfig' with the minimum fields required to make a request.
--
-- * 'domainName' - The Elasticsearch domain that you want to get information about.
mkDescribeElasticsearchDomainConfig ::
  -- | 'domainName'
  Lude.Text ->
  DescribeElasticsearchDomainConfig
mkDescribeElasticsearchDomainConfig pDomainName_ =
  DescribeElasticsearchDomainConfig' {domainName = pDomainName_}

-- | The Elasticsearch domain that you want to get information about.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcDomainName :: Lens.Lens' DescribeElasticsearchDomainConfig Lude.Text
dedcDomainName = Lens.lens (domainName :: DescribeElasticsearchDomainConfig -> Lude.Text) (\s a -> s {domainName = a} :: DescribeElasticsearchDomainConfig)
{-# DEPRECATED dedcDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DescribeElasticsearchDomainConfig where
  type
    Rs DescribeElasticsearchDomainConfig =
      DescribeElasticsearchDomainConfigResponse
  request = Req.get elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeElasticsearchDomainConfigResponse'
            Lude.<$> (x Lude..:> "DomainConfig") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeElasticsearchDomainConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeElasticsearchDomainConfig where
  toPath DescribeElasticsearchDomainConfig' {..} =
    Lude.mconcat
      ["/2015-01-01/es/domain/", Lude.toBS domainName, "/config"]

instance Lude.ToQuery DescribeElasticsearchDomainConfig where
  toQuery = Lude.const Lude.mempty

-- | The result of a @DescribeElasticsearchDomainConfig@ request. Contains the configuration information of the requested domain.
--
-- /See:/ 'mkDescribeElasticsearchDomainConfigResponse' smart constructor.
data DescribeElasticsearchDomainConfigResponse = DescribeElasticsearchDomainConfigResponse'
  { -- | The configuration information of the domain requested in the @DescribeElasticsearchDomainConfig@ request.
    domainConfig :: ElasticsearchDomainConfig,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeElasticsearchDomainConfigResponse' with the minimum fields required to make a request.
--
-- * 'domainConfig' - The configuration information of the domain requested in the @DescribeElasticsearchDomainConfig@ request.
-- * 'responseStatus' - The response status code.
mkDescribeElasticsearchDomainConfigResponse ::
  -- | 'domainConfig'
  ElasticsearchDomainConfig ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeElasticsearchDomainConfigResponse
mkDescribeElasticsearchDomainConfigResponse
  pDomainConfig_
  pResponseStatus_ =
    DescribeElasticsearchDomainConfigResponse'
      { domainConfig =
          pDomainConfig_,
        responseStatus = pResponseStatus_
      }

-- | The configuration information of the domain requested in the @DescribeElasticsearchDomainConfig@ request.
--
-- /Note:/ Consider using 'domainConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcrsDomainConfig :: Lens.Lens' DescribeElasticsearchDomainConfigResponse ElasticsearchDomainConfig
dedcrsDomainConfig = Lens.lens (domainConfig :: DescribeElasticsearchDomainConfigResponse -> ElasticsearchDomainConfig) (\s a -> s {domainConfig = a} :: DescribeElasticsearchDomainConfigResponse)
{-# DEPRECATED dedcrsDomainConfig "Use generic-lens or generic-optics with 'domainConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcrsResponseStatus :: Lens.Lens' DescribeElasticsearchDomainConfigResponse Lude.Int
dedcrsResponseStatus = Lens.lens (responseStatus :: DescribeElasticsearchDomainConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeElasticsearchDomainConfigResponse)
{-# DEPRECATED dedcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
