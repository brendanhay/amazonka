{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe Elasticsearch Limits for a given InstanceType and ElasticsearchVersion. When modifying existing Domain, specify the @'DomainName' @ to know what Limits are supported for modifying.
module Network.AWS.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
  ( -- * Creating a request
    DescribeElasticsearchInstanceTypeLimits (..),
    mkDescribeElasticsearchInstanceTypeLimits,

    -- ** Request lenses
    deitlDomainName,
    deitlInstanceType,
    deitlElasticsearchVersion,

    -- * Destructuring the response
    DescribeElasticsearchInstanceTypeLimitsResponse (..),
    mkDescribeElasticsearchInstanceTypeLimitsResponse,

    -- ** Response lenses
    deitlrsLimitsByRole,
    deitlrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to @'DescribeElasticsearchInstanceTypeLimits' @ operation.
--
-- /See:/ 'mkDescribeElasticsearchInstanceTypeLimits' smart constructor.
data DescribeElasticsearchInstanceTypeLimits = DescribeElasticsearchInstanceTypeLimits'
  { domainName ::
      Lude.Maybe
        Lude.Text,
    instanceType ::
      ESPartitionInstanceType,
    elasticsearchVersion ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeElasticsearchInstanceTypeLimits' with the minimum fields required to make a request.
--
-- * 'domainName' - DomainName represents the name of the Domain that we are trying to modify. This should be present only if we are querying for Elasticsearch @'Limits' @ for existing domain.
-- * 'elasticsearchVersion' - Version of Elasticsearch for which @'Limits' @ are needed.
-- * 'instanceType' - The instance type for an Elasticsearch cluster for which Elasticsearch @'Limits' @ are needed.
mkDescribeElasticsearchInstanceTypeLimits ::
  -- | 'instanceType'
  ESPartitionInstanceType ->
  -- | 'elasticsearchVersion'
  Lude.Text ->
  DescribeElasticsearchInstanceTypeLimits
mkDescribeElasticsearchInstanceTypeLimits
  pInstanceType_
  pElasticsearchVersion_ =
    DescribeElasticsearchInstanceTypeLimits'
      { domainName =
          Lude.Nothing,
        instanceType = pInstanceType_,
        elasticsearchVersion = pElasticsearchVersion_
      }

-- | DomainName represents the name of the Domain that we are trying to modify. This should be present only if we are querying for Elasticsearch @'Limits' @ for existing domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitlDomainName :: Lens.Lens' DescribeElasticsearchInstanceTypeLimits (Lude.Maybe Lude.Text)
deitlDomainName = Lens.lens (domainName :: DescribeElasticsearchInstanceTypeLimits -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: DescribeElasticsearchInstanceTypeLimits)
{-# DEPRECATED deitlDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The instance type for an Elasticsearch cluster for which Elasticsearch @'Limits' @ are needed.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitlInstanceType :: Lens.Lens' DescribeElasticsearchInstanceTypeLimits ESPartitionInstanceType
deitlInstanceType = Lens.lens (instanceType :: DescribeElasticsearchInstanceTypeLimits -> ESPartitionInstanceType) (\s a -> s {instanceType = a} :: DescribeElasticsearchInstanceTypeLimits)
{-# DEPRECATED deitlInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Version of Elasticsearch for which @'Limits' @ are needed.
--
-- /Note:/ Consider using 'elasticsearchVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitlElasticsearchVersion :: Lens.Lens' DescribeElasticsearchInstanceTypeLimits Lude.Text
deitlElasticsearchVersion = Lens.lens (elasticsearchVersion :: DescribeElasticsearchInstanceTypeLimits -> Lude.Text) (\s a -> s {elasticsearchVersion = a} :: DescribeElasticsearchInstanceTypeLimits)
{-# DEPRECATED deitlElasticsearchVersion "Use generic-lens or generic-optics with 'elasticsearchVersion' instead." #-}

instance Lude.AWSRequest DescribeElasticsearchInstanceTypeLimits where
  type
    Rs DescribeElasticsearchInstanceTypeLimits =
      DescribeElasticsearchInstanceTypeLimitsResponse
  request = Req.get elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeElasticsearchInstanceTypeLimitsResponse'
            Lude.<$> (x Lude..?> "LimitsByRole" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeElasticsearchInstanceTypeLimits where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeElasticsearchInstanceTypeLimits where
  toPath DescribeElasticsearchInstanceTypeLimits' {..} =
    Lude.mconcat
      [ "/2015-01-01/es/instanceTypeLimits/",
        Lude.toBS elasticsearchVersion,
        "/",
        Lude.toBS instanceType
      ]

instance Lude.ToQuery DescribeElasticsearchInstanceTypeLimits where
  toQuery DescribeElasticsearchInstanceTypeLimits' {..} =
    Lude.mconcat ["domainName" Lude.=: domainName]

-- | Container for the parameters received from @'DescribeElasticsearchInstanceTypeLimits' @ operation.
--
-- /See:/ 'mkDescribeElasticsearchInstanceTypeLimitsResponse' smart constructor.
data DescribeElasticsearchInstanceTypeLimitsResponse = DescribeElasticsearchInstanceTypeLimitsResponse'
  { limitsByRole ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Limits)
        ),
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DescribeElasticsearchInstanceTypeLimitsResponse' with the minimum fields required to make a request.
--
-- * 'limitsByRole' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeElasticsearchInstanceTypeLimitsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeElasticsearchInstanceTypeLimitsResponse
mkDescribeElasticsearchInstanceTypeLimitsResponse pResponseStatus_ =
  DescribeElasticsearchInstanceTypeLimitsResponse'
    { limitsByRole =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'limitsByRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitlrsLimitsByRole :: Lens.Lens' DescribeElasticsearchInstanceTypeLimitsResponse (Lude.Maybe (Lude.HashMap Lude.Text (Limits)))
deitlrsLimitsByRole = Lens.lens (limitsByRole :: DescribeElasticsearchInstanceTypeLimitsResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Limits))) (\s a -> s {limitsByRole = a} :: DescribeElasticsearchInstanceTypeLimitsResponse)
{-# DEPRECATED deitlrsLimitsByRole "Use generic-lens or generic-optics with 'limitsByRole' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitlrsResponseStatus :: Lens.Lens' DescribeElasticsearchInstanceTypeLimitsResponse Lude.Int
deitlrsResponseStatus = Lens.lens (responseStatus :: DescribeElasticsearchInstanceTypeLimitsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeElasticsearchInstanceTypeLimitsResponse)
{-# DEPRECATED deitlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
