{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.ListElasticsearchInstanceTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all Elasticsearch instance types that are supported for given ElasticsearchVersion
--
-- This operation returns paginated results.
module Network.AWS.ElasticSearch.ListElasticsearchInstanceTypes
  ( -- * Creating a request
    ListElasticsearchInstanceTypes (..),
    mkListElasticsearchInstanceTypes,

    -- ** Request lenses
    leitNextToken,
    leitDomainName,
    leitMaxResults,
    leitElasticsearchVersion,

    -- * Destructuring the response
    ListElasticsearchInstanceTypesResponse (..),
    mkListElasticsearchInstanceTypesResponse,

    -- ** Response lenses
    leitrsElasticsearchInstanceTypes,
    leitrsNextToken,
    leitrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'ListElasticsearchInstanceTypes' @ operation.
--
-- /See:/ 'mkListElasticsearchInstanceTypes' smart constructor.
data ListElasticsearchInstanceTypes = ListElasticsearchInstanceTypes'
  { nextToken ::
      Lude.Maybe Lude.Text,
    domainName ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Int,
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

-- | Creates a value of 'ListElasticsearchInstanceTypes' with the minimum fields required to make a request.
--
-- * 'domainName' - DomainName represents the name of the Domain that we are trying to modify. This should be present only if we are querying for list of available Elasticsearch instance types when modifying existing domain.
-- * 'elasticsearchVersion' - Version of Elasticsearch for which list of supported elasticsearch instance types are needed.
-- * 'maxResults' - Set this value to limit the number of results returned. Value provided must be greater than 30 else it wont be honored.
-- * 'nextToken' - NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
mkListElasticsearchInstanceTypes ::
  -- | 'elasticsearchVersion'
  Lude.Text ->
  ListElasticsearchInstanceTypes
mkListElasticsearchInstanceTypes pElasticsearchVersion_ =
  ListElasticsearchInstanceTypes'
    { nextToken = Lude.Nothing,
      domainName = Lude.Nothing,
      maxResults = Lude.Nothing,
      elasticsearchVersion = pElasticsearchVersion_
    }

-- | NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leitNextToken :: Lens.Lens' ListElasticsearchInstanceTypes (Lude.Maybe Lude.Text)
leitNextToken = Lens.lens (nextToken :: ListElasticsearchInstanceTypes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListElasticsearchInstanceTypes)
{-# DEPRECATED leitNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | DomainName represents the name of the Domain that we are trying to modify. This should be present only if we are querying for list of available Elasticsearch instance types when modifying existing domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leitDomainName :: Lens.Lens' ListElasticsearchInstanceTypes (Lude.Maybe Lude.Text)
leitDomainName = Lens.lens (domainName :: ListElasticsearchInstanceTypes -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: ListElasticsearchInstanceTypes)
{-# DEPRECATED leitDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Set this value to limit the number of results returned. Value provided must be greater than 30 else it wont be honored.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leitMaxResults :: Lens.Lens' ListElasticsearchInstanceTypes (Lude.Maybe Lude.Int)
leitMaxResults = Lens.lens (maxResults :: ListElasticsearchInstanceTypes -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListElasticsearchInstanceTypes)
{-# DEPRECATED leitMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Version of Elasticsearch for which list of supported elasticsearch instance types are needed.
--
-- /Note:/ Consider using 'elasticsearchVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leitElasticsearchVersion :: Lens.Lens' ListElasticsearchInstanceTypes Lude.Text
leitElasticsearchVersion = Lens.lens (elasticsearchVersion :: ListElasticsearchInstanceTypes -> Lude.Text) (\s a -> s {elasticsearchVersion = a} :: ListElasticsearchInstanceTypes)
{-# DEPRECATED leitElasticsearchVersion "Use generic-lens or generic-optics with 'elasticsearchVersion' instead." #-}

instance Page.AWSPager ListElasticsearchInstanceTypes where
  page rq rs
    | Page.stop (rs Lens.^. leitrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. leitrsElasticsearchInstanceTypes) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& leitNextToken Lens..~ rs Lens.^. leitrsNextToken

instance Lude.AWSRequest ListElasticsearchInstanceTypes where
  type
    Rs ListElasticsearchInstanceTypes =
      ListElasticsearchInstanceTypesResponse
  request = Req.get elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListElasticsearchInstanceTypesResponse'
            Lude.<$> (x Lude..?> "ElasticsearchInstanceTypes" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListElasticsearchInstanceTypes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListElasticsearchInstanceTypes where
  toPath ListElasticsearchInstanceTypes' {..} =
    Lude.mconcat
      ["/2015-01-01/es/instanceTypes/", Lude.toBS elasticsearchVersion]

instance Lude.ToQuery ListElasticsearchInstanceTypes where
  toQuery ListElasticsearchInstanceTypes' {..} =
    Lude.mconcat
      [ "nextToken" Lude.=: nextToken,
        "domainName" Lude.=: domainName,
        "maxResults" Lude.=: maxResults
      ]

-- | Container for the parameters returned by @'ListElasticsearchInstanceTypes' @ operation.
--
-- /See:/ 'mkListElasticsearchInstanceTypesResponse' smart constructor.
data ListElasticsearchInstanceTypesResponse = ListElasticsearchInstanceTypesResponse'
  { elasticsearchInstanceTypes ::
      Lude.Maybe
        [ESPartitionInstanceType],
    nextToken ::
      Lude.Maybe
        Lude.Text,
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListElasticsearchInstanceTypesResponse' with the minimum fields required to make a request.
--
-- * 'elasticsearchInstanceTypes' - List of instance types supported by Amazon Elasticsearch service for given @'ElasticsearchVersion' @
-- * 'nextToken' - In case if there are more results available NextToken would be present, make further request to the same API with received NextToken to paginate remaining results.
-- * 'responseStatus' - The response status code.
mkListElasticsearchInstanceTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListElasticsearchInstanceTypesResponse
mkListElasticsearchInstanceTypesResponse pResponseStatus_ =
  ListElasticsearchInstanceTypesResponse'
    { elasticsearchInstanceTypes =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of instance types supported by Amazon Elasticsearch service for given @'ElasticsearchVersion' @
--
-- /Note:/ Consider using 'elasticsearchInstanceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leitrsElasticsearchInstanceTypes :: Lens.Lens' ListElasticsearchInstanceTypesResponse (Lude.Maybe [ESPartitionInstanceType])
leitrsElasticsearchInstanceTypes = Lens.lens (elasticsearchInstanceTypes :: ListElasticsearchInstanceTypesResponse -> Lude.Maybe [ESPartitionInstanceType]) (\s a -> s {elasticsearchInstanceTypes = a} :: ListElasticsearchInstanceTypesResponse)
{-# DEPRECATED leitrsElasticsearchInstanceTypes "Use generic-lens or generic-optics with 'elasticsearchInstanceTypes' instead." #-}

-- | In case if there are more results available NextToken would be present, make further request to the same API with received NextToken to paginate remaining results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leitrsNextToken :: Lens.Lens' ListElasticsearchInstanceTypesResponse (Lude.Maybe Lude.Text)
leitrsNextToken = Lens.lens (nextToken :: ListElasticsearchInstanceTypesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListElasticsearchInstanceTypesResponse)
{-# DEPRECATED leitrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leitrsResponseStatus :: Lens.Lens' ListElasticsearchInstanceTypesResponse Lude.Int
leitrsResponseStatus = Lens.lens (responseStatus :: ListElasticsearchInstanceTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListElasticsearchInstanceTypesResponse)
{-# DEPRECATED leitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
