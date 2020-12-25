{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    leitElasticsearchVersion,
    leitDomainName,
    leitMaxResults,
    leitNextToken,

    -- * Destructuring the response
    ListElasticsearchInstanceTypesResponse (..),
    mkListElasticsearchInstanceTypesResponse,

    -- ** Response lenses
    leitrrsElasticsearchInstanceTypes,
    leitrrsNextToken,
    leitrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'ListElasticsearchInstanceTypes' @ operation.
--
-- /See:/ 'mkListElasticsearchInstanceTypes' smart constructor.
data ListElasticsearchInstanceTypes = ListElasticsearchInstanceTypes'
  { -- | Version of Elasticsearch for which list of supported elasticsearch instance types are needed.
    elasticsearchVersion :: Types.ElasticsearchVersion,
    -- | DomainName represents the name of the Domain that we are trying to modify. This should be present only if we are querying for list of available Elasticsearch instance types when modifying existing domain.
    domainName :: Core.Maybe Types.DomainName,
    -- | Set this value to limit the number of results returned. Value provided must be greater than 30 else it wont be honored.
    maxResults :: Core.Maybe Core.Int,
    -- | NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListElasticsearchInstanceTypes' value with any optional fields omitted.
mkListElasticsearchInstanceTypes ::
  -- | 'elasticsearchVersion'
  Types.ElasticsearchVersion ->
  ListElasticsearchInstanceTypes
mkListElasticsearchInstanceTypes elasticsearchVersion =
  ListElasticsearchInstanceTypes'
    { elasticsearchVersion,
      domainName = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Version of Elasticsearch for which list of supported elasticsearch instance types are needed.
--
-- /Note:/ Consider using 'elasticsearchVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leitElasticsearchVersion :: Lens.Lens' ListElasticsearchInstanceTypes Types.ElasticsearchVersion
leitElasticsearchVersion = Lens.field @"elasticsearchVersion"
{-# DEPRECATED leitElasticsearchVersion "Use generic-lens or generic-optics with 'elasticsearchVersion' instead." #-}

-- | DomainName represents the name of the Domain that we are trying to modify. This should be present only if we are querying for list of available Elasticsearch instance types when modifying existing domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leitDomainName :: Lens.Lens' ListElasticsearchInstanceTypes (Core.Maybe Types.DomainName)
leitDomainName = Lens.field @"domainName"
{-# DEPRECATED leitDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Set this value to limit the number of results returned. Value provided must be greater than 30 else it wont be honored.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leitMaxResults :: Lens.Lens' ListElasticsearchInstanceTypes (Core.Maybe Core.Int)
leitMaxResults = Lens.field @"maxResults"
{-# DEPRECATED leitMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leitNextToken :: Lens.Lens' ListElasticsearchInstanceTypes (Core.Maybe Types.NextToken)
leitNextToken = Lens.field @"nextToken"
{-# DEPRECATED leitNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListElasticsearchInstanceTypes where
  type
    Rs ListElasticsearchInstanceTypes =
      ListElasticsearchInstanceTypesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2015-01-01/es/instanceTypes/"
                Core.<> (Core.toText elasticsearchVersion)
            ),
        Core._rqQuery =
          Core.toQueryValue "domainName" Core.<$> domainName
            Core.<> (Core.toQueryValue "maxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListElasticsearchInstanceTypesResponse'
            Core.<$> (x Core..:? "ElasticsearchInstanceTypes")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListElasticsearchInstanceTypes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"elasticsearchInstanceTypes" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Container for the parameters returned by @'ListElasticsearchInstanceTypes' @ operation.
--
-- /See:/ 'mkListElasticsearchInstanceTypesResponse' smart constructor.
data ListElasticsearchInstanceTypesResponse = ListElasticsearchInstanceTypesResponse'
  { -- | List of instance types supported by Amazon Elasticsearch service for given @'ElasticsearchVersion' @
    elasticsearchInstanceTypes :: Core.Maybe [Types.ESPartitionInstanceType],
    -- | In case if there are more results available NextToken would be present, make further request to the same API with received NextToken to paginate remaining results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListElasticsearchInstanceTypesResponse' value with any optional fields omitted.
mkListElasticsearchInstanceTypesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListElasticsearchInstanceTypesResponse
mkListElasticsearchInstanceTypesResponse responseStatus =
  ListElasticsearchInstanceTypesResponse'
    { elasticsearchInstanceTypes =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | List of instance types supported by Amazon Elasticsearch service for given @'ElasticsearchVersion' @
--
-- /Note:/ Consider using 'elasticsearchInstanceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leitrrsElasticsearchInstanceTypes :: Lens.Lens' ListElasticsearchInstanceTypesResponse (Core.Maybe [Types.ESPartitionInstanceType])
leitrrsElasticsearchInstanceTypes = Lens.field @"elasticsearchInstanceTypes"
{-# DEPRECATED leitrrsElasticsearchInstanceTypes "Use generic-lens or generic-optics with 'elasticsearchInstanceTypes' instead." #-}

-- | In case if there are more results available NextToken would be present, make further request to the same API with received NextToken to paginate remaining results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leitrrsNextToken :: Lens.Lens' ListElasticsearchInstanceTypesResponse (Core.Maybe Types.NextToken)
leitrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED leitrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leitrrsResponseStatus :: Lens.Lens' ListElasticsearchInstanceTypesResponse Core.Int
leitrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED leitrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
