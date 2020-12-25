{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.ListElasticsearchVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all supported Elasticsearch versions
--
-- This operation returns paginated results.
module Network.AWS.ElasticSearch.ListElasticsearchVersions
  ( -- * Creating a request
    ListElasticsearchVersions (..),
    mkListElasticsearchVersions,

    -- ** Request lenses
    levMaxResults,
    levNextToken,

    -- * Destructuring the response
    ListElasticsearchVersionsResponse (..),
    mkListElasticsearchVersionsResponse,

    -- ** Response lenses
    levrrsElasticsearchVersions,
    levrrsNextToken,
    levrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'ListElasticsearchVersions' @ operation. Use @'MaxResults' @ to control the maximum number of results to retrieve in a single call.
--
-- Use @'NextToken' @ in response to retrieve more results. If the received response does not contain a NextToken, then there are no more results to retrieve.
--
--
-- /See:/ 'mkListElasticsearchVersions' smart constructor.
data ListElasticsearchVersions = ListElasticsearchVersions'
  { -- | Set this value to limit the number of results returned. Value provided must be greater than 10 else it wont be honored.
    maxResults :: Core.Maybe Core.Int,
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListElasticsearchVersions' value with any optional fields omitted.
mkListElasticsearchVersions ::
  ListElasticsearchVersions
mkListElasticsearchVersions =
  ListElasticsearchVersions'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Set this value to limit the number of results returned. Value provided must be greater than 10 else it wont be honored.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
levMaxResults :: Lens.Lens' ListElasticsearchVersions (Core.Maybe Core.Int)
levMaxResults = Lens.field @"maxResults"
{-# DEPRECATED levMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
levNextToken :: Lens.Lens' ListElasticsearchVersions (Core.Maybe Types.NextToken)
levNextToken = Lens.field @"nextToken"
{-# DEPRECATED levNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListElasticsearchVersions where
  type
    Rs ListElasticsearchVersions =
      ListElasticsearchVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2015-01-01/es/versions",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListElasticsearchVersionsResponse'
            Core.<$> (x Core..:? "ElasticsearchVersions")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListElasticsearchVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"elasticsearchVersions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Container for the parameters for response received from @'ListElasticsearchVersions' @ operation.
--
-- /See:/ 'mkListElasticsearchVersionsResponse' smart constructor.
data ListElasticsearchVersionsResponse = ListElasticsearchVersionsResponse'
  { elasticsearchVersions :: Core.Maybe [Types.ElasticsearchVersionString],
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListElasticsearchVersionsResponse' value with any optional fields omitted.
mkListElasticsearchVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListElasticsearchVersionsResponse
mkListElasticsearchVersionsResponse responseStatus =
  ListElasticsearchVersionsResponse'
    { elasticsearchVersions =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'elasticsearchVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
levrrsElasticsearchVersions :: Lens.Lens' ListElasticsearchVersionsResponse (Core.Maybe [Types.ElasticsearchVersionString])
levrrsElasticsearchVersions = Lens.field @"elasticsearchVersions"
{-# DEPRECATED levrrsElasticsearchVersions "Use generic-lens or generic-optics with 'elasticsearchVersions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
levrrsNextToken :: Lens.Lens' ListElasticsearchVersionsResponse (Core.Maybe Types.NextToken)
levrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED levrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
levrrsResponseStatus :: Lens.Lens' ListElasticsearchVersionsResponse Core.Int
levrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED levrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
