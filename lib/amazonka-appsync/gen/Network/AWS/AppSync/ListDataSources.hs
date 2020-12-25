{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.ListDataSources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the data sources for a given API.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListDataSources
  ( -- * Creating a request
    ListDataSources (..),
    mkListDataSources,

    -- ** Request lenses
    ldsApiId,
    ldsMaxResults,
    ldsNextToken,

    -- * Destructuring the response
    ListDataSourcesResponse (..),
    mkListDataSourcesResponse,

    -- ** Response lenses
    ldsrrsDataSources,
    ldsrrsNextToken,
    ldsrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDataSources' smart constructor.
data ListDataSources = ListDataSources'
  { -- | The API ID.
    apiId :: Types.String,
    -- | The maximum number of results you want the request to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDataSources' value with any optional fields omitted.
mkListDataSources ::
  -- | 'apiId'
  Types.String ->
  ListDataSources
mkListDataSources apiId =
  ListDataSources'
    { apiId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsApiId :: Lens.Lens' ListDataSources Types.String
ldsApiId = Lens.field @"apiId"
{-# DEPRECATED ldsApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The maximum number of results you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsMaxResults :: Lens.Lens' ListDataSources (Core.Maybe Core.Natural)
ldsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ldsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsNextToken :: Lens.Lens' ListDataSources (Core.Maybe Types.PaginationToken)
ldsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListDataSources where
  type Rs ListDataSources = ListDataSourcesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/datasources")),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataSourcesResponse'
            Core.<$> (x Core..:? "dataSources")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDataSources where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"dataSources" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListDataSourcesResponse' smart constructor.
data ListDataSourcesResponse = ListDataSourcesResponse'
  { -- | The @DataSource@ objects.
    dataSources :: Core.Maybe [Types.DataSource],
    -- | An identifier to be passed in the next request to this operation to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDataSourcesResponse' value with any optional fields omitted.
mkListDataSourcesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDataSourcesResponse
mkListDataSourcesResponse responseStatus =
  ListDataSourcesResponse'
    { dataSources = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The @DataSource@ objects.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsrrsDataSources :: Lens.Lens' ListDataSourcesResponse (Core.Maybe [Types.DataSource])
ldsrrsDataSources = Lens.field @"dataSources"
{-# DEPRECATED ldsrrsDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

-- | An identifier to be passed in the next request to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsrrsNextToken :: Lens.Lens' ListDataSourcesResponse (Core.Maybe Types.PaginationToken)
ldsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsrrsResponseStatus :: Lens.Lens' ListDataSourcesResponse Core.Int
ldsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
