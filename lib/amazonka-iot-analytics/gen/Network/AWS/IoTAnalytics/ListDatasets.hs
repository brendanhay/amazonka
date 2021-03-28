{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.ListDatasets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about data sets.
--
-- This operation returns paginated results.
module Network.AWS.IoTAnalytics.ListDatasets
    (
    -- * Creating a request
      ListDatasets (..)
    , mkListDatasets
    -- ** Request lenses
    , lMaxResults
    , lNextToken

    -- * Destructuring the response
    , ListDatasetsResponse (..)
    , mkListDatasetsResponse
    -- ** Response lenses
    , lrsDatasetSummaries
    , lrsNextToken
    , lrsResponseStatus
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDatasets' smart constructor.
data ListDatasets = ListDatasets'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in this request.
--
-- The default value is 100.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDatasets' value with any optional fields omitted.
mkListDatasets
    :: ListDatasets
mkListDatasets
  = ListDatasets'{maxResults = Core.Nothing,
                  nextToken = Core.Nothing}

-- | The maximum number of results to return in this request.
--
-- The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListDatasets (Core.Maybe Core.Natural)
lMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListDatasets (Core.Maybe Types.NextToken)
lNextToken = Lens.field @"nextToken"
{-# INLINEABLE lNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListDatasets where
        toQuery ListDatasets{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListDatasets where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListDatasets where
        type Rs ListDatasets = ListDatasetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/datasets",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDatasetsResponse' Core.<$>
                   (x Core..:? "datasetSummaries") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListDatasets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"datasetSummaries" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListDatasetsResponse' smart constructor.
data ListDatasetsResponse = ListDatasetsResponse'
  { datasetSummaries :: Core.Maybe [Types.DatasetSummary]
    -- ^ A list of @DatasetSummary@ objects.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to retrieve the next set of results, or @null@ if there are no more results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListDatasetsResponse' value with any optional fields omitted.
mkListDatasetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDatasetsResponse
mkListDatasetsResponse responseStatus
  = ListDatasetsResponse'{datasetSummaries = Core.Nothing,
                          nextToken = Core.Nothing, responseStatus}

-- | A list of @DatasetSummary@ objects.
--
-- /Note:/ Consider using 'datasetSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsDatasetSummaries :: Lens.Lens' ListDatasetsResponse (Core.Maybe [Types.DatasetSummary])
lrsDatasetSummaries = Lens.field @"datasetSummaries"
{-# INLINEABLE lrsDatasetSummaries #-}
{-# DEPRECATED datasetSummaries "Use generic-lens or generic-optics with 'datasetSummaries' instead"  #-}

-- | The token to retrieve the next set of results, or @null@ if there are no more results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListDatasetsResponse (Core.Maybe Types.NextToken)
lrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListDatasetsResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
