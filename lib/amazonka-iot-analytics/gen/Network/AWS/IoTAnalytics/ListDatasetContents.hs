{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.ListDatasetContents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about data set contents that have been created.
--
-- This operation returns paginated results.
module Network.AWS.IoTAnalytics.ListDatasetContents
    (
    -- * Creating a request
      ListDatasetContents (..)
    , mkListDatasetContents
    -- ** Request lenses
    , ldcDatasetName
    , ldcMaxResults
    , ldcNextToken
    , ldcScheduledBefore
    , ldcScheduledOnOrAfter

    -- * Destructuring the response
    , ListDatasetContentsResponse (..)
    , mkListDatasetContentsResponse
    -- ** Response lenses
    , ldcrrsDatasetContentSummaries
    , ldcrrsNextToken
    , ldcrrsResponseStatus
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDatasetContents' smart constructor.
data ListDatasetContents = ListDatasetContents'
  { datasetName :: Types.DatasetName
    -- ^ The name of the data set whose contents information you want to list.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in this request.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results.
  , scheduledBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter to limit results to those data set contents whose creation is scheduled before the given time. See the field @triggers.schedule@ in the @CreateDataset@ request. (timestamp)
  , scheduledOnOrAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter to limit results to those data set contents whose creation is scheduled on or after the given time. See the field @triggers.schedule@ in the @CreateDataset@ request. (timestamp)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListDatasetContents' value with any optional fields omitted.
mkListDatasetContents
    :: Types.DatasetName -- ^ 'datasetName'
    -> ListDatasetContents
mkListDatasetContents datasetName
  = ListDatasetContents'{datasetName, maxResults = Core.Nothing,
                         nextToken = Core.Nothing, scheduledBefore = Core.Nothing,
                         scheduledOnOrAfter = Core.Nothing}

-- | The name of the data set whose contents information you want to list.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcDatasetName :: Lens.Lens' ListDatasetContents Types.DatasetName
ldcDatasetName = Lens.field @"datasetName"
{-# INLINEABLE ldcDatasetName #-}
{-# DEPRECATED datasetName "Use generic-lens or generic-optics with 'datasetName' instead"  #-}

-- | The maximum number of results to return in this request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcMaxResults :: Lens.Lens' ListDatasetContents (Core.Maybe Core.Natural)
ldcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ldcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcNextToken :: Lens.Lens' ListDatasetContents (Core.Maybe Types.NextToken)
ldcNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A filter to limit results to those data set contents whose creation is scheduled before the given time. See the field @triggers.schedule@ in the @CreateDataset@ request. (timestamp)
--
-- /Note:/ Consider using 'scheduledBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcScheduledBefore :: Lens.Lens' ListDatasetContents (Core.Maybe Core.NominalDiffTime)
ldcScheduledBefore = Lens.field @"scheduledBefore"
{-# INLINEABLE ldcScheduledBefore #-}
{-# DEPRECATED scheduledBefore "Use generic-lens or generic-optics with 'scheduledBefore' instead"  #-}

-- | A filter to limit results to those data set contents whose creation is scheduled on or after the given time. See the field @triggers.schedule@ in the @CreateDataset@ request. (timestamp)
--
-- /Note:/ Consider using 'scheduledOnOrAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcScheduledOnOrAfter :: Lens.Lens' ListDatasetContents (Core.Maybe Core.NominalDiffTime)
ldcScheduledOnOrAfter = Lens.field @"scheduledOnOrAfter"
{-# INLINEABLE ldcScheduledOnOrAfter #-}
{-# DEPRECATED scheduledOnOrAfter "Use generic-lens or generic-optics with 'scheduledOnOrAfter' instead"  #-}

instance Core.ToQuery ListDatasetContents where
        toQuery ListDatasetContents{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "scheduledBefore")
                scheduledBefore
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "scheduledOnOrAfter")
                scheduledOnOrAfter

instance Core.ToHeaders ListDatasetContents where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListDatasetContents where
        type Rs ListDatasetContents = ListDatasetContentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/datasets/" Core.<> Core.toText datasetName Core.<> "/contents",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDatasetContentsResponse' Core.<$>
                   (x Core..:? "datasetContentSummaries") Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListDatasetContents where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"datasetContentSummaries" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListDatasetContentsResponse' smart constructor.
data ListDatasetContentsResponse = ListDatasetContentsResponse'
  { datasetContentSummaries :: Core.Maybe [Types.DatasetContentSummary]
    -- ^ Summary information about data set contents that have been created.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to retrieve the next set of results, or @null@ if there are no more results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListDatasetContentsResponse' value with any optional fields omitted.
mkListDatasetContentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDatasetContentsResponse
mkListDatasetContentsResponse responseStatus
  = ListDatasetContentsResponse'{datasetContentSummaries =
                                   Core.Nothing,
                                 nextToken = Core.Nothing, responseStatus}

-- | Summary information about data set contents that have been created.
--
-- /Note:/ Consider using 'datasetContentSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsDatasetContentSummaries :: Lens.Lens' ListDatasetContentsResponse (Core.Maybe [Types.DatasetContentSummary])
ldcrrsDatasetContentSummaries = Lens.field @"datasetContentSummaries"
{-# INLINEABLE ldcrrsDatasetContentSummaries #-}
{-# DEPRECATED datasetContentSummaries "Use generic-lens or generic-optics with 'datasetContentSummaries' instead"  #-}

-- | The token to retrieve the next set of results, or @null@ if there are no more results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsNextToken :: Lens.Lens' ListDatasetContentsResponse (Core.Maybe Types.NextToken)
ldcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsResponseStatus :: Lens.Lens' ListDatasetContentsResponse Core.Int
ldcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
