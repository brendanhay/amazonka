{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListDatasetContents (..),
    mkListDatasetContents,

    -- ** Request lenses
    ldcDatasetName,
    ldcMaxResults,
    ldcNextToken,
    ldcScheduledBefore,
    ldcScheduledOnOrAfter,

    -- * Destructuring the response
    ListDatasetContentsResponse (..),
    mkListDatasetContentsResponse,

    -- ** Response lenses
    ldcrrsDatasetContentSummaries,
    ldcrrsNextToken,
    ldcrrsResponseStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDatasetContents' smart constructor.
data ListDatasetContents = ListDatasetContents'
  { -- | The name of the data set whose contents information you want to list.
    datasetName :: Types.DatasetName,
    -- | The maximum number of results to return in this request.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A filter to limit results to those data set contents whose creation is scheduled before the given time. See the field @triggers.schedule@ in the @CreateDataset@ request. (timestamp)
    scheduledBefore :: Core.Maybe Core.NominalDiffTime,
    -- | A filter to limit results to those data set contents whose creation is scheduled on or after the given time. See the field @triggers.schedule@ in the @CreateDataset@ request. (timestamp)
    scheduledOnOrAfter :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDatasetContents' value with any optional fields omitted.
mkListDatasetContents ::
  -- | 'datasetName'
  Types.DatasetName ->
  ListDatasetContents
mkListDatasetContents datasetName =
  ListDatasetContents'
    { datasetName,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      scheduledBefore = Core.Nothing,
      scheduledOnOrAfter = Core.Nothing
    }

-- | The name of the data set whose contents information you want to list.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcDatasetName :: Lens.Lens' ListDatasetContents Types.DatasetName
ldcDatasetName = Lens.field @"datasetName"
{-# DEPRECATED ldcDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | The maximum number of results to return in this request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcMaxResults :: Lens.Lens' ListDatasetContents (Core.Maybe Core.Natural)
ldcMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ldcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcNextToken :: Lens.Lens' ListDatasetContents (Core.Maybe Types.NextToken)
ldcNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A filter to limit results to those data set contents whose creation is scheduled before the given time. See the field @triggers.schedule@ in the @CreateDataset@ request. (timestamp)
--
-- /Note:/ Consider using 'scheduledBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcScheduledBefore :: Lens.Lens' ListDatasetContents (Core.Maybe Core.NominalDiffTime)
ldcScheduledBefore = Lens.field @"scheduledBefore"
{-# DEPRECATED ldcScheduledBefore "Use generic-lens or generic-optics with 'scheduledBefore' instead." #-}

-- | A filter to limit results to those data set contents whose creation is scheduled on or after the given time. See the field @triggers.schedule@ in the @CreateDataset@ request. (timestamp)
--
-- /Note:/ Consider using 'scheduledOnOrAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcScheduledOnOrAfter :: Lens.Lens' ListDatasetContents (Core.Maybe Core.NominalDiffTime)
ldcScheduledOnOrAfter = Lens.field @"scheduledOnOrAfter"
{-# DEPRECATED ldcScheduledOnOrAfter "Use generic-lens or generic-optics with 'scheduledOnOrAfter' instead." #-}

instance Core.AWSRequest ListDatasetContents where
  type Rs ListDatasetContents = ListDatasetContentsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/datasets/" Core.<> (Core.toText datasetName)
                Core.<> ("/contents")
            ),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "scheduledBefore" Core.<$> scheduledBefore)
            Core.<> ( Core.toQueryValue "scheduledOnOrAfter"
                        Core.<$> scheduledOnOrAfter
                    ),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatasetContentsResponse'
            Core.<$> (x Core..:? "datasetContentSummaries")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDatasetContents where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"datasetContentSummaries" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListDatasetContentsResponse' smart constructor.
data ListDatasetContentsResponse = ListDatasetContentsResponse'
  { -- | Summary information about data set contents that have been created.
    datasetContentSummaries :: Core.Maybe [Types.DatasetContentSummary],
    -- | The token to retrieve the next set of results, or @null@ if there are no more results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDatasetContentsResponse' value with any optional fields omitted.
mkListDatasetContentsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDatasetContentsResponse
mkListDatasetContentsResponse responseStatus =
  ListDatasetContentsResponse'
    { datasetContentSummaries =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Summary information about data set contents that have been created.
--
-- /Note:/ Consider using 'datasetContentSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsDatasetContentSummaries :: Lens.Lens' ListDatasetContentsResponse (Core.Maybe [Types.DatasetContentSummary])
ldcrrsDatasetContentSummaries = Lens.field @"datasetContentSummaries"
{-# DEPRECATED ldcrrsDatasetContentSummaries "Use generic-lens or generic-optics with 'datasetContentSummaries' instead." #-}

-- | The token to retrieve the next set of results, or @null@ if there are no more results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsNextToken :: Lens.Lens' ListDatasetContentsResponse (Core.Maybe Types.NextToken)
ldcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsResponseStatus :: Lens.Lens' ListDatasetContentsResponse Core.Int
ldcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
