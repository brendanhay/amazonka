{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Queries for available tag keys and tag values for a specified period. You can search the tag values for an arbitrary string.
module Network.AWS.CostExplorer.GetTags
  ( -- * Creating a request
    GetTags (..),
    mkGetTags,

    -- ** Request lenses
    gtTimePeriod,
    gtNextPageToken,
    gtSearchString,
    gtTagKey,

    -- * Destructuring the response
    GetTagsResponse (..),
    mkGetTagsResponse,

    -- ** Response lenses
    gtrrsTags,
    gtrrsReturnSize,
    gtrrsTotalSize,
    gtrrsNextPageToken,
    gtrrsResponseStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTags' smart constructor.
data GetTags = GetTags'
  { -- | The start and end dates for retrieving the dimension values. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
    timePeriod :: Types.DateInterval,
    -- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | The value that you want to search for.
    searchString :: Core.Maybe Types.SearchString,
    -- | The key of the tag that you want to return values for.
    tagKey :: Core.Maybe Types.TagKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTags' value with any optional fields omitted.
mkGetTags ::
  -- | 'timePeriod'
  Types.DateInterval ->
  GetTags
mkGetTags timePeriod =
  GetTags'
    { timePeriod,
      nextPageToken = Core.Nothing,
      searchString = Core.Nothing,
      tagKey = Core.Nothing
    }

-- | The start and end dates for retrieving the dimension values. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTimePeriod :: Lens.Lens' GetTags Types.DateInterval
gtTimePeriod = Lens.field @"timePeriod"
{-# DEPRECATED gtTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtNextPageToken :: Lens.Lens' GetTags (Core.Maybe Types.NextPageToken)
gtNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED gtNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The value that you want to search for.
--
-- /Note:/ Consider using 'searchString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtSearchString :: Lens.Lens' GetTags (Core.Maybe Types.SearchString)
gtSearchString = Lens.field @"searchString"
{-# DEPRECATED gtSearchString "Use generic-lens or generic-optics with 'searchString' instead." #-}

-- | The key of the tag that you want to return values for.
--
-- /Note:/ Consider using 'tagKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTagKey :: Lens.Lens' GetTags (Core.Maybe Types.TagKey)
gtTagKey = Lens.field @"tagKey"
{-# DEPRECATED gtTagKey "Use generic-lens or generic-optics with 'tagKey' instead." #-}

instance Core.FromJSON GetTags where
  toJSON GetTags {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TimePeriod" Core..= timePeriod),
            ("NextPageToken" Core..=) Core.<$> nextPageToken,
            ("SearchString" Core..=) Core.<$> searchString,
            ("TagKey" Core..=) Core.<$> tagKey
          ]
      )

instance Core.AWSRequest GetTags where
  type Rs GetTags = GetTagsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSInsightsIndexService.GetTags")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTagsResponse'
            Core.<$> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..: "ReturnSize")
            Core.<*> (x Core..: "TotalSize")
            Core.<*> (x Core..:? "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { -- | The tags that match your request.
    tags :: [Types.Entity],
    -- | The number of query results that AWS returns at a time.
    returnSize :: Core.Int,
    -- | The total number of query results.
    totalSize :: Core.Int,
    -- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTagsResponse' value with any optional fields omitted.
mkGetTagsResponse ::
  -- | 'returnSize'
  Core.Int ->
  -- | 'totalSize'
  Core.Int ->
  -- | 'responseStatus'
  Core.Int ->
  GetTagsResponse
mkGetTagsResponse returnSize totalSize responseStatus =
  GetTagsResponse'
    { tags = Core.mempty,
      returnSize,
      totalSize,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | The tags that match your request.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsTags :: Lens.Lens' GetTagsResponse [Types.Entity]
gtrrsTags = Lens.field @"tags"
{-# DEPRECATED gtrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The number of query results that AWS returns at a time.
--
-- /Note:/ Consider using 'returnSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsReturnSize :: Lens.Lens' GetTagsResponse Core.Int
gtrrsReturnSize = Lens.field @"returnSize"
{-# DEPRECATED gtrrsReturnSize "Use generic-lens or generic-optics with 'returnSize' instead." #-}

-- | The total number of query results.
--
-- /Note:/ Consider using 'totalSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsTotalSize :: Lens.Lens' GetTagsResponse Core.Int
gtrrsTotalSize = Lens.field @"totalSize"
{-# DEPRECATED gtrrsTotalSize "Use generic-lens or generic-optics with 'totalSize' instead." #-}

-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsNextPageToken :: Lens.Lens' GetTagsResponse (Core.Maybe Types.NextPageToken)
gtrrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED gtrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsResponseStatus :: Lens.Lens' GetTagsResponse Core.Int
gtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
