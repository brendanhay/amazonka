{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.ListHarvestJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of HarvestJob records.
--
-- This operation returns paginated results.
module Network.AWS.MediaPackage.ListHarvestJobs
  ( -- * Creating a request
    ListHarvestJobs (..),
    mkListHarvestJobs,

    -- ** Request lenses
    lhjIncludeChannelId,
    lhjIncludeStatus,
    lhjMaxResults,
    lhjNextToken,

    -- * Destructuring the response
    ListHarvestJobsResponse (..),
    mkListHarvestJobsResponse,

    -- ** Response lenses
    lhjrrsHarvestJobs,
    lhjrrsNextToken,
    lhjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListHarvestJobs' smart constructor.
data ListHarvestJobs = ListHarvestJobs'
  { -- | When specified, the request will return only HarvestJobs associated with the given Channel ID.
    includeChannelId :: Core.Maybe Core.Text,
    -- | When specified, the request will return only HarvestJobs in the given status.
    includeStatus :: Core.Maybe Core.Text,
    -- | The upper bound on the number of records to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListHarvestJobs' value with any optional fields omitted.
mkListHarvestJobs ::
  ListHarvestJobs
mkListHarvestJobs =
  ListHarvestJobs'
    { includeChannelId = Core.Nothing,
      includeStatus = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | When specified, the request will return only HarvestJobs associated with the given Channel ID.
--
-- /Note:/ Consider using 'includeChannelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhjIncludeChannelId :: Lens.Lens' ListHarvestJobs (Core.Maybe Core.Text)
lhjIncludeChannelId = Lens.field @"includeChannelId"
{-# DEPRECATED lhjIncludeChannelId "Use generic-lens or generic-optics with 'includeChannelId' instead." #-}

-- | When specified, the request will return only HarvestJobs in the given status.
--
-- /Note:/ Consider using 'includeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhjIncludeStatus :: Lens.Lens' ListHarvestJobs (Core.Maybe Core.Text)
lhjIncludeStatus = Lens.field @"includeStatus"
{-# DEPRECATED lhjIncludeStatus "Use generic-lens or generic-optics with 'includeStatus' instead." #-}

-- | The upper bound on the number of records to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhjMaxResults :: Lens.Lens' ListHarvestJobs (Core.Maybe Core.Natural)
lhjMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lhjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A token used to resume pagination from the end of a previous request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhjNextToken :: Lens.Lens' ListHarvestJobs (Core.Maybe Core.Text)
lhjNextToken = Lens.field @"nextToken"
{-# DEPRECATED lhjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListHarvestJobs where
  type Rs ListHarvestJobs = ListHarvestJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/harvest_jobs",
        Core._rqQuery =
          Core.toQueryValue "includeChannelId" Core.<$> includeChannelId
            Core.<> (Core.toQueryValue "includeStatus" Core.<$> includeStatus)
            Core.<> (Core.toQueryValue "maxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHarvestJobsResponse'
            Core.<$> (x Core..:? "harvestJobs")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListHarvestJobs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"harvestJobs" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListHarvestJobsResponse' smart constructor.
data ListHarvestJobsResponse = ListHarvestJobsResponse'
  { -- | A list of HarvestJob records.
    harvestJobs :: Core.Maybe [Types.HarvestJob],
    -- | A token that can be used to resume pagination from the end of the collection.
    nextToken :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListHarvestJobsResponse' value with any optional fields omitted.
mkListHarvestJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListHarvestJobsResponse
mkListHarvestJobsResponse responseStatus =
  ListHarvestJobsResponse'
    { harvestJobs = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of HarvestJob records.
--
-- /Note:/ Consider using 'harvestJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhjrrsHarvestJobs :: Lens.Lens' ListHarvestJobsResponse (Core.Maybe [Types.HarvestJob])
lhjrrsHarvestJobs = Lens.field @"harvestJobs"
{-# DEPRECATED lhjrrsHarvestJobs "Use generic-lens or generic-optics with 'harvestJobs' instead." #-}

-- | A token that can be used to resume pagination from the end of the collection.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhjrrsNextToken :: Lens.Lens' ListHarvestJobsResponse (Core.Maybe Core.Text)
lhjrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lhjrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhjrrsResponseStatus :: Lens.Lens' ListHarvestJobsResponse Core.Int
lhjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lhjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
