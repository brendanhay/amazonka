{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ListPipelines
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListPipelines operation gets a list of the pipelines associated with the current AWS account.
--
-- This operation returns paginated results.
module Network.AWS.ElasticTranscoder.ListPipelines
  ( -- * Creating a request
    ListPipelines (..),
    mkListPipelines,

    -- ** Request lenses
    lpAscending,
    lpPageToken,

    -- * Destructuring the response
    ListPipelinesResponse (..),
    mkListPipelinesResponse,

    -- ** Response lenses
    lprrsNextPageToken,
    lprrsPipelines,
    lprrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @ListPipelineRequest@ structure.
--
-- /See:/ 'mkListPipelines' smart constructor.
data ListPipelines = ListPipelines'
  { -- | To list pipelines in chronological order by the date and time that they were created, enter @true@ . To list pipelines in reverse chronological order, enter @false@ .
    ascending :: Core.Maybe Types.Ascending,
    -- | When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
    pageToken :: Core.Maybe Types.PageToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPipelines' value with any optional fields omitted.
mkListPipelines ::
  ListPipelines
mkListPipelines =
  ListPipelines'
    { ascending = Core.Nothing,
      pageToken = Core.Nothing
    }

-- | To list pipelines in chronological order by the date and time that they were created, enter @true@ . To list pipelines in reverse chronological order, enter @false@ .
--
-- /Note:/ Consider using 'ascending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpAscending :: Lens.Lens' ListPipelines (Core.Maybe Types.Ascending)
lpAscending = Lens.field @"ascending"
{-# DEPRECATED lpAscending "Use generic-lens or generic-optics with 'ascending' instead." #-}

-- | When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPageToken :: Lens.Lens' ListPipelines (Core.Maybe Types.PageToken)
lpPageToken = Lens.field @"pageToken"
{-# DEPRECATED lpPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.AWSRequest ListPipelines where
  type Rs ListPipelines = ListPipelinesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2012-09-25/pipelines",
        Core._rqQuery =
          Core.toQueryValue "Ascending" Core.<$> ascending
            Core.<> (Core.toQueryValue "PageToken" Core.<$> pageToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelinesResponse'
            Core.<$> (x Core..:? "NextPageToken")
            Core.<*> (x Core..:? "Pipelines")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListPipelines where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"pipelines" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"pageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )

-- | A list of the pipelines associated with the current AWS account.
--
-- /See:/ 'mkListPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { -- | A value that you use to access the second and subsequent pages of results, if any. When the pipelines fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | An array of @Pipeline@ objects.
    pipelines :: Core.Maybe [Types.Pipeline],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPipelinesResponse' value with any optional fields omitted.
mkListPipelinesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPipelinesResponse
mkListPipelinesResponse responseStatus =
  ListPipelinesResponse'
    { nextPageToken = Core.Nothing,
      pipelines = Core.Nothing,
      responseStatus
    }

-- | A value that you use to access the second and subsequent pages of results, if any. When the pipelines fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsNextPageToken :: Lens.Lens' ListPipelinesResponse (Core.Maybe Types.NextPageToken)
lprrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED lprrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of @Pipeline@ objects.
--
-- /Note:/ Consider using 'pipelines' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsPipelines :: Lens.Lens' ListPipelinesResponse (Core.Maybe [Types.Pipeline])
lprrsPipelines = Lens.field @"pipelines"
{-# DEPRECATED lprrsPipelines "Use generic-lens or generic-optics with 'pipelines' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListPipelinesResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
