{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.ListProgressUpdateStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists progress update streams associated with the user account making this call.
--
-- This operation returns paginated results.
module Network.AWS.MigrationHub.ListProgressUpdateStreams
  ( -- * Creating a request
    ListProgressUpdateStreams (..),
    mkListProgressUpdateStreams,

    -- ** Request lenses
    lpusMaxResults,
    lpusNextToken,

    -- * Destructuring the response
    ListProgressUpdateStreamsResponse (..),
    mkListProgressUpdateStreamsResponse,

    -- ** Response lenses
    lpusrrsNextToken,
    lpusrrsProgressUpdateStreamSummaryList,
    lpusrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListProgressUpdateStreams' smart constructor.
data ListProgressUpdateStreams = ListProgressUpdateStreams'
  { -- | Filter to limit the maximum number of results to list per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProgressUpdateStreams' value with any optional fields omitted.
mkListProgressUpdateStreams ::
  ListProgressUpdateStreams
mkListProgressUpdateStreams =
  ListProgressUpdateStreams'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Filter to limit the maximum number of results to list per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpusMaxResults :: Lens.Lens' ListProgressUpdateStreams (Core.Maybe Core.Natural)
lpusMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lpusMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpusNextToken :: Lens.Lens' ListProgressUpdateStreams (Core.Maybe Types.NextToken)
lpusNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpusNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListProgressUpdateStreams where
  toJSON ListProgressUpdateStreams {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListProgressUpdateStreams where
  type
    Rs ListProgressUpdateStreams =
      ListProgressUpdateStreamsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSMigrationHub.ListProgressUpdateStreams")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProgressUpdateStreamsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "ProgressUpdateStreamSummaryList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListProgressUpdateStreams where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"progressUpdateStreamSummaryList" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListProgressUpdateStreamsResponse' smart constructor.
data ListProgressUpdateStreamsResponse = ListProgressUpdateStreamsResponse'
  { -- | If there are more streams created than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
    nextToken :: Core.Maybe Types.NextToken,
    -- | List of progress update streams up to the max number of results passed in the input.
    progressUpdateStreamSummaryList :: Core.Maybe [Types.ProgressUpdateStreamSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProgressUpdateStreamsResponse' value with any optional fields omitted.
mkListProgressUpdateStreamsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListProgressUpdateStreamsResponse
mkListProgressUpdateStreamsResponse responseStatus =
  ListProgressUpdateStreamsResponse'
    { nextToken = Core.Nothing,
      progressUpdateStreamSummaryList = Core.Nothing,
      responseStatus
    }

-- | If there are more streams created than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpusrrsNextToken :: Lens.Lens' ListProgressUpdateStreamsResponse (Core.Maybe Types.NextToken)
lpusrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpusrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of progress update streams up to the max number of results passed in the input.
--
-- /Note:/ Consider using 'progressUpdateStreamSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpusrrsProgressUpdateStreamSummaryList :: Lens.Lens' ListProgressUpdateStreamsResponse (Core.Maybe [Types.ProgressUpdateStreamSummary])
lpusrrsProgressUpdateStreamSummaryList = Lens.field @"progressUpdateStreamSummaryList"
{-# DEPRECATED lpusrrsProgressUpdateStreamSummaryList "Use generic-lens or generic-optics with 'progressUpdateStreamSummaryList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpusrrsResponseStatus :: Lens.Lens' ListProgressUpdateStreamsResponse Core.Int
lpusrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lpusrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
