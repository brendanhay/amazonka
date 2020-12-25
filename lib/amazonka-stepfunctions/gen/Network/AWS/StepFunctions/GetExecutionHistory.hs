{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.GetExecutionHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the history of the specified execution as a list of events. By default, the results are returned in ascending order of the @timeStamp@ of the events. Use the @reverseOrder@ parameter to get the latest events first.
--
-- If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
-- This API action is not supported by @EXPRESS@ state machines.
--
-- This operation returns paginated results.
module Network.AWS.StepFunctions.GetExecutionHistory
  ( -- * Creating a request
    GetExecutionHistory (..),
    mkGetExecutionHistory,

    -- ** Request lenses
    gehExecutionArn,
    gehIncludeExecutionData,
    gehMaxResults,
    gehNextToken,
    gehReverseOrder,

    -- * Destructuring the response
    GetExecutionHistoryResponse (..),
    mkGetExecutionHistoryResponse,

    -- ** Response lenses
    gehrrsEvents,
    gehrrsNextToken,
    gehrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkGetExecutionHistory' smart constructor.
data GetExecutionHistory = GetExecutionHistory'
  { -- | The Amazon Resource Name (ARN) of the execution.
    executionArn :: Types.Arn,
    -- | You can select whether execution data (input or output of a history event) is returned. The default is @true@ .
    includeExecutionData :: Core.Maybe Core.Bool,
    -- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
    --
    -- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
    maxResults :: Core.Maybe Core.Natural,
    -- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Lists events in descending order of their @timeStamp@ .
    reverseOrder :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetExecutionHistory' value with any optional fields omitted.
mkGetExecutionHistory ::
  -- | 'executionArn'
  Types.Arn ->
  GetExecutionHistory
mkGetExecutionHistory executionArn =
  GetExecutionHistory'
    { executionArn,
      includeExecutionData = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      reverseOrder = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the execution.
--
-- /Note:/ Consider using 'executionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehExecutionArn :: Lens.Lens' GetExecutionHistory Types.Arn
gehExecutionArn = Lens.field @"executionArn"
{-# DEPRECATED gehExecutionArn "Use generic-lens or generic-optics with 'executionArn' instead." #-}

-- | You can select whether execution data (input or output of a history event) is returned. The default is @true@ .
--
-- /Note:/ Consider using 'includeExecutionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehIncludeExecutionData :: Lens.Lens' GetExecutionHistory (Core.Maybe Core.Bool)
gehIncludeExecutionData = Lens.field @"includeExecutionData"
{-# DEPRECATED gehIncludeExecutionData "Use generic-lens or generic-optics with 'includeExecutionData' instead." #-}

-- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehMaxResults :: Lens.Lens' GetExecutionHistory (Core.Maybe Core.Natural)
gehMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gehMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehNextToken :: Lens.Lens' GetExecutionHistory (Core.Maybe Types.NextToken)
gehNextToken = Lens.field @"nextToken"
{-# DEPRECATED gehNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Lists events in descending order of their @timeStamp@ .
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehReverseOrder :: Lens.Lens' GetExecutionHistory (Core.Maybe Core.Bool)
gehReverseOrder = Lens.field @"reverseOrder"
{-# DEPRECATED gehReverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead." #-}

instance Core.FromJSON GetExecutionHistory where
  toJSON GetExecutionHistory {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("executionArn" Core..= executionArn),
            ("includeExecutionData" Core..=) Core.<$> includeExecutionData,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("reverseOrder" Core..=) Core.<$> reverseOrder
          ]
      )

instance Core.AWSRequest GetExecutionHistory where
  type Rs GetExecutionHistory = GetExecutionHistoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSStepFunctions.GetExecutionHistory")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExecutionHistoryResponse'
            Core.<$> (x Core..:? "events" Core..!= Core.mempty)
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetExecutionHistory where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"events") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetExecutionHistoryResponse' smart constructor.
data GetExecutionHistoryResponse = GetExecutionHistoryResponse'
  { -- | The list of events that occurred in the execution.
    events :: [Types.HistoryEvent],
    -- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
    nextToken :: Core.Maybe Types.PageToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetExecutionHistoryResponse' value with any optional fields omitted.
mkGetExecutionHistoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetExecutionHistoryResponse
mkGetExecutionHistoryResponse responseStatus =
  GetExecutionHistoryResponse'
    { events = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of events that occurred in the execution.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehrrsEvents :: Lens.Lens' GetExecutionHistoryResponse [Types.HistoryEvent]
gehrrsEvents = Lens.field @"events"
{-# DEPRECATED gehrrsEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehrrsNextToken :: Lens.Lens' GetExecutionHistoryResponse (Core.Maybe Types.PageToken)
gehrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gehrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehrrsResponseStatus :: Lens.Lens' GetExecutionHistoryResponse Core.Int
gehrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gehrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
