{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetJourneyExecutionMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard execution metric that applies to a journey.
module Network.AWS.Pinpoint.GetJourneyExecutionMetrics
  ( -- * Creating a request
    GetJourneyExecutionMetrics (..),
    mkGetJourneyExecutionMetrics,

    -- ** Request lenses
    gjemApplicationId,
    gjemJourneyId,
    gjemNextToken,
    gjemPageSize,

    -- * Destructuring the response
    GetJourneyExecutionMetricsResponse (..),
    mkGetJourneyExecutionMetricsResponse,

    -- ** Response lenses
    gjemrrsJourneyExecutionMetricsResponse,
    gjemrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetJourneyExecutionMetrics' smart constructor.
data GetJourneyExecutionMetrics = GetJourneyExecutionMetrics'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The unique identifier for the journey.
    journeyId :: Core.Text,
    -- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetJourneyExecutionMetrics' value with any optional fields omitted.
mkGetJourneyExecutionMetrics ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'journeyId'
  Core.Text ->
  GetJourneyExecutionMetrics
mkGetJourneyExecutionMetrics applicationId journeyId =
  GetJourneyExecutionMetrics'
    { applicationId,
      journeyId,
      nextToken = Core.Nothing,
      pageSize = Core.Nothing
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjemApplicationId :: Lens.Lens' GetJourneyExecutionMetrics Core.Text
gjemApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gjemApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the journey.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjemJourneyId :: Lens.Lens' GetJourneyExecutionMetrics Core.Text
gjemJourneyId = Lens.field @"journeyId"
{-# DEPRECATED gjemJourneyId "Use generic-lens or generic-optics with 'journeyId' instead." #-}

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjemNextToken :: Lens.Lens' GetJourneyExecutionMetrics (Core.Maybe Core.Text)
gjemNextToken = Lens.field @"nextToken"
{-# DEPRECATED gjemNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjemPageSize :: Lens.Lens' GetJourneyExecutionMetrics (Core.Maybe Core.Text)
gjemPageSize = Lens.field @"pageSize"
{-# DEPRECATED gjemPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Core.AWSRequest GetJourneyExecutionMetrics where
  type
    Rs GetJourneyExecutionMetrics =
      GetJourneyExecutionMetricsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/journeys/")
                Core.<> (Core.toText journeyId)
                Core.<> ("/execution-metrics")
            ),
        Core._rqQuery =
          Core.toQueryValue "next-token" Core.<$> nextToken
            Core.<> (Core.toQueryValue "page-size" Core.<$> pageSize),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJourneyExecutionMetricsResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetJourneyExecutionMetricsResponse' smart constructor.
data GetJourneyExecutionMetricsResponse = GetJourneyExecutionMetricsResponse'
  { journeyExecutionMetricsResponse :: Types.JourneyExecutionMetricsResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetJourneyExecutionMetricsResponse' value with any optional fields omitted.
mkGetJourneyExecutionMetricsResponse ::
  -- | 'journeyExecutionMetricsResponse'
  Types.JourneyExecutionMetricsResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetJourneyExecutionMetricsResponse
mkGetJourneyExecutionMetricsResponse
  journeyExecutionMetricsResponse
  responseStatus =
    GetJourneyExecutionMetricsResponse'
      { journeyExecutionMetricsResponse,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeyExecutionMetricsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjemrrsJourneyExecutionMetricsResponse :: Lens.Lens' GetJourneyExecutionMetricsResponse Types.JourneyExecutionMetricsResponse
gjemrrsJourneyExecutionMetricsResponse = Lens.field @"journeyExecutionMetricsResponse"
{-# DEPRECATED gjemrrsJourneyExecutionMetricsResponse "Use generic-lens or generic-optics with 'journeyExecutionMetricsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjemrrsResponseStatus :: Lens.Lens' GetJourneyExecutionMetricsResponse Core.Int
gjemrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gjemrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
